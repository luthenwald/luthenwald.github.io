const std   = @import("std");
const types = @import("types.zig");

const Allocator = std.mem.Allocator;

pub fn highlightCode(
    alloc:    Allocator,
    filepath: []const u8,
    content:  []types.ParsedBlock, ) ![]types.ParsedBlock {

    var line_map = try invokeTreeSitter(alloc, filepath); defer { var it = line_map.iterator(); while (it.next()) |entry| { alloc.free(entry.value_ptr.*); } line_map.deinit(); }

    var result: std.ArrayList(types.ParsedBlock) = .{}; errdefer { for (result.items) |*block| { block.deinit(alloc); } result.deinit(alloc); }

    for (content) |block| {
        switch (block) {
            .code => |code| {
                var lines = std.mem.splitScalar(u8, code.lines, '\n');
                var result_lines: std.ArrayList(u8) = .{}; defer result_lines.deinit(alloc);
                var has_content = false;

                while (lines.next()) |line| {
                    if (line.len == 0) { try result_lines.append(alloc, '\n'); continue; }

                    const line_num = extractLineNumber(line) catch {
                        const trimmed_line = std.mem.trim(u8, line, " \t");
                        if (trimmed_line.len > 0) { has_content = true; }
                        try result_lines.appendSlice(alloc, line);
                        try result_lines.append(alloc, '\n'); continue; };

                    if (line_map.get(line_num)) |html| {
                        const trimmed_html = std.mem.trim(u8, html, " \t");
                        if (trimmed_html.len > 0) { has_content = true; }
                        try result_lines.appendSlice(alloc, html);
                        try result_lines.append(alloc, '\n'); }
                    else { try result_lines.append(alloc, '\n'); } }

                if (has_content) {
                    const html_content = try result_lines.toOwnedSlice(alloc);
                    try result.append(alloc, .{ .code = .{ .lines = html_content }, }); } },

            .heading => |h| {
                var content_copy = try alloc.alloc(types.InlineElement, h.content.len);
                for (h.content, 0..) |elem, i| { content_copy[i] = try copyInlineElement(alloc, elem); }
                try result.append(alloc, .{ .heading = .{ .level = h.level, .content = content_copy, .id = try alloc.dupe(u8, h.id), }, }); },

            .paragraph => |p| {
                var content_copy = try alloc.alloc(types.InlineElement, p.content.len);
                for (p.content, 0..) |elem, i| { content_copy[i] = try copyInlineElement(alloc, elem); }
                try result.append(alloc, .{ .paragraph = .{ .content = content_copy }, }); },

            .footnote => |f| {
                var content_copy = try alloc.alloc(types.InlineElement, f.content.len);
                for (f.content, 0..) |elem, i| { content_copy[i] = try copyInlineElement(alloc, elem); }
                try result.append(alloc, .{ .footnote = .{ .number = f.number, .content = content_copy, }, }); },

            .list => |l| {
                var content_copy = try alloc.alloc(types.InlineElement, l.content.len);
                for (l.content, 0..) |elem, i| { content_copy[i] = try copyInlineElement(alloc, elem); }
                try result.append(alloc, .{ .list = .{ .level = l.level, .content = content_copy, }, }); },

            .verbatim => |v| { try result.append(alloc, .{ .verbatim = .{ .content = try alloc.dupe(u8, v.content) }, }); },

            .callout => |c| {
                var content_copy = try alloc.alloc(types.InlineElement, c.content.len);
                for (c.content, 0..) |elem, i| { content_copy[i] = try copyInlineElement(alloc, elem); }
                try result.append(alloc, .{ .callout = .{ .content = content_copy }, }); },

            .insert => |i| { try result.append(alloc, .{ .insert = .{ .path = try alloc.dupe(u8, i.path) }, }); }, } }

    return result.toOwnedSlice(alloc); }

fn copyInlineElement(alloc: Allocator, elem: types.InlineElement) !types.InlineElement {
   return switch (elem) {
      .text         => |t| .{ .text = try alloc.dupe(u8, t) },
      .link         => |l| .{ .link = .{ .text = try alloc.dupe(u8, l.text), .url = try alloc.dupe(u8, l.url), }, },
      .highlight    => |h| .{ .highlight = try alloc.dupe(u8, h) },
      .inlinecode   => |c| .{ .inlinecode = try alloc.dupe(u8, c) },
      .italic       => |i| .{ .italic = try alloc.dupe(u8, i) },
      .footnote_ref => |num| .{ .footnote_ref = num }, }; }

fn invokeTreeSitter(alloc: Allocator, filepath: []const u8) !std.AutoHashMap(usize, []const u8) {
   const argv = [_][]const u8{ "tree-sitter", "highlight", "-H", "--css-classes", filepath };

   const result = std.process.Child.run(.{
      .allocator = alloc,
      .argv = &argv,
      .max_output_bytes = 1024 * 1024, })
      catch |err| { std.debug.print("error: failed to execute tree-sitter: {}\n", .{err}); std.debug.print("ensure tree-sitter is installed and in PATH\n", .{}); return error.TreeSitterFailed; };

   defer alloc.free(result.stdout); defer alloc.free(result.stderr);

   if (result.term != .Exited or result.term.Exited != 0) {
      std.debug.print("error: tree-sitter failed for {s}\n", .{filepath});
      std.debug.print("stderr: {s}\n", .{result.stderr});
      return error.TreeSitterFailed; }

   return parseTreeSitterOutput(alloc, result.stdout); }

fn parseTreeSitterOutput(alloc: Allocator, html: []const u8) !std.AutoHashMap(usize, []const u8) {
   var line_map = std.AutoHashMap(usize, []const u8).init(alloc); errdefer { var iter = line_map.iterator(); while (iter.next()) |entry| { alloc.free(entry.value_ptr.*); } line_map.deinit(); }

   const tr_start = "<tr><td class=line-number>";
   const tr_mid = "</td><td class=line>";
   const tr_end = "</td></tr>";

   var pos: usize = 0;
   while (pos < html.len) {
      const row_start = std.mem.indexOfPos(u8, html, pos, tr_start) orelse break;
      const row_pos = row_start + tr_start.len;

      const mid_start = std.mem.indexOfPos(u8, html, row_pos, tr_mid) orelse { pos = row_start + 1; continue; };

      const line_num_str = html[row_pos..mid_start];
      const line_num = std.fmt.parseInt(usize, line_num_str, 10) catch { pos = row_start + 1; continue; };

      const content_start = mid_start + tr_mid.len;
      const content_end = std.mem.indexOfPos(u8, html, content_start, tr_end) orelse { pos = row_start + 1; continue; };

      var content = html[content_start..content_end];
      if (std.mem.endsWith(u8, content, "\n")) { content = content[0 .. content.len - 1]; }

      const content_copy = try alloc.dupe(u8, content);
      try line_map.put(line_num, content_copy);

      pos = content_end + tr_end.len; }

   return line_map; }

fn extractLineNumber(placeholder: []const u8) !usize {
   if (!std.mem.startsWith(u8, placeholder, "{{") or !std.mem.endsWith(u8, placeholder, "}}")) { return error.InvalidPlaceholder; }

   const num_str = placeholder[2 .. placeholder.len - 2];
   return try std.fmt.parseInt(usize, num_str, 10); }
