const std   = @import("std");
const types = @import("types.zig");

const Allocator = std.mem.Allocator;

pub const ParsedContent = struct {
    blocks:  []types.ParsedBlock,
    outline: []types.OutlineItem,
    alloc:   Allocator,

    pub fn deinit(self: *ParsedContent) void {
        for (self.blocks) |*block| { block.deinit(self.alloc); } self.alloc.free(self.blocks);
        for (self.outline) |*item| { self.alloc.free(item.text); self.alloc.free(item.id); } self.alloc.free(self.outline); } };

const Region = union(enum) {
    comment: types.CommentBlock,
    code:    types.CodeBlock,

    fn startLine(self: Region) usize { return switch (self) { .comment => |c| c.start_line, .code => |c| c.start_line, }; } };

fn compareRegions(context: void, a: Region, b: Region) bool { _ = context; return a.startLine() < b.startLine(); }

fn appendBlock(alloc: Allocator, blocks: *std.ArrayList(types.ParsedBlock), block: types.ParsedBlock) !void {
   try blocks.append(alloc, block); }

pub fn parseMarkup(
    alloc:       Allocator,
    comments:    []types.CommentBlock,
    code_blocks: []types.CodeBlock,
    filepath:    []const u8, ) !ParsedContent {

    var blocks: std.ArrayList(types.ParsedBlock) = .{}; errdefer { for (blocks.items) |*block| { block.deinit(alloc); } blocks.deinit(alloc); }
    var outline: std.ArrayList(types.OutlineItem) = .{}; errdefer { for (outline.items) |*item| {item.deinit(alloc); } outline.deinit(alloc); }
    var footnotes: std.ArrayList(types.ParsedBlock) = .{}; defer footnotes.deinit(alloc);

    var regions: std.ArrayList(Region) = .{}; defer regions.deinit(alloc);

    for (comments) |comment| { try regions.append(alloc, .{ .comment = comment }); }
    for (code_blocks) |code_block| { try regions.append(alloc, .{ .code = code_block }); }

    std.sort.block(Region, regions.items, {}, compareRegions);

    for (regions.items) |region| {
        switch (region) {
            .comment => |comment| {
                const lines = try splitLines(alloc, comment.content); defer { for (lines) |line| alloc.free(line); alloc.free(lines); }

                var i: usize = 0;
                while (i < lines.len) {
                    const line = std.mem.trim(u8, lines[i], " \t\r");
                    const current_line_num = comment.start_line + i;

                    if (line.len == 0) { i += 1; continue; }

                    if (isVerbatimBoundary(line)) {
                        var verbatim_content: std.ArrayList(u8) = .{}; defer verbatim_content.deinit(alloc);

                        i += 1;
                        while (i < lines.len) {
                            const verbatim_line = lines[i];
                            if (isVerbatimBoundary(verbatim_line)) { i += 1; break; }
                            try verbatim_content.appendSlice(alloc, verbatim_line);
                            if (i + 1 < lines.len) { try verbatim_content.append(alloc, '\n'); }
                            i += 1; }

                        try appendBlock(alloc, &blocks, .{ .verbatim = .{ .content = try verbatim_content.toOwnedSlice(alloc) }, });
                        continue; }

                    if (std.mem.startsWith(u8, line, "@insert")) {
                        try appendBlock(alloc, &blocks, try parseInsert(alloc, line, filepath));
                        i += 1; continue; }

                    if (std.mem.startsWith(u8, line, "@img")) {
                        try appendBlock(alloc, &blocks, try parseImage(alloc, line, filepath));
                        i += 1; continue; }

                    if (std.mem.startsWith(u8, line, "^") and isValidFootnote(line)) {
                        const result = try consumeBlockLines(alloc, lines, i); defer alloc.free(result.content);

                        const footnote = try parseFootnote(alloc, result.content, filepath, current_line_num);
                        try footnotes.append(alloc, footnote);
                        i = result.next_index; continue; }

                    if (std.mem.startsWith(u8, line, "|")) {
                        const heading = try parseHeading(alloc, line);
                        const heading_text = try getPlainText(alloc, heading.heading.content); defer alloc.free(heading_text);

                        try outline.append(alloc, .{ .level = heading.heading.level, .text = try alloc.dupe(u8, heading_text), .id = try alloc.dupe(u8, heading.heading.id), });
                        try appendBlock(alloc, &blocks, heading);
                        i += 1; continue; }

                    if (std.mem.startsWith(u8, line, "-")) {
                        const result = try consumeBlockLines(alloc, lines, i); defer alloc.free(result.content);
                        try appendBlock(alloc, &blocks, try parseList(alloc, result.content));
                        i = result.next_index; continue; }

                    if (std.mem.startsWith(u8, line, ">")) {
                        const result = try consumeBlockLines(alloc, lines, i); defer alloc.free(result.content);
                        try appendBlock(alloc, &blocks, try parseCallout(alloc, result.content));
                        i = result.next_index; continue; }

                    var para_lines: std.ArrayList([]const u8) = .{}; defer para_lines.deinit(alloc);
                    try para_lines.append(alloc, line);

                    i += 1;
                    while (i < lines.len) {
                        const next_line = std.mem.trim(u8, lines[i], " \t\r");
                        if (next_line.len == 0) break;
                        if (std.mem.startsWith(u8, next_line, "^") and isValidFootnote(next_line)) break;
                        if (std.mem.startsWith(u8, next_line, "|")) break;
                        if (std.mem.startsWith(u8, next_line, "-")) break;
                        if (std.mem.startsWith(u8, next_line, ">")) break;
                        if (std.mem.startsWith(u8, next_line, "@insert")) break;
                        if (std.mem.startsWith(u8, next_line, "@img")) break;

                        try para_lines.append(alloc, next_line); i += 1; }

                    const combined = try std.mem.join(alloc, " ", para_lines.items); defer alloc.free(combined);
                    try appendBlock(alloc, &blocks, try parseParagraph(alloc, combined)); } },

            .code => |code_block| {
                var code_lines: std.ArrayList(u8) = .{}; defer code_lines.deinit(alloc);
                var has_content = false;

                for (code_block.start_line..code_block.end_line + 1) |line_num| {
                    const placeholder = try std.fmt.allocPrint(alloc, "{{{{{d}}}}}", .{line_num}); defer alloc.free(placeholder);
                    try code_lines.appendSlice(alloc, placeholder);
                    if (line_num < code_block.end_line) { try code_lines.append(alloc, '\n'); }
                    has_content = true; }

                if (has_content) {
                    try appendBlock(alloc, &blocks, .{ .code = .{ .lines = try code_lines.toOwnedSlice(alloc) }, }); } }, } }

    for (footnotes.items) |footnote| { try blocks.append(alloc, footnote); }

    return .{ .blocks = try blocks.toOwnedSlice(alloc), .outline = try outline.toOwnedSlice(alloc), .alloc = alloc, }; }

fn consumeBlockLines(
    alloc: Allocator,
    lines: [][]const u8,
    start_index: usize, ) !struct { content: []u8, next_index: usize } {

    var content_lines: std.ArrayList([]const u8) = .{}; defer content_lines.deinit(alloc);

    try content_lines.append(alloc, std.mem.trim(u8, lines[start_index], " \t\r"));

    var i = start_index + 1;
    while (i < lines.len) {
        const raw_line = lines[i];
        const trimmed = std.mem.trim(u8, raw_line, " \t\r");

        if (trimmed.len == 0) {
            var j = i + 1;
            var found_continuation = false;
            while (j < lines.len) {
                const peek_line = lines[j];
                const peek_trimmed = std.mem.trim(u8, peek_line, " \t\r");
                if (peek_trimmed.len > 0) {
                    if (isIndented(peek_line) and !isBlockStart(peek_trimmed)) {
                        found_continuation = true;
                        i = j; break; }
                    else { return .{ .content = try std.mem.join(alloc, " ", content_lines.items), .next_index = i, }; } }
                j += 1; }
            if (!found_continuation) { return .{ .content = try std.mem.join(alloc, " ", content_lines.items), .next_index = i, }; } }

        const current_raw = lines[i];
        const current_trimmed = std.mem.trim(u8, current_raw, " \t\r");

        if   (isIndented(current_raw)) {
             if (isBlockStart(current_trimmed)) { return .{ .content = try std.mem.join(alloc, " ", content_lines.items), .next_index = i, }; }
             try content_lines.append(alloc, current_trimmed);
             i += 1; }
        else { return .{ .content = try std.mem.join(alloc, " ", content_lines.items), .next_index = i, }; } }

    return .{ .content = try std.mem.join(alloc, " ", content_lines.items), .next_index = i, }; }

fn isIndented(line: []const u8) bool { if (line.len == 0) return false; return line[0] == ' ' or line[0] == '\t'; }

fn isBlockStart(line: []const u8) bool {
   return std.mem.startsWith(u8, line, "-") or
          std.mem.startsWith(u8, line, ">") or
          std.mem.startsWith(u8, line, "|") or
          std.mem.startsWith(u8, line, "^") or
          std.mem.startsWith(u8, line, "@insert") or
          std.mem.startsWith(u8, line, "@img"); }

fn parsePrefixLevel(line: []const u8, ch: u8) struct { level: u8, rest: []const u8 } {
   var level: u8 = 0;
   var i: usize = 0;
   while (i < line.len and line[i] == ch) : (i += 1) { level += 1; }
   return .{ .level = level, .rest = std.mem.trim(u8, line[i..], " \t") }; }

fn splitLines(alloc: Allocator, content: []const u8) ![][]const u8 {
   var lines: std.ArrayList([]const u8) = .{}; errdefer { for (lines.items) |line| alloc.free(line); lines.deinit(alloc); }

   var iter = std.mem.splitScalar(u8, content, '\n');
   while (iter.next()) |line| { try lines.append(alloc, try alloc.dupe(u8, line)); }

   return lines.toOwnedSlice(alloc); }

fn parseHeading(alloc: Allocator, line: []const u8) !types.ParsedBlock {
   const pre = parsePrefixLevel(line, '|');
   const content = try parseInlineElements(alloc, pre.rest);
   const id_text = try getPlainText(alloc, content); defer alloc.free(id_text);
   const id = try generateId(alloc, id_text);
   return .{ .heading = .{ .level = pre.level, .content = content, .id = id, }, }; }

fn parseParagraph(alloc: Allocator, line: []const u8) !types.ParsedBlock {
   const content = try parseInlineElements(alloc, line);
   return .{ .paragraph = .{ .content = content }, }; }

fn isValidFootnote(line: []const u8) bool {
   if (line.len < 3) return false;
   if (line[0] != '^') return false;

   var i: usize = 1;
   var has_digit = false;

   while (i < line.len) : (i += 1) {
       const c = line[i];
       if      (c >= '0' and c <= '9') { has_digit = true; }
       else if (c == '.') { return has_digit and i + 1 < line.len; }
       else if (c == ' ' or c == '\t') { continue; }
       else    { return false; } } return false; }

fn parseFootnote(alloc: Allocator, line: []const u8, filepath: []const u8, line_num: usize) !types.ParsedBlock {
   const dot_pos = std.mem.indexOf(u8, line, ".") orelse {
       std.debug.print("error: {s}:{d}: invalid footnote format, expected '^N. text': {s}\n", .{ filepath, line_num, line });
       return error.InvalidFootnote; };
   const num_str = std.mem.trim(u8, line[1..dot_pos], " \t");
   const number = std.fmt.parseInt(usize, num_str, 10) catch |err| {
       std.debug.print("error: {s}:{d}: invalid footnote number '{s}': {any}\n", .{ filepath, line_num, num_str, err });
       return err; };

   const text = std.mem.trim(u8, line[dot_pos + 1 ..], " \t");
   const content = try parseInlineElements(alloc, text);

   return .{ .footnote = .{ .number = number, .content = content, }, }; }

fn parseList(alloc: Allocator, line: []const u8) !types.ParsedBlock {
   const pre = parsePrefixLevel(line, '-');
   const content = try parseInlineElements(alloc, pre.rest);
   return .{ .list = .{ .level = pre.level, .content = content, }, }; }

fn parseCallout(alloc: Allocator, line: []const u8) !types.ParsedBlock {
   const text = std.mem.trim(u8, line[1..], " \t");
   const content = try parseInlineElements(alloc, text);

   return .{ .callout = .{ .content = content, }, }; }

fn parseInsert(alloc: Allocator, line: []const u8, filepath: []const u8) !types.ParsedBlock {
   const text = std.mem.trim(u8, line[7..], " \t");
   if (text.len == 0) { return error.InvalidMetadata; }

   const dir = std.fs.path.dirname(filepath) orelse ".";
   const full_path = try std.fs.path.join(alloc, &.{ dir, text });

   return .{ .insert = .{ .path = full_path, }, }; }

fn parseImage(alloc: Allocator, line: []const u8, filepath: []const u8) !types.ParsedBlock {
   const text = std.mem.trim(u8, line[4..], " \t");
   if (text.len == 0) { return error.InvalidMetadata; }

   const dir = std.fs.path.dirname(filepath) orelse ".";
   const full_path = try std.fs.path.join(alloc, &.{ dir, text });

   return .{ .image = .{ .path = full_path, }, }; }

fn flushText(alloc: Allocator, elements: *std.ArrayList(types.InlineElement), current: *std.ArrayList(u8)) !void {
   if (current.items.len > 0) {
      try elements.append(alloc, .{ .text = try current.toOwnedSlice(alloc) });
      current.clearRetainingCapacity(); } }

fn parseInlineElements(alloc: Allocator, text: []const u8) ![]types.InlineElement {
   var elements: std.ArrayList(types.InlineElement) = .{}; errdefer { for (elements.items) |*elem| { elem.deinit(alloc); } elements.deinit(alloc); }
   var i: usize = 0;
   var current: std.ArrayList(u8) = .{}; defer current.deinit(alloc);

   while (i < text.len) {
      if (text[i] == '[') {
         const close_pos = std.mem.indexOf(u8, text[i..], "]") orelse {
            try current.append(alloc, text[i]);
            i += 1; continue; };

         if (i + close_pos + 1 < text.len and text[i + close_pos + 1] == '(') {
            const url_start = i + close_pos + 2;
            var paren_depth: i32 = 1;
            var paren_close: ?usize = null;
            var j = url_start;
            while (j < text.len) : (j += 1) {
               if      (text[j] == '(') { paren_depth += 1; }
               else if (text[j] == ')') { paren_depth -= 1; if (paren_depth == 0) { paren_close = j - url_start; break; } } }
            if (paren_close == null) { try current.append(alloc, text[i]); i += 1; continue; }

            try flushText(alloc, &elements, &current);
            const link_text = text[i + 1 .. i + close_pos];
            const url = text[url_start .. url_start + paren_close.?];
            try elements.append(alloc, .{ .link = .{ .text = try alloc.dupe(u8, link_text), .url = try alloc.dupe(u8, url), }, });
            i = url_start + paren_close.? + 1; continue; } }

      if (text[i] == '|') {
         const close_pos = std.mem.indexOfPos(u8, text, i + 1, "|") orelse { try current.append(alloc, text[i]); i += 1; continue; };
         try flushText(alloc, &elements, &current);
         try elements.append(alloc, .{ .highlight = try alloc.dupe(u8, text[i + 1 .. close_pos]) });
         i = close_pos + 1; continue; }

      if (text[i] == '`') {
         const close_pos = std.mem.indexOfPos(u8, text, i + 1, "`") orelse { try current.append(alloc, text[i]); i += 1; continue; };
         try flushText(alloc, &elements, &current);
         try elements.append(alloc, .{ .inlinecode = try alloc.dupe(u8, text[i + 1 .. close_pos]) });
         i = close_pos + 1; continue; }

      if (text[i] == '*') {
         const close_pos = std.mem.indexOfPos(u8, text, i + 1, "*") orelse { try current.append(alloc, text[i]); i += 1; continue; };
         try flushText(alloc, &elements, &current);
         try elements.append(alloc, .{ .italic = try alloc.dupe(u8, text[i + 1 .. close_pos]) });
         i = close_pos + 1; continue; }

      if (i + 2 < text.len and text[i] == '[' and text[i + 1] == '^') {
         const close_pos = std.mem.indexOfPos(u8, text, i + 2, "]") orelse { try current.append(alloc, text[i]); i += 1; continue; };
         const num_str = text[i + 2 .. close_pos];
         const number = std.fmt.parseInt(usize, num_str, 10) catch { try current.append(alloc, text[i]); i += 1; continue; };
         try flushText(alloc, &elements, &current);
         try elements.append(alloc, .{ .footnote_ref = number });
         i = close_pos + 1; continue; }

      try current.append(alloc, text[i]); i += 1; }

   try flushText(alloc, &elements, &current);
   return elements.toOwnedSlice(alloc); }

fn getPlainText(alloc: Allocator, elements: []types.InlineElement) ![]const u8 {
   var result: std.ArrayList(u8) = .{}; defer result.deinit(alloc);

   for (elements) |elem| {
       switch (elem) {
          .text         => |t| try result.appendSlice(alloc, t),
          .link         => |l| try result.appendSlice(alloc, l.text),
          .highlight    => |h| try result.appendSlice(alloc, h),
          .inlinecode   => |c| try result.appendSlice(alloc, c),
          .italic       => |i| try result.appendSlice(alloc, i),
          .footnote_ref => {}, } }

    return result.toOwnedSlice(alloc); }

fn generateId(alloc: Allocator, text: []const u8) ![]const u8 {
   var result: std.ArrayList(u8) = .{}; defer result.deinit(alloc);

   for (text) |c| {
       if       (c == ' ' or c == '\t') { try result.append(alloc, '-'); }
       else if ((c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '-' or c == '_') { try result.append(alloc, std.ascii.toLower(c)); } }

   return result.toOwnedSlice(alloc); }

fn isVerbatimBoundary(line: []const u8) bool {
   var backtick_count: usize = 0;
   for (line) |c| { if (c == '`') { backtick_count += 1; if (backtick_count >= 6) return true; } else { backtick_count = 0; } }
   return false; }
