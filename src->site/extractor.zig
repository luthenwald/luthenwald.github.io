const std    = @import("std");
const config = @import("config.zig");
const types  = @import("types.zig");
const utils  = @import("utils.zig");

const Allocator = std.mem.Allocator;
const line_trim_chars = " \t\r";

fn trimLine(line: []const u8) []const u8 { return std.mem.trim(u8, line, line_trim_chars); }

pub const ExtractedContent = struct {
    alloc:       Allocator,
    comments:    []types.CommentBlock,
    code_blocks: []types.CodeBlock,

    pub fn deinit(self: *ExtractedContent) void { for (self.comments) |*comment| { comment.deinit(self.alloc); } self.alloc.free(self.comments); self.alloc.free(self.code_blocks); } };

pub fn extractBlocks(
    alloc:    Allocator,
    filepath: []const u8,
    lang:     []const u8, ) !ExtractedContent {

    const lang_config = config.getLangConfig(lang) orelse { std.debug.print("error: {s}: unsupported lang '{s}'\n", .{ filepath, lang }); return error.UnsupportedLang; };

    const stat = try std.fs.cwd().statFile(filepath);
    const bufsize = utils.nextPowerOf2(stat.size);

    const file = try std.fs.cwd().openFile(filepath, .{ .mode = .read_only }); defer file.close();

    const content = try file.readToEndAlloc(alloc, bufsize); defer alloc.free(content);

    var lines: std.ArrayList([]const u8) = .{}; defer lines.deinit(alloc);

    var line_iter = std.mem.splitScalar(u8, content, '\n');
    while (line_iter.next()) |line| { try lines.append(alloc, line); }

    if (lines.items.len > 0 and lines.items[lines.items.len - 1].len == 0 and content.len > 0 and content[content.len - 1] == '\n') { _ = lines.pop(); }

    var comments: std.ArrayList(types.CommentBlock) = .{}; errdefer { for (comments.items) |*comment| { comment.deinit(alloc); } comments.deinit(alloc); }
    var code_blocks: std.ArrayList(types.CodeBlock) = .{}; errdefer code_blocks.deinit(alloc);

    var i: usize = 0;
    while (i < lines.items.len) {
        const line = trimLine(lines.items[i]);

        if (lang_config.single_line_comment) |sl_comment| {
            if (std.mem.startsWith(u8, line, sl_comment)) {
                const is_isolated_before = i == 0 or isEmptyLine(lines.items[i - 1]) or isCommentLineStart(lines.items[i - 1], lang_config);

                if (is_isolated_before) {
                    const start_line = i;
                    var end_line = i;

                    var block_content: std.ArrayList(u8) = .{}; defer block_content.deinit(alloc);

                    while (end_line < lines.items.len) {
                        const current = trimLine(lines.items[end_line]);
                        if (!std.mem.startsWith(u8, current, sl_comment)) break;

                        const raw_current = std.mem.trimRight(u8, lines.items[end_line], line_trim_chars);
                        const comment_pos = std.mem.indexOf(u8, raw_current, sl_comment).?;

                        var comment_text = raw_current[comment_pos + sl_comment.len ..];
                        if (comment_text.len > 0 and comment_text[0] == ' ') { comment_text = comment_text[1..]; }

                        try block_content.appendSlice(alloc, comment_text);

                        if (end_line + 1 < lines.items.len) { const next = trimLine(lines.items[end_line + 1]);
                                                              if (std.mem.startsWith(u8, next, sl_comment)) { try block_content.append(alloc, '\n'); } }

                        end_line += 1; }

                    const is_isolated_after = end_line >= lines.items.len or isEmptyLine(lines.items[end_line]);

                    if (is_isolated_after) {
                        try comments.append(alloc, .{ .content = try block_content.toOwnedSlice(alloc), .start_line = start_line + 1, .end_line = end_line, });
                        i = end_line; continue; } } } }

        i += 1; }

    try identifyCodeBlocks(alloc, &code_blocks, comments.items, lines.items.len);

    return .{ .comments = try comments.toOwnedSlice(alloc), .code_blocks = try code_blocks.toOwnedSlice(alloc), .alloc = alloc, }; }

fn isEmptyLine(line: []const u8) bool { return trimLine(line).len == 0; }

fn isCommentLineStart(line: []const u8, lang_config: config.LangConfig) bool {
   const trimmed = trimLine(line);
   if (lang_config.single_line_comment) |sl| { if (std.mem.startsWith(u8, trimmed, sl)) return true; }
   return false; }

fn identifyCodeBlocks(
   alloc:       Allocator,
   code_blocks: *std.ArrayList(types.CodeBlock),
   comments:    []types.CommentBlock,
   total_lines: usize, ) !void {
   if (comments.len == 0) { if (total_lines > 0) { try code_blocks.append(alloc, .{ .start_line = 1, .end_line = total_lines, }); } return; }
   if (comments[0].start_line > 1) { try code_blocks.append(alloc, .{ .start_line = 1, .end_line = comments[0].start_line - 1, }); }

   for (0..comments.len - 1) |i| {
       const gap_start = comments[i].end_line + 1;
       const gap_end = comments[i + 1].start_line - 1;
       if (gap_start <= gap_end) { try code_blocks.append(alloc, .{ .start_line = gap_start, .end_line = gap_end, }); } }

   const last_comment = comments[comments.len - 1];
   if (last_comment.end_line < total_lines) { try code_blocks.append(alloc, .{ .start_line = last_comment.end_line + 1, .end_line = total_lines, }); } }
