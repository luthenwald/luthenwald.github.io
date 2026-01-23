const std = @import("std");

pub const LangConfig = struct { name:                 []const u8,
                                single_line_comment: ?[]const u8,
                                multi_line_start:    ?[]const u8,
                                multi_line_end:      ?[]const u8, };

pub const langs = [_]LangConfig{
    .{ .name = "haskell", .single_line_comment = "--", .multi_line_start = "{-", .multi_line_end = "-}", },
    .{ .name = "fish",    .single_line_comment = "#",  .multi_line_start = "/*", .multi_line_end = "*/", }, };

pub const ext_map = std.StaticStringMap([]const u8).initComptime(.{
    .{ ".fish", "fish" },
    .{ ".hs", "haskell" }, });

pub fn getLangByExt(ext: []const u8) ?[]const u8 { return ext_map.get(ext); }
pub fn getLangConfig(lang: []const u8) ?LangConfig { for (langs) |config| { if (std.mem.eql(u8, config.name, lang)) { return config; } } return null; }
