const std = @import("std");
const builder = @import("builder.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init; defer _ = gpa.deinit();

    const alloc = gpa.allocator();

    const args = try std.process.argsAlloc(alloc); defer std.process.argsFree(alloc, args);

    if (args.len != 4) { try printUsage(); return error.InvalidArgs; }

    try builder.srcToSite(alloc, args[1], args[2], args[3]);
    std.debug.print("build completed\n", .{}); }

fn printUsage() !void { std.debug.print("usage: src->site <src_dir> <output_dir> <base_url>", .{}); }
