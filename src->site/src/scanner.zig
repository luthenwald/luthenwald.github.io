const std    = @import("std");
const config = @import("config.zig");
const types  = @import("types.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const SourceFile = types.SourceFile;

pub fn scanSourceFiles(alloc: Allocator, src_dir: []const u8) ![]SourceFile {
    var files: ArrayList(SourceFile) = .{}; errdefer { for (files.items) |*f| {f.deinit(alloc); } files.deinit(alloc); }

    var dir = std.fs.cwd().openDir(src_dir, .{.iterate = true }) catch |err| {
        std.debug.print("error: failed to open directory '{s}': {any}\n", .{ src_dir, err });
        return err; }; defer dir.close();

    try walkDir(alloc, &files, dir, src_dir);
    return files.toOwnedSlice(alloc); }

fn walkDir(alloc: Allocator, files: *ArrayList(SourceFile), dir: std.fs.Dir, base_path: []const u8) !void {
   var it = dir.iterate();

   while (try it.next()) |x| {
      const full_path = try std.fs.path.join(alloc, &.{ base_path, x.name }); defer alloc.free(full_path);

      switch (x.kind) {
         .directory => { var subdir = try dir.openDir(x.name, .{ .iterate = true }); defer subdir.close();
                         try walkDir(alloc, files, subdir, full_path); },

         .file      => { const ext = std.fs.path.extension(x.name);
                         if (config.getLangByExt(ext)) |lang| { const path_copy = try alloc.dupe(u8, full_path);
                                                                try files.append(alloc, .{ .path = path_copy, .lang = lang, }); } },
         else       => {}, } } }
