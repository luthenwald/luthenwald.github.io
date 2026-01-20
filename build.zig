const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const mod = b.addModule("src->site", .{ .root_source_file = b.path("src->site/main.zig"), .single_threaded = false, .target = target, .optimize = optimize, });
    const exe = b.addExecutable(.{ .name = "src->site", .root_module = mod, });

    b.installArtifact(exe); }
