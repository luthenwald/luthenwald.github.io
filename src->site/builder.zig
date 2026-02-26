const std = @import("std");
const types = @import("types.zig");
const scanner = @import("scanner.zig");
const extractor = @import("extractor.zig");
const metadata = @import("metadata.zig");
const markup = @import("markup.zig");
const highlighter = @import("highlighter.zig");
const html = @import("html.zig");
const feed = @import("feed.zig");

const Allocator = std.mem.Allocator;

const reset_css = @embedFile("assets/reset.css");
const prima_css = @embedFile("assets/prima.css");

pub fn srcToSite(
    alloc:      Allocator,
    src_dir:    []const u8,
    output_dir: []const u8,
    base_url:   []const u8, ) !void {
    std.debug.print("scanning src\n",  .{});

    const source_files = try scanner.scanSourceFiles(alloc, src_dir); defer { for (source_files) |*f| { f.deinit(alloc); } alloc.free(source_files); }

    if (source_files.len == 0) { std.debug.print("no source files in {s}\n", .{src_dir}); return error.NoSourceFIle; } else { std.debug.print("found {d} source files\n", .{source_files.len}); }

    var blogs: std.ArrayList(types.Blog) = .{}; defer { for (blogs.items) |*blog| { blog.deinit(); } blogs.deinit(alloc); }

    std.debug.print("processing source files\n", .{});

    for (source_files) |file| {
        std.debug.print("   {s}\n", .{file.path});

        var extracted = extractor.extractBlocks(alloc, file.path, file.lang) catch |err| { std.debug.print("error: {s}: failed to extract comments: {any}\n", .{ file.path, err }); return err; }; defer extracted.deinit();

        if (extracted.comments.len == 0) { std.debug.print("warning: {s}: no comment blocks found, skipping\n", .{file.path}); continue; }

        const meta = metadata.parseMetadata(
              alloc,
              extracted.comments[0].content,
              file.path,
              extracted.comments[0].start_line, ) catch |err| { std.debug.print("error: {s}: failed to parse metadata: {any}\n", .{ file.path, err }); return err; }; errdefer { var meta_copy = meta; meta_copy.deinit(alloc); }

        const tags   = metadata.parseTags(alloc, meta.tags.?) catch |err| { std.debug.print("error: {s}: failed to parse tags: {any}\n", .{ file.path, err }); return err; }; errdefer { for (tags) |tag| alloc.free(tag); alloc.free(tags); }
        const pageid = metadata.filepathToPageId(alloc, file.path) catch |err| { std.debug.print("error: {s}: failed to generate page id: {any}\n", .{ file.path, err }); return err; }; errdefer alloc.free(pageid);

        const content_comments = if (extracted.comments.len > 1) extracted.comments[1..] else extracted.comments[0..0];

        const parsed = markup.parseMarkup(alloc, content_comments, extracted.code_blocks, file.path) catch |err| { std.debug.print("error: {s}: failed to parse markup: {any}\n", .{ file.path, err }); return err; };
        errdefer { for (parsed.outline) |*item| { item.deinit(alloc); } alloc.free(parsed.outline); for (parsed.blocks) |*block| { block.deinit(alloc); } alloc.free(parsed.blocks); }

        const highlighted = highlighter.highlightCode(alloc, file.path, parsed.blocks) catch |err| { std.debug.print("error: {s}: failed to highlight code: {any}\n", .{ file.path, err }); return err; };

        for (parsed.blocks) |*block| { block.deinit(alloc); } alloc.free(parsed.blocks);

        try blogs.append(alloc, .{
            .title       = try alloc.dupe(u8, meta.title.?),
            .pageid      = pageid,
            .pub_date    = try alloc.dupe(u8, meta.pub_date.?),
            .tags        = tags,
            .description = try alloc.dupe(u8, meta.description.?),
            .content     = highlighted,
            .filepath    = try alloc.dupe(u8, file.path),
            .outline     = parsed.outline,
            .alloc       = alloc, });

        var meta_copy = meta; meta_copy.deinit(alloc); }


    std.debug.print("sorting blogs by date...\n", .{});
    std.sort.block(types.Blog, blogs.items, {}, compareBlogsByDate);

    std.debug.print("creating output directories...\n", .{});
    try createOutputStructure(output_dir);

    std.debug.print("generating tag index...\n", .{});
    var tags_map = std.StringHashMap(types.Tag).init(alloc); defer { var iter = tags_map.valueIterator(); while (iter.next()) |tag| { tag.deinit(alloc); } tags_map.deinit(); }

    for (blogs.items) |*blog| {
        for (blog.tags) |tag_name| {
            const result = try tags_map.getOrPut(tag_name);
            if (!result.found_existing) { result.value_ptr.* = types.Tag.init(alloc, try alloc.dupe(u8, tag_name)); }
            try result.value_ptr.blogs.append(alloc, blog); } }

    std.debug.print("generating HTML pages...\n", .{});

    for (blogs.items) |*blog| {
        const blog_html = try html.generateBlogPage(alloc, blog, base_url); defer alloc.free(blog_html);
        const blog_filename = try std.fmt.allocPrint(alloc, "{s}.html", .{blog.pageid}); defer alloc.free(blog_filename);
        const blog_path = try std.fs.path.join(alloc, &.{ output_dir, "blogs", blog_filename }); defer alloc.free(blog_path);

        try writeFile(blog_path, blog_html); }

    const homepage_html = try html.generateHomepage(alloc, blogs.items); defer alloc.free(homepage_html);

    const homepage_path = try std.fs.path.join(alloc, &.{ output_dir, "index.html" }); defer alloc.free(homepage_path);
    try writeFile(homepage_path, homepage_html);

    var tags_list: std.ArrayList(types.Tag) = .{}; defer tags_list.deinit(alloc);

    var tags_iter = tags_map.valueIterator(); while (tags_iter.next()) |tag| { try tags_list.append(alloc, tag.*); }

    std.sort.block(types.Tag, tags_list.items, {}, compareTagsByName);

    const tagcloud_html = try html.generateTagCloud(alloc, tags_list.items); defer alloc.free(tagcloud_html);
    const tagcloud_path = try std.fs.path.join(alloc, &.{ output_dir, "tags", "tagcloud.html" }); defer alloc.free(tagcloud_path);
    try writeFile(tagcloud_path, tagcloud_html);

    for (tags_list.items) |*tag| {
        const tag_html = try html.generateTagPage(alloc, tag); defer alloc.free(tag_html);
        const tag_filename = try std.fmt.allocPrint(alloc, "{s}.html", .{tag.name}); defer alloc.free(tag_filename);
        const tag_path = try std.fs.path.join(alloc, &.{ output_dir, "tags", tag_filename }); defer alloc.free(tag_path);

        try writeFile(tag_path, tag_html); }

    std.debug.print("generating RSS feed...\n", .{});

    const feed_xml = try feed.generateFeed(alloc, blogs.items, base_url); defer alloc.free(feed_xml);
    const feed_path = try std.fs.path.join(alloc, &.{ output_dir, "feed.xml" }); defer alloc.free(feed_path);
    try writeFile(feed_path, feed_xml);

    std.debug.print("copying CSS...\n", .{});

    const reset_css_path = try std.fs.path.join(alloc, &.{ output_dir, "styles", "reset.css" }); defer alloc.free(reset_css_path);
    try writeFile(reset_css_path, reset_css);

    const prima_css_path = try std.fs.path.join(alloc, &.{ output_dir, "styles", "prima.css" }); defer alloc.free(prima_css_path);
    try writeFile(prima_css_path, prima_css);

    std.debug.print("build complete!\n", .{}); }

fn compareBlogsByDate(context: void, a: types.Blog, b: types.Blog) bool { _ = context; return std.mem.order(u8, b.pub_date, a.pub_date) == .lt; }

fn createOutputStructure(output_dir: []const u8) !void {
    std.fs.cwd().makeDir(output_dir) catch |err| { if (err != error.PathAlreadyExists) return err; };

    const blogs_dir = try std.fs.path.join(std.heap.page_allocator, &.{ output_dir, "blogs" }); defer std.heap.page_allocator.free(blogs_dir);
    std.fs.cwd().makeDir(blogs_dir) catch |err| { if (err != error.PathAlreadyExists) return err; };

    const tags_dir = try std.fs.path.join(std.heap.page_allocator, &.{ output_dir, "tags" }); defer std.heap.page_allocator.free(tags_dir);
    std.fs.cwd().makeDir(tags_dir) catch |err| { if (err != error.PathAlreadyExists) return err; }; }

fn writeFile(path: []const u8, content: []const u8) !void { const file = try std.fs.cwd().createFile(path, .{}); defer file.close(); try file.writeAll(content); }

fn compareTagsByName(context: void, a: types.Tag, b: types.Tag) bool { _ = context; return std.mem.order(u8, a.name, b.name) == .lt; }
