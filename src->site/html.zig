const std = @import("std");
const types = @import("types.zig");

const Allocator = std.mem.Allocator;

pub fn generateBlogPage(
    alloc:    Allocator,
    blog:     *const types.Blog,
    base_url: []const u8, ) ![]const u8 {

    var html: std.ArrayList(u8) = .{}; defer html.deinit(alloc);
    const w = html.writer(alloc);

    try renderHtmlHead(w, blog.title, "../styles/", "blog-page");

    try html.appendSlice(alloc, "   <aside class=\"outline-sidebar\">\n");
    try html.appendSlice(alloc, "      <nav class=\"outline\">\n");
    for (blog.outline) |item| { try std.fmt.format(w, "         <a href=\"#{s}\" class=\"h{d}\">{s}</a>\n", .{ item.id, item.level, item.text }); }
    try html.appendSlice(alloc, "      </nav>\n");
    try renderBottomNav(w, .in_blogs);
    try html.appendSlice(alloc, "   </aside>\n");

    try html.appendSlice(alloc, "   <main class=\"main-content\">\n");
    try std.fmt.format(w, "      <h1>{s}</h1>\n", .{blog.title});
    try std.fmt.format(w, "      <div class=\"metadata\">\n", .{});
    try std.fmt.format(w, "         <span class=\"date\">{s}</span>\n", .{blog.pub_date});
    try html.appendSlice(alloc, "         <span class=\"tags\">");
    try renderTagLinks(w, blog.tags, "../tags/");
    try html.appendSlice(alloc, "</span>\n");
    try html.appendSlice(alloc, "      </div>\n");
    try std.fmt.format(w, "      <div class=\"description\">{s}</div>\n", .{blog.description});

    var block_counter: usize = 0;
    for (blog.content) |block| {
       try renderBlock(w, block, base_url, block_counter);
       try w.writeAll("\n");
       block_counter += 1; }

    try html.appendSlice(alloc, "   </main>\n");
    try renderThemeScript(w);
    try renderHtmlTail(w);

    return html.toOwnedSlice(alloc);
}

pub fn generateHomepage(
    alloc: Allocator,
    blogs: []types.Blog, ) ![]const u8 {

    var html: std.ArrayList(u8) = .{}; defer html.deinit(alloc);
    const w = html.writer(alloc);

    try renderHtmlHead(w, "home", "styles/", "home-page");

    try html.appendSlice(alloc, "   <aside class=\"outline-sidebar\">\n");
    try html.appendSlice(alloc, "      <div class=\"spacer\"></div>\n");
    try renderBottomNav(w, .root);
    try html.appendSlice(alloc, "   </aside>\n");

    try html.appendSlice(alloc, "   <main class=\"main-content\">\n");
    try html.appendSlice(alloc, "      <h1>all posts</h1>\n");
    for (blogs) |*blog| { try renderBlogEntry(w, blog, "blogs/", "tags/"); }

    try html.appendSlice(alloc, "   </main>\n");
    try renderThemeScript(w);
    try renderHtmlTail(w);

    return html.toOwnedSlice(alloc); }

pub fn generateTagCloud(
    alloc: Allocator,
    tags:  []types.Tag, ) ![]const u8 {

    var html: std.ArrayList(u8) = .{}; defer html.deinit(alloc);
    const w = html.writer(alloc);

    try renderHtmlHead(w, "tags", "../styles/", "tagcloud-page");

    try html.appendSlice(alloc, "   <aside class=\"outline-sidebar\">\n");
    try html.appendSlice(alloc, "      <div class=\"spacer\"></div>\n");
    try renderBottomNav(w, .in_tags);
    try html.appendSlice(alloc, "   </aside>\n");

    try html.appendSlice(alloc, "   <main class=\"main-content\">\n");
    try html.appendSlice(alloc, "      <h1>tag cloud</h1>\n");
    try html.appendSlice(alloc, "      <div class=\"tag-cloud\">\n");
    for (tags) |tag| { const size = tag.blogs.items.len; try std.fmt.format(w, "         <a href=\"{s}.html\">{s}({d})</a>\n", .{ tag.name, tag.name, size }); }
    try html.appendSlice(alloc, "      </div>\n");
    try html.appendSlice(alloc, "   </main>\n");
    try renderThemeScript(w);
    try renderHtmlTail(w);

    return html.toOwnedSlice(alloc); }

pub fn generateTagPage(
    alloc: Allocator,
    tag:   *const types.Tag, ) ![]const u8 {

    var html: std.ArrayList(u8) = .{}; defer html.deinit(alloc);
    const w = html.writer(alloc);

    var title_buf: [256]u8 = undefined;
    const title = std.fmt.bufPrint(&title_buf, "tag: {s}", .{tag.name}) catch tag.name;

    try renderHtmlHead(w, title, "../styles/", "tag-page");

    try html.appendSlice(alloc, "   <aside class=\"outline-sidebar\">\n");
    try html.appendSlice(alloc, "      <div class=\"spacer\"></div>\n");
    try renderBottomNav(w, .in_tags);
    try html.appendSlice(alloc, "   </aside>\n");

    try html.appendSlice(alloc, "   <main class=\"main-content\">\n");
    try std.fmt.format(w, "      <h1>tag: {s}</h1>\n", .{tag.name});
    for (tag.blogs.items) |blog| { try renderBlogEntry(w, blog, "../blogs/", "../tags/"); }

    try html.appendSlice(alloc, "   </main>\n");
    try renderThemeScript(w);
    try renderHtmlTail(w);

    return html.toOwnedSlice(alloc); }

const NavPath = enum { root, in_tags, in_blogs };

fn renderHtmlHead(writer: anytype, title: []const u8, css_prefix: []const u8, body_class: []const u8) !void {
   try writer.writeAll("<!DOCTYPE html>\n<html lang=\"en\" data-theme=\"light\">\n<head>\n");
   try writer.writeAll("   <meta charset=\"UTF-8\">\n");
   try writer.writeAll("   <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n");
   try std.fmt.format(writer, "   <title>{s}</title>\n", .{title});
   try std.fmt.format(writer, "   <link rel=\"stylesheet\" href=\"{s}reset.css\">\n", .{css_prefix});
   try std.fmt.format(writer, "   <link rel=\"stylesheet\" href=\"{s}prima.css\">\n", .{css_prefix});
   try std.fmt.format(writer, "</head>\n<body class=\"{s}\">\n", .{body_class}); }

fn renderThemeScript(writer: anytype) !void {
   try writer.writeAll(
      \\   <script>
      \\      document.addEventListener('DOMContentLoaded', function() {
      \\         const themeToggle = document.getElementById('theme-toggle');
      \\         const html = document.documentElement;
      \\         const savedTheme = localStorage.getItem('theme') || 'light';
      \\         html.setAttribute('data-theme', savedTheme);
      \\         themeToggle.addEventListener('click', function(e) {
      \\            e.preventDefault();
      \\            const currentTheme = html.getAttribute('data-theme');
      \\            const newTheme = currentTheme === 'light' ? 'dark' : 'light';
      \\            html.setAttribute('data-theme', newTheme);
      \\            localStorage.setItem('theme', newTheme);
      \\         });
      \\      });
      \\   </script>
      \\
   ); }

fn renderHtmlTail(writer: anytype) !void { try writer.writeAll("</body>\n</html>\n"); }

fn renderTagLinks(writer: anytype, tags: [][]const u8, href_prefix: []const u8) !void {
   for (tags, 0..) |tag, i| {
      if (i > 0) try writer.writeAll(", ");
      try std.fmt.format(writer, "<a href=\"{s}{s}.html\">{s}</a>", .{ href_prefix, tag, tag }); } }

fn renderBlogEntry(writer: anytype, blog: *const types.Blog, blog_link_prefix: []const u8, tag_href_prefix: []const u8) !void {
   try writer.writeAll("      <article class=\"blog-entry\">\n");
   try std.fmt.format(writer, "         <h2><a href=\"{s}{s}.html\">{s}</a></h2>\n", .{ blog_link_prefix, blog.pageid, blog.title });
   try std.fmt.format(writer, "         <div class=\"date\">{s}</div>\n", .{blog.pub_date});
   try std.fmt.format(writer, "         <div class=\"description\">{s}</div>\n", .{blog.description});
   try writer.writeAll("         <div class=\"tags\">");
   try renderTagLinks(writer, blog.tags, tag_href_prefix);
   try writer.writeAll("</div>\n");
   try writer.writeAll("      </article>\n"); }

fn renderNavLinks(writer: anytype, path: NavPath, with_theme: bool) !void {
   const Hrefs = struct { home: []const u8, tags: []const u8, feed: []const u8 };
   const hrefs: Hrefs = switch (path) {
      .root => .{ .home = "index.html", .tags = "tags/tagcloud.html", .feed = "feed.xml" },
      .in_tags => .{ .home = "../index.html", .tags = "tagcloud.html", .feed = "../feed.xml" },
      .in_blogs => .{ .home = "../index.html", .tags = "../tags/tagcloud.html", .feed = "../feed.xml" },
   };
   try std.fmt.format(writer, "         <a href=\"{s}\">Home</a>\n", .{hrefs.home});
   try std.fmt.format(writer, "         <a href=\"{s}\">Tags</a>\n", .{hrefs.tags});
   try std.fmt.format(writer, "         <a href=\"{s}\">Feed</a>\n", .{hrefs.feed});
   if (with_theme) try writer.writeAll("         <a href=\"#\" id=\"theme-toggle\" title=\"toggle theme\">Theme</a>\n");
}

fn renderBottomNav(writer: anytype, path: NavPath) !void {
   try writer.writeAll("      <nav class=\"bottom-nav\">\n");
   try renderNavLinks(writer, path, true);
   try writer.writeAll("      </nav>\n");
}

fn renderNavSidebar(writer: anytype, path: NavPath) !void {
   try writer.writeAll("   <aside class=\"nav-sidebar\">\n");
   try writer.writeAll("      <nav>\n");
   try renderNavLinks(writer, path, false);
   try writer.writeAll("      </nav>\n");
   try writer.writeAll("   </aside>\n");
}

fn renderBlock(writer: anytype, block: types.ParsedBlock, base_url: []const u8, block_id: usize) !void {
   _ = base_url;

   switch (block) {
      .heading => |h| {
         try std.fmt.format(writer, "      <h{d} id=\"{s}\" data-block-id=\"block-{d}\">", .{ h.level, h.id, block_id });
         try renderInlineElements(writer, h.content);
         try std.fmt.format(writer, "</h{d}>\n", .{h.level}); },

      .paragraph => |p| {
         try std.fmt.format(writer, "      <p data-block-id=\"block-{d}\">", .{block_id});
         try renderInlineElements(writer, p.content);
         try writer.writeAll("</p>\n"); },

      .footnote => |f| {
         try std.fmt.format(writer, "      <div class=\"footnote\" id=\"fn-{d}\"><sup>{d}</sup> ", .{ f.number, f.number });
         try renderInlineElements(writer, f.content);
         try writer.writeAll("</div>\n"); },

      .code => |c| {
         try std.fmt.format(writer, "      <pre data-block-id=\"block-{d}\"><code>", .{block_id});
         try writer.writeAll(c.lines);
         try writer.writeAll("</code></pre>\n"); },

      .list => |l| {
         try std.fmt.format(writer, "      <p class=\"l{d}-list\" data-block-id=\"block-{d}\">", .{ l.level, block_id });
         try renderInlineElements(writer, l.content);
         try writer.writeAll("</p>\n"); },

      .verbatim => |v| {
         try std.fmt.format(writer, "      <pre class=\"verbatim\" data-block-id=\"block-{d}\"><code>", .{block_id});
         try escapeHtml(writer, v.content);
         try writer.writeAll("</code></pre>\n"); },

      .callout => |c| {
         try std.fmt.format(writer, "      <blockquote class=\"callout\" data-block-id=\"block-{d}\">", .{block_id});
         try renderInlineElements(writer, c.content);
         try writer.writeAll("</blockquote>\n"); },

      .insert => |i| {
         const file = std.fs.cwd().openFile(i.path, .{}) catch |err| {
             std.debug.print("error: failed to open insert file '{s}': {any}\n", .{ i.path, err });
             try std.fmt.format(writer, "<!-- failed to insert file: {s} -->\n", .{i.path});
             return;
         }; defer file.close();

         try std.fmt.format(writer, "<div class=\"insert-container\" data-block-id=\"block-{d}\">", .{block_id});

         var buf: [4096]u8 = undefined;
         while (true) { const n = try file.read(&buf); if (n == 0) break; try writer.writeAll(buf[0..n]); }
         try writer.writeAll("</div>\n"); }, } }

fn renderInlineElements(writer: anytype, elements: []types.InlineElement) !void {
   for (elements) |elem| {
       switch (elem) {
          .text         => |t| try writer.writeAll(t),
          .link         => |l| try std.fmt.format(writer, "<a href=\"{s}\">{s}</a>", .{ l.url, l.text }),
          .highlight    => |h| try std.fmt.format(writer, "<span class=\"highlight\">{s}</span>", .{h}),
          .inlinecode   => |c| try std.fmt.format(writer, "<code class=\"inlinecode\">{s}</code>", .{c}),
          .italic       => |i| try std.fmt.format(writer, "<em>{s}</em>", .{i}),
          .footnote_ref => |num| try std.fmt.format(writer, "<sup><a href=\"#fn-{d}\">{d}</a></sup>", .{ num, num }), } } }

fn escapeHtml(writer: anytype, text: []const u8) !void {
   for (text) |c| {
       switch (c) {
          '<'  => try writer.writeAll("&lt;"),
          '>'  => try writer.writeAll("&gt;"),
          '&'  => try writer.writeAll("&amp;"),
          '"'  => try writer.writeAll("&quot;"),
          '\'' => try writer.writeAll("&#39;"),
          else => try writer.writeByte(c), } } }
