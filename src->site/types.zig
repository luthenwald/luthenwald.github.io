const std = @import("std");
const Allocator = std.mem.Allocator;

pub const SourceFile = struct {
    path: []const u8,
    lang: []const u8,

    pub fn deinit(self: *SourceFile, alloc: Allocator) void { alloc.free(self.path); } };

pub const Blog = struct {
    title:       []const u8,
    pageid:      []const u8,
    pub_date:    []const u8,
    tags:        [][]const u8,
    description: []const u8,
    content:     []ParsedBlock,
    filepath:    []const u8,
    outline:     []OutlineItem,
    alloc:       Allocator,

    pub fn deinit(self: *Blog) void {
        self.alloc.free(self.title);
        self.alloc.free(self.pageid);
        self.alloc.free(self.pub_date);
        for (self.tags) |tag| { self.alloc.free(tag); } self.alloc.free(self.tags);
        self.alloc.free(self.description);
        for (self.content) |*block| { block.deinit(self.alloc); } self.alloc.free(self.content);
        self.alloc.free(self.filepath);
        for (self.outline) |*item| { item.deinit(self.alloc); } self.alloc.free(self.outline); } };


pub const OutlineItem = struct {
    level: u8,
    text:  []const u8,
    id:    []const u8,

    pub fn deinit(self: *OutlineItem, alloc: Allocator) void { alloc.free(self.text); alloc.free(self.id); } };

pub const ParsedBlock = union(enum) {
    heading:   Heading,
    paragraph: Paragraph,
    footnote:  Footnote,
    code:      Code,
    list:      List,
    verbatim:  Verbatim,
    callout:   Callout,
    insert:    Insert,

    pub fn deinit(self: *ParsedBlock, alloc: Allocator) void {
        switch (self.*) {
            .heading   => |*h| h.deinit(alloc),
            .paragraph => |*p| p.deinit(alloc),
            .footnote  => |*f| f.deinit(alloc),
            .code      => |*c| c.deinit(alloc),
            .list      => |*l| l.deinit(alloc),
            .verbatim  => |*v| v.deinit(alloc),
            .callout   => |*c| c.deinit(alloc),
            .insert    => |*i| i.deinit(alloc), } } };

pub const Heading = struct {
    level:   u8,
    content: []InlineElement,
    id:      []const u8,

    pub fn deinit(self: *Heading, alloc: Allocator) void { for (self.content) |*elem| { elem.deinit(alloc); } alloc.free(self.content); alloc.free(self.id); } };

pub const Paragraph = struct {
    content: []InlineElement,

    pub fn deinit(self: *Paragraph, alloc: Allocator) void { for (self.content) |*elem| { elem.deinit(alloc); } alloc.free(self.content); } };

pub const Footnote = struct {
    number:  usize,
    content: []InlineElement,

    pub fn deinit(self: *Footnote, alloc: Allocator) void { for (self.content) |*elem| { elem.deinit(alloc); } alloc.free(self.content); } };

pub const Code = struct {
    lines: []const u8,

    pub fn deinit(self: *Code, alloc: Allocator) void { alloc.free(self.lines); } };

pub const List = struct {
    level:   u8,
    content: []InlineElement,

    pub fn deinit(self: *List, alloc: Allocator) void { for (self.content) |*elem| { elem.deinit(alloc); } alloc.free(self.content); } };

pub const Verbatim = struct {
    content: []const u8,

    pub fn deinit(self: *Verbatim, alloc: Allocator) void { alloc.free(self.content); } };

pub const Callout = struct {
    content: []InlineElement,

    pub fn deinit(self: *Callout, alloc: Allocator) void { for (self.content) |*elem| { elem.deinit(alloc); } alloc.free(self.content); } };

pub const Insert = struct {
    path: []const u8,

    pub fn deinit(self: *Insert, alloc: Allocator) void { alloc.free(self.path); } };

pub const InlineElement = union(enum) {
    text:         []const u8,
    link:         Link,
    highlight:    []const u8,
    inlinecode:   []const u8,
    italic:       []const u8,
    footnote_ref: usize,

    pub fn deinit(self: *InlineElement, alloc: Allocator) void {
        switch (self.*) {
            .text         => |t| alloc.free(t),
            .link         => |*l| { alloc.free(l.text); alloc.free(l.url); },
            .highlight    => |h| alloc.free(h),
            .inlinecode   => |c| alloc.free(c),
            .italic       => |i| alloc.free(i),
            .footnote_ref => {}, } } };

pub const Link = struct { text: []const u8, url: []const u8, };

pub const CommentBlock = struct {
    content:      []const u8,
    start_line:   usize,
    end_line:     usize,
    is_multiline: bool,

    pub fn deinit(self: *CommentBlock, alloc: Allocator) void { alloc.free(self.content); } };

pub const CodeBlock = struct { start_line: usize, end_line: usize, };

pub const Metadata = struct {
    title:       ?[]const u8 = null,
    pub_date:    ?[]const u8 = null,
    tags:        ?[]const u8 = null,
    description: ?[]const u8 = null,

    pub fn deinit(self: *Metadata, alloc: Allocator) void { if (self.title) |t| alloc.free(t); if (self.pub_date) |d| alloc.free(d); if (self.tags) |t| alloc.free(t); if (self.description) |d| alloc.free(d); } };

pub const Tag = struct {
    name:  []const u8,
    blogs: std.ArrayList(*Blog),

    pub fn init(alloc: Allocator, name: []const u8) Tag { _ = alloc; return .{ .name = name, .blogs = .{}, }; }
    pub fn deinit(self: *Tag, alloc: Allocator) void { alloc.free(self.name); self.blogs.deinit(alloc); } };
