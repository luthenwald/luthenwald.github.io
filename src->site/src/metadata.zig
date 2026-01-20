const std   = @import("std");
const types = @import("types.zig");

const Allocator = std.mem.Allocator;

pub fn parseMetadata(
    alloc:       Allocator,
    content:     []const u8,
    filepath:    []const u8,
    line_number: usize, ) !types.Metadata {

    var metadata = types.Metadata{}; errdefer metadata.deinit(alloc);

    var lines = std.mem.splitScalar(u8, content, '\n');
    var current_line = line_number;

    var collecting_description = false;
    var description_parts: std.ArrayList([]const u8) = .{}; defer { for (description_parts.items) |part| { alloc.free(part); } description_parts.deinit(alloc); }

    while (lines.next()) |line| {
       defer current_line += 1;

       const trimmed = std.mem.trim(u8, line, " \t\r");
       if (trimmed.len == 0) {
          if (collecting_description) {
             const joined = try joinDescription(alloc, description_parts.items); errdefer alloc.free(joined);
             metadata.description = joined;
             collecting_description = false;
             for (description_parts.items) |part| { alloc.free(part); }
             description_parts.clearRetainingCapacity(); } continue; }

       const eq_pos = std.mem.indexOf(u8, trimmed, "=");
       if (eq_pos == null) {
          if (collecting_description) {
             const continuation = std.mem.trim(u8, line, " \t");
             if (continuation.len > 0) { try description_parts.append(alloc, try alloc.dupe(u8, continuation)); } }
             else { std.debug.print("error: {s}:{d}: metadata line missing '='\n", .{ filepath, current_line }); return error.InvalidMetadata; } continue; }

       if (collecting_description) {
          const joined = try joinDescription(alloc, description_parts.items); errdefer alloc.free(joined);
          metadata.description = joined;
          collecting_description = false;
          for (description_parts.items) |part| { alloc.free(part); }
          description_parts.clearRetainingCapacity(); }

       const key = std.mem.trim(u8, trimmed[0..eq_pos.?], " \t");
       const value = std.mem.trim(u8, trimmed[eq_pos.? + 1 ..], " \t");

       if (std.mem.eql(u8, key, "description")) {
          collecting_description = true;
          if (value.len > 0) { try description_parts.append(alloc, try alloc.dupe(u8, value)); } }
          else { try setMetadataField(alloc, &metadata, key, value); } }

    if (collecting_description and description_parts.items.len > 0) {
       const joined = try joinDescription(alloc, description_parts.items); errdefer alloc.free(joined);
       metadata.description = joined; }

    try validateMetadata(&metadata, filepath, line_number);

    return metadata; }

fn joinDescription(alloc: Allocator, parts: []const []const u8) ![]const u8 {
   if (parts.len == 0) return try alloc.dupe(u8, "");
   if (parts.len == 1) return try alloc.dupe(u8, parts[0]);

   var result: std.ArrayList(u8) = .{}; defer result.deinit(alloc);

   for (parts, 0..) |part, i| { if (i > 0) { try result.append(alloc, ' '); } try result.appendSlice(alloc, part); }

   return result.toOwnedSlice(alloc); }

fn setMetadataField(
   alloc:    Allocator,
   metadata: *types.Metadata,
   key:      []const u8,
   value:    []const u8, ) !void {
   if (std.mem.eql(u8, key, "title")) { metadata.title = try alloc.dupe(u8, value); }
   else if (std.mem.eql(u8, key, "pubDate")) { metadata.pub_date = try alloc.dupe(u8, value); }
   else if (std.mem.eql(u8, key, "tags")) { metadata.tags = try alloc.dupe(u8, value); }
   else if (std.mem.eql(u8, key, "description")) { metadata.description = try alloc.dupe(u8, value); } }

fn validateMetadata(metadata: *types.Metadata, filepath: []const u8, line_number: usize) !void {
   if (metadata.title == null or metadata.title.?.len == 0) { std.debug.print("error: {s}:{d}: missing required field 'title'\n", .{ filepath, line_number }); return error.MissingRequiredField; }
   if (metadata.pub_date == null or metadata.pub_date.?.len == 0) { std.debug.print("error: {s}:{d}: missing required field 'pubDate'\n", .{ filepath, line_number }); return error.MissingRequiredField; }
   if (!isValidDate(metadata.pub_date.?)) { std.debug.print("error: {s}:{d}: invalid date format for 'pubDate', expected yyyy-mm-dd\n", .{ filepath, line_number }); return error.InvalidDateFormat; }
   if (metadata.tags == null or metadata.tags.?.len == 0) { std.debug.print("error: {s}:{d}: missing required field 'tags'\n", .{ filepath, line_number }); return error.MissingRequiredField; }
   if (metadata.description == null or metadata.description.?.len == 0) { std.debug.print("error: {s}:{d}: missing required field 'description'\n", .{ filepath, line_number }); return error.MissingRequiredField; } }

fn isValidDate(date: []const u8) bool {
   if (date.len != 10) return false;
   if (date[4] != '-' or date[7] != '-') return false;

   for (date[0..4])  |c| { if (c < '0' or c > '9') return false; }
   for (date[5..7])  |c| { if (c < '0' or c > '9') return false; }
   for (date[8..10]) |c| { if (c < '0' or c > '9') return false; }

   return true; }

pub fn parseTags(alloc: Allocator, tags_str: []const u8) ![][]const u8 {
    var tags: std.ArrayList([]const u8) = .{};
    errdefer { for (tags.items) |tag| { alloc.free(tag); } tags.deinit(alloc); }

    var it = std.mem.splitScalar(u8, tags_str, ',');
    while (it.next()) |tag| { const trimmed = std.mem.trim(u8, tag, " \t"); if (trimmed.len > 0) { try tags.append(alloc, try alloc.dupe(u8, trimmed)); } }

    return tags.toOwnedSlice(alloc); }

pub fn titleToPageId(alloc: Allocator, title: []const u8) ![]const u8 {
    var result: std.ArrayList(u8) = .{}; defer result.deinit(alloc);

    for (title) |c| { if (c == ' ' or c == '\t') { try result.append(alloc, '-'); } else { try result.append(alloc, c); } }

    return result.toOwnedSlice(alloc); }

pub fn filepathToPageId(alloc: Allocator, filepath: []const u8) ![]const u8 {
    const basename = std.fs.path.basename(filepath);
    const ext = std.fs.path.extension(basename);
    const stem = if (ext.len > 0) basename[0 .. basename.len - ext.len] else basename;
    return try alloc.dupe(u8, stem); }
