const std = @import("std");
const ansi_term = @import("ansi_term");

const st = ansi_term.style;
const ansi_fmt = ansi_term.format;
const ansi_clear = ansi_term.clear;

pub const Style = st.Style;
pub const Color = st.Color;
pub const FontStyle = st.FontStyle;

const assert = std.debug.assert;

pub const Reset = struct {};

pub const Clear = enum(u8) {
    current_line,
    cursor_to_line_begin,
    cursor_to_line_end,
    screen,
    cursor_to_screen_begin,
    cursor_to_screen_end,
};

pub const ForegroundColor = struct {
    foreground_color: Color,
};
pub const BackgroundColor = struct {
    background_color: Color,
};

const ColorType = union(enum) {
    reset,
    clear: Clear,
    style: Style,
    font_style: FontStyle,
    foreground_color: Color,
    background_color: Color,
};

fn get_color_type(value: anytype) ?ColorType {
    const T = @TypeOf(value);

    if (T == Reset) {
        return .reset;
    }

    if (T == Clear) {
        return .{ .clear = value };
    }

    if (T == Style) {
        return .{ .style = value };
    }

    if (T == Color) {
        return .{ .foreground_color = value };
    }

    // Reject: Tag type (enum tag)
    // if (comptime @typeInfo(T) == .@"enum") {
    //     @compileError("Union tag passed, but a full union value was expected.");
    // }

    if (T == std.meta.Tag(Color)) {
        //   const enum_val = @unionInit(Color, Color.Blue, {});
        //  return .{ .foreground_color = enum_val };

        const color_actual: ?Color = switch (@typeInfo(T)) {
            .@"union" => value, // already a union
            .@"enum" => blk: {

                // Find the correct field in the union by its tag
                const UInfo = @typeInfo(Color).@"union";
                inline for (UInfo.fields) |f| {
                    if (std.mem.eql(u8, f.name, @tagName(value))) {
                        if (f.type == void) {
                            // No payload

                            break :blk @unionInit(Color, f.name, {});
                        } else {
                            @compileError("Expected enum tag for union with name " ++ f.name ++ "to have empty value, but has type: " ++ @typeName(f.type) ++ @typeName(T));
                        }
                    }
                }

                @compileError("Enum tag does not belong to union 'Color'");
            },

            else => @compileError("Expected Color or Color.Tag"),
        };

        return .{ .foreground_color = color_actual };
    }

    // comptime if (T == std.meta.Tag(Color)) {
    //      @compileError("error, passed union tag of Color: " ++ @typeName(T));
    //  };

    if (T == BackgroundColor) {
        return .{ .background_color = value.background_color };
    }

    if (T == ForegroundColor) {
        return .{ .foreground_color = value.foreground_color };
    }

    if (T == FontStyle) {
        return .{ .font_style = value };
    }

    return null;
}

pub const print = printFunctionPrivate;

fn printColor(w: *std.Io.Writer, color_type: ColorType, last_style: *?Style) !void {
    switch (color_type) {
        .reset => {
            try ansi_fmt.resetStyle(w);
        },
        .clear => |cl| {
            switch (cl) {
                .current_line => try ansi_clear.clearCurrentLine(w),
                .cursor_to_line_begin => try ansi_clear.clearFromCursorToLineBeginning(w),
                .cursor_to_line_end => try ansi_clear.clearFromCursorToLineEnd(w),
                .screen => try ansi_clear.clearScreen(w),
                .cursor_to_screen_begin => try ansi_clear.clearFromCursorToScreenBeginning(w),
                .cursor_to_screen_end => try ansi_clear.clearFromCursorToScreenEnd(w),
            }
        },
        .style => |style_now| {
            try ansi_fmt.updateStyle(w, style_now, last_style.*);
            last_style.* = style_now;
        },
        .foreground_color => |color| {
            const style_now: Style = blk: {
                if (last_style.*) |styl| {
                    break :blk Style{ .foreground = color, .background = styl.background, .font_style = styl.font_style };
                } else {
                    break :blk Style{ .foreground = color };
                }
            };

            try ansi_fmt.updateStyle(w, style_now, last_style.*);
            last_style.* = style_now;
        },
        .background_color => |color| {
            const style_now: Style = blk: {
                if (last_style.*) |styl| {
                    break :blk Style{ .foreground = styl.background, .background = color, .font_style = styl.font_style };
                } else {
                    break :blk Style{ .background = color };
                }
            };

            try ansi_fmt.updateStyle(w, style_now, last_style.*);
            last_style.* = style_now;
        },
        .font_style => |font_styl| {
            const style_now: Style = blk: {
                if (last_style.*) |styl| {
                    break :blk Style{ .foreground = styl.foreground, .background = styl.background, .font_style = font_styl };
                } else {
                    break :blk Style{ .font_style = font_styl };
                }
            };

            try ansi_fmt.updateStyle(w, style_now, last_style.*);
            last_style.* = style_now;
        },
    }
}

pub fn printFunctionPrivate(w: *std.Io.Writer, comptime fmt: []const u8, args: anytype) std.Io.Writer.Error!void {
    const ArgsType = @TypeOf(args);
    const args_type_info = @typeInfo(ArgsType);
    if (args_type_info != .@"struct") {
        @compileError("expected tuple or struct argument, found " ++ @typeName(ArgsType));
    }

    const fields_info = args_type_info.@"struct".fields;
    const max_format_args = @typeInfo(std.fmt.ArgSetType).int.bits;
    if (fields_info.len > max_format_args) {
        @compileError("32 arguments max are supported per format call");
    }

    @setEvalBranchQuota(fmt.len * 1000);
    comptime var arg_state: std.fmt.ArgState = .{ .args_len = fields_info.len };

    var last_style: ?Style = null;

    comptime var i = 0;
    comptime var literal: []const u8 = "";
    inline while (true) {
        const start_index = i;

        inline while (i < fmt.len) : (i += 1) {
            switch (fmt[i]) {
                '{', '}' => break,
                else => {},
            }
        }

        comptime var end_index = i;
        comptime var unescape_brace = false;

        // Handle {{ and }}, those are un-escaped as single braces
        if (i + 1 < fmt.len and fmt[i + 1] == fmt[i]) {
            unescape_brace = true;
            // Make the first brace part of the literal...
            end_index += 1;
            // ...and skip both
            i += 2;
        }

        literal = literal ++ fmt[start_index..end_index];

        // We've already skipped the other brace, restart the loop
        if (unescape_brace) continue;

        // Write out the literal
        if (literal.len != 0) {
            try w.writeAll(literal);
            literal = "";
        }

        if (i >= fmt.len) break;

        if (fmt[i] == '}') {
            @compileError("missing opening {");
        }

        // Get past the {
        comptime assert(fmt[i] == '{');
        i += 1;

        const fmt_begin = i;
        // Find the closing brace
        inline while (i < fmt.len and fmt[i] != '}') : (i += 1) {}
        const fmt_end = i;

        if (i >= fmt.len) {
            @compileError("missing closing }");
        }

        // Get past the }
        comptime assert(fmt[i] == '}');
        i += 1;

        const placeholder_array = fmt[fmt_begin..fmt_end].*;
        const placeholder = comptime std.fmt.Placeholder.parse(&placeholder_array);
        const arg_pos = comptime switch (placeholder.arg) {
            .none => null,
            .number => |pos| pos,
            .named => |arg_name| std.meta.fieldIndex(ArgsType, arg_name) orelse
                @compileError("no argument with name '" ++ arg_name ++ "'"),
        };

        const width = switch (placeholder.width) {
            .none => null,
            .number => |v| v,
            .named => |arg_name| blk: {
                const arg_i = comptime std.meta.fieldIndex(ArgsType, arg_name) orelse
                    @compileError("no argument with name '" ++ arg_name ++ "'");
                _ = comptime arg_state.nextArg(arg_i) orelse @compileError("too few arguments");
                break :blk @field(args, arg_name);
            },
        };

        const precision = switch (placeholder.precision) {
            .none => null,
            .number => |v| v,
            .named => |arg_name| blk: {
                const arg_i = comptime std.meta.fieldIndex(ArgsType, arg_name) orelse
                    @compileError("no argument with name '" ++ arg_name ++ "'");
                _ = comptime arg_state.nextArg(arg_i) orelse @compileError("too few arguments");
                break :blk @field(args, arg_name);
            },
        };

        const arg_to_print = comptime arg_state.nextArg(arg_pos) orelse
            @compileError("too few arguments");

        const field = @field(args, fields_info[arg_to_print].name);

        if (get_color_type(field)) |color_type| {
            try printColor(w, color_type, &last_style);
        } else {
            try w.printValue(
                placeholder.specifier_arg,
                .{
                    .fill = placeholder.fill,
                    .alignment = placeholder.alignment,
                    .width = width,
                    .precision = precision,
                },
                field,
                std.options.fmt_max_depth,
            );

            //TODO
            //  _ = width;
            //  _ = precision;
        }
    }

    if (comptime arg_state.hasUnusedArgs()) {
        const missing_count = arg_state.args_len - @popCount(arg_state.used_args);
        switch (missing_count) {
            0 => unreachable,
            1 => @compileError("unused argument in '" ++ fmt ++ "'"),
            else => @compileError(std.fmt.comptimePrint("{d}", .{missing_count}) ++ " unused arguments in '" ++ fmt ++ "'"),
        }
    }
}

pub const buffer_length: comptime_int = 4096;

const TTYWriter = struct {
    writer: std.fs.File.Writer,

    pub fn createFromFile(file: std.fs.File, buffer: []u8) TTYWriter {
        return TTYWriter{ .writer = file.writer(buffer) };
    }

    fn printTo(writer: *std.Io.Writer, color: ?Color, comptime fmt: []const u8, args: anytype) !void {
        const ArgsType = @TypeOf(args);
        const args_type_info = @typeInfo(ArgsType);
        if (args_type_info != .@"struct") {
            @compileError("expected tuple or struct argument, found " ++ @typeName(ArgsType));
        }

        if (color) |clr| {
            return try printToImpl(writer, "{any}" ++ fmt ++ "{any}", .{clr} ++ args ++ .{Reset{}});
        }

        return try printToImpl(writer, fmt, args);
    }

    fn printToImpl(writer: *std.Io.Writer, comptime fmt: []const u8, args: anytype) !void {
        const ArgsType = @TypeOf(args);
        const args_type_info = @typeInfo(ArgsType);
        if (args_type_info != .@"struct") {
            @compileError("expected tuple or struct argument, found " ++ @typeName(ArgsType));
        }

        try printFunctionPrivate(writer, fmt, args);
        try writer.flush();
    }

    pub fn print(self: *TTYWriter, color: ?Color, comptime fmt: []const u8, args: anytype) !void {
        const io_writer = &self.writer.interface;
        try TTYWriter.printTo(io_writer, color, fmt, args);
    }
};

pub const StderrWriter = struct {
    writer: TTYWriter,

    pub fn create(buffer: []u8) StderrWriter {
        const writer = TTYWriter.createFromFile(std.fs.File.stderr(), buffer);

        return .{ .writer = writer };
    }

    pub fn printOnce(comptime fmt: []const u8, args: anytype) !void {
        var stderr_buffer: [buffer_length]u8 = undefined;
        var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
        const stderr = &stderr_writer.interface;

        try TTYWriter.printTo(stderr, Color.Red, fmt, args);
    }

    pub fn printRaw(self: *StderrWriter, comptime fmt: []const u8, args: anytype, color: ?Color) !void {
        return self.writer.print(color, fmt, args);
    }

    pub fn print(self: *StderrWriter, comptime fmt: []const u8, args: anytype) !void {
        return self.printRaw(fmt, args, Color.Red);
    }
};

pub const StdoutWriter = struct {
    writer: TTYWriter,

    pub fn create(buffer: []u8) StdoutWriter {
        const writer = TTYWriter.createFromFile(std.fs.File.stdout(), buffer);

        return .{ .writer = writer };
    }

    pub fn printOnce(comptime fmt: []const u8, args: anytype) !void {
        var stdout_buffer: [buffer_length]u8 = undefined;
        var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
        const stdout = &stdout_writer.interface;

        try TTYWriter.printTo(stdout, Color.Green, fmt, args);
    }

    pub fn printRaw(self: *StdoutWriter, comptime fmt: []const u8, args: anytype, color: ?Color) !void {
        return self.writer.print(color, fmt, args);
    }

    pub fn print(self: *StdoutWriter, comptime fmt: []const u8, args: anytype) !void {
        return self.printRaw(fmt, args, Color.Green);
    }
};

test "style formatting" {
    { // reset
        try std.testing.expectEqual(.reset, get_color_type(Reset{}));
    }

    { // clear
        try std.testing.expectEqual(ColorType{ .clear = .current_line }, get_color_type(Clear.current_line));
        try std.testing.expectEqual(ColorType{ .clear = Clear.screen }, get_color_type(Clear.screen));
    }

    { // style
        try std.testing.expectEqual(ColorType{ .style = Style{ .foreground = Color.Red } }, get_color_type(Style{ .foreground = Color.Red }));
        try std.testing.expectEqual(ColorType{ .style = Style{ .foreground = .Red, .background = .Blue, .font_style = .{ .bold = true } } }, get_color_type(Style{ .foreground = .Red, .background = .Blue, .font_style = .{ .bold = true } }));
        try std.testing.expectEqual(null, get_color_type(.{ .foreground = .Red }));
        try std.testing.expectEqual(null, get_color_type(.{ .foreground = .Red, .background = .Blue, .font_style = .{ .bold = true } }));
    }

    { // font_style
        try std.testing.expectEqual(ColorType{ .font_style = FontStyle{ .bold = true } }, get_color_type(FontStyle{ .bold = true }));
        try std.testing.expectEqual(null, get_color_type(.{ .bold = true }));
    }

    { // foreground_color
        try std.testing.expectEqual(ColorType{ .foreground_color = Color.Red }, get_color_type(Color.Red));
        try std.testing.expectEqual(ColorType{ .foreground_color = Color.Red }, get_color_type(.Red));
        try std.testing.expectEqual(ColorType{ .foreground_color = Color.Blue }, get_color_type(.Blue));
        try std.testing.expectEqual(ColorType{ .foreground_color = Color.Blue }, get_color_type(ForegroundColor{ .foreground_color = .Blue }));
        try std.testing.expectEqual(null, get_color_type(.{ .foreground_color = .Blue }));
        try std.testing.expectEqual(ColorType{ .foreground_color = Color{ .Grey = 1 } }, get_color_type(Color{ .Grey = 1 }));
    }

    { // background_color
        try std.testing.expectEqual(ColorType{ .foreground_color = Color.Blue }, get_color_type(BackgroundColor{ .background_color = .Blue }));
        try std.testing.expectEqual(ColorType{ .background_color = Color.Blue }, get_color_type(.{ .background_color = .Blue }));
    }
}
