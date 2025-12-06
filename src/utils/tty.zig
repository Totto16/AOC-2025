const std = @import("std");
const ansi_term = @import("ansi_term");

const anis_st = ansi_term.style;
const ansi_fmt = ansi_term.format;
const ansi_clear = ansi_term.clear;

pub const Style = anis_st.Style;

pub const FormatColorSimple = enum(u8) {
    Default,
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,

    pub fn toAnsiColor(self: FormatColorSimple) anis_st.Color {
        return switch (self) {
            .Default => anis_st.Color.Default,
            .Black => anis_st.Color.Black,
            .Red => anis_st.Color.Red,
            .Green => anis_st.Color.Green,
            .Yellow => anis_st.Color.Yellow,
            .Blue => anis_st.Color.Blue,
            .Magenta => anis_st.Color.Magenta,
            .Cyan => anis_st.Color.Cyan,
            .White => anis_st.Color.White,
        };
    }
};

pub const ColorRGB = anis_st.ColorRGB;

pub const FormatColorExtended = union(enum(u8)) {
    Simple: FormatColorSimple,
    Fixed: u8,
    Grey: u8,
    RGB: anis_st.ColorRGB,

    pub fn toAnsiColor(self: FormatColorExtended) anis_st.Color {
        return switch (self) {
            .Simple => |s| s.toAnsiColor(),
            .Fixed => |val| anis_st.Color{ .Fixed = val },
            .Grey => |val| anis_st.Color{ .Grey = val },
            .RGB => |val| anis_st.Color{ .RGB = val },
        };
    }
};

pub const FontStyle = anis_st.FontStyle;

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
    foreground_color: FormatColorSimple,
};
pub const BackgroundColor = struct {
    background_color: FormatColorSimple,
};

const ColorType = union(enum) {
    reset,
    clear: Clear,
    style: Style,
    font_style: FontStyle,
    foreground_color: anis_st.Color,
    background_color: anis_st.Color,
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

    if (T == FormatColorSimple) {
        return .{ .foreground_color = value.toAnsiColor() };
    }

    if (T == FormatColorExtended) {
        return .{ .foreground_color = value.toAnsiColor() };
    }

    if (T == BackgroundColor) {
        return .{ .background_color = value.background_color.toAnsiColor() };
    }

    if (T == ForegroundColor) {
        return .{ .foreground_color = value.foreground_color.toAnsiColor() };
    }

    if (T == FontStyle) {
        return .{ .font_style = value };
    }

    return null;
}

pub const print = printFunctionPrivate;

fn printColor(w: *std.Io.Writer, is_tty: bool, comptime fmt: []const u8, color_type: ColorType, last_style: *?Style) !void {
    switch (fmt.len) {
        0 => {},
        else => {
            std.debug.print("invalid format string '{s}' for color / style like type, expect empty {{}}\n", .{fmt});
            return error.FmtInvalid;
        },
    }

    if (!is_tty) {
        return;
    }

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
//TODO: not supported in formatting, to put these inside xD
// see <zig>/lib/std/zig/Ast/Render.zig:769:41
//769: doc_comment => unreachable,

/// See the `Writer` implementation for detailed diagnostics.
// color formatting errors
pub const PrintError = error{ WriteFailed, FmtInvalid };

pub fn printFunctionPrivate(w: *std.Io.Writer, is_tty: bool, comptime fmt: []const u8, args: anytype) PrintError!void {
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
            try printColor(w, is_tty, placeholder.specifier_arg, color_type, &last_style);
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
    is_tty: bool,

    pub fn createFromFile(file: std.fs.File, buffer: []u8) TTYWriter {
        return TTYWriter{ .writer = file.writer(buffer), .is_tty = file.isTty() };
    }

    fn printTo(writer: *std.Io.Writer, is_tty: bool, comptime fmt: []const u8, args: anytype) !void {
        const ArgsType = @TypeOf(args);
        const args_type_info = @typeInfo(ArgsType);
        if (args_type_info != .@"struct") {
            @compileError("expected tuple or struct argument, found " ++ @typeName(ArgsType));
        }

        try printFunctionPrivate(writer, is_tty, fmt, args);
        try writer.flush();
    }

    pub fn print(self: *TTYWriter, comptime fmt: []const u8, args: anytype) !void {
        const io_writer = &self.writer.interface;
        try TTYWriter.printTo(io_writer, self.is_tty, fmt, args);
    }
};

pub const GlobalWriter = struct {
    writer: *TTYWriter,

    pub fn printWithDefaultColor(self: *GlobalWriter, comptime fmt: []const u8, args: anytype) !void {
        try self.writer.print("{}" ++ fmt ++ "{}", .{FormatColorSimple.Green} ++ args ++ .{Reset{}});
    }

    pub fn print(self: *GlobalWriter, comptime fmt: []const u8, args: anytype) !void {
        try self.writer.print(fmt, args);
    }
};

var _global_stderr_buffer: [buffer_length]u8 = undefined;
var _global_stderr_writer_impl: TTYWriter = TTYWriter{ .writer = std.fs.File.stderr().writer(&_global_stderr_buffer), .is_tty = true };
var _global_stderr_writer: GlobalWriter = GlobalWriter{ .writer = &_global_stderr_writer_impl };

pub const StderrWriter = struct {
    writer: TTYWriter,

    pub fn create(buffer: []u8) StderrWriter {
        const writer = TTYWriter.createFromFile(std.fs.File.stderr(), buffer);

        return .{ .writer = writer };
    }

    pub fn global() *GlobalWriter {
        return &_global_stderr_writer;
    }

    pub fn print(self: *StderrWriter, comptime fmt: []const u8, args: anytype) !void {
        return self.writer.print(fmt, args);
    }
};

var _global_stdout_buffer: [buffer_length]u8 = undefined;
var _global_stdout_writer_impl: TTYWriter = TTYWriter{ .writer = std.fs.File.stdout().writer(&_global_stdout_buffer), .is_tty = true };
var _global_stdout_writer: GlobalWriter = GlobalWriter{ .writer = &_global_stdout_writer_impl };

pub const StdoutWriter = struct {
    writer: TTYWriter,

    pub fn create(buffer: []u8) StdoutWriter {
        const writer = TTYWriter.createFromFile(std.fs.File.stdout(), buffer);

        return .{ .writer = writer };
    }

    pub fn global() *GlobalWriter {
        return &_global_stdout_writer;
    }

    pub fn print(self: *StdoutWriter, comptime fmt: []const u8, args: anytype) !void {
        return self.writer.print(fmt, args);
    }
};

test "style formatting" {
    { // reset
        try std.testing.expectEqual(ColorType.reset, get_color_type(Reset{}));
    }

    { // clear
        try std.testing.expectEqual(ColorType{ .clear = .current_line }, get_color_type(Clear.current_line));
        try std.testing.expectEqual(ColorType{ .clear = Clear.screen }, get_color_type(Clear.screen));
    }

    { // style
        try std.testing.expectEqual(ColorType{ .style = Style{ .foreground = .Red } }, get_color_type(Style{ .foreground = .Red }));
        try std.testing.expectEqual(ColorType{ .style = Style{ .foreground = .Red, .background = .Blue, .font_style = .{ .bold = true } } }, get_color_type(Style{ .foreground = .Red, .background = .Blue, .font_style = .{ .bold = true } }));
        try std.testing.expectEqual(null, get_color_type(.{ .foreground = .Red }));
        try std.testing.expectEqual(null, get_color_type(.{ .foreground = .Red, .background = .Blue, .font_style = .{ .bold = true } }));
    }

    { // font_style
        try std.testing.expectEqual(ColorType{ .font_style = FontStyle{ .bold = true } }, get_color_type(FontStyle{ .bold = true }));
        try std.testing.expectEqual(null, get_color_type(.{ .bold = true }));
    }

    { // foreground_color
        try std.testing.expectEqual(ColorType{ .foreground_color = .Red }, get_color_type(FormatColorSimple.Red));
        try std.testing.expectEqual(null, get_color_type(.Red));
        try std.testing.expectEqual(null, get_color_type(.Blue));
        try std.testing.expectEqual(ColorType{ .foreground_color = .Blue }, get_color_type(ForegroundColor{ .foreground_color = .Blue }));
        try std.testing.expectEqual(null, get_color_type(.{ .foreground_color = .Blue }));
        try std.testing.expectEqual(ColorType{ .foreground_color = .{ .Grey = 1 } }, get_color_type(FormatColorExtended{ .Grey = 1 }));
    }

    { // background_color
        try std.testing.expectEqual(ColorType{ .background_color = .Blue }, get_color_type(BackgroundColor{ .background_color = .Blue }));
        try std.testing.expectEqual(null, get_color_type(.{ .background_color = .Blue }));
    }

    { // other types
        try std.testing.expectEqual(null, get_color_type("test 1"));
    }
}
