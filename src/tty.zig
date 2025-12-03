const std = @import("std");
const ansi_term = @import("ansi_term");

const st = ansi_term.style;
const st_fmt = ansi_term.format;

pub const Style = ansi_term.style.Style;
pub const Color = ansi_term.style.Color;

const assert = std.debug.assert;

pub const Reset = struct {};

pub const ForegroundColor = struct { color: st.Color };
pub const BackgroundColor = struct { color: st.Color };

const ColorType = union(enum) {
    reset,
    style: st.Style,
    font_style: st.FontStyle,
    foreground_color: st.Color,
    background_color: st.Color,
};

fn get_color_type(value: anytype) ?ColorType {
    const T = @TypeOf(value);

    if (T == Reset) {
        return .reset;
    }

    if (T == st.Style) {
        return .{ .style = value };
    }

    if (T == st.Color) {
        return .{ .foreground_color = value };
    }

    if (T == ForegroundColor) {
        return .{ .foreground_color = value.color };
    }

    if (T == BackgroundColor) {
        return .{ .background_color = value.color };
    }

    if (T == st.FontStyle) {
        return .{ .font_style = value };
    }

    return null;
}

pub const print = printFunctionPrivate;

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

    var last_style: ?st.Style = null;

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

        if (get_color_type(@field(args, fields_info[arg_to_print].name))) |color_type| {
            switch (color_type) {
                .reset => {
                    try st_fmt.resetStyle(w);
                },
                .style => |style_now| {
                    try st_fmt.updateStyle(w, style_now, last_style);
                    last_style = style_now;
                },
                .foreground_color => |color| {
                    const style_now: st.Style = blk: {
                        if (last_style) |styl| {
                            break :blk st.Style{ .foreground = color, .background = styl.background, .font_style = styl.font_style };
                        } else {
                            break :blk st.Style{ .foreground = color };
                        }
                    };

                    try st_fmt.updateStyle(w, style_now, last_style);
                    last_style = style_now;
                },
                .background_color => |color| {
                    const style_now: st.Style = blk: {
                        if (last_style) |styl| {
                            break :blk st.Style{ .foreground = styl.background, .background = color, .font_style = styl.font_style };
                        } else {
                            break :blk st.Style{ .background = color };
                        }
                    };

                    try st_fmt.updateStyle(w, style_now, last_style);
                    last_style = style_now;
                },
                .font_style => |font_styl| {
                    const style_now: st.Style = blk: {
                        if (last_style) |styl| {
                            break :blk st.Style{ .foreground = styl.foreground, .background = styl.background, .font_style = font_styl };
                        } else {
                            break :blk st.Style{ .font_style = font_styl };
                        }
                    };

                    try st_fmt.updateStyle(w, style_now, last_style);
                    last_style = style_now;
                },
            }
        } else {
            try w.printValue(
                placeholder.specifier_arg,
                .{
                    .fill = placeholder.fill,
                    .alignment = placeholder.alignment,
                    .width = width,
                    .precision = precision,
                },
                @field(args, fields_info[arg_to_print].name),
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

const buffer_length: comptime_int = 4096;

const TTYWriter = struct {
    buffer: [buffer_length]u8 = undefined,
    writer: std.Io.Writer,

    pub fn createFromFile(file: std.fs.File) TTYWriter {
        const result = TTYWriter{ .buffer = undefined, .stderr_writer = undefined };

        result.writer = file.writer(&result.buffer);

        return result;
    }

    fn printTo(writer: *std.Io.Writer, color: st.Color, comptime fmt: []const u8, args: anytype) !void {
        const ArgsType = @TypeOf(args);
        const args_type_info = @typeInfo(ArgsType);
        if (args_type_info != .@"struct") {
            @compileError("expected tuple or struct argument, found " ++ @typeName(ArgsType));
        }

        const new_fmt = "{any}" ++ fmt ++ "{any}";
        const new_args = .{color} ++ args ++ .{Reset{}};

        try printFunctionPrivate(writer, new_fmt, new_args);
        try writer.flush();
    }

    pub fn print(self: *StderrWriter, color: st.Color, comptime fmt: []const u8, args: anytype) !void {
        const io_writer = &self.writer.interface;
        self.printTo(io_writer, color, fmt, args);
    }
};

pub const StderrWriter = struct {
    writer: TTYWriter,

    pub fn create() StderrWriter {
        const writer = TTYWriter.createFromFile(std.fs.File.stderr());

        return .{ .writer = writer };
    }

    pub fn printOnce(comptime fmt: []const u8, args: anytype) !void {
        var stderr_buffer: [buffer_length]u8 = undefined;
        var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
        const stderr = &stderr_writer.interface;

        try TTYWriter.printTo(stderr, st.Color.Red, fmt, args);
    }

    pub fn print(self: *StderrWriter, comptime fmt: []const u8, args: anytype) !void {
        return self.writer.print(st.Color.Red, fmt, args);
    }
};

pub const StdoutWriter = struct {
    writer: TTYWriter,

    pub fn create() StdoutWriter {
        const writer = TTYWriter.createFromFile(std.fs.File.stdout());

        return .{ .writer = writer };
    }

    pub fn printOnce(comptime fmt: []const u8, args: anytype) !void {
        var stdout_buffer: [buffer_length]u8 = undefined;
        var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
        const stdout = &stdout_writer.interface;

        try TTYWriter.printTo(stdout, st.Color.Green, fmt, args);
    }

    pub fn print(self: *StderrWriter, comptime fmt: []const u8, args: anytype) !void {
        return self.writer.print(st.Color.Green, fmt, args);
    }
};
