// Modded from from https://gist.github.com/karlseguin/c6bea5b35e4e8d26af6f81c22cb5d76b by nurpax
// By me: from https://gist.github.com/nurpax/4afcb6e4ef3f03f0d282f7c462005f12

// LIECENSE: MIT

// Note: doesn't support std.testing.fuzz()

const std = @import("std");
const builtin = @import("builtin");
const tty = @import("tty");
const terminal_progress = @import("terminal_progress");

const BORDER = "=" ** 80;

const Status = enum {
    pass,
    fail,
    skip,
    text,
};

fn printHelp(program: []const u8) !void {
    var stdout_buffer: [tty.buffer_length]u8 = undefined;
    var stdout = tty.StdoutWriter.create(&stdout_buffer);

    try stdout.print("Usage: {s} [options]\n", .{program});
    try stdout.print("\t--fail-first: fail on first test failure\n", .{});
    try stdout.print("\t--filter=<filter>: specify filter for tests\n", .{});
    try stdout.print("\t--help, -h, -?: print this help\n", .{});
}

fn getColor(status: Status) tty.FormatColorSimple {
    return switch (status) {
        .pass => tty.FormatColorSimple.Green,
        .fail => tty.FormatColorSimple.Red,
        .skip => tty.FormatColorSimple.Yellow,
        else => tty.FormatColorSimple.Default,
    };
}

pub fn main() !void {
    @disableInstrumentation();

    var gpa = std.heap.GeneralPurposeAllocator(.{ .stack_trace_frames = 12 }){};
    const alloc = gpa.allocator();

    var fail_first: bool = false;
    var filter: ?[]const u8 = null;

    const args = std.process.argsAlloc(alloc) catch
        @panic("unable to parse command line args");
    defer std.process.argsFree(alloc, args);

    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "--fail-first")) {
            fail_first = true;
        } else if (std.mem.startsWith(u8, arg, "--filter=")) {
            filter = arg["--filter=".len..];
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "-?")) {
            try printHelp(args[0]);
            return;
        } else {
            try printHelp(args[0]);
            std.debug.panic("unrecognized command line argument: {s}", .{arg});
        }
    }

    if (builtin.fuzz) {
        @compileError("fuzz not allowed");
    }

    var stdout_buffer: [tty.buffer_length]u8 = undefined;
    var stdout = tty.StdoutWriter.create(&stdout_buffer);

    var stderr_buffer: [tty.buffer_length]u8 = undefined;
    var stderr = tty.StderrWriter.create(&stderr_buffer);

    try stdout.print("\r{}", .{tty.Clear.cursor_to_line_end});

    var pass: usize = 0;
    var fail: usize = 0;
    var skip: usize = 0;
    var leak: usize = 0;

    var stdout_buffer_progress: [terminal_progress.buffer_length]u8 = undefined;

    var progress_manager = terminal_progress.ProgressManager.init(&stdout_buffer_progress, @intCast(builtin.test_functions.len));

    try progress_manager.start();

    for (builtin.test_functions) |test_fn| {
        std.testing.allocator_instance = .{};
        var status = Status.pass;

        if (filter) |flt| {
            if (std.mem.indexOf(u8, test_fn.name, flt) == null) {
                continue;
            }
        }

        try stdout.print(
            "{}Testing {}{s}{}: {}",
            .{ tty.FormatColorSimple.Cyan, tty.Style{ .foreground = .Magenta, .font_style = .{ .bold = true } }, test_fn.name, tty.Style{ .foreground = .Cyan, .font_style = .{ .bold = false } }, tty.Reset{} },
        );
        const result = test_fn.func();
        try progress_manager.finishOne();

        if (std.testing.allocator_instance.deinit() == .leak) {
            leak += 1;
            try stderr.print("{}\n{s}\n\"{}{s}{}\" - {}Memory Leak{}\n{s}\n{}", .{
                getColor(.fail),
                BORDER,
                tty.Style{ .foreground = .Magenta, .font_style = .{ .bold = true } },
                test_fn.name,
                tty.Style{ .foreground = .Red, .font_style = .{ .bold = false } },
                tty.FormatColorSimple.Yellow,
                tty.FormatColorSimple.Red,
                BORDER,
                tty.Reset{},
            });
        }

        if (result) |_| {
            pass += 1;
        } else |err| {
            switch (err) {
                error.SkipZigTest => {
                    skip += 1;
                    status = .skip;
                },
                else => {
                    status = .fail;
                    fail += 1;
                    try stderr.print(
                        "{}\n{s}\n\"{}{s}{}\" - {}{s}{}\n{s}\n{}",
                        .{
                            getColor(.fail),
                            BORDER,
                            tty.Style{ .foreground = .Magenta, .font_style = .{ .bold = true } },
                            test_fn.name,
                            tty.Style{ .foreground = .Red, .font_style = .{ .bold = false } },
                            tty.FormatColorSimple.Yellow,
                            @errorName(err),
                            tty.FormatColorSimple.Red,
                            BORDER,
                            tty.Reset{},
                        },
                    );

                    if (@errorReturnTrace()) |trace| {
                        std.debug.dumpStackTrace(trace.*);
                    }
                    if (fail_first) {
                        break;
                    }
                },
            }
        }

        {
            switch (status) {
                .fail => try stderr.print(
                    "{}[{}{s}{}]{}\n",
                    .{
                        tty.FormatColorSimple.Yellow,
                        getColor(status),
                        @tagName(status),
                        tty.FormatColorSimple.Yellow,
                        tty.Reset{},
                    },
                ),

                else => try stdout.print("{}[{}{s}{}]{}\n", .{
                    tty.FormatColorSimple.Cyan,
                    getColor(status),
                    @tagName(status),
                    tty.FormatColorSimple.Cyan,
                    tty.Reset{},
                }),
            }
        }
    }

    const total_tests = pass + fail;
    const status: Status = if (fail == 0) .pass else .fail;

    {
        switch (status) {
            .fail => {
                try stderr.print("{}\n{}{d}{} of {}{d}{} test{s} passed\n{}", .{
                    getColor(status),
                    tty.FormatColorSimple.Yellow,
                    pass,
                    getColor(status),
                    tty.FormatColorSimple.Cyan,
                    total_tests,
                    getColor(status),
                    if (total_tests != 1) "s" else "",
                    tty.Reset{},
                });
            },
            else => {
                try stdout.print("{}\n{}{d}{} of {}{d}{} test{s} passed\n{}", .{
                    getColor(status),
                    tty.FormatColorSimple.Yellow,
                    pass,
                    getColor(status),
                    tty.FormatColorSimple.Cyan,
                    total_tests,
                    getColor(status),
                    if (total_tests != 1) "s" else "",
                    tty.Reset{},
                });
            },
        }
    }

    if (skip > 0) {
        try stdout.print("{}{}{d}{} test{s} skipped\n{}", .{
            getColor(.skip),
            tty.FormatColorSimple.Yellow,
            skip,
            getColor(.skip),
            if (skip != 1) "s" else "",
            tty.Reset{},
        });
    }
    if (leak > 0) {
        try stderr.print(
            "{}{}{d}{} test{s} leaked\n{}",
            .{
                getColor(.fail),
                tty.FormatColorSimple.Yellow,
                leak,
                getColor(.fail),
                if (leak != 1) "s" else "",
                tty.Reset{},
            },
        );
    }

    try progress_manager.end();

    std.process.exit(if (fail == 0) 0 else 1);
}
