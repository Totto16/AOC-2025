// Modded from from https://gist.github.com/karlseguin/c6bea5b35e4e8d26af6f81c22cb5d76b by nurpax
// By me: from https://gist.github.com/nurpax/4afcb6e4ef3f03f0d282f7c462005f12

// LIECENSE: MIT

// Note: doesn't support std.testing.fuzz()

const std = @import("std");
const builtin = @import("builtin");
const tty = @import("tty");

const BORDER = "=" ** 80;

const Status = enum {
    pass,
    fail,
    skip,
    text,
};

fn printHelp(program: []const u8) void {
    //TODO
    _ = program;
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
    defer alloc.free(args);

    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "--fail-first")) {
            fail_first = true;
        } else if (std.mem.startsWith(u8, arg, "--filter=")) {
            filter = arg["--filter=".len..];
        } else if (std.mem.startsWith(u8, arg, "--help")) {
            printHelp(args[0]);
            return;
        } else {
            printHelp(args[0]);
            @panic("unrecognized command line argument");
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
                        getColor(status),
                        tty.FormatColorSimple.Yellow,
                        @tagName(status),
                        getColor(status),
                        tty.Reset{},
                    },
                ),

                else => try stdout.print("{}[{}{s}{}]{}\n", .{
                    getColor(status),
                    tty.FormatColorSimple.Cyan,
                    @tagName(status),
                    getColor(status),
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
    std.process.exit(if (fail == 0) 0 else 1);
}
