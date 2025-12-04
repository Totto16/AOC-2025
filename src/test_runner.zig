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

fn getColor(status: Status) tty.Color {
    return switch (status) {
        .pass => tty.Color.Green,
        .fail => tty.Color.Red,
        .skip => tty.Color.Yellow,
        else => tty.Color.Default,
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

    var stdout = tty.StdoutWriter.create();
    var stderr = tty.StderrWriter.create();

    try stdout.printRaw("\r{any}", .{tty.Clear.cursor_to_line_end}, null);

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

        try stdout.printRaw("Testing {any}{s}{any}: ", .{
            tty.Style{ .foreground = tty.Color.Magenta, .font_style = .{ .bold = true } },
            test_fn.name,
            tty.Style{ .foreground = tty.Color.Cyan, .font_style = .{ .bold = false } },
        }, tty.Color.Cyan);
        const result = test_fn.func();

        if (std.testing.allocator_instance.deinit() == .leak) {
            leak += 1;
            try stderr.printRaw("\n{s}\n\"{any}{s}{any}\" - {any}Memory Leak{any}\n{s}\n", .{
                BORDER,
                tty.Style{ .foreground = tty.Color.Magenta, .font_style = .{ .bold = true } },
                test_fn.name,
                tty.Style{ .foreground = tty.Color.Red, .font_style = .{ .bold = false } },
                tty.Color.Yellow,
                tty.Color.Red,
                BORDER,
            }, getColor(.fail));
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
                    try stderr.printRaw("\n{s}\n\"{any}{s}{any}\" - {any}{s}{any}\n{s}\n", .{
                        BORDER,
                        tty.Style{ .foreground = tty.Color.Magenta, .font_style = .{ .bold = true } },
                        test_fn.name,
                        tty.Style{ .foreground = tty.Color.Red, .font_style = .{ .bold = false } },
                        tty.Color.Yellow,
                        @errorName(err),
                        tty.Color.Red,
                        BORDER,
                    }, getColor(.fail));

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
                .fail => try stderr.printRaw("[{any}{s}{any}]\n", .{ tty.Color.Yellow, @tagName(status), getColor(status) }, getColor(status)),
                else => try stdout.printRaw("[{any}{s}{any}]\n", .{ tty.Color.Cyan, @tagName(status), getColor(status) }, getColor(status)),
            }
        }
    }

    const total_tests = pass + fail;
    const status: Status = if (fail == 0) .pass else .fail;

    {
        switch (status) {
            .fail => {
                try stderr.printRaw("\n{any}{d}{any} of {any}{d}{any} test{s} passed\n", .{ tty.Color.Yellow, pass, getColor(status), tty.Color.Cyan, total_tests, getColor(status), if (total_tests != 1) "s" else "" }, getColor(status));
            },
            else => {
                try stdout.printRaw("\n{any}{d}{any} of {any}{d}{any} test{s} passed\n", .{ tty.Color.Yellow, pass, getColor(status), tty.Color.Cyan, total_tests, getColor(status), if (total_tests != 1) "s" else "" }, getColor(status));
            },
        }
    }

    if (skip > 0) {
        try stdout.printRaw("{any}{d}{any} test{s} skipped\n", .{ tty.Color.Yellow, skip, getColor(.skip), if (skip != 1) "s" else "" }, getColor(.skip));
    }
    if (leak > 0) {
        try stderr.printRaw("{any}{d}{any} test{s} leaked\n", .{ tty.Color.Yellow, leak, getColor(.fail), if (leak != 1) "s" else "" }, getColor(.fail));
    }
    std.process.exit(if (fail == 0) 0 else 1);
}
