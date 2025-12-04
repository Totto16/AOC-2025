const std = @import("std");
const utils = @import("utils");
const tty = @import("tty");

const Options = struct {
    day: ?u32,
};

fn printHelp(program: []const u8) !void {
    var stdout_buffer: [tty.buffer_length]u8 = undefined;
    var stdout = tty.StdoutWriter.create(&stdout_buffer);

    try stdout.print("Usage: {s} [options]\n", .{program});
    try stdout.print("\t--day=<day>: specify the day to run\n", .{});
    try stdout.print("\t--help, -h, -?: print this help\n", .{});
}

fn parseOptions(alloc: utils.Allocator) !Options {
    const args = std.process.argsAlloc(alloc) catch
        @panic("unable to parse command line args");
    defer std.process.argsFree(alloc, args);

    var options: Options = Options{ .day = null };

    for (args[1..]) |arg| {
        if (std.mem.startsWith(u8, arg, "--day=")) {
            const day_str = arg["--day=".len..];
            options.day = std.fmt.parseUnsigned(u32, day_str, 0) catch {
                std.debug.panic("unable to parse --day command line argument: {s}", .{day_str});
            };
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "-?")) {
            try printHelp(args[0]);
            std.process.exit(0);
        } else {
            try printHelp(args[0]);
            std.debug.panic("unrecognized command line argument: {s}", .{arg});
        }
    }

    return options;
}

fn runDay(day: utils.Day, alloc: utils.Allocator) !void {
    day.run(alloc);
}

const days = @import("main_helper").days;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const options = try parseOptions(gpa.allocator());

    if (options.day) |got_day| {
        if (got_day == 0) {
            for (days) |day| {
                runDay(day, gpa.allocator());
            }
            return;
        }

        for (days) |day| {
            if (day.day == options.day) {
                runDay(day, gpa.allocator());
                return;
            }
        }

        std.debug.panic("invalid day: {}", .{got_day});
        return error.NoSuchDay;
    } else {
        for (days) |day| {
            runDay(day, gpa.allocator());
        }
        return;
    }
}
