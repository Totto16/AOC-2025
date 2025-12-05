const std = @import("std");
const utils = @import("utils");
const tty = @import("tty");

const Options = struct {
    day: ?u32,
    profile: bool = false,
};

fn printHelp(program: []const u8) !void {
    var stdout_buffer: [tty.buffer_length]u8 = undefined;
    var stdout = tty.StdoutWriter.create(&stdout_buffer);

    try stdout.print("Usage: {s} [options]\n", .{program});
    try stdout.print("\t--day=<day>: specify the day to run\n", .{});
    try stdout.print("\t--help, -h, -?: print this help\n", .{});
    try stdout.print("\t--profile, -p: profile times\n", .{});
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
        } else if (std.mem.eql(u8, arg, "--profile") or std.mem.eql(u8, arg, "-p")) {
            options.profile = true;
        } else {
            try printHelp(args[0]);
            std.debug.panic("unrecognized command line argument: {s}", .{arg});
        }
    }

    return options;
}

fn runDay(day: utils.Day, alloc: utils.Allocator, profile: bool) !void {
    var stdout_buffer: [tty.buffer_length]u8 = undefined;

    var progress_manager = tty.ProgressManager.init(&stdout_buffer, 1);

    try progress_manager.start();

    var sub_manager = progress_manager.sub_manager();

    try day.runAdvanced(alloc, utils.DayOptions{ .profile = profile }, &sub_manager);

    try sub_manager.end();
    try progress_manager.end();
}

fn runAllDays(days: std.array_list.AlignedManaged(utils.Day, null), alloc: utils.Allocator, profile: bool) !void {
    var stdout_buffer: [tty.buffer_length]u8 = undefined;

    var progress_manager = tty.ProgressManager.init(&stdout_buffer, @intCast(days.items.len));

    try progress_manager.start();
    for (days.items) |
        day,
    | {
        var sub_manager = progress_manager.sub_manager();
        defer sub_manager.end() catch @panic("End failed!");

        try day.runAdvanced(alloc, utils.DayOptions{ .profile = profile }, &sub_manager);
    }
    try progress_manager.end();
}

const main_helper = @import("main_helper");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const options = try parseOptions(gpa.allocator());

    const days: std.array_list.AlignedManaged(utils.Day, null) = try main_helper.getDays(gpa.allocator());
    defer days.deinit();

    if (options.day) |got_day| {
        if (got_day == 0) {
            try runAllDays(days, gpa.allocator(), options.profile);
            return;
        }

        for (days.items) |day| {
            if (day.num == options.day) {
                try runDay(day, gpa.allocator(), options.profile);
                return;
            }
        }

        std.debug.panic("invalid day: {}", .{got_day});
        return error.NoSuchDay;
    } else {
        try runAllDays(days, gpa.allocator(), options.profile);
        return;
    }
}
