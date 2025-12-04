const std = @import("std");
pub const Allocator = std.mem.Allocator;
pub const List = std.ArrayList;
pub const ListManaged = std.array_list.AlignedManaged;
pub const Map = std.AutoHashMap;
pub const StrMap = std.StringHashMap;
pub const BitSet = std.DynamicBitSet;
pub const Str = []const u8;
const tty = @import("tty");

// Add utility functions here

// Useful stdlib functions
pub const tokenizeAny = std.mem.tokenizeAny;
pub const tokenizeSeq = std.mem.tokenizeSequence;
pub const tokenizeSca = std.mem.tokenizeScalar;
pub const splitAny = std.mem.splitAny;
pub const splitSeq = std.mem.splitSequence;
pub const splitSca = std.mem.splitScalar;
pub const indexOf = std.mem.indexOfScalar;
pub const indexOfAny = std.mem.indexOfAny;
pub const indexOfStr = std.mem.indexOfPosLinear;
pub const lastIndexOf = std.mem.lastIndexOfScalar;
pub const lastIndexOfAny = std.mem.lastIndexOfAny;
pub const lastIndexOfStr = std.mem.lastIndexOfLinear;
pub const trim = std.mem.trim;
pub const sliceMin = std.mem.min;
pub const sliceMax = std.mem.max;

pub const parseInt = std.fmt.parseInt;
pub const parseFloat = std.fmt.parseFloat;

pub const print = std.debug.print;
pub const assert = std.debug.assert;

pub const sort = std.sort.block;
pub const asc = std.sort.asc;
pub const desc = std.sort.desc;

pub const StdoutWriter = tty.StdoutWriter;
pub const StderrWriter = tty.StderrWriter;

pub const Solution = union(enum) {
    u64: u64,
    string: Str,

    pub fn format(
        self: Solution,
        writer: anytype,
    ) !void {
        switch (self) {
            .u64 => |num| {
                try writer.print("number {d}", .{num});
            },
            .string => |str| {
                try writer.print("string {s}", .{str});
            },
        }
    }
};

pub const SolveErrors = error{ PredicateNotMet, ParseError, NotSolved, OutOfMemory, OtherError };

pub const SolveResult = SolveErrors!Solution;

const SolveFn = *const fn (allocator: Allocator, input: Str) SolveResult;

const IndividualSolver = struct {
    first: SolveFn,
    second: SolveFn,
};

pub const Solver = union(enum) { both: SolveFn, individual: IndividualSolver };

pub const Example = struct {
    solution: Solution,
    file: ?Str = null,
    real_value: ?Solution = null,
};

pub const ExampleWrapper = union(enum) { pending, implemented: Example };

pub const Examples = struct { first: ExampleWrapper, second: ExampleWrapper };

const WhichPart = enum(u1) { first = 0, second = 1 };

const day_fmt = "{}[Day {}{d}{}]{}";

fn getDayFmtArgs(day: u32, style_after: tty.Style) struct { tty.FormatColorSimple, tty.FormatColorSimple, u32, tty.FormatColorSimple, tty.Style } {
    const day_color: tty.FormatColorSimple = tty.FormatColorSimple.Yellow;

    return .{ day_color, tty.FormatColorSimple.Cyan, day, day_color, style_after };
}

pub const DayOptions = struct {
    profile: bool = false,
};

pub const Day = struct {
    solver: Solver,
    examples: Examples,
    root: Str,
    num: u32,
    same_input: bool = false,

    fn solve(self: *const Day, allocator: Allocator, input: Str, which: WhichPart) !Solution {
        switch (self.solver) {
            .both => |f| return f(allocator, input),
            .individual => |individual| {
                switch (which) {
                    .first => return individual.first(allocator, input),
                    .second => return individual.second(allocator, input),
                }
            },
        }
    }

    fn getExample(example: ExampleWrapper) ?Example {
        switch (example) {
            .pending => return null,
            .implemented => |i| {
                return i;
            },
        }
    }

    fn readFileAbs(allocator: Allocator, path: []const u8) ![]u8 {
        var opened_file = try std.fs.openFileAbsolute(path, .{ .mode = .read_only });
        defer opened_file.close();

        var buf: [4096]u8 = undefined;

        var file_reader = opened_file.reader(&buf);
        const reader = &file_reader.interface;

        return try reader.allocRemaining(allocator, .unlimited);
    }

    fn getExampleFileName(self: *const Day, allocator: Allocator, example: Example, which: WhichPart) !Str {
        const file_name: Str = if (example.file) |f| f else (if (which == .first) "example_01.txt" else "example_02.txt");

        const file = if (std.fs.path.isAbsolute(file_name)) try allocator.dupe(u8, file_name) else try std.fs.path.join(allocator, &[_]Str{ self.root, file_name });

        std.debug.assert(std.fs.path.isAbsolute(file));

        return file;
    }

    fn getNormalFile(self: *const Day, allocator: Allocator, which: WhichPart) !?Str {
        const file_name: Str = if (which == .first) "input_01.txt" else "input_02.txt";

        const file = if (std.fs.path.isAbsolute(file_name)) try allocator.dupe(u8, file_name) else try std.fs.path.join(allocator, &[_]Str{ self.root, file_name });
        defer allocator.free(file);

        std.debug.assert(std.fs.path.isAbsolute(file));

        const result = readFileAbs(allocator, file) catch |err| blk: {
            if (err != error.FileNotFound) {
                return err;
            }
            break :blk null;
        };

        return result;
    }

    fn printErrorStr(self: *const Day, which: WhichPart, is_normal: bool, comptime fmt: []const u8, args: anytype) !void {
        const part = if (which == .first) "1" else "2";
        const type_ = if (is_normal) "Part" else "Example";

        try StderrWriter.printOnceWithDefaultColor(day_fmt ++ " {s} {s}: {}" ++ fmt ++ "\n", getDayFmtArgs(self.num, tty.Style{ .foreground = .Red }) ++ .{ type_, part, tty.Style{ .foreground = .Red, .font_style = .{ .bold = true } } } ++ args);
    }

    fn printError(self: *const Day, which: WhichPart, is_normal: bool, err: SolveErrors) !void {
        switch (err) {
            error.PredicateNotMet => {
                try self.printErrorStr(which, is_normal, "predicate not met", .{});
                return;
            },
            error.ParseError => {
                try self.printErrorStr(which, is_normal, "parse error", .{});
                return err;
            },
            error.NotSolved => {
                try self.printErrorStr(which, is_normal, "not solved", .{});
                return;
            },
            error.OutOfMemory => {
                try self.printErrorStr(which, is_normal, "OutOfMemory", .{});
                return;
            },
            error.OtherError => {
                try self.printErrorStr(which, is_normal, "other error", .{});
                return;
            },
        }

        unreachable;
    }

    fn printResult(self: *const Day, which: WhichPart, solution: Solution) !void {
        const part = if (which == .first) "1" else "2";

        try StdoutWriter.printOnceWithDefaultColor(day_fmt ++ " Solution for part {}{s}{} is: {}{f}\n", getDayFmtArgs(self.num, tty.Style{ .foreground = .Green }) ++ .{ tty.Style{ .foreground = .Cyan, .font_style = .{ .bold = true } }, part, tty.Style{ .foreground = .Green }, tty.Style{ .foreground = .Magenta, .font_style = .{ .bold = true } }, solution });
    }

    pub fn run(self: *const Day, allocator: Allocator) !void {
        return self.runAdvanced(allocator, null, null);
    }

    pub fn runAdvanced(self: *const Day, allocator: Allocator, options: ?DayOptions, progress_node: ?std.Progress.Node) !void {
        var profile: bool = false;

        if (options) |opt| {
            profile = opt.profile;
        }

        var tracker: Tracker = Tracker.init();

        var sub_node: ?std.Progress.Node = null;

        if (progress_node) |p| {
            const dayStr = try std.fmt.allocPrint(allocator, "run day {d}", .{self.num});
            sub_node = p.start(dayStr, 0);
            defer allocator.free(dayStr);
        }

        //TODO. also allow customizations of normal file paths
        {
            const file_1 = try self.getNormalFile(allocator, .first);

            if (file_1) |input_1| {
                defer allocator.free(input_1);

                tracker.startTiming();
                if (sub_node) |s| s.increaseEstimatedTotalItems(1);

                const solution_1 = self.solve(allocator, input_1, .first) catch |err| {
                    try self.printError(.first, true, err);
                    return;
                };

                const tracker_result = tracker.endTiming();
                if (profile) {
                    try tracker_result.display();
                }
                if (sub_node) |s| s.completeOne();

                try self.printResult(.first, solution_1);
            } else {
                try StderrWriter.printOnceWithDefaultColor(day_fmt ++ " No file for part 1 found\n", getDayFmtArgs(self.num, tty.Style{ .foreground = .Red }) ++ .{});
            }
        }

        {
            const which_one: WhichPart = if (self.same_input) .first else .second;
            const file_2 = try self.getNormalFile(allocator, which_one);

            if (file_2) |input_2| {
                defer allocator.free(input_2);

                tracker.startTiming();
                if (sub_node) |s| s.increaseEstimatedTotalItems(1);

                const solution_2 = self.solve(allocator, input_2, .second) catch |err| {
                    try self.printError(.second, true, err);
                    return;
                };

                const tracker_result = tracker.endTiming();
                if (profile) {
                    try tracker_result.display();
                }
                if (sub_node) |s| s.completeOne();

                try self.printResult(.second, solution_2);
            } else {
                try StderrWriter.printOnceWithDefaultColor(day_fmt ++ " No file for part 2 found\n", getDayFmtArgs(self.num, tty.Style{ .foreground = .Red }) ++ .{});
            }
        }

        if (sub_node) |s| s.end();
    }

    pub fn @"test"(self: *const Day, allocator: Allocator) !void {
        {
            const example_1 = Day.getExample(self.examples.first);

            if (example_1) |ex1| {
                const file_1 = try self.getExampleFileName(allocator, ex1, .first);
                defer allocator.free(file_1);

                const input_1 = readFileAbs(allocator, file_1) catch |err| {
                    if (err == error.FileNotFound) {
                        try self.printErrorStr(.first, false, "File not found: '{s}'", .{file_1});
                        try std.testing.expect(false);
                        return;
                    }
                    return err;
                };
                defer allocator.free(input_1);

                const solution_1 = self.solve(allocator, input_1, .first) catch |err| {
                    try self.printError(.first, false, err);
                    try std.testing.expect(false);
                    return;
                };

                try std.testing.expectEqual(ex1.solution, solution_1);
            }
        }

        {
            const example_2 = Day.getExample(self.examples.second);

            if (example_2) |ex2| {
                const which_one: WhichPart = if (self.same_input) .first else .second;
                const file_2 = try self.getExampleFileName(allocator, ex2, which_one);
                defer allocator.free(file_2);

                const input_2 = readFileAbs(allocator, file_2) catch |err| {
                    if (err == error.FileNotFound) {
                        try self.printErrorStr(.second, false, "File not found: '{s}'", .{file_2});
                        try std.testing.expect(false);
                        return;
                    }
                    return err;
                };
                defer allocator.free(input_2);

                const solution_2 = self.solve(allocator, input_2, .second) catch |err| {
                    try self.printError(.second, false, err);
                    try std.testing.expect(false);
                    return;
                };

                try std.testing.expectEqual(ex2.solution, solution_2);
            }
        }

        // "real" solutions, to test everything again!

        {
            const example_1 = Day.getExample(self.examples.first);

            if (example_1) |ex1| {
                if (ex1.real_value) |real_sol| {
                    const file_1 = try self.getNormalFile(allocator, .first);
                    if (file_1) |input_1| {
                        defer allocator.free(input_1);

                        const solution_1 = self.solve(allocator, input_1, .first) catch |err| {
                            try self.printError(.first, false, err);
                            try std.testing.expect(false);
                            return;
                        };

                        try std.testing.expectEqual(real_sol, solution_1);
                    } else {
                        try StderrWriter.printOnceWithDefaultColor(day_fmt ++ " No file for part 1 found\n", getDayFmtArgs(self.num, tty.Style{ .foreground = .Red }) ++ .{});
                        try std.testing.expect(false);
                    }
                }
            }
        }

        {
            const example_2 = Day.getExample(self.examples.second);

            if (example_2) |ex2| {
                if (ex2.real_value) |real_sol| {
                    const which_one: WhichPart = if (self.same_input) .first else .second;
                    const file_2 = try self.getNormalFile(allocator, which_one);
                    if (file_2) |input_2| {
                        defer allocator.free(input_2);

                        const solution_2 = self.solve(allocator, input_2, .second) catch |err| {
                            try self.printError(.second, false, err);
                            try std.testing.expect(false);
                            return;
                        };

                        try std.testing.expectEqual(real_sol, solution_2);
                    } else {
                        try StderrWriter.printOnceWithDefaultColor(day_fmt ++ " No file for part 2 found\n", getDayFmtArgs(self.num, tty.Style{ .foreground = .Red }) ++ .{});
                        try std.testing.expect(false);
                    }
                }
            }
        }
    }
};

//from: https://gist.github.com/karlseguin/c6bea5b35e4e8d26af6f81c22cb5d76b/1f317ebc9cd09bc50fd5591d09c34255e15d1d85
// modified heavily
const Tracker = struct {
    timer: std.time.Timer,

    fn init() Tracker {
        const timer = std.time.Timer.start() catch @panic("failed to start timer");

        return .{
            .timer = timer,
        };
    }

    const TestInfo = struct {
        ns: u64,
        fn display(self: *const TestInfo) !void {
            const ms = @as(f64, @floatFromInt(self.ns)) / 1_000_000.0;
            try StdoutWriter.printOnceWithDefaultColor("Took: {}{d:.2}{} ms\n", .{
                tty.Style{ .foreground = .Cyan, .font_style = .{ .bold = true } },
                ms,
                tty.Style{ .foreground = .Blue },
            });
        }
    };

    fn startTiming(self: *Tracker) void {
        self.timer.reset();
    }

    fn endTiming(self: *Tracker) TestInfo {
        var timer = self.timer;
        const ns = timer.lap();
        return TestInfo{ .ns = ns };
    }

    fn compareTiming(context: void, a: TestInfo, b: TestInfo) std.math.Order {
        _ = context;
        return std.math.order(a.ns, b.ns);
    }
};
