const std = @import("std");
const tty = @import("tty");
const terminal_progress = @import("terminal_progress");

pub const Solution = union(enum) {
    u64: u64,
    string: []const u8,

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

const SolveFn = *const fn (allocator: std.mem.Allocator, input: []const u8) SolveResult;

const SolveFnBoth = *const fn (allocator: std.mem.Allocator, input: []const u8, which: WhichPart) SolveResult;

const IndividualSolver = struct {
    first: SolveFn,
    second: SolveFn,
};

pub const Solver = union(enum) { both: SolveFnBoth, individual: IndividualSolver };

pub const Solutions = struct {
    solution: Solution,
    real_value: ?Solution = null,
};

pub const SolutionsWrapper = union(enum) { pending, implemented: Solutions };

pub const BothSolutions = struct { first: SolutionsWrapper, second: SolutionsWrapper };

pub const SingleCustomInput = struct {
    input: []const u8,
    example_input: []const u8,
};

pub const CustomInputs = struct {
    first: SingleCustomInput,
    second: SingleCustomInput,
};

pub const Inputs = union(enum) {
    default,
    both_same,
    custom: CustomInputs,
};

pub const WhichPart = enum(u1) {
    first = 0,
    second = 1,
};

const FileType = enum(u1) {
    FileTypeNormal,
    FileTypeExample,
};

const day_fmt = "{}[Day {}{d}{}]{}";

fn getDayFmtArgs(day: u32, style_after: tty.Style) struct { tty.FormatColorSimple, tty.FormatColorSimple, u32, tty.FormatColorSimple, tty.Style } {
    const day_color: tty.FormatColorSimple = tty.FormatColorSimple.Yellow;

    return .{ day_color, tty.FormatColorSimple.Cyan, day, day_color, style_after };
}

const FileGetResult = union(enum) {
    found: []const u8,
    not_found: []const u8,
};

pub const DayOptions = struct {
    profile: bool = false,
};

pub const Day = struct {
    solver: Solver,
    solutions: BothSolutions,
    inputs: Inputs,
    root: []const u8,
    num: u32,

    fn solve(self: *const Day, allocator: std.mem.Allocator, input: []const u8, which: WhichPart) !Solution {
        switch (self.solver) {
            .both => |func| return func(allocator, input, which),
            .individual => |individual| {
                switch (which) {
                    .first => return individual.first(allocator, input),
                    .second => return individual.second(allocator, input),
                }
            },
        }
    }

    fn getSolutions(example: SolutionsWrapper) ?Solutions {
        switch (example) {
            .pending => return null,
            .implemented => |sol| {
                return sol;
            },
        }
    }

    fn readFileAbs(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
        var opened_file = try std.fs.openFileAbsolute(path, .{ .mode = .read_only });
        defer opened_file.close();

        var buf: [4096]u8 = undefined;

        var file_reader = opened_file.reader(&buf);
        const reader = &file_reader.interface;

        return try reader.allocRemaining(allocator, .unlimited);
    }

    fn getFileName(self: *const Day, allocator: std.mem.Allocator, file_type: FileType, which: WhichPart) ![]const u8 {
        const file_name = blk: {
            switch (self.inputs) {
                .default => {
                    switch (file_type) {
                        .FileTypeNormal => {
                            const file_name: []const u8 = if (which == .first) "input_01.txt" else "input_02.txt";
                            break :blk file_name;
                        },
                        .FileTypeExample => {
                            const file_name: []const u8 = if (which == .first) "example_01.txt" else "example_02.txt";
                            break :blk file_name;
                        },
                    }
                },
                .both_same => {
                    switch (file_type) {
                        .FileTypeNormal => {
                            break :blk "input.txt";
                        },
                        .FileTypeExample => {
                            break :blk "example.txt";
                        },
                    }
                },
                .custom => |custom_inp| {
                    switch (file_type) {
                        .FileTypeNormal => {
                            const file_name: []const u8 = if (which == .first) custom_inp.first.input else custom_inp.second.input;
                            break :blk file_name;
                        },
                        .FileTypeExample => {
                            const file_name: []const u8 = if (which == .first) custom_inp.first.example_input else custom_inp.second.example_input;
                            break :blk file_name;
                        },
                    }
                },
            }
        };

        const file = if (std.fs.path.isAbsolute(file_name)) try allocator.dupe(u8, file_name) else try std.fs.path.join(allocator, &[_][]const u8{ self.root, file_name });

        std.debug.assert(std.fs.path.isAbsolute(file));

        return file;
    }

    fn getFile(self: *const Day, allocator: std.mem.Allocator, file_type: FileType, which: WhichPart) !FileGetResult {
        const file_name = try self.getFileName(allocator, file_type, which);
        errdefer allocator.free(file_name);

        const file = if (std.fs.path.isAbsolute(file_name)) try allocator.dupe(u8, file_name) else try std.fs.path.join(allocator, &[_][]const u8{ self.root, file_name });
        defer allocator.free(file);

        std.debug.assert(std.fs.path.isAbsolute(file));

        const result = readFileAbs(allocator, file) catch |err| blk: {
            if (err != error.FileNotFound) {
                return err;
            }
            break :blk null;
        };

        if (result) |r| {
            allocator.free(file_name);

            return FileGetResult{ .found = r };
        } else {
            return FileGetResult{ .not_found = file_name };
        }
    }

    fn getNormalFile(self: *const Day, allocator: std.mem.Allocator, which: WhichPart) !FileGetResult {
        return self.getFile(allocator, .FileTypeNormal, which);
    }

    fn getExampleFile(self: *const Day, allocator: std.mem.Allocator, which: WhichPart) !FileGetResult {
        return self.getFile(allocator, .FileTypeExample, which);
    }

    fn printErrorStr(which: WhichPart, is_normal: bool, num: u32, comptime fmt: []const u8, args: anytype) !void {
        const part = if (which == .first) "1" else "2";
        const type_ = if (is_normal) "Part" else "Example";

        try tty.StderrWriter.global().printWithDefaultColor(day_fmt ++ " {s} {s}: {}" ++ fmt ++ "\n", getDayFmtArgs(num, tty.Style{ .foreground = .Red }) ++ .{ type_, part, tty.Style{ .foreground = .Red, .font_style = .{ .bold = true } } } ++ args);
    }

    fn printErrorImpl(which: WhichPart, is_normal: bool, err: SolveErrors, num: u32) !void {
        switch (err) {
            error.PredicateNotMet => {
                try Day.printErrorStr(which, is_normal, num, "predicate not met", .{});
                return;
            },
            error.ParseError => {
                try Day.printErrorStr(which, is_normal, num, "parse error", .{});
                return err;
            },
            error.NotSolved => {
                try Day.printErrorStr(which, is_normal, num, "not solved", .{});
                return;
            },
            error.OutOfMemory => {
                try Day.printErrorStr(which, is_normal, num, "OutOfMemory", .{});
                return;
            },
            error.OtherError => {
                try Day.printErrorStr(which, is_normal, num, "other error", .{});
                return;
            },
        }

        unreachable;
    }

    fn printError(self: *const Day, which: WhichPart, is_normal: bool, err: SolveErrors) !void {
        return Day.printErrorImpl(which, is_normal, err, self.num);
    }

    fn printResult(self: *const Day, which: WhichPart, solution: Solution) !void {
        const part = if (which == .first) "1" else "2";

        try tty.StdoutWriter.global().printWithDefaultColor(day_fmt ++ " Solution for part {}{s}{} is: {}{f}\n", getDayFmtArgs(self.num, tty.Style{ .foreground = .Green }) ++ .{ tty.Style{ .foreground = .Cyan, .font_style = .{ .bold = true } }, part, tty.Style{ .foreground = .Green }, tty.Style{ .foreground = .Magenta, .font_style = .{ .bold = true } }, solution });
    }

    pub fn run(self: *const Day, allocator: std.mem.Allocator) !void {
        return self.runAdvanced(allocator, null, null);
    }

    pub fn runAdvanced(self: *const Day, allocator: std.mem.Allocator, options: ?DayOptions, progress_sub_manager: ?*terminal_progress.ProgressSubManager) !void {
        if (progress_sub_manager) |p| {
            try self.runImpl(allocator, options, p);
            return;
        }

        var stdout_buffer: [terminal_progress.buffer_length]u8 = undefined;

        var progress_manager = terminal_progress.ProgressManager.init(&stdout_buffer, 1);

        try progress_manager.start();

        var sub_manager = progress_manager.sub_manager();

        try self.runImpl(allocator, options, &sub_manager);

        try sub_manager.end();
        try progress_manager.end();
    }

    fn runImpl(self: *const Day, allocator: std.mem.Allocator, options: ?DayOptions, progress_sub_manager: *terminal_progress.ProgressSubManager) !void {
        var profile: bool = false;

        if (options) |opt| {
            profile = opt.profile;
        }

        var tracker: Tracker = Tracker.init();

        //TODO. collect the two things in advance, so that the progress manager knows, the amount of progress in advance
        var sub_node: terminal_progress.ProgressNode = progress_sub_manager.start(0);

        {
            const file_1 = try self.getNormalFile(allocator, .first);

            switch (file_1) {
                .found => |input_1| {
                    defer allocator.free(input_1);

                    tracker.startTiming();
                    try sub_node.addItems(1);

                    const solution_1 = self.solve(allocator, input_1, .first) catch |err| {
                        try self.printError(.first, true, err);
                        return;
                    };

                    const tracker_result = tracker.endTiming();
                    if (profile) {
                        try tracker_result.display();
                    }
                    try sub_node.completeOne();

                    try self.printResult(.first, solution_1);
                },
                .not_found => |file_name| {
                    defer allocator.free(file_name);
                    try tty.StderrWriter.global().printWithDefaultColor(day_fmt ++ " File for part 1 not found: {s}\n", getDayFmtArgs(self.num, tty.Style{ .foreground = .Red }) ++ .{file_name});
                },
            }
        }

        {
            const file_2 = try self.getNormalFile(allocator, .second);

            switch (file_2) {
                .found => |input_2| {
                    defer allocator.free(input_2);

                    tracker.startTiming();
                    try sub_node.addItems(1);

                    const solution_2 = self.solve(allocator, input_2, .second) catch |err| {
                        try self.printError(.second, true, err);
                        return;
                    };

                    const tracker_result = tracker.endTiming();
                    if (profile) {
                        try tracker_result.display();
                    }
                    try sub_node.completeOne();

                    try self.printResult(.second, solution_2);
                },
                .not_found => |file_name| {
                    defer allocator.free(file_name);
                    try tty.StderrWriter.global().printWithDefaultColor(day_fmt ++ " File for part 2 not found: {s}\n", getDayFmtArgs(self.num, tty.Style{ .foreground = .Red }) ++ .{file_name});
                },
            }
        }

        try progress_sub_manager.end();
    }

    pub fn testExample(allocator: std.mem.Allocator, solveFn: SolveFn, solution: Solution, root: []const u8, file_name: []const u8) !void {
        const file = if (std.fs.path.isAbsolute(file_name)) try allocator.dupe(u8, file_name) else try std.fs.path.join(allocator, &[_][]const u8{ root, file_name });
        defer allocator.free(file);

        std.debug.assert(std.fs.path.isAbsolute(file));

        const result = readFileAbs(allocator, file) catch |err| blk: {
            if (err != error.FileNotFound) {
                return err;
            }
            break :blk null;
        };

        if (result) |input| {
            defer allocator.free(input);

            const solution_1 = solveFn(allocator, input) catch |err| {
                try Day.printErrorImpl(.first, false, err, 0);
                try std.testing.expect(false);
                return;
            };

            try std.testing.expectEqual(solution, solution_1);
        } else {
            defer allocator.free(file_name);
            try tty.StderrWriter.global().printWithDefaultColor(day_fmt ++ " File for example 1 not found: {s}\n", getDayFmtArgs(0, tty.Style{ .foreground = .Red }) ++ .{file_name});
        }
    }

    pub fn @"test"(self: *const Day, allocator: std.mem.Allocator) !void {
        {
            const solutions_1m = Day.getSolutions(self.solutions.first);

            if (solutions_1m) |solutions_1| {
                const file_1 = try self.getExampleFile(allocator, .first);
                switch (file_1) {
                    .found => |input_1| {
                        defer allocator.free(input_1);

                        const solution_1 = self.solve(allocator, input_1, .first) catch |err| {
                            try self.printError(.first, false, err);
                            try std.testing.expect(false);
                            return;
                        };

                        try std.testing.expectEqual(solutions_1.solution, solution_1);
                    },
                    .not_found => |file_name| {
                        defer allocator.free(file_name);
                        try tty.StderrWriter.global().printWithDefaultColor(day_fmt ++ " File for example 1 not found: {s}\n", getDayFmtArgs(self.num, tty.Style{ .foreground = .Red }) ++ .{file_name});
                    },
                }
            }
        }

        {
            const solutions_2m = Day.getSolutions(self.solutions.second);

            if (solutions_2m) |solutions_2| {
                const file_2 = try self.getExampleFile(allocator, .second);
                switch (file_2) {
                    .found => |input_2| {
                        defer allocator.free(input_2);

                        const solution_2 = self.solve(allocator, input_2, .second) catch |err| {
                            try self.printError(.second, false, err);
                            try std.testing.expect(false);
                            return;
                        };

                        try std.testing.expectEqual(solutions_2.solution, solution_2);
                    },
                    .not_found => |file_name| {
                        defer allocator.free(file_name);
                        try tty.StderrWriter.global().printWithDefaultColor(day_fmt ++ " File for example 2 not found: {s}\n", getDayFmtArgs(self.num, tty.Style{ .foreground = .Red }) ++ .{file_name});
                    },
                }
            }
        }

        // "real" solutions, to test everything again!

        {
            const solutions_1m = Day.getSolutions(self.solutions.first);

            if (solutions_1m) |solutions_1| {
                if (solutions_1.real_value) |real_sol| {
                    const file_1 = try self.getNormalFile(allocator, .first);
                    switch (file_1) {
                        .found => |input_1| {
                            defer allocator.free(input_1);

                            const solution_1 = self.solve(allocator, input_1, .first) catch |err| {
                                try self.printError(.first, false, err);
                                try std.testing.expect(false);
                                return;
                            };

                            try std.testing.expectEqual(real_sol, solution_1);
                        },
                        .not_found => |file_name| {
                            defer allocator.free(file_name);
                            try tty.StderrWriter.global().printWithDefaultColor(day_fmt ++ " File for part 1 not found: {s}\n", getDayFmtArgs(self.num, tty.Style{ .foreground = .Red }) ++ .{file_name});
                        },
                    }
                }
            }
        }

        {
            const solutions_2m = Day.getSolutions(self.solutions.second);

            if (solutions_2m) |solutions_2| {
                if (solutions_2.real_value) |real_sol| {
                    const file_2 = try self.getNormalFile(allocator, .second);
                    switch (file_2) {
                        .found => |input_2| {
                            defer allocator.free(input_2);

                            const solution_2 = self.solve(allocator, input_2, .second) catch |err| {
                                try self.printError(.second, false, err);
                                try std.testing.expect(false);
                                return;
                            };

                            try std.testing.expectEqual(real_sol, solution_2);
                        },
                        .not_found => |file_name| {
                            defer allocator.free(file_name);
                            try tty.StderrWriter.global().printWithDefaultColor(day_fmt ++ " File for part 2 not found: {s}\n", getDayFmtArgs(self.num, tty.Style{ .foreground = .Red }) ++ .{file_name});
                        },
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
            try tty.StdoutWriter.global().printWithDefaultColor("Took: {}{d:.2}{} ms\n", .{
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
