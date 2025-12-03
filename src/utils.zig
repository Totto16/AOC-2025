const std = @import("std");
pub const Allocator = std.mem.Allocator;
pub const List = std.ArrayList;
pub const Map = std.AutoHashMap;
pub const StrMap = std.StringHashMap;
pub const BitSet = std.DynamicBitSet;
pub const Str = []const u8;

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

pub const Solution = union(enum) { u64: u64, string: Str };

pub const SolveErrors = error{ PredicateNotMet, ParseError, OtherError };

pub const SolveResult = SolveErrors!Solution;

const SolveFn = fn (allocator: Allocator, input: Str) SolveResult;

const IndividualSolver = struct {
    first: SolveFn,
    second: SolveFn,
};

pub const Solver = union(enum) { both: SolveFn, individual: IndividualSolver };

pub const Example = struct {
    solution: Solution,
    file: ?Str = null,
};

pub const ExampleWrapper = union(enum) { todo, implemented: Example };

pub const Examples = struct { first: ExampleWrapper, second: ExampleWrapper };

const WhichStep = enum(u1) { first = 0, second = 1 };

pub const Day = struct {
    solver: Solver,
    examples: Examples,
    root: Str,

    fn solve(self: *const Day, allocator: Allocator, input: Str, which: WhichStep) !Solution {
        const fn_for_solving = switch (self.solver) {
            .both => |f| f,
            .individual => |individual| {
                return switch (which) {
                    .first => individual.first,
                    .second => individual.second,
                };
            },
        };

        return fn_for_solving(allocator, input);
    }

    fn getExample(example: ExampleWrapper) ?Example {
        switch (example) {
            .todo => return null,
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

    fn getExampleFile(self: *const Day, allocator: Allocator, example: Example, which: WhichStep) !Str {
        const file_name: Str = if (example.file) |f| f else (if (which == .first) "example_01.txt" else "example_02.txt");

        const file = if (std.fs.path.isAbsolute(file_name)) file_name else try std.fs.path.join(allocator, &[_]Str{ self.root, file_name });

        std.debug.assert(std.fs.path.isAbsolute(file));

        return readFileAbs(allocator, file);
    }

    pub fn run(self: *const Day, allocator: Allocator) !void {
        _ = self;
        _ = allocator;
        unreachable;
    }

    pub fn @"test"(self: *const Day, allocator: Allocator) !void {
        {
            const example_1 = Day.getExample(self.examples.first);

            if (example_1) |ex1| {
                const input = try self.getExampleFile(allocator, ex1, .first);

                const solution_1 = try self.solve(allocator, input, .first);

                try std.testing.expectEqual(solution_1, ex1.solution);
            }
        }

        {
            const example_2 = Day.getExample(self.examples.second);

            if (example_2) |ex2| {
                const input = try self.getExampleFile(allocator, ex2, .second);

                const solution_2 = try self.solve(allocator, input, .second);

                try std.testing.expectEqual(solution_2, ex2.solution);
            }
        }
    }
};
