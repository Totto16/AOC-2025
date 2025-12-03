const std = @import("std");

const utils = @import("utils");

fn solve(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    _ = allocator;

    var dial: u8 = 50;

    var iter = utils.splitSeq(u8, input, "\n");

    while (iter.next()) |line| {
        std.debug.print("here {s}\n", .{line});
        dial += 1;
    }

    return error.NotSolved;
}

const day = utils.Day{ .solver = utils.Solver{ .both = solve }, .examples = .{ .first = .{ .implemented = .{ .solution = .{ .u64 = 0 } } }, .second = .todo }, .root = @import("generated").root };

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try day.run(gpa.allocator());
}

test "day 01" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try day.@"test"(gpa.allocator());
}
