const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Map = std.AutoHashMap;
const StrMap = std.StringHashMap;
const BitSet = std.DynamicBitSet;

const utils = @import("utils");

fn solve(input: utils.Str) utils.SolveResult {
    _ = input;
    unreachable;
}

const day = utils.Day{ .solver = utils.Solver{ .both = solve }, .examples = .{ .first = .{ .solution = .{ .u64 = 0 } }, .second = .todo } };

pub fn main() !void {
    try day.run();
}

test "day 01" {
    try day.@"test"();
}
