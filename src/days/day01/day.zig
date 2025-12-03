const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Map = std.AutoHashMap;
const StrMap = std.StringHashMap;
const BitSet = std.DynamicBitSet;

const utils = @import("utils");

fn solve(input: utils.Str) !utils.Solution {
    _ = input;
    unreachable;
}

const day = utils.Day{ .solver = utils.Solver{ .both = solve }, .examples = .{ .first = .{ .u64 = 0 }, .second = .{.todo} } };

pub fn main() !void {
    day.run();
}

test "day 01" {
    day.@"test"();
}
