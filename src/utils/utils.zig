const std = @import("std");
const tty = @import("tty");
const day = @import("day.zig");
const stack = @import("stack.zig");

// Add utility functions here

pub const Allocator = std.mem.Allocator;
pub const List = std.ArrayList;
pub fn ListManaged(comptime T: type) type {
    return std.array_list.AlignedManaged(T, null);
}
pub const Map = std.AutoHashMap;
pub const StrMap = std.StringHashMap;
pub const BitSet = std.DynamicBitSet;
pub const Str = []const u8;

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

pub const Day = day.Day;
pub const Solver = day.Solver;
pub const SolveResult = day.SolveResult;
pub const SolveErrors = day.SolveErrors;
pub const Solution = day.Solution;
pub const DayOptions = day.DayOptions;

pub const StackManaged = stack.StackManaged;
