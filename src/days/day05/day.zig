const std = @import("std");
const utils = @import("utils");

fn splitIterToArray(allocator: utils.Allocator, iter: *std.mem.SplitIterator(u8, .scalar)) utils.SolveErrors!utils.ListManaged(utils.Str, null) {
    var array: utils.ListManaged(utils.Str, null) = try utils.ListManaged(utils.Str, null).initCapacity(allocator, 10);

    while (iter.next()) |val| {
        try array.append(val);
    }

    return array;
}

const RangeInt = u64;

const IdRange = struct {
    first: RangeInt,
    last: RangeInt,

    pub fn fromStr(allocator: utils.Allocator, str: utils.Str) utils.SolveErrors!IdRange {
        if (str.len == 0) {
            return error.ParseError;
        }

        var iter = utils.splitSca(u8, str, '-');

        const values = try splitIterToArray(allocator, &iter);
        defer values.deinit();

        if (values.items.len != 2) {
            return error.ParseError;
        }

        const first: RangeInt = utils.parseInt(RangeInt, values.items[0], 10) catch {
            return error.ParseError;
        };

        const last: RangeInt = utils.parseInt(RangeInt, values.items[1], 10) catch {
            return error.ParseError;
        };

        return IdRange{ .first = first, .last = last };
    }
};

const ParsedState = struct {
    ingredientRanges: utils.ListManaged(IdRange, null),
    ingredients: utils.ListManaged(RangeInt, null),

    pub fn deinit(self: *ParsedState) void {
        self.ingredientRanges.deinit();
        self.ingredients.deinit();
    }
};

fn parseIds(allocator: utils.Allocator, input: utils.Str) utils.SolveErrors!ParsedState {
    var ingredientRanges: utils.ListManaged(IdRange, null) = try utils.ListManaged(IdRange, null).initCapacity(allocator, 10);
    errdefer ingredientRanges.deinit();

    var ingredients: utils.ListManaged(RangeInt, null) = try utils.ListManaged(RangeInt, null).initCapacity(allocator, 10);
    errdefer ingredients.deinit();

    var iter = utils.splitSca(u8, input, '\n');

    var parse_state: bool = false;

    while (iter.next()) |line| {
        if (line.len == 0) {
            parse_state = true;
            continue;
        }

        if (parse_state) {
            const number: RangeInt = utils.parseInt(RangeInt, line, 10) catch {
                return error.ParseError;
            };
            try ingredients.append(number);
        } else {
            const value = try IdRange.fromStr(allocator, line);
            try ingredientRanges.append(value);
        }
    }

    return ParsedState{ .ingredientRanges = ingredientRanges, .ingredients = ingredients };
}

fn isIngredientFresh(list: utils.ListManaged(IdRange, null), value: RangeInt) bool {
    for (list.items) |range| {
        if (value >= range.first and value <= range.last) {
            return true;
        }
    }

    return false;
}

fn solveFirst(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    var state = try parseIds(allocator, input);
    defer state.deinit();

    var sum: u64 = 0;

    for (state.ingredients.items) |ingredient| {
        const isFresh = isIngredientFresh(state.ingredientRanges, ingredient);

        if (isFresh) {
            sum += 1;
        }
    }

    return utils.Solution{ .u64 = sum };
}

const OverlapState = enum(u8) {
    OverlapStateNone,
    OverlapStateStart,
    OverlapStateEnd,
    OverlapStateBoth,
    OverlapStateIn,
};

fn getOverlapState(range1: IdRange, range2: IdRange) OverlapState {

    //as they are inclusive, a +1 is needed in some places

    if (range2.last + 1 < range1.first) {
        // range 2 is before range1
        return .OverlapStateNone;
    }

    if (range2.first > range1.last + 1) {
        // range 2 is after range1
        return .OverlapStateNone;
    }

    // 2.last +1 >= 1.first an
    // 2.last <= 1.last +1

    if (range2.last <= range1.last) {
        // the end is not overflowing

        if (range2.first >= range1.first) {
            // the start is not underflowing
            // => it is in the range

            return .OverlapStateIn;
        } else {
            // the start is underflowing
            // => it overlaps at the start
            return .OverlapStateStart;
        }
    } else {
        // the end is overflowing

        if (range2.first > range1.first) {
            // the start is not underflowing
            // => it overflowing at the end

            return .OverlapStateEnd;
        } else {
            // the start is underflowing
            // => it overlaps at the start AND the end
            return .OverlapStateBoth;
        }
    }
}

const LoopState = union(enum) {
    LoopStateNothing,
    LoopStateChanged: IdRange,
    LoopStateRemoved,
};

fn mergeRanges(allocator: utils.Allocator, ranges: *utils.ListManaged(IdRange, null)) utils.SolveErrors!void {
    var toCompareStack: utils.ListManaged(IdRange, null) = try utils.ListManaged(IdRange, null).initCapacity(allocator, 10);
    defer toCompareStack.deinit();

    try toCompareStack.appendSlice(ranges.items);

    var result: utils.ListManaged(IdRange, null) = try utils.ListManaged(IdRange, null).initCapacity(allocator, 10);
    defer result.deinit();

    ranges.clearRetainingCapacity();

    while (toCompareStack.items.len != 0) {
        const rang = toCompareStack.pop();

        if (rang) |range1| {
            var loopState: LoopState = .LoopStateNothing;

            var i: usize = 0;

            mod: while (i < toCompareStack.items.len) : (i += 1) {
                const range2 = toCompareStack.items[i];

                const overlapState = getOverlapState(range1, range2);

                switch (overlapState) {
                    .OverlapStateNone => {},
                    .OverlapStateIn => {
                        // removes range2, as this is a unnecessary range
                        _ = toCompareStack.swapRemove(i);
                    },
                    .OverlapStateEnd => {
                        const range1mod = IdRange{ .first = range1.first, .last = range2.last };
                        // removes range2, as this is a unnecessary range
                        _ = toCompareStack.swapRemove(i);

                        loopState = .{ .LoopStateChanged = range1mod };
                        break :mod;
                    },
                    .OverlapStateStart => {
                        const range1mod = IdRange{ .first = range2.first, .last = range1.last };
                        // removes range2, as this is a unnecessary range
                        _ = toCompareStack.swapRemove(i);

                        loopState = .{ .LoopStateChanged = range1mod };
                        break :mod;
                    },
                    .OverlapStateBoth => {
                        // this removes the range, as it no longer is processed
                        loopState = .LoopStateRemoved;
                        break :mod;
                    },
                }
            }

            switch (loopState) {
                .LoopStateNothing => {
                    // nothing overlaps with this, just push it to the output!
                    try result.append(range1);
                },
                .LoopStateChanged => |rangemod| {
                    // it was changed, check this range later again
                    try toCompareStack.append(rangemod);
                },
                .LoopStateRemoved => {
                    //do nothing discards the range
                },
            }
        }
    }

    try ranges.appendSlice(result.items);
}

fn solveSecond(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    var state = try parseIds(allocator, input);
    defer state.deinit();

    try mergeRanges(allocator, &state.ingredientRanges);

    var sum: u64 = 0;

    for (state.ingredientRanges.items) |ingredientRange| {
        const span = ingredientRange.last - ingredientRange.first + 1;

        sum += span;
    }

    return utils.Solution{ .u64 = sum };
}

const generated = @import("generated");

pub const day = utils.Day{
    .solver = utils.Solver{ .individual = .{ .first = solveFirst, .second = solveSecond } },
    .examples = .{ .first = .{ .implemented = .{ .solution = .{ .u64 = 3 }, .real_value = .{ .u64 = 640 } } }, .second = .{ .implemented = .{ .solution = .{ .u64 = 14 } } } },
    .root = generated.root,
    .num = generated.num,
    .same_input = true,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try day.run(gpa.allocator());
}

test "day 05" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try day.@"test"(gpa.allocator());
}

const ManualTest = struct {
    range1: IdRange,
    range2: IdRange,
    result: OverlapState,
};

test "day 05 - manual" {
    const tests = [_]ManualTest{
        ManualTest{ .range1 = .{ .first = 3, .last = 5 }, .range2 = .{ .first = 10, .last = 14 }, .result = .OverlapStateNone },
        ManualTest{ .range1 = .{ .first = 10, .last = 14 }, .range2 = .{ .first = 3, .last = 5 }, .result = .OverlapStateNone },

        ManualTest{ .range1 = .{ .first = 1, .last = 5 }, .range2 = .{ .first = 2, .last = 4 }, .result = .OverlapStateIn },
        ManualTest{ .range1 = .{ .first = 1, .last = 5 }, .range2 = .{ .first = 1, .last = 5 }, .result = .OverlapStateIn },

        ManualTest{ .range1 = .{ .first = 3, .last = 5 }, .range2 = .{ .first = 2, .last = 4 }, .result = .OverlapStateStart },
        ManualTest{ .range1 = .{ .first = 3, .last = 5 }, .range2 = .{ .first = 1, .last = 2 }, .result = .OverlapStateStart },

        ManualTest{ .range1 = .{ .first = 1, .last = 6 }, .range2 = .{ .first = 5, .last = 8 }, .result = .OverlapStateEnd },
        ManualTest{ .range1 = .{ .first = 1, .last = 5 }, .range2 = .{ .first = 5, .last = 8 }, .result = .OverlapStateEnd },
    };

    for (tests) |t| {
        const result = getOverlapState(t.range1, t.range2);

        try std.testing.expectEqual(t.result, result);
    }
}
