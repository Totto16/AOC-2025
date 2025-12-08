const std = @import("std");
const utils = @import("utils");

fn splitIterToArray(allocator: utils.Allocator, iter: *std.mem.SplitIterator(u8, .scalar)) utils.SolveErrors!utils.ListManaged(utils.Str) {
    var array: utils.ListManaged(utils.Str) = utils.ListManaged(utils.Str).init(allocator);

    while (iter.next()) |val| {
        try array.append(val);
    }

    return array;
}

const RangeInt = u64;

const IdRange = struct {
    first: RangeInt,
    last_exclusive: RangeInt,

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

        return IdRange{ .first = first, .last_exclusive = last + 1 };
    }

    pub fn eq(self: IdRange, second: IdRange) bool {
        if (self.first != second.first) {
            return false;
        }

        return self.last_exclusive == second.last_exclusive;
    }
};

const ParsedState = struct {
    ingredientRanges: utils.ListManaged(IdRange),
    ingredients: utils.ListManaged(RangeInt),

    pub fn deinit(self: *ParsedState) void {
        self.ingredientRanges.deinit();
        self.ingredients.deinit();
    }
};

fn parseIds(allocator: utils.Allocator, input: utils.Str) utils.SolveErrors!ParsedState {
    var ingredientRanges: utils.ListManaged(IdRange) = utils.ListManaged(IdRange).init(allocator);
    errdefer ingredientRanges.deinit();

    var ingredients: utils.ListManaged(RangeInt) = utils.ListManaged(RangeInt).init(allocator);
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

fn isIngredientFresh(list: utils.ListManaged(IdRange), value: RangeInt) bool {
    for (list.items) |range| {
        if (value >= range.first and value <= range.last_exclusive) {
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

    //as they are inclusive, ais needed in some places

    if (range2.last_exclusive <= range1.first) {
        // range 2 is before range1
        return .OverlapStateNone;
    }

    if (range2.first >= range1.last_exclusive) {
        // range 2 is after range1
        return .OverlapStateNone;
    }

    // 2.last_exclusive > 1.first
    // 2.first < 1.last_exclusive

    if (range2.last_exclusive <= range1.last_exclusive) {
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

        if (range2.first >= range1.first) {
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
    LoopStateReiterate,
};

fn mergeRanges(allocator: utils.Allocator, ranges: *utils.ListManaged(IdRange)) utils.SolveErrors!void {
    var toCompareStack: utils.StackManaged(IdRange) = utils.StackManaged(IdRange).init(allocator);
    defer toCompareStack.deinit();

    try toCompareStack.appendSlice(ranges.items);

    var result: utils.ListManaged(IdRange) = utils.ListManaged(IdRange).init(allocator);
    defer result.deinit();

    ranges.clearRetainingCapacity();

    while (!toCompareStack.empty()) {
        const rang = toCompareStack.popFirst();

        if (rang) |range1| {
            var loopState: LoopState = .LoopStateNothing;

            var it = toCompareStack.iter();

            modified: while (it.next()) |range2_node| {
                const range2 = range2_node.value;
                const overlapState = getOverlapState(range1, range2);

                switch (overlapState) {
                    .OverlapStateNone => {},
                    .OverlapStateIn => {
                        // removes range2, as this is a unnecessary range
                        toCompareStack.remove(range2_node);
                        // this invalidates it, so redo this, by adding range1 to the stack again

                        std.debug.assert(range1.first <= range2.first);
                        std.debug.assert(range1.last_exclusive >= range2.last_exclusive);

                        loopState = .LoopStateReiterate;
                        break :modified;
                    },
                    .OverlapStateEnd => {
                        const range1mod = IdRange{ .first = range1.first, .last_exclusive = range2.last_exclusive };
                        // removes range2, as this is a unnecessary range
                        toCompareStack.remove(range2_node);

                        std.debug.assert(range1.first <= range2.first);
                        std.debug.assert(range1.last_exclusive <= range2.last_exclusive);

                        loopState = .{ .LoopStateChanged = range1mod };
                        break :modified;
                    },
                    .OverlapStateStart => {
                        const range1mod = IdRange{ .first = range2.first, .last_exclusive = range1.last_exclusive };
                        // removes range2, as this is a unnecessary range
                        toCompareStack.remove(range2_node);

                        std.debug.assert(range1.first >= range2.first);
                        std.debug.assert(range1.last_exclusive >= range2.last_exclusive);

                        loopState = .{ .LoopStateChanged = range1mod };
                        break :modified;
                    },
                    .OverlapStateBoth => {
                        // this removes the range, as it no longer is processed

                        std.debug.assert(range1.first >= range2.first);
                        std.debug.assert(range1.last_exclusive <= range2.last_exclusive);

                        loopState = .LoopStateRemoved;
                        break :modified;
                    },
                }
            }

            switch (loopState) {
                .LoopStateReiterate => {
                    try toCompareStack.append(range1);
                },
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

    sortRange(&state.ingredientRanges.items);

    for (state.ingredientRanges.items) |ingredientRange| {
        const span = ingredientRange.last_exclusive - ingredientRange.first;

        sum += span;
    }

    return utils.Solution{ .u64 = sum };
}

const generated = @import("generated");

pub const day = utils.Day{
    .solver = utils.Solver{ .individual = .{ .first = solveFirst, .second = solveSecond } },
    .solutions = .{
        .first = .{ .implemented = .{
            .solution = .{ .u64 = 3 },
            .real_value = .{ .u64 = 640 },
        } },
        .second = .{
            .implemented = .{
                .solution = .{ .u64 = 14 },
                .real_value = .{ .u64 = 365804144481581 },
            },
        },
    },
    .inputs = .both_same,
    .root = generated.root,
    .num = generated.num,
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
        ManualTest{ .range1 = .{ .first = 3, .last_exclusive = 6 }, .range2 = .{ .first = 10, .last_exclusive = 15 }, .result = .OverlapStateNone },
        ManualTest{ .range1 = .{ .first = 10, .last_exclusive = 15 }, .range2 = .{ .first = 3, .last_exclusive = 6 }, .result = .OverlapStateNone },

        ManualTest{ .range1 = .{ .first = 1, .last_exclusive = 6 }, .range2 = .{ .first = 2, .last_exclusive = 5 }, .result = .OverlapStateIn },
        ManualTest{ .range1 = .{ .first = 1, .last_exclusive = 6 }, .range2 = .{ .first = 1, .last_exclusive = 6 }, .result = .OverlapStateIn },

        ManualTest{ .range1 = .{ .first = 3, .last_exclusive = 6 }, .range2 = .{ .first = 2, .last_exclusive = 5 }, .result = .OverlapStateStart },
        ManualTest{ .range1 = .{ .first = 3, .last_exclusive = 6 }, .range2 = .{ .first = 1, .last_exclusive = 4 }, .result = .OverlapStateStart },

        ManualTest{ .range1 = .{ .first = 1, .last_exclusive = 7 }, .range2 = .{ .first = 5, .last_exclusive = 9 }, .result = .OverlapStateEnd },
        ManualTest{ .range1 = .{ .first = 1, .last_exclusive = 6 }, .range2 = .{ .first = 5, .last_exclusive = 9 }, .result = .OverlapStateEnd },

        ManualTest{ .range1 = IdRange{ .first = 205, .last_exclusive = 206 }, .range2 = IdRange{ .first = 206, .last_exclusive = 210 }, .result = .OverlapStateNone },
        ManualTest{ .range1 = IdRange{ .first = 206, .last_exclusive = 210 }, .range2 = IdRange{ .first = 205, .last_exclusive = 206 }, .result = .OverlapStateNone },

        ManualTest{ .range1 = .{ .first = 1, .last_exclusive = 6 }, .range2 = .{ .first = 1, .last_exclusive = 9 }, .result = .OverlapStateEnd },
        ManualTest{ .range1 = .{ .first = 2, .last_exclusive = 6 }, .range2 = .{ .first = 1, .last_exclusive = 9 }, .result = .OverlapStateBoth },
        ManualTest{ .range1 = .{ .first = 1, .last_exclusive = 9 }, .range2 = .{ .first = 1, .last_exclusive = 6 }, .result = .OverlapStateIn },
    };

    for (tests) |t| {
        const result = getOverlapState(t.range1, t.range2);

        try std.testing.expectEqual(t.result, result);
    }
}

fn cmpRange(ctx: void, a: IdRange, b: IdRange) bool {
    if (a.first == b.first) {
        return utils.asc(RangeInt)(ctx, a.last_exclusive, b.last_exclusive);
    }
    return utils.asc(RangeInt)(ctx, a.first, b.first);
}

fn sortRange(ranges: *[]IdRange) void {
    utils.sort(IdRange, ranges.*, {}, cmpRange);
}

test "day 05 - manual advanced" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const input =
        \\1-5
        \\2-17
        \\13-34
        \\45-76
        \\78-103
        \\104-156
        \\107-165
        \\102-121
        \\205-205
        \\206-209
        \\
    ;

    var values = try parseIds(gpa.allocator(), input);
    defer values.deinit();

    try mergeRanges(gpa.allocator(), &values.ingredientRanges);

    var expected_mut = [_]IdRange{
        IdRange{ .first = 1, .last_exclusive = 35 },
        IdRange{ .first = 45, .last_exclusive = 77 },
        IdRange{ .first = 78, .last_exclusive = 166 },
        IdRange{ .first = 205, .last_exclusive = 206 },
        IdRange{ .first = 206, .last_exclusive = 210 },
    };
    const expected: []IdRange = &expected_mut;

    sortRange(&values.ingredientRanges.items);

    try std.testing.expectEqualSlices(IdRange, expected, values.ingredientRanges.items);
}

test "day 05 - edge case" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const input =
        \\328188028014249-328648944286494
        \\327646151858731-328188028014249
        \\327788252572518-328188028014249
        \\
    ;

    var values = try parseIds(gpa.allocator(), input);
    defer values.deinit();

    try mergeRanges(gpa.allocator(), &values.ingredientRanges);

    var expected_mut = [_]IdRange{
        IdRange{ .first = 327646151858731, .last_exclusive = 328648944286495 },
    };
    const expected: []IdRange = &expected_mut;

    sortRange(&values.ingredientRanges.items);

    try std.testing.expectEqualSlices(IdRange, expected, values.ingredientRanges.items);
}
