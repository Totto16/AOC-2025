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

fn solveSecond(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    _ = allocator;
    _ = input;

    return utils.Solution{ .u64 = 0 };
}

const generated = @import("generated");

pub const day = utils.Day{
    .solver = utils.Solver{ .individual = .{ .first = solveFirst, .second = solveSecond } },
    .examples = .{ .first = .{ .implemented = .{ .solution = .{ .u64 = 3 }, .real_value = .{ .u64 = 640 } } }, .second = .pending },
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
