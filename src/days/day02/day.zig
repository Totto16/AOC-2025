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

        const first = utils.parseInt(RangeInt, values.items[0], 10) catch {
            return error.ParseError;
        };

        const last = utils.parseInt(RangeInt, values.items[1], 10) catch {
            return error.ParseError;
        };

        return IdRange{ .first = first, .last = last };
    }
};

fn parseIds(allocator: utils.Allocator, input: utils.Str) utils.SolveErrors!utils.ListManaged(IdRange, null) {
    var array: utils.ListManaged(IdRange, null) = try utils.ListManaged(IdRange, null).initCapacity(allocator, 10);
    errdefer array.deinit();

    var iter = utils.splitSca(u8, input, '\n');

    while (iter.next()) |line| {
        var iter_2 = utils.splitSca(u8, line, ',');

        while (iter_2.next()) |ids| {
            if (ids.len == 0) {
                continue;
            }

            const value = try IdRange.fromStr(allocator, ids);

            try array.append(value);
        }
    }

    return array;
}

fn isInvalidId(allocator: utils.Allocator, id: u64) utils.SolveErrors!bool {
    const str = try std.fmt.allocPrint(allocator, "{d}", .{id});

    if (str.len % 2 != 0) {
        return false;
    }

    const first_part = str[0 .. str.len / 2];
    const second_part = str[str.len / 2 ..];
    return std.mem.eql(u8, first_part, second_part);
}

fn solveFirst(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    const values = try parseIds(allocator, input);
    defer values.deinit();

    var sum: u64 = 0;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const fast_alloc = arena.allocator();

    for (values.items) |range| {
        for (range.first..range.last + 1) |id| {
            if (try isInvalidId(fast_alloc, id)) {
                sum += id;
            }
        }
    }

    return utils.Solution{ .u64 = sum };
}

fn isRepeated(str: []const u8, amount: u64) bool {
    std.debug.assert(str.len % amount == 0);

    const end_idx = (str.len / amount) - 1;

    if (end_idx == 0) {
        return false;
    }

    for (0..end_idx) |i| {
        const start = i * amount;
        const mid = (i + 1) * amount;
        const end = (i + 2) * amount;

        const first_part = str[start..mid];
        const second_part = str[mid..end];

        if (!std.mem.eql(u8, first_part, second_part)) {
            return false;
        }
    }

    return true;
}

fn isInvalidIdSecond(allocator: utils.Allocator, id: u64) utils.SolveErrors!bool {
    const str = try std.fmt.allocPrint(allocator, "{d}", .{id});

    if (str.len == 1) {
        return false;
    }

    const end_num = (str.len / 2) + 1;

    for (1..end_num + 1) |repeated_count| {
        if (str.len % repeated_count != 0) {
            continue;
        }

        const is_repeated_res = isRepeated(str, repeated_count);

        if (is_repeated_res) {
            return true;
        }
    }
    return false;
}

fn solveSecond(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    const values = try parseIds(allocator, input);
    defer values.deinit();

    var sum: u64 = 0;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const fast_alloc = arena.allocator();

    for (values.items) |range| {
        for (range.first..range.last + 1) |id| {
            if (try isInvalidIdSecond(fast_alloc, id)) {
                sum += id;
            }
        }
    }

    return utils.Solution{ .u64 = sum };
}

const generated = @import("generated");

pub const day = utils.Day{
    .solver = utils.Solver{ .individual = .{ .first = solveFirst, .second = solveSecond } },
    .examples = .{ .first = .{ .implemented = .{
        .solution = .{ .u64 = 1227775554 },
        .real_value = .{ .u64 = 28846518423 },
    } }, .second = .{ .implemented = .{ .solution = .{ .u64 = 4174379265 }, .real_value = .{ .u64 = 31578210022 } } } },
    .root = generated.root,
    .day = generated.day,
    .same_input = true,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try day.run(gpa.allocator());
}

test "day 02" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try day.@"test"(gpa.allocator());
}

test "day 02 - manual - 2. part" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    const fast_alloc = arena.allocator();

    try std.testing.expectEqual(true, try isInvalidIdSecond(fast_alloc, 11));
    try std.testing.expectEqual(false, try isInvalidIdSecond(fast_alloc, 12));
    try std.testing.expectEqual(false, try isInvalidIdSecond(fast_alloc, 102));
    try std.testing.expectEqual(true, try isInvalidIdSecond(fast_alloc, 1188511885));
    try std.testing.expectEqual(false, try isInvalidIdSecond(fast_alloc, 1698523));
    try std.testing.expectEqual(true, try isInvalidIdSecond(fast_alloc, 38593859));
}
