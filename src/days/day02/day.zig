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

fn solveSecond(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    _ = allocator;
    _ = input;

    //

    return utils.Solution{ .string = "" };
}

const day = utils.Day{
    .solver = utils.Solver{ .individual = .{ .first = solveFirst, .second = solveSecond } },
    .examples = .{ .first = .{ .implemented = .{
        .solution = .{ .u64 = 1227775554 },
    } }, .second = .todo },
    .root = @import("generated").root,
    .same_input = false,
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
