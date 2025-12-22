const std = @import("std");
const utils = @import("utils");

const TilePosInt = u64;

const AreaNum = u64;

const Tile = struct {
    x: TilePosInt,
    y: TilePosInt,

    pub fn area(self: *const Tile, other: Tile) AreaNum {
        const height: u64 = @abs(@as(i64, @intCast(self.y)) - @as(i64, @intCast(other.y))) + 1;

        const width: u64 = @abs(@as(i64, @intCast(self.x)) - @as(i64, @intCast(other.x))) + 1;

        const area_num: u64 = height * width;

        return area_num;
    }
};

fn splitIterToArray(allocator: utils.Allocator, iter: *std.mem.SplitIterator(u8, .scalar)) utils.SolveErrors!utils.ListManaged(utils.Str) {
    var array: utils.ListManaged(utils.Str) = utils.ListManaged(utils.Str).init(allocator);

    while (iter.next()) |val| {
        try array.append(val);
    }

    return array;
}

fn parseTiles(allocator: utils.Allocator, input: utils.Str) utils.SolveErrors!utils.ListManaged(Tile) {
    var tiles: utils.ListManaged(Tile) = utils.ListManaged(Tile).init(allocator);

    var iter = utils.splitSca(u8, input, '\n');

    while (iter.next()) |line| {
        if (line.len == 0) {
            continue;
        }

        var iter2 = utils.splitSca(u8, line, ',');

        const values = try splitIterToArray(allocator, &iter2);
        defer values.deinit();

        if (values.items.len != 2) {
            return error.ParseError;
        }

        const x = utils.parseInt(TilePosInt, values.items[0], 10) catch {
            return error.ParseError;
        };

        const y = utils.parseInt(TilePosInt, values.items[1], 10) catch {
            return error.ParseError;
        };

        try tiles.append(Tile{ .x = x, .y = y });
    }

    return tiles;
}

fn solveFirst(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    var tiles = try parseTiles(allocator, input);
    defer tiles.deinit();

    var max_area: u64 = 0;

    for (0..tiles.items.len) |i| {
        for (i..tiles.items.len) |j| {
            const tile1 = tiles.items[i];
            const tile2 = tiles.items[j];

            const area_val = tile1.area(tile2);

            if (area_val > max_area) {
                max_area = area_val;
            }
        }
    }

    return utils.Solution{ .u64 = max_area };
}

fn solveSecond(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    _ = allocator;
    _ = input;

    return utils.Solution{ .u64 = 1 };
}

const generated = @import("generated");

pub const day = utils.Day{
    .solver = utils.Solver{ .individual = .{ .first = solveFirst, .second = solveSecond } },
    .solutions = .{
        .first = .{ .implemented = .{
            .solution = .{ .u64 = 50 },
            .real_value = .{ .u64 = 4777816465 },
        } },
        .second = .pending,
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

test "day 09" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try day.@"test"(gpa.allocator());
}
