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

    pub fn eq(self: *const Tile, other: Tile) bool {
        return self.x == other.x and self.y == other.y;
    }

    pub fn apply(self: *const Tile, continuation: TileChainContinuation) !Tile {
        switch (continuation.direction) {
            .DirectionX => {
                const new_x: i64 = @as(i64, @intCast(self.x)) + continuation.amount;

                if (new_x < 0) {
                    return error.ContinuationNegative;
                }

                return Tile{ .x = @as(u64, @intCast(new_x)), .y = self.y };
            },
            .DirectionY => {
                const new_y: i64 = @as(i64, @intCast(self.y)) + continuation.amount;

                if (new_y < 0) {
                    return error.ContinuationNegative;
                }

                return Tile{ .x = self.x, .y = @as(u64, @intCast(new_y)) };
            },
        }
    }
};

const TileChainDirection = enum(u8) {
    DirectionX,
    DirectionY,
};

const TileChainContinuation = struct {
    direction: TileChainDirection,
    amount: i64,
};

const TileChainList = struct {
    tiles: utils.ListManaged(Tile),

    start_tile: Tile,
    continuations: utils.ListManaged(TileChainContinuation),

    pub fn init(allocator: utils.Allocator, tiles: utils.ListManaged(Tile)) !TileChainList {
        if (tiles.items.len == 0) {
            return utils.SolveErrors.PredicateNotMet;
        }

        return TileChainList{
            .tiles = tiles,
            .start_tile = tiles.items[0],
            .continuations = utils.ListManaged(TileChainContinuation).init(allocator),
        };
    }

    pub fn deinit(self: *TileChainList) void {
        self.tiles.deinit();
        self.continuations.deinit();
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

fn parseTiles2(allocator: utils.Allocator, input: utils.Str) utils.SolveErrors!TileChainList {
    const tiles = try parseTiles(allocator, input);

    var chain_list: TileChainList = try TileChainList.init(allocator, tiles);
    errdefer chain_list.deinit();

    // validate adjacent tiles have at least one same coordinate
    for (0..tiles.items.len) |i| {
        const tile1 = tiles.items[i];
        const tile2 = tiles.items[@mod(i + 1, tiles.items.len)];

        const continuation: TileChainContinuation = blk: {
            if (tile1.x == tile2.x) {
                const amount = @as(i64, @intCast(tile2.y)) - @as(i64, @intCast(tile1.y));
                break :blk TileChainContinuation{ .direction = .DirectionY, .amount = amount };
            }

            if (tile1.y == tile2.y) {
                const amount = @as(i64, @intCast(tile2.x)) - @as(i64, @intCast(tile1.x));
                break :blk TileChainContinuation{ .direction = .DirectionX, .amount = amount };
            }

            return utils.SolveErrors.PredicateNotMet;
        };

        try chain_list.continuations.append(continuation);
    }

    { //explicitly assert, that the last tile and the first one are connected
        const lastCont = chain_list.continuations.getLastOrNull() orelse return utils.SolveErrors.PredicateNotMet;

        const lastTile = tiles.getLastOrNull() orelse return utils.SolveErrors.PredicateNotMet;

        const firstTile = tiles.items[0];

        const calcFirstTile = lastTile.apply(lastCont) catch return utils.SolveErrors.PredicateNotMet;

        if (!calcFirstTile.eq(firstTile)) {
            std.debug.print("Expected last tile to connect to the first, but got: {} -> {} -> ( expected {}, got {} )\n", .{ lastTile, lastCont, firstTile, calcFirstTile });
            return utils.SolveErrors.PredicateNotMet;
        }
    }

    // not checked here, but given by the input: no lines cross each other, it is one continuous shape!

    return chain_list;
}

const Line = struct {
    start: Tile,
    end: Tile,
};

const AreaEntry = struct {
    start_ref: usize,
    end_ref: usize,
    area: AreaNum,

    pub fn isValid(self: *const AreaEntry, allocator: utils.Allocator, tail_chain_list: TileChainList) bool {
        const start_tile = tail_chain_list.tiles.items[self.start_ref];
        const end_tile = tail_chain_list.tiles.items[self.end_ref];

        const continuation_ref = self.start_ref;

        var linesToCheck = utils.ListManaged(Line).init(allocator);

        {
            var current_tile = start_tile;
            for (0..4) |i| {
                const cont = tail_chain_list.continuations.items[continuation_ref + i];

                const new_tile = current_tile.apply(cont) catch {
                    std.debug.print("ERROR: predicate not met, continuation list has invalid entries!\n", .{});
                    return false;
                };
                linesToCheck.append(Line{ .start = current_tile, .end = new_tile });
                current_tile = new_tile;
            }
        }

        for (0..tail_chain_list.continuations.items.len) |i| {}

        return false;
    }
};

fn cmpArea(ctx: void, a: AreaEntry, b: AreaEntry) bool {
    return utils.asc(AreaNum)(ctx, a.area, b.area);
}

fn solveSecond(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    var tiles: TileChainList = try parseTiles2(allocator, input);
    defer tiles.deinit();

    var areas = utils.ListManaged(AreaEntry).init(allocator);
    defer areas.deinit();

    for (0..tiles.tiles.items.len) |i| {
        for (i..tiles.tiles.items.len) |j| {
            const tile1 = tiles.tiles.items[i];
            const tile2 = tiles.tiles.items[j];

            const area_val = tile1.area(tile2);

            try areas.append(AreaEntry{ .start_ref = i, .end_ref = j, .area = area_val });
        }
    }

    utils.sort(AreaEntry, areas.items, {}, cmpArea);

    for (areas.items) |entry| {
        if (entry.isValid(tiles)) {
            return utils.Solution{ .u64 = entry.area };
        }
    }

    return utils.SolveErrors.NotSolved;
}

const generated = @import("generated");

pub const day = utils.Day{
    .solver = utils.Solver{ .individual = .{ .first = solveFirst, .second = solveSecond } },
    .solutions = .{
        .first = .{ .implemented = .{
            .solution = .{ .u64 = 50 },
            .real_value = .{ .u64 = 4777816465 },
        } },
        .second = .{ .implemented = .{
            .solution = .{ .u64 = 24 },
            .real_value = .{ .u64 = 1 },
        } },
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
