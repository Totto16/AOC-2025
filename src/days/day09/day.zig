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
};

fn lineInRange(start: u64, end: u64, pos: u64) bool {
    const sorted = sort2(start, end);

    return pos >= sorted.first and pos <= sorted.last;
}

const Line = union(enum) {
    direction_x: struct {
        start_y: u64,
        end_y: u64,
        x_fixed: u64,
    },
    direction_y: struct {
        start_x: u64,
        end_x: u64,
        y_fixed: u64,
    },

    pub fn fromTiles(tile1: Tile, tile2: Tile) utils.SolveErrors!Line {
        const line: Line = blk: {
            if (tile1.x == tile2.x) {
                const line = Line{
                    .direction_x = .{
                        .x_fixed = tile1.x,
                        .start_y = tile1.y,
                        .end_y = tile2.y,
                    },
                };

                break :blk line;
            }

            if (tile1.y == tile2.y) {
                const line = Line{
                    .direction_y = .{
                        .y_fixed = tile1.y,
                        .start_x = tile1.x,
                        .end_x = tile2.x,
                    },
                };

                break :blk line;
            }

            return utils.SolveErrors.PredicateNotMet;
        };

        return line;
    }

    pub fn dest(self: *const Line) Tile {
        switch (self.*) {
            .direction_x => |x| {
                return Tile{ .x = x.x_fixed, .y = x.end_y };
            },
            .direction_y => |y| {
                return Tile{ .x = y.end_x, .y = y.y_fixed };
            },
        }
    }

    pub fn intersects(self: *const Line, other: Line) bool {
        switch (self.*) {
            .direction_x => |self_as_x| {
                switch (other) {
                    .direction_x => {
                        // both x, not intersecting ads per definition

                        return false;
                    },
                    .direction_y => |other_as_y| {
                        if (!lineInRange(other_as_y.start_x, other_as_y.end_x, self_as_x.x_fixed)) {
                            return false;
                        }

                        return lineInRange(self_as_x.start_y, self_as_x.end_y, other_as_y.y_fixed);
                    },
                }
            },
            .direction_y => |self_as_y| {
                switch (other) {
                    .direction_x => |other_as_x| {
                        if (!lineInRange(self_as_y.start_x, self_as_y.end_x, other_as_x.x_fixed)) {
                            return false;
                        }

                        return lineInRange(other_as_x.start_y, other_as_x.end_y, self_as_y.y_fixed);
                    },
                    .direction_y => {
                        // both y, not intersecting ads per definition

                        return false;
                    },
                }
            },
        }
    }
};

const Direction = enum(u8) {
    Positive,
    Negative,
};

const LineWithDirection = struct {
    line: Line,
    direction: Direction,
};

fn intersectsInward(rect_line: LineWithDirection, other: Line) bool {}

const TilesAndLines = struct {
    tiles: utils.ListManaged(Tile),

    lines: utils.ListManaged(Line),

    pub fn init(allocator: utils.Allocator, tiles: utils.ListManaged(Tile)) TilesAndLines {
        return TilesAndLines{
            .tiles = tiles,
            .lines = utils.ListManaged(Line).init(allocator),
        };
    }

    pub fn deinit(self: *TilesAndLines) void {
        self.tiles.deinit();
        self.lines.deinit();
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

const Sorted = struct { first: u64, last: u64 };

fn sort2(a: u64, b: u64) Sorted {
    if (a > b) {
        return .{
            .first = b,
            .last = a,
        };
    }

    return .{
        .first = a,
        .last = b,
    };
}

fn parseTiles2(allocator: utils.Allocator, input: utils.Str) utils.SolveErrors!TilesAndLines {
    const tiles = try parseTiles(allocator, input);

    var tiles_and_lines: TilesAndLines = TilesAndLines.init(allocator, tiles);
    errdefer tiles_and_lines.deinit();

    // validate adjacent tiles have at least one same coordinate
    for (0..tiles.items.len) |i| {
        const tile1 = tiles.items[i];
        const tile2 = tiles.items[@mod(i + 1, tiles.items.len)];

        const line: Line = try Line.fromTiles(tile1, tile2);

        try tiles_and_lines.lines.append(line);
    }

    { //explicitly assert, that the last tile and the first one are connected
        const lastLine = tiles_and_lines.lines.getLastOrNull() orelse return utils.SolveErrors.PredicateNotMet;

        const lastTile = tiles.getLastOrNull() orelse return utils.SolveErrors.PredicateNotMet;

        const firstTile = tiles.items[0];

        const calcFirstTile = lastLine.dest();

        if (!calcFirstTile.eq(firstTile)) {
            std.debug.print("Expected last tile to connect to the first, but got: {} -> {} -> ( expected {}, got {} )\n", .{ lastTile, lastLine, firstTile, calcFirstTile });
            return utils.SolveErrors.PredicateNotMet;
        }
    }

    // not checked here, but given by the input: no lines cross each other, it is one continuous shape!

    return tiles_and_lines;
}

const Rect = struct {
    start: Tile,
    end: Tile,
};

const RectLines = struct {
    lines: [4]LineWithDirection,

    fn directionFromEdges(rect: Rect, line: Line, edg1: Tile, edg2: Tile) Direction {
        // TODO
    }

    fn directionLineFromEdges(rect: Rect, edg1: Tile, edg2: Tile) utils.SolveErrors!LineWithDirection {
        const line = try Line.fromTiles(edg1, edg2);
        return LineWithDirection{ .line = line, .direction = directionFromEdges(rect, line, edg1, edg2) };
    }

    pub fn init(rect: Rect) utils.SolveErrors!RectLines {
        const edge1 = Tile{
            .x = rect.end.x,
            .y = rect.start.y,
        };

        const edge2 = Tile{
            .x = rect.start.x,
            .y = rect.end.y,
        };

        return RectLines{ .lines = [4]LineWithDirection{
            try directionLineFromEdges(rect, rect.start, edge1),
            try directionLineFromEdges(rect, edge1, rect.end),
            try directionLineFromEdges(rect, rect.end, edge2),
            try directionLineFromEdges(rect, edge2, rect.start),
        } };
    }
};

const Error = union(enum) { static: []const u8, line_intersect: struct {
    line: Line,
    tile_line: Line,
} };

const Valid = union(enum) {
    valid,
    invalid: Error,
};

const AreaEntry = struct {
    start: Tile,
    end: Tile,
    area: AreaNum,

    pub fn isValidExt(self: *const AreaEntry, tiles_and_lines: TilesAndLines) utils.SolveErrors!Valid {
        const rect_lines = try RectLines.init(self.start, self.end);

        for (rect_lines.lines) |rect_line| {
            for (tiles_and_lines.lines.items) |line| {
                if (intersectsInward(rect_line, line)) {
                    return .{ .invalid = .{ .line_intersect = .{
                        .line = rect_line,
                        .tile_line = line,
                    } } };
                }
            }
        }

        return .valid;
    }

    pub fn isValid(self: *const AreaEntry, tiles_and_lines: TilesAndLines) utils.SolveErrors!bool {
        return try self.isValidExt(tiles_and_lines) == .valid;
    }
};

fn cmpArea(ctx: void, a: AreaEntry, b: AreaEntry) bool {
    return utils.asc(AreaNum)(ctx, a.area, b.area);
}

fn solveSecond(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    var tiles_and_lines: TilesAndLines = try parseTiles2(allocator, input);
    defer tiles_and_lines.deinit();

    var areas = utils.ListManaged(AreaEntry).init(allocator);
    defer areas.deinit();

    for (0..tiles_and_lines.tiles.items.len) |i| {
        for (i..tiles_and_lines.tiles.items.len) |j| {
            const tile1 = tiles_and_lines.tiles.items[i];
            const tile2 = tiles_and_lines.tiles.items[j];

            const area_val = tile1.area(tile2);

            try areas.append(AreaEntry{ .start = tile1, .end = tile2, .area = area_val });
        }
    }

    utils.sort(AreaEntry, areas.items, {}, cmpArea);

    for (areas.items) |entry| {
        std.debug.print("Is valid {}: {}\n", .{ entry, try entry.isValidExt(tiles_and_lines) });
        if (try entry.isValid(tiles_and_lines)) {
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
            .real_value = null,
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
