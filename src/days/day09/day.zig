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

const Line = union(enum) {
    direction_y: struct {
        start_y: u64,
        end_y: u64,
        x_fixed: u64,
    },
    direction_x: struct {
        start_x: u64,
        end_x: u64,
        y_fixed: u64,
    },

    pub fn fromTiles(tile1: Tile, tile2: Tile) utils.SolveErrors!Line {
        const line: Line = blk: {
            if (tile1.x == tile2.x) {
                const line = Line{
                    .direction_y = .{
                        .x_fixed = tile1.x,
                        .start_y = tile1.y,
                        .end_y = tile2.y,
                    },
                };

                break :blk line;
            }

            if (tile1.y == tile2.y) {
                const line = Line{
                    .direction_x = .{
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
            .direction_y => |y_dir| {
                return Tile{ .x = y_dir.x_fixed, .y = y_dir.end_y };
            },
            .direction_x => |x_dir| {
                return Tile{ .x = x_dir.end_x, .y = x_dir.y_fixed };
            },
        }
    }

    pub fn isPoint(self: *const Line) bool {
        switch (self.*) {
            .direction_y => |y_dir| {
                return y_dir.start_y == y_dir.end_y;
            },
            .direction_x => |x_dir| {
                return x_dir.start_x == x_dir.end_x;
            },
        }
    }
};

const Direction = enum(u8) {
    Positive,
    Negative,
};

const RectLine = struct {
    line: Line,
    inner_rect_dir: Direction,
};

fn lineInRangeInwards(start: u64, end: u64, pos: u64) bool {
    const sorted = sort2(start, end);

    return pos >= sorted.first and pos <= sorted.last;
}

fn lineOverlapsInwards(start: u64, end: u64, inner_rect_dir: Direction, pos: u64) bool {
    const sorted = sort2(start, end);

    // 6 cases

    // 1+2: no overlaps from above or below
    if (pos <= sorted.first or pos >= sorted.last) {
        return false;
    }

    // explicitly exclude that edge case!
    if (sorted.first == sorted.last) {
        unreachable;
    }

    // 3: overlaps at the start but doesn't go into it
    if (pos == sorted.first) {
        // only overlaps inwards, if the rect is open in that direction it overlaps

        // here it means downwards (positive)
        return (inner_rect_dir == Direction.Positive);
    }

    // 4: overlaps at the end but doesn't go into it
    if (pos == sorted.last) {
        // only overlaps inwards, if the rect is open in that direction it overlaps

        // here it means upwards (negative)
        return (inner_rect_dir == Direction.Negative);
    }

    // case 5+6: it overlaps on both sites, always overlaps inwards
    return true;
}

fn intersectsInward(rect_line: RectLine, other: Line) bool {
    switch (rect_line.line) {
        .direction_y => |rect_line_y| {
            switch (other) {
                .direction_y => {
                    // both x, not intersecting inwards as per definition

                    return false;
                },
                .direction_x => |other_as_x| {
                    if (!lineInRangeInwards(other_as_x.start_x, other_as_x.end_x, rect_line_y.x_fixed)) {
                        return false;
                    }

                    return lineOverlapsInwards(rect_line_y.start_y, rect_line_y.end_y, rect_line.inner_rect_dir, other_as_x.y_fixed);
                },
            }
        },
        .direction_x => |rect_line_x| {
            switch (other) {
                .direction_y => |other_as_y| {
                    if (!lineInRangeInwards(rect_line_x.start_x, rect_line_x.end_x, other_as_y.x_fixed)) {
                        return false;
                    }

                    return lineOverlapsInwards(other_as_y.start_y, other_as_y.end_y, rect_line.inner_rect_dir, rect_line_x.y_fixed);
                },
                .direction_x => {
                    // both y, not intersecting inwards as per definition

                    return false;
                },
            }
        },
    }
}

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
    lines: [4]RectLine,

    fn directionForRectLine(rect: Rect, line: Line) Direction {
        switch (line) {
            .direction_y => |y_line| {

                // get the upper (lower) x coordinate, if it is that, it is downwards (positive) otherwise upwards (negative)
                const sorted_x = sort2(rect.start.x, rect.end.x);

                if (sorted_x.first == y_line.x_fixed) {
                    return Direction.Positive;
                }

                if (sorted_x.last == y_line.x_fixed) {
                    return Direction.Negative;
                }

                unreachable;
            },
            .direction_x => |x_line| {
                // get the upper (lower) y coordinate, if it is that, it is downwards (positive) otherwise upwards (negative)
                const sorted_y = sort2(rect.start.y, rect.end.y);

                if (sorted_y.first == x_line.y_fixed) {
                    return Direction.Positive;
                }

                if (sorted_y.last == x_line.y_fixed) {
                    return Direction.Negative;
                }

                unreachable;
            },
        }
    }

    fn directionLineFromEdges(rect: Rect, edg1: Tile, edg2: Tile) utils.SolveErrors!RectLine {
        const line = try Line.fromTiles(edg1, edg2);
        return RectLine{ .line = line, .inner_rect_dir = directionForRectLine(rect, line) };
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

        return RectLines{ .lines = [4]RectLine{
            try directionLineFromEdges(rect, rect.start, edge1),
            try directionLineFromEdges(rect, edge1, rect.end),
            try directionLineFromEdges(rect, rect.end, edge2),
            try directionLineFromEdges(rect, edge2, rect.start),
        } };
    }
};

const Error = union(enum) { static: []const u8, line_intersect: struct {
    line: RectLine,
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

    pub fn fromEdges(start: Tile, end: Tile) AreaEntry {
        const area_val = start.area(end);

        return AreaEntry{
            .start = start,
            .end = end,
            .area = area_val,
        };
    }

    pub fn isValidExt(self: *const AreaEntry, tiles_and_lines: TilesAndLines) utils.SolveErrors!Valid {
        const rect_lines = try RectLines.init(Rect{ .start = self.start, .end = self.end });

        for (rect_lines.lines) |rect_line| {
            if (rect_line.line.isPoint()) {
                continue;
            }

            for (tiles_and_lines.lines.items) |line| {
                //TODO:
                _ =
                    \\..............
                    \\.......#XXX#..
                    \\.......X...X..
                    \\..#XXXX#...X..
                    \\..X........X..
                    \\..#XXXXXX#.X..
                    \\.........X.X..
                    \\.........#X#..
                    \\..............
                ;

                //TODO:
                _ =
                    \\..............
                    \\.......#X_X#..
                    \\.......X...X..
                    \\..ÖXXXX#–Ä.X..
                    \\..X......|.X..
                    \\..#X_XXXX#.X..
                    \\..|......X.X..
                    \\..Ä––––––ÖX#..
                    \\..............
                ;

                std.debug.print("intersectsInward {} {}: {}\n", .{ rect_line, line, intersectsInward(rect_line, line) });
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
    return utils.desc(AreaNum)(ctx, a.area, b.area);
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

            try areas.append(AreaEntry.fromEdges(tile1, tile2));
        }
    }

    utils.sort(AreaEntry, areas.items, {}, cmpArea);

    for (areas.items) |entry| {
        std.debug.print("is valid {}: {}\n", .{ entry, try entry.isValidExt(tiles_and_lines) });
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

test "day 09 - intersectsInward" {
    const TestCase = struct { rect_line: RectLine, line: Line, intersects: bool };

    //TODO
    const FALSE = false;

    const test_cases = [_]TestCase{
        TestCase{
            .rect_line = RectLine{ .line = .{ .direction_y = .{
                .start_y = 7,
                .end_y = 3,
                .x_fixed = 2,
            } }, .inner_rect_dir = .Positive },
            .line = .{ .direction_x = .{
                .start_x = 9,
                .end_x = 2,
                .y_fixed = 5,
            } },
            .intersects = true,
        },
        TestCase{
            .rect_line = RectLine{ .line = .{ .direction_x = .{
                .start_x = 9,
                .end_x = 2,
                .y_fixed = 5,
            } }, .inner_rect_dir = .Negative },
            .line = .{ .direction_x = .{
                .start_x = 7,
                .end_x = 11,
                .y_fixed = 1,
            } },
            .intersects = false,
        },
        TestCase{
            .rect_line = .{ .line = .{ .direction_x = .{
                .start_x = 9,
                .end_x = 2,
                .y_fixed = 7,
            } }, .inner_rect_dir = .Negative },
            .line = .{ .direction_y = .{
                .start_y = 1,
                .end_y = 7,
                .x_fixed = 11,
            } },
            .intersects = false,
        },
        //TODO
        TestCase{
            .rect_line = .{ .line = .{ .direction_x = .{ .start_x = 9, .end_x = 2, .y_fixed = 7 } }, .inner_rect_dir = .Negative },
            .line = .{ .direction_y = .{ .start_y = 1, .end_y = 7, .x_fixed = 11 } },
            .intersects = false,
        },
        TestCase{
            .rect_line = .{ .line = .{ .direction_x = .{ .start_x = 9, .end_x = 2, .y_fixed = 7 } }, .inner_rect_dir = .Negative },
            .line = .{ .direction_x = .{ .start_x = 11, .end_x = 9, .y_fixed = 7 } },
            .intersects = false,
        },
        TestCase{
            .rect_line = .{ .line = .{ .direction_x = .{ .start_x = 9, .end_x = 2, .y_fixed = 7 } }, .inner_rect_dir = .Negative },
            .line = .{ .direction_y = .{ .start_y = 7, .end_y = 5, .x_fixed = 9 } },
            .intersects = false,
        },
        TestCase{
            .rect_line = .{ .line = .{ .direction_x = .{ .start_x = 9, .end_x = 2, .y_fixed = 5 } }, .inner_rect_dir = .Negative },
            .line = .{ .direction_x = .{ .start_x = 7, .end_x = 11, .y_fixed = 1 } },
            .intersects = false,
        },
        TestCase{
            .rect_line = .{ .line = .{ .direction_x = .{ .start_x = 9, .end_x = 2, .y_fixed = 5 } }, .inner_rect_dir = .Negative },
            .line = .{ .direction_y = .{ .start_y = 1, .end_y = 7, .x_fixed = 11 } },
            .intersects = false,
        },
        TestCase{
            .rect_line = .{ .line = .{ .direction_x = .{ .start_x = 9, .end_x = 2, .y_fixed = 5 } }, .inner_rect_dir = .Negative },
            .line = .{ .direction_x = .{ .start_x = 11, .end_x = 9, .y_fixed = 7 } },
            .intersects = false,
        },
        TestCase{
            .rect_line = .{ .line = .{ .direction_x = .{ .start_x = 9, .end_x = 2, .y_fixed = 5 } }, .inner_rect_dir = .Negative },
            .line = .{ .direction_y = .{ .start_y = 7, .end_y = 5, .x_fixed = 9 } },
            .intersects = false,
        },
        TestCase{
            .rect_line = .{ .line = .{ .direction_x = .{ .start_x = 9, .end_x = 2, .y_fixed = 5 } }, .inner_rect_dir = .Negative },
            .line = .{ .direction_x = .{ .start_x = 9, .end_x = 2, .y_fixed = 5 } },
            .intersects = false,
        },
        TestCase{
            .rect_line = .{ .line = .{ .direction_x = .{ .start_x = 9, .end_x = 2, .y_fixed = 5 } }, .inner_rect_dir = .Negative },
            .line = .{ .direction_y = .{ .start_y = 5, .end_y = 3, .x_fixed = 2 } },
            .intersects = FALSE,
        },

        //TODO
        // intersectsInward .{ .line = .{ .direction_x = .{ .start_x = 9, .end_x = 2, .y_fixed = 5 } }, .inner_rect_dir = .Negative } .{ .direction_y = .{ .start_y = 1, .end_y = 7, .x_fixed = 11 } }: true
    };

    for (test_cases) |test_case| {
        const result = intersectsInward(test_case.rect_line, test_case.line);

        if (result != test_case.intersects) {
            std.debug.print("Wrong test result for {} {}\n", .{ test_case.rect_line, test_case.line });
            try std.testing.expectEqual(test_case.intersects, result);
        }
    }
}

test "day 09 - isValid" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const input =
        \\7,1
        \\11,1
        \\11,7
        \\9,7
        \\9,5
        \\2,5
        \\2,3
        \\7,3
    ;

    var tiles_and_lines: TilesAndLines = try parseTiles2(gpa.allocator(), input);
    defer tiles_and_lines.deinit();

    const TestCase = struct { area: AreaEntry, valid: bool };

    const test_cases = [_]TestCase{
        TestCase{ .area = AreaEntry.fromEdges(.{ .x = 9, .y = 7 }, .{ .x = 2, .y = 3 }), .valid = false },
        TestCase{ .area = AreaEntry.fromEdges(.{ .x = 9, .y = 5 }, .{ .x = 2, .y = 3 }), .valid = true },
    };

    for (test_cases) |test_case| {
        const result = try test_case.area.isValid(tiles_and_lines);

        if (result != test_case.valid) {
            std.debug.print("Wrong test result for {}\n", .{test_case.area});
            try std.testing.expectEqual(test_case.valid, result);
        }
    }
}
