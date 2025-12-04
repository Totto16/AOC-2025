const std = @import("std");
const utils = @import("utils");

const Paper = enum(u8) {
    Empty = 0,
    Paper = 1,
};

const Row = struct {
    inner: utils.ListManaged(Paper, null),

    pub fn deinit(self: *const Row) void {
        self.inner.deinit();
    }
};

const Rows = struct {
    inner: utils.ListManaged(Row, null),

    pub fn deinit(self: *const Rows) void {
        for (self.inner.items) |value| {
            value.deinit();
        }

        self.inner.deinit();
    }
};

fn parseRows(allocator: utils.Allocator, input: utils.Str) utils.SolveErrors!Rows {
    var inner: utils.ListManaged(Row, null) = try utils.ListManaged(Row, null).initCapacity(allocator, 10);
    errdefer inner.deinit();

    var iter = utils.splitSca(u8, input, '\n');

    while (iter.next()) |line| {
        if (line.len == 0) {
            continue;
        }

        var row: utils.ListManaged(Paper, null) = try utils.ListManaged(Paper, null).initCapacity(allocator, 10);
        errdefer row.deinit();

        for (line) |c| {
            const paper = blk: {
                switch (c) {
                    '@' => {
                        break :blk Paper.Paper;
                    },
                    '.' => {
                        break :blk Paper.Empty;
                    },
                    else => {
                        utils.StderrWriter.printOnceWithDefaultColor("parse error: {c} was the start char", .{c}) catch {
                            return error.OtherError;
                        };
                        return error.ParseError;
                    },
                }
            };

            try row.append(paper);
        }

        try inner.append(Row{ .inner = row });
    }

    return Rows{ .inner = inner };
}

fn solveFirst(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    var rows = try parseRows(allocator, input);
    defer rows.deinit();

    const tresh = 4;
    var result: u64 = 0;

    const position = [3]i32{ -1, 0, 1 };

    const height = rows.inner.items.len;

    for (rows.inner.items, 0..) |row, y_u| {
        const y_i: i32 = @intCast(y_u);
        const width = row.inner.items.len;

        for (row.inner.items, 0..) |cell, x_u| {
            const x_i: i32 = @intCast(x_u);
            if (cell != Paper.Paper) {
                continue;
            }

            var sum: u8 = 0;

            for (position) |i| {
                for (position) |j| {
                    if (i == 0 and j == 0) {
                        continue;
                    }

                    const x: i32 = x_i + i;
                    const y: i32 = y_i + j;

                    if (x >= width or x < 0) {
                        continue;
                    }

                    if (y >= height or y < 0) {
                        continue;
                    }

                    const paper_at_pos: Paper = rows.inner.items[@intCast(y)].inner.items[@intCast(x)];

                    if (paper_at_pos == Paper.Paper) {
                        sum += 1;
                    }
                }
            }

            if (sum < tresh) {
                result += 1;
            }
        }
    }

    return utils.Solution{ .u64 = result };
}

fn solveSecond(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    var rows = try parseRows(allocator, input);
    defer rows.deinit();

    const tresh = 4;
    var total_result: u64 = 0;

    outer: while (true) {
        var result: u64 = 0;

        const position = [3]i32{ -1, 0, 1 };

        const height = rows.inner.items.len;

        for (rows.inner.items, 0..) |row, y_u| {
            const y_i: i32 = @intCast(y_u);
            const width = row.inner.items.len;

            for (row.inner.items, 0..) |cell, x_u| {
                const x_i: i32 = @intCast(x_u);
                if (cell != Paper.Paper) {
                    continue;
                }

                var sum: u8 = 0;

                for (position) |i| {
                    for (position) |j| {
                        if (i == 0 and j == 0) {
                            continue;
                        }

                        const x: i32 = x_i + i;
                        const y: i32 = y_i + j;

                        if (x >= width or x < 0) {
                            continue;
                        }

                        if (y >= height or y < 0) {
                            continue;
                        }

                        const paper_at_pos: Paper = rows.inner.items[@intCast(y)].inner.items[@intCast(x)];

                        if (paper_at_pos == Paper.Paper) {
                            sum += 1;
                        }
                    }
                }

                if (sum < tresh) {
                    result += 1;

                    // we could store the positions and remove the papers later, but we can do it now, so everything termintes faster, as we might get to remove more, as this current one is also empty now!
                    rows.inner.items[y_u].inner.items[x_u] = Paper.Empty;
                }
            }
        }

        if (result == 0) {
            break :outer;
        }

        total_result += result;
    }

    return utils.Solution{ .u64 = total_result };
}

const generated = @import("generated");

pub const day = utils.Day{
    .solver = utils.Solver{ .individual = .{ .first = solveFirst, .second = solveSecond } },
    .examples = .{ .first = .{ .implemented = .{ .solution = .{ .u64 = 13 }, .real_value = .{ .u64 = 1549 } } }, .second = .{ .implemented = .{
        .solution = .{ .u64 = 43 },
    } } },
    .root = generated.root,
    .day = generated.day,
    .same_input = true,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try day.run(gpa.allocator());
}

test "day 04" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try day.@"test"(gpa.allocator());
}
