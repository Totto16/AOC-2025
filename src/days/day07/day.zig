const std = @import("std");
const utils = @import("utils");

const CellType = enum(u8) {
    Space = '.',
    TachyonStart = 'S',
    TachyonBeam = '|',
    Splitter = '^',

    pub fn fromChar(char: u8) CellType {
        return @enumFromInt(char);
    }
};

const Field = struct {
    inner: utils.ListManaged(utils.ListManaged(CellType)),

    pub fn deinit(self: *Field) void {
        for (self.inner.items) |*operation| {
            operation.deinit();
        }
        self.inner.deinit();
    }
};

fn parseField(allocator: utils.Allocator, input: utils.Str) utils.SolveErrors!Field {
    var field: utils.ListManaged(utils.ListManaged(CellType)) = utils.ListManaged(utils.ListManaged(CellType)).init(allocator);

    var iter = utils.splitSca(u8, input, '\n');

    while (iter.next()) |line| {
        if (line.len == 0) {
            continue;
        }

        var list: utils.ListManaged(CellType) = utils.ListManaged(CellType).init(allocator);

        for (line) |char| {
            const cell = CellType.fromChar(char);
            try list.append(cell);
        }

        try field.append(list);
    }

    return Field{ .inner = field };
}

fn solveFirst(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    var field = try parseField(allocator, input);
    defer field.deinit();

    var splits: u64 = 0;

    var i: u64 = 0;

    while (i + 1 < field.inner.items.len) : (i += 1) {
        const line = field.inner.items[i];

        const down_row = &(field.inner.items[i + 1].items);

        for (line.items, 0..) |cell, x| {
            switch (cell) {
                .TachyonStart, .TachyonBeam => {
                    const down = &(down_row.*[x]);

                    switch (down.*) {
                        .Space => {
                            down.* = .TachyonBeam;
                            // do nothing. make this field a tachyon
                        },
                        .TachyonBeam => {
                            // do nothing, already a splitted tachyon
                        },
                        .Splitter => {
                            if (x > 0) {
                                const down_left = &(down_row.*[x - 1]);
                                if (down_left.* == .Space) {
                                    down_left.* = .TachyonBeam;
                                }
                            }

                            if (x + 1 < down_row.*.len) {
                                const down_right = &(down_row.*[x + 1]);
                                if (down_right.* == .Space) {
                                    down_right.* = .TachyonBeam;
                                }
                            }

                            splits += 1;
                        },
                        .TachyonStart => {
                            std.debug.panic("TachyonStart not at the top\n", .{});
                        },
                    }
                },
                .Space, .Splitter => {},
            }
        }
    }

    return utils.Solution{ .u64 = splits };
}

fn solveSecond(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    var field = try parseField(allocator, input);
    defer field.deinit();

    // this map store the amount of different paths there are for the current line, it is updated every line
    var timelines_map: []u64 = try allocator.alloc(u64, field.inner.items[0].items.len);
    defer allocator.free(timelines_map);

    for (field.inner.items[0].items, 0..) |item, i| {
        if (item == .TachyonStart) {
            timelines_map[i] = 1;
        } else {
            timelines_map[i] = 0;
        }
    }

    var timelines: u64 = 1;

    var i: u64 = 0;

    while (i + 1 < field.inner.items.len) : (i += 1) {
        const line = field.inner.items[i];

        const down_row = &(field.inner.items[i + 1].items);

        for (line.items, 0..) |cell, x| {
            switch (cell) {
                .TachyonStart, .TachyonBeam => {
                    const down = &(down_row.*[x]);

                    switch (down.*) {
                        .Space => {
                            down.* = .TachyonBeam;
                            // do nothing. make this field a tachyon
                        },
                        .TachyonBeam => {
                            // do nothing, already a splitted tachyon
                        },
                        .Splitter => {
                            const value_x: u64 = timelines_map[x];

                            if (x > 0) {
                                const down_left = &(down_row.*[x - 1]);
                                switch (down_left.*) {
                                    .Space, .TachyonBeam => {
                                        down_left.* = .TachyonBeam;

                                        timelines_map[x - 1] += value_x;
                                    },
                                    .TachyonStart, .Splitter => {
                                        std.debug.panic("invalid cell under splitter\n", .{});
                                    },
                                }
                            }

                            if (x + 1 < down_row.*.len) {
                                const down_right = &(down_row.*[x + 1]);

                                switch (down_right.*) {
                                    .Space, .TachyonBeam => {
                                        down_right.* = .TachyonBeam;

                                        timelines_map[x + 1] += value_x;
                                    },
                                    .TachyonStart, .Splitter => {
                                        std.debug.panic("invalid cell under splitter\n", .{});
                                    },
                                }
                            }

                            // this "resets" the map for the next line, as under a splitter no more tachyons can appear, this works and the is no need for a "double buffer" like map
                            timelines += value_x;
                            timelines_map[x] = 0;
                        },
                        .TachyonStart => {
                            std.debug.panic("TachyonStart not at the top\n", .{});
                        },
                    }
                },
                .Space, .Splitter => {},
            }
        }
    }

    return utils.Solution{ .u64 = timelines };
}

const generated = @import("generated");

pub const day = utils.Day{
    .solver = utils.Solver{ .individual = .{ .first = solveFirst, .second = solveSecond } },
    .examples = .{
        .first = .{ .implemented = .{
            .solution = .{ .u64 = 21 },
            .real_value = .{ .u64 = 1555 },
        } },
        .second = .{ .implemented = .{
            .solution = .{ .u64 = 40 },
            .real_value = .{ .u64 = 12895232295789 },
        } },
    },
    .root = generated.root,
    .num = generated.num,
    .same_input = true,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try day.run(gpa.allocator());
}

test "day 07" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try day.@"test"(gpa.allocator());
}

test "day 07 - small" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const day_small_test = utils.Day{
        .solver = utils.Solver{ .individual = .{ .first = solveFirst, .second = solveSecond } },
        .examples = .{
            .first = .pending,
            .second = .{ .implemented = .{ .solution = .{ .u64 = 4 }, .file = "example_01_small.txt" } },
        },
        .root = generated.root,
        .num = generated.num,
        .same_input = true,
    };

    try day_small_test.@"test"(gpa.allocator());
}
