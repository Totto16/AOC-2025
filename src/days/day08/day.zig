const std = @import("std");
const utils = @import("utils");

const BoxInt = u64;

const DistanceNum = f64;

fn pow2Sub(val1: u64, val2: u64) i64 {
    const val: i64 = @as(i64, @intCast(val1)) - @as(i64, @intCast(val2));
    return val * val;
}

const Box = struct {
    x: BoxInt,
    y: BoxInt,
    z: BoxInt,

    pub fn distance(self: *const Box, other: Box) DistanceNum {
        const distance_f: f64 = @floatFromInt(pow2Sub(self.x, other.x) + pow2Sub(self.y, other.y) + pow2Sub(self.z, other.z));

        return std.math.sqrt(distance_f);
    }
};

fn splitIterToArray(allocator: utils.Allocator, iter: *std.mem.SplitIterator(u8, .scalar)) utils.SolveErrors!utils.ListManaged(utils.Str) {
    var array: utils.ListManaged(utils.Str) = utils.ListManaged(utils.Str).init(allocator);

    while (iter.next()) |val| {
        try array.append(val);
    }

    return array;
}

fn parseBoxes(allocator: utils.Allocator, input: utils.Str) utils.SolveErrors!utils.ListManaged(Box) {
    var boxes: utils.ListManaged(Box) = utils.ListManaged(Box).init(allocator);

    var iter = utils.splitSca(u8, input, '\n');

    while (iter.next()) |line| {
        if (line.len == 0) {
            continue;
        }

        var iter2 = utils.splitSca(u8, line, ',');

        const values = try splitIterToArray(allocator, &iter2);
        defer values.deinit();

        if (values.items.len != 3) {
            return error.ParseError;
        }

        const x = utils.parseInt(BoxInt, values.items[0], 10) catch {
            return error.ParseError;
        };

        const y = utils.parseInt(BoxInt, values.items[1], 10) catch {
            return error.ParseError;
        };

        const z = utils.parseInt(BoxInt, values.items[2], 10) catch {
            return error.ParseError;
        };

        try boxes.append(Box{ .x = x, .y = y, .z = z });
    }

    return boxes;
}

const DistanceStruct = struct {
    distance: DistanceNum,
    i: usize,
    j: usize,
};

fn cmpDistance(ctx: void, a: DistanceStruct, b: DistanceStruct) bool {
    return utils.asc(DistanceNum)(ctx, a.distance, b.distance);
}

fn cmpAmount(ctx: void, a: utils.ListManaged(usize), b: utils.ListManaged(usize)) bool {
    return utils.desc(usize)(ctx, a.items.len, b.items.len);
}

const CircuitMap = struct {
    map: utils.ListManaged(usize),

    circuitList: utils.ListManaged(utils.ListManaged(usize)),

    pub fn init(allocator: utils.Allocator, size: usize) !CircuitMap {
        var circuitList = utils.ListManaged(utils.ListManaged(usize)).init(allocator);
        try circuitList.ensureTotalCapacity(size);

        var map = utils.ListManaged(usize).init(allocator);
        try map.ensureTotalCapacity(size);
        map.expandToCapacity();

        for (0..size) |idx| {
            var list = utils.ListManaged(usize).init(allocator);
            try list.append(idx);

            try circuitList.append(list);

            map.items[idx] = idx;
        }

        return CircuitMap{ .map = map, .circuitList = circuitList };
    }

    pub fn sort(self: *CircuitMap) void {
        utils.sort(utils.ListManaged(usize), self.circuitList.items, {}, cmpAmount);
    }

    pub fn connect(self: *CircuitMap, idx1: usize, idx2: usize) !bool {
        const circuitIdx1 = self.map.items[idx1];
        const circuitIdx2 = self.map.items[idx2];

        if (circuitIdx1 == circuitIdx2) {
            // already in the same circuit
            return false;
        }

        // move both to the first circuit
        self.map.items[idx2] = circuitIdx1;

        var idx2Content = &(self.circuitList.items[circuitIdx2]);

        try self.circuitList.items[circuitIdx1].appendSlice(idx2Content.*.items);

        idx2Content.clearRetainingCapacity();
        return true;
    }

    pub fn deinit(self: *CircuitMap) void {
        for (self.circuitList.items) |value| {
            value.deinit();
        }

        self.circuitList.deinit();
        self.map.deinit();
    }
};

fn solveFirst(allocator: utils.Allocator, input: utils.Str, category: utils.SolveCategory) utils.SolveResult {
    var boxes = try parseBoxes(allocator, input);
    defer boxes.deinit();

    const amountToCheck: u32 = if (category == .example) 10 else 1000;

    std.debug.print("solve {any}\n", .{category});

    var distances = utils.ListManaged(DistanceStruct).init(allocator);
    defer distances.deinit();

    for (0..boxes.items.len) |i| {
        for (i + 1..boxes.items.len) |j| {
            const box1 = boxes.items[i];
            const box2 = boxes.items[j];

            const dist = box1.distance(box2);

            try distances.append(DistanceStruct{ .distance = dist, .i = i, .j = j });
        }
    }

    std.debug.print("sort begin\n", .{});

    utils.sort(DistanceStruct, distances.items, {}, cmpDistance);

    std.debug.print("sort end\n", .{});

    // this is a "map" from index to connected circuits, as the key is a index, it is represented as array
    var circuitMap = try CircuitMap.init(allocator, boxes.items.len);
    defer circuitMap.deinit();

    {
        var i: u64 = 0;
        var idx: u64 = 0;

        while (i < amountToCheck) : (idx += 1) {
            const distance = distances.items[idx];

            // make these two boxes connected
            const hasConnected = try circuitMap.connect(distance.i, distance.j);
            if (hasConnected) {
                i += 1;
            } else {
                i += 1;
            }
        }
    }

    circuitMap.sort();

    const amountToMultiply: u32 = 3;

    var result: u64 = 1;

    for (0..amountToMultiply) |i| {
        const map = circuitMap.circuitList.items[i];
        result *= map.items.len;
        std.debug.print("result: {any}\n", .{map.items.len});
    }

    return utils.Solution{ .u64 = result };
}

fn solveSecond(allocator: utils.Allocator, input: utils.Str, category: utils.SolveCategory) utils.SolveResult {
    _ = allocator;
    _ = input;
    _ = category;

    return utils.Solution{ .u64 = 0 };
}

const generated = @import("generated");

pub const day = utils.Day{
    .solver = utils.Solver{ .individual = .{ .first = solveFirst, .second = solveSecond } },
    .solutions = .{
        .first = .{ .implemented = .{
            .solution = .{ .u64 = 40 },
            .real_value = .{ .u64 = 90036 },
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

test "day 08" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try day.@"test"(gpa.allocator());
}
