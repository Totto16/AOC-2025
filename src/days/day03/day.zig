const std = @import("std");
const utils = @import("utils");

const BatterieJolt = u8;

const BatterieBank = struct {
    inner: utils.ListManaged(BatterieJolt, null),

    pub fn deinit(self: *const BatterieBank) void {
        self.inner.deinit();
    }
};

const Batteries = struct {
    inner: utils.ListManaged(BatterieBank, null),

    pub fn deinit(self: *const Batteries) void {
        for (self.inner.items) |value| {
            value.deinit();
        }

        self.inner.deinit();
    }
};

fn parseBatteries(allocator: utils.Allocator, input: utils.Str) utils.SolveErrors!Batteries {
    var inner: utils.ListManaged(BatterieBank, null) = try utils.ListManaged(BatterieBank, null).initCapacity(allocator, 10);
    errdefer inner.deinit();

    var iter = utils.splitSca(u8, input, '\n');

    while (iter.next()) |line| {
        if (line.len == 0) {
            continue;
        }

        var batteries: utils.ListManaged(BatterieJolt, null) = try utils.ListManaged(BatterieJolt, null).initCapacity(allocator, 10);
        errdefer batteries.deinit();

        for (line) |c| {
            if (c < '0') {
                return error.ParseError;
            }

            if (c > '9') {
                return error.ParseError;
            }

            const jolt: BatterieJolt = c - '0';

            try batteries.append(jolt);
        }

        const bank = BatterieBank{ .inner = batteries };

        try inner.append(bank);
    }

    return Batteries{ .inner = inner };
}

pub fn max2DigitNumber(values: []const BatterieJolt) u64 {
    var best_tens: u64 = 0;
    var best: u64 = 0;

    for (values) |d| {
        const num = best_tens * 10 + d;
        if (num > best) best = num;

        if (d > best_tens) best_tens = d;
    }

    return best;
}

fn solveFirst(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    var batteries = try parseBatteries(allocator, input);
    defer batteries.deinit();

    var sum: u64 = 0;

    for (batteries.inner.items) |bank| {
        const jolts = bank.inner.items;

        const max_jolt: u64 = max2DigitNumber(jolts);

        sum += max_jolt;
    }

    return utils.Solution{ .u64 = sum };
}

fn solveSecond(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    _ = allocator;
    _ = input;

    return utils.Solution{ .u64 = 0 };
}

const day = utils.Day{
    .solver = utils.Solver{ .individual = .{ .first = solveFirst, .second = solveSecond } },
    .examples = .{ .first = .{ .implemented = .{
        .solution = .{ .u64 = 357 },
        .real_value = .{ .u64 = 16927 },
    } }, .second = .todo },
    .root = @import("generated").root,
    .same_input = true,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try day.run(gpa.allocator());
}

test "day 03" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try day.@"test"(gpa.allocator());
}

fn testGetBatteries(allocator: utils.Allocator, val: utils.Str) ![]const BatterieJolt {
    var res = try allocator.dupe(u8, val);

    for (0..res.len) |i| {
        const r: *u8 = &res[i];
        std.debug.assert(r.* >= '0' and r.* <= '9');
        r.* = r.* - '0';
    }

    return res;
}

test "day 03 - manual - 1 . part - faster solution" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    const fast_alloc = arena.allocator();

    try std.testing.expectEqual(98, max2DigitNumber(try testGetBatteries(fast_alloc, "987654321111111")));
    try std.testing.expectEqual(89, max2DigitNumber(try testGetBatteries(fast_alloc, "811111111111119")));
    try std.testing.expectEqual(78, max2DigitNumber(try testGetBatteries(fast_alloc, "234234234234278")));
    try std.testing.expectEqual(92, max2DigitNumber(try testGetBatteries(fast_alloc, "818181911112111")));
}
