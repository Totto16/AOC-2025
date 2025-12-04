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

    for (values) |val| {
        const num = best_tens * 10 + val;
        if (num > best) best = num;

        if (val > best_tens) best_tens = val;
    }

    return best;
}

fn solveFirst(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    var batteries = try parseBatteries(allocator, input);
    defer batteries.deinit();

    var sum: u64 = 0;

    for (batteries.inner.items) |bank| {
        const jolts = bank.inner.items;

        std.debug.assert(jolts.len >= 2);
        const max_jolt: u64 = max2DigitNumber(jolts);

        sum += max_jolt;
    }

    return utils.Solution{ .u64 = sum };
}

pub fn maxSubsequenceDigitsImpl(
    values: []const u8,
    out: []u8,
) void {
    const out_len = out.len;

    var stack_len: usize = 0;
    var to_remove: usize = values.len - out_len;

    for (values) |val| {
        while (stack_len > 0 and to_remove > 0 and out[stack_len - 1] < val) {
            stack_len -= 1;
            to_remove -= 1;
        }

        if (stack_len < out_len) {
            out[stack_len] = val;
            stack_len += 1;
        } else {
            to_remove -= 1; // can't push, but consumed a digit, effectively skipping that digit
        }
    }
}

pub fn max12DigitNumber(values: []const BatterieJolt) u64 {
    // solve the maximum subsequence problem using a stack

    var out: [12]u8 = undefined;

    maxSubsequenceDigitsImpl(values, &out);

    var multiplier: u64 = 1;
    var result: u64 = 0;
    var i = out.len;

    while (i != 0) : (i -= 1) {
        const val = out[i - 1];

        result += val * multiplier;
        multiplier *= 10;
    }

    return result;
}

fn solveSecond(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    var batteries = try parseBatteries(allocator, input);
    defer batteries.deinit();

    var sum: u64 = 0;

    for (batteries.inner.items) |bank| {
        const jolts = bank.inner.items;

        std.debug.assert(jolts.len >= 12);
        const max_jolt: u64 = max12DigitNumber(jolts);

        sum += max_jolt;
    }

    return utils.Solution{ .u64 = sum };
}

const generated = @import("generated");

pub const day = utils.Day{
    .solver = utils.Solver{ .individual = .{ .first = solveFirst, .second = solveSecond } },
    .examples = .{ .first = .{ .implemented = .{
        .solution = .{ .u64 = 357 },
        .real_value = .{ .u64 = 16927 },
    } }, .second = .{ .implemented = .{
        .solution = .{ .u64 = 3121910778619 },
        .real_value = .{ .u64 = 167384358365132 },
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

test "day 03 - manual - 1. part - faster solution" {
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

test "day 03 - manual - 2. part - faster solution" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    const fast_alloc = arena.allocator();

    try std.testing.expectEqual(987654321111, max12DigitNumber(try testGetBatteries(fast_alloc, "987654321111111")));
    try std.testing.expectEqual(811111111119, max12DigitNumber(try testGetBatteries(fast_alloc, "811111111111119")));
    try std.testing.expectEqual(434234234278, max12DigitNumber(try testGetBatteries(fast_alloc, "234234234234278")));
    try std.testing.expectEqual(888911112111, max12DigitNumber(try testGetBatteries(fast_alloc, "818181911112111")));
}
