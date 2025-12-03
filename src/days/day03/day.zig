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

fn solveFirst(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    var batteries = try parseBatteries(allocator, input);
    defer batteries.deinit();

    var sum: u64 = 0;

    for (batteries.inner.items) |bank| {
        const jolts = bank.inner.items;

        var max_jolt: u64 = 0;

        // naive impl, improve
        for (0..jolts.len) |i| {
            for (i + 1..jolts.len) |j| {
                const jolt_val = jolts[i] * 10 + jolts[j];

                if (jolt_val > max_jolt) {
                    max_jolt = jolt_val;
                }
            }
        }

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
    } }, .second = .{ .implemented = .{ .solution = .{ .u64 = 0 }, .real_value = .{ .u64 = 1 } } } },
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
