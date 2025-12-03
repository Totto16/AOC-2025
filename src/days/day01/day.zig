const std = @import("std");
const utils = @import("utils");

fn modHelper(input: i32, amount: u8) u8 {
    return @intCast(@mod(input, amount));
}

const LineValue = struct { typ: u8, number: i32 };

fn getLineValues(line: []const u8) ?LineValue {
    if (line.len == 0) {
        return null;
    }

    const typ = line[0];

    const rest = line[1..];

    const number = utils.parseInt(u32, rest, 10) catch {
        return null;
    };

    return .{ .typ = typ, .number = @intCast(number) };
}

fn solveFirst(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    _ = allocator;

    var dial: u8 = 50;
    var sum: u64 = 0;

    var iter = utils.splitSca(u8, input, '\n');

    while (iter.next()) |line| {
        if (line.len == 0) {
            continue;
        }

        const val = getLineValues(line);

        if (val == null) {
            return error.ParseError;
        }

        switch (val.?.typ) {
            'L' => {
                const dial_i32: i32 = dial;

                dial = modHelper(dial_i32 - (val.?.number), 100);
            },
            'R' => {
                const dial_i32: i32 = dial;

                dial = modHelper(dial_i32 + (val.?.number), 100);
            },
            else => |c| {
                utils.StderrWriter.printOnce("parse error: {c} was the start char", .{c}) catch {
                    return error.OtherError;
                };
                return error.ParseError;
            },
        }

        if (dial == 0) {
            sum += 1;
        }
    }

    return utils.Solution{ .u64 = sum };
}

fn getOverflowCount(dial: i32, num: i32, pos: bool) u32 {
    if (pos) {
        const amount: u32 = @intCast(@divTrunc(dial + num, 100));

        return amount;
    }

    const amount: u32 = @intCast(@divTrunc(num, 100));

    const aSum: i32 = @intCast(amount * 100);

    const rest: i32 = num - aSum;

    if (dial == 0) {
        return amount;
    }
    if (dial <= rest) {
        return amount + 1;
    }

    return amount;
}

fn solveSecond(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    _ = allocator;

    var dial: u8 = 50;
    var sum: u64 = 0;

    var iter = utils.splitSca(u8, input, '\n');

    while (iter.next()) |line| {
        if (line.len == 0) {
            continue;
        }

        const val = getLineValues(line);

        if (val == null) {
            return error.ParseError;
        }

        switch (val.?.typ) {
            'L' => {
                const dial_i32: i32 = dial;
                const num: i32 = val.?.number;

                sum += getOverflowCount(dial_i32, num, false);

                dial = modHelper(dial_i32 - num, 100);
            },
            'R' => {
                const dial_i32: i32 = dial;
                const num: i32 = val.?.number;

                sum += getOverflowCount(dial_i32, num, true);

                dial = modHelper(dial_i32 + (val.?.number), 100);
            },
            else => |c| {
                utils.StderrWriter.printOnce("parse error: {c} was the start char", .{c}) catch {
                    return error.OtherError;
                };
                return error.ParseError;
            },
        }
    }

    return utils.Solution{ .u64 = sum };
}

const generated = @import("generated");

const day = utils.Day{
    .solver = utils.Solver{ .individual = .{ .first = solveFirst, .second = solveSecond } },
    .examples = .{ .first = .{ .implemented = .{ .solution = .{ .u64 = 3 }, .real_value = .{ .u64 = 982 } } }, .second = .{ .implemented = .{ .solution = .{ .u64 = 6 }, .real_value = .{ .u64 = 6106 } } } },
    .root = generated.root,
    .day = generated.day,
    .same_input = true,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try day.run(gpa.allocator());
}

test "day 01" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try day.@"test"(gpa.allocator());
}

test "day 01 - manual" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const rotation_test_1 = try solveSecond(gpa.allocator(), "R1000");

    try std.testing.expectEqual(utils.Solution{ .u64 = 10 }, rotation_test_1);

    const rotation_test_2 = try solveSecond(gpa.allocator(), "R1000");

    try std.testing.expectEqual(utils.Solution{ .u64 = 10 }, rotation_test_2);
}
