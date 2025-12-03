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

fn solve(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
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
                std.debug.print("parse error: {c} was the start char", .{c});
                return error.ParseError;
            },
        }

        if (dial == 0) {
            sum += 1;
        }
    }

    return .{ .u64 = sum };
}

const day = utils.Day{ .solver = utils.Solver{ .both = solve }, .examples = .{ .first = .{ .implemented = .{ .solution = .{ .u64 = 3 } } }, .second = .todo }, .root = @import("generated").root };

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
