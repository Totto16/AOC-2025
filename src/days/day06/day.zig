const std = @import("std");
const utils = @import("utils");

const OperationInt = u64;

const OperationType = enum(u8) {
    OperationTypeAdd = '+',
    OperationTypeMul = '*',
    OperationTypeInvalid = ' ',
};

const Operation = struct {
    op: OperationType,
    list: utils.ListManaged(OperationInt),

    pub fn deinit(self: *Operation) void {
        self.list.deinit();
    }

    pub fn perform(self: *const Operation) OperationInt {
        return switch (self.op) {
            .OperationTypeInvalid => {
                std.debug.panic("invalid op\n", .{});
            },
            .OperationTypeAdd => {
                var total: OperationInt = 0;

                for (self.list.items) |item| {
                    total += item;
                }

                return total;
            },
            .OperationTypeMul => {
                var total: OperationInt = 1;

                for (self.list.items) |item| {
                    total *= item;
                }

                return total;
            },
        };
    }

    pub fn deinitOperations(operations: *utils.ListManaged(Operation)) void {
        for (operations.items) |*operation| {
            operation.deinit();
        }
        operations.deinit();
    }
};

const ParseState = enum(u8) {
    ParseStateStart,
    ParseStateNumbers,
    ParseStateOperations,
};

fn parseOperations(allocator: utils.Allocator, input: utils.Str) utils.SolveErrors!utils.ListManaged(Operation) {
    var operations: utils.ListManaged(Operation) = utils.ListManaged(Operation).init(allocator);
    errdefer Operation.deinitOperations(&operations);

    var state: ParseState = .ParseStateStart;

    var iter = utils.splitSca(u8, input, '\n');

    while (iter.next()) |line| {
        if (line.len == 0) {
            continue;
        }

        switch (state) {
            .ParseStateStart => {
                state = .ParseStateNumbers;

                var iter2 = utils.splitSca(u8, line, ' ');

                while (iter2.next()) |num| {
                    if (num.len == 0) {
                        continue;
                    }

                    const number: OperationInt = utils.parseInt(OperationInt, num, 10) catch {
                        return error.ParseError;
                    };

                    var list: utils.ListManaged(OperationInt) = utils.ListManaged(OperationInt).init(allocator);
                    try list.append(number);

                    try operations.append(Operation{
                        .op = .OperationTypeInvalid,
                        .list = list,
                    });
                }
            },
            .ParseStateNumbers => {
                var iter2 = utils.splitSca(u8, line, ' ');

                if (std.mem.startsWith(u8, line, "+") or std.mem.startsWith(u8, line, "*")) {
                    state = .ParseStateOperations;
                }

                var i: u64 = 0;

                while (iter2.next()) |num| {
                    if (num.len == 0) {
                        continue;
                    }
                    defer i += 1;

                    if (state == .ParseStateNumbers) {
                        const number: OperationInt = utils.parseInt(OperationInt, num, 10) catch {
                            return error.ParseError;
                        };

                        try operations.items[i].list.append(number);
                    } else {
                        if (operations.items[i].op != .OperationTypeInvalid) {
                            return error.ParseError;
                        }

                        if (num.len != 1) {
                            return error.ParseError;
                        }

                        const op: OperationType = @enumFromInt(num[0]);

                        operations.items[i].op = op;
                    }
                }
            },
            .ParseStateOperations => {
                std.debug.panic("Invalid state, after parsing operations, no more lines should follow", .{});
            },
        }
    }

    for (operations.items) |item| {
        if (item.op == .OperationTypeInvalid) {
            return error.PredicateNotMet;
        }

        if (item.list.items.len != operations.items[0].list.items.len) {
            return error.PredicateNotMet;
        }
    }

    return operations;
}

fn solveFirst(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    var operations = try parseOperations(allocator, input);
    defer Operation.deinitOperations(&operations);

    var sum: u64 = 0;

    for (operations.items) |operation| {
        const result = operation.perform();

        sum += result;
    }

    return utils.Solution{ .u64 = sum };
}

fn solveSecond(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    _ = allocator;
    _ = input;

    return utils.Solution{ .u64 = 0 };
}

const generated = @import("generated");

pub const day = utils.Day{
    .solver = utils.Solver{ .individual = .{ .first = solveFirst, .second = solveSecond } },
    .examples = .{ .first = .{ .implemented = .{
        .solution = .{ .u64 = 4277556 },
    } }, .second = .pending },
    .root = generated.root,
    .num = generated.num,
    .same_input = true,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try day.run(gpa.allocator());
}

test "day 06" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try day.@"test"(gpa.allocator());
}
