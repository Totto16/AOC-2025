const std = @import("std");
const utils = @import("utils");

const OperationInt = u64;

const OperationType = enum(u8) {
    OperationTypeAdd = '+',
    OperationTypeMul = '*',
    OperationTypeInvalid = ' ',

    pub fn fromString(str: []const u8) utils.SolveErrors!OperationType {
        if (std.mem.eql(u8, str, "+")) {
            return .OperationTypeAdd;
        } else if (std.mem.eql(u8, str, "*")) {
            return .OperationTypeMul;
        }

        return error.ParseError;
    }

    pub fn fromChar(char: u8) utils.SolveErrors!OperationType {
        if (char == '+') {
            return .OperationTypeAdd;
        } else if (char == '*') {
            return .OperationTypeMul;
        }

        return error.ParseError;
    }
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

                        const op: OperationType = try OperationType.fromString(num);

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

fn splitIterToArray2(allocator: utils.Allocator, iter: *std.mem.SplitIterator(u8, .scalar)) utils.SolveErrors!utils.ListManaged(utils.Str) {
    var array: utils.ListManaged(utils.Str) = utils.ListManaged(utils.Str).init(allocator);

    while (iter.next()) |val| {
        if (val.len == 0) {
            continue;
        }
        try array.append(val);
    }

    return array;
}

fn parseOperations2(allocator: utils.Allocator, input: utils.Str) utils.SolveErrors!utils.ListManaged(Operation) {
    var operations: utils.ListManaged(Operation) = utils.ListManaged(Operation).init(allocator);
    errdefer Operation.deinitOperations(&operations);

    var iter = utils.splitSca(u8, input, '\n');

    const strings = try splitIterToArray2(allocator, &iter);
    defer strings.deinit();

    const stringLength = strings.items[0].len;

    for (strings.items) |string| {
        if (string.len != stringLength) {
            return error.ParseError;
        }
    }

    const rotatedStringLength = strings.items.len;

    var rotatedStrings = utils.ListManaged(utils.ListManaged(u8)).init(allocator);
    try rotatedStrings.ensureTotalCapacity(rotatedStringLength);
    defer {
        for (rotatedStrings.items) |str| {
            str.deinit();
        }
        rotatedStrings.deinit();
    }

    for (0..stringLength) |_| {
        var list = utils.ListManaged(u8).init(allocator);
        try list.ensureTotalCapacity(rotatedStringLength);
        try rotatedStrings.append(list);
    }

    for (strings.items) |item| {
        for (item, 0..) |char, i| {
            try rotatedStrings.items[i].append(char);
        }
    }

    var current_op = Operation{ .list = utils.ListManaged(OperationInt).init(allocator), .op = .OperationTypeInvalid };

    const trimValues: []const u8 = " ";

    for (rotatedStrings.items) |list| {
        const string = list.items;

        if (std.mem.count(u8, string, " ") == rotatedStringLength) {
            if (current_op.op == .OperationTypeInvalid) {
                return error.PredicateNotMet;
            }

            try operations.append(current_op);

            current_op = Operation{ .list = utils.ListManaged(OperationInt).init(allocator), .op = .OperationTypeInvalid };

            continue;
        }

        const num = std.mem.trim(u8, string[0 .. string.len - 1], trimValues);
        const op_char = string[string.len - 1];

        const number: OperationInt = utils.parseInt(OperationInt, num, 10) catch {
            return error.ParseError;
        };

        try current_op.list.append(number);

        const op: OperationType = blk: {
            const op = OperationType.fromChar(op_char) catch {
                break :blk .OperationTypeInvalid;
            };
            break :blk op;
        };

        if (op != .OperationTypeInvalid) {
            if (current_op.op != .OperationTypeInvalid) {
                return error.PredicateNotMet;
            }

            current_op.op = op;
        }
    }

    if (current_op.op != .OperationTypeInvalid) {
        try operations.append(current_op);
    }

    return operations;
}

fn solveSecond(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    var operations = try parseOperations2(allocator, input);
    defer Operation.deinitOperations(&operations);

    var sum: u64 = 0;

    for (operations.items) |operation| {
        const result = operation.perform();

        sum += result;
    }

    return utils.Solution{ .u64 = sum };
}

const generated = @import("generated");

pub const day = utils.Day{
    .solver = utils.Solver{ .individual = .{ .first = solveFirst, .second = solveSecond } },
    .examples = .{
        .first = .{ .implemented = .{
            .solution = .{ .u64 = 4277556 },
            .real_value = .{ .u64 = 5595593539811 },
        } },
        .second = .{ .implemented = .{
            .solution = .{ .u64 = 3263827 },
            .real_value = .{ .u64 = 10153315705125 },
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

test "day 06" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try day.@"test"(gpa.allocator());
}
