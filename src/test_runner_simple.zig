const std = @import("std");
const builtin = @import("builtin");
const tty = @import("tty");

pub fn main() !void {
    for (builtin.test_functions) |t| {
        t.func() catch |err| {
            try tty.StdoutWriter.print("{s} fail: {}\n", .{ t.name, err });
            continue;
        };
        try tty.StdoutWriter.print("{s} passed\n", .{t.name});
    }
}
