const double_linked_list = @import("double_linked_list.zig");
const std = @import("std");

pub fn StackManaged(comptime T: type) type {
    return struct {
        const Self = @This();
        pub const InnerType = double_linked_list.DoublyLinkedListManaged(T);

        inner: InnerType,

        pub const Slice = []T;
        pub const Node = InnerType.Node;
        pub const Iterator = InnerType.Iterator;

        pub fn init(allocator: std.mem.Allocator) Self {
            return Self{
                .inner = InnerType.init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.inner.deinit();
        }

        pub fn append(self: *Self, item: T) std.mem.Allocator.Error!void {
            try self.inner.append(item);
        }

        pub fn prepend(self: *Self, item: T) std.mem.Allocator.Error!void {
            try self.inner.prepend(item);
        }

        pub fn appendSlice(self: *Self, items: []const T) std.mem.Allocator.Error!void {
            for (items) |item| {
                try self.append(item);
            }
        }

        pub fn popLast(self: *Self) ?T {
            return self.inner.popLast();
        }

        pub fn popFirst(self: *Self) ?T {
            return self.inner.popLast();
        }

        pub fn empty(self: *Self) bool {
            return self.inner.empty();
        }

        pub fn iter(self: *Self) Iterator {
            return Iterator.init(self.inner);
        }

        pub fn remove(self: *Self, node: *Node) void {
            self.inner.remove(node);
        }
    };
}
