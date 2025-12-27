const double_linked_list = @import("double_linked_list.zig");
const std = @import("std");

//TODO: some memory management is duplicated in DoublyLinkedListManaged
pub fn StackManaged(comptime T: type) type {
    return struct {
        const Self = @This();
        pub const InnerType = double_linked_list.DoublyLinkedList(T);

        inner: InnerType,
        allocator: std.mem.Allocator,

        pub const Slice = []T;
        const Node = InnerType.Node;
        const Iterator = InnerType.Iterator;

        pub fn init(gpa: std.mem.Allocator) Self {
            return Self{
                .inner = InnerType.init(),
                .allocator = gpa,
            };
        }

        pub fn deinit(self: *Self) void {
            var it: ?*InnerType.Node = self.inner.first();

            while (it) |element| {
                it = element.next();
                self.allocator.destroy(element);
            }

            self.inner.deinit();
        }

        pub fn append(self: *Self, item: T) std.mem.Allocator.Error!void {
            const node_ptr: *Node = try self.allocator.create(Node);
            node_ptr.value = item;
            self.inner.append(node_ptr);
        }

        pub fn prepend(self: *Self, item: T) std.mem.Allocator.Error!void {
            const item_ptr: *T = try self.allocator.create(T);
            item_ptr.* = item;
            self.inner.prepend(item_ptr);
        }

        pub fn appendSlice(self: *Self, items: []const T) std.mem.Allocator.Error!void {
            for (items) |item| {
                try self.append(item);
            }
        }

        pub fn popLast(self: *Self) ?T {
            const result = self.inner.popLast();

            if (result) |node| {
                const value = node.value;
                self.allocator.destroy(node);
                return value;
            }
            return null;
        }

        pub fn popFirst(self: *Self) ?T {
            const result = self.inner.popFirst();

            if (result) |node| {
                const value = node.value;
                self.allocator.destroy(node);
                return value;
            }
            return null;
        }

        pub fn empty(self: *Self) bool {
            return self.inner.empty();
        }

        pub fn iter(self: *Self) Iterator {
            return Iterator.init(self.inner);
        }

        pub fn remove(self: *Self, node: *Node) void {
            self.inner.remove(node);
            self.allocator.destroy(node);
        }
    };
}
