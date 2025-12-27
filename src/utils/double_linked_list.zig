const std = @import("std");

pub fn DoublyLinkedList(comptime T: type) type {
    return struct {
        const Self = @This();

        fn untyped_node_from_typed(typed_node: *Node) *InnerType.Node {
            if (comptime @offsetOf(Node, "node") != 0) {
                @compileError("Field `node` must be the first member of Node.");
            }

            return &typed_node.node;
        }

        fn typed_node_from_typed(typed_node: *InnerType.Node) *Node {
            return @fieldParentPtr("node", typed_node);
        }

        pub const Node = struct {
            node: std.DoublyLinkedList.Node,
            value: T,

            pub fn next(self: *Node) ?*Node {
                const next_node = self.node.next;

                if (next_node) |node| {
                    return typed_node_from_typed(node);
                }
                return null;
            }
        };

        pub const Iterator = struct {
            current: ?*Node,

            pub fn init(self: Self) Iterator {
                return Iterator{ .current = self.first() };
            }

            pub fn next(self: *Iterator) ?*Node {
                if (self.current) |node| {
                    const next_node = node.next();
                    self.current = next_node;
                    return node;
                }

                return null;
            }
        };

        const InnerType = std.DoublyLinkedList;

        inner: InnerType,

        pub fn init() Self {
            return Self{
                .inner = InnerType{
                    .first = null,
                    .last = null,
                },
            };
        }

        pub fn deinit(self: *Self) void {
            self.inner.first = null;
            self.inner.last = null;
        }

        pub fn first(self: *const Self) ?*Node {
            const first_node = self.inner.first;
            if (first_node) |node| {
                return typed_node_from_typed(node);
            }

            return null;
        }

        pub fn last(self: *const Self) ?*Node {
            const last_node = self.inner.last;
            if (last_node) |node| {
                return typed_node_from_typed(node);
            }

            return null;
        }

        pub fn append(self: *Self, new_node: *Node) void {
            const node_ptr = untyped_node_from_typed(new_node);
            self.inner.append(node_ptr);
        }

        pub fn popLast(self: *Self) ?*Node {
            const result = self.inner.pop();
            if (result) |node| {
                return typed_node_from_typed(node);
            }

            return null;
        }

        pub fn popFirst(self: *Self) ?*Node {
            const result = self.inner.popFirst();
            if (result) |node| {
                return typed_node_from_typed(node);
            }

            return null;
        }

        pub fn prepend(self: *Self, new_node: *Node) void {
            self.inner.prepend(new_node);
        }

        pub fn empty(self: *const Self) bool {
            return self.inner.first == null;
        }

        pub fn iter(self: *Self) Iterator {
            return Iterator{
                .current = self.inner.first,
            };
        }

        pub fn remove(self: *Self, node: *Node) void {
            return self.inner.remove(untyped_node_from_typed(node));
        }
    };
}

pub fn DoublyLinkedListManaged(comptime T: type) type {
    return struct {
        const Self = @This();

        const InnerType = DoublyLinkedList(T);

        const Node = InnerType.Node;

        inner: InnerType,
        alloc: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) Self {
            return Self{
                .inner = InnerType.init(),
                .alloc = allocator,
            };
        }

        pub fn deinit(self: *Self) void {
            while (self.inner.popLast()) |item| {
                self.alloc.destroy(item);
            }
        }

        pub fn first(self: *const Self) ?T {
            if (self.inner.first()) |f| {
                return f.value;
            }

            return null;
        }

        pub fn last(self: *const Self) ?T {
            if (self.inner.last()) |l| {
                return l.value;
            }

            return null;
        }

        pub fn append(self: *Self, item: T) std.mem.Allocator.Error!void {
            const managed_node: *Node = try self.alloc.create(Node);
            managed_node.value = item;
            self.inner.append(managed_node);
        }

        pub fn popLast(self: *Self) ?T {
            const result = self.inner.popLast();

            if (result) |node| {
                const value = node.value;
                self.alloc.destroy(node);
                return value;
            }
            return null;
        }

        pub fn popFirst(self: *Self) ?T {
            const result = self.inner.popFirst();

            if (result) |node| {
                const value = node.value;
                self.alloc.destroy(node);
                return value;
            }
            return null;
        }

        pub fn prepend(self: *Self, item: T) void {
            const managed_node: *Node = try self.alloc.create(Node);
            managed_node.value = item;
            self.inner.prepend(managed_node);
        }

        pub fn empty(self: *const Self) bool {
            return self.inner.empty();
        }

        pub const Iterator = struct {
            inner: InnerType.Iterator,

            pub fn init(self: Self) Iterator {
                return Iterator{
                    .inner = self.inner.iter(),
                };
            }

            pub fn next(self: *Iterator) ?T {
                if (self.inner.next()) |node| {
                    const next_node = node.next();
                    self.current = next_node;
                    return node.value;
                }

                return null;
            }
        };

        pub fn iter(self: *Self) Iterator {
            return Iterator{
                .inner = self.inner.iter(),
            };
        }
    };
}
