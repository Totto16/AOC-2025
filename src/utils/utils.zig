const std = @import("std");
const tty = @import("tty");
const day = @import("day.zig");

// Add utility functions here

pub const Allocator = std.mem.Allocator;
pub const List = std.ArrayList;
pub fn ListManaged(comptime T: type) type {
    return std.array_list.AlignedManaged(T, null);
}
pub const Map = std.AutoHashMap;
pub const StrMap = std.StringHashMap;
pub const BitSet = std.DynamicBitSet;
pub const Str = []const u8;

// Useful stdlib functions
pub const tokenizeAny = std.mem.tokenizeAny;
pub const tokenizeSeq = std.mem.tokenizeSequence;
pub const tokenizeSca = std.mem.tokenizeScalar;
pub const splitAny = std.mem.splitAny;
pub const splitSeq = std.mem.splitSequence;
pub const splitSca = std.mem.splitScalar;
pub const indexOf = std.mem.indexOfScalar;
pub const indexOfAny = std.mem.indexOfAny;
pub const indexOfStr = std.mem.indexOfPosLinear;
pub const lastIndexOf = std.mem.lastIndexOfScalar;
pub const lastIndexOfAny = std.mem.lastIndexOfAny;
pub const lastIndexOfStr = std.mem.lastIndexOfLinear;
pub const trim = std.mem.trim;
pub const sliceMin = std.mem.min;
pub const sliceMax = std.mem.max;

pub const parseInt = std.fmt.parseInt;
pub const parseFloat = std.fmt.parseFloat;

pub const print = std.debug.print;
pub const assert = std.debug.assert;

pub const sort = std.sort.block;
pub const asc = std.sort.asc;
pub const desc = std.sort.desc;

pub const StdoutWriter = tty.StdoutWriter;
pub const StderrWriter = tty.StderrWriter;

pub const Day = day.Day;
pub const Solver = day.Solver;
pub const SolveResult = day.SolveResult;
pub const SolveErrors = day.SolveErrors;
pub const Solution = day.Solution;
pub const DayOptions = day.DayOptions;

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

            pub fn init(value: Self) Iterator {
                return Iterator{ .current = value.first() };
            }

            pub fn next(self: *Iterator) ?*Node {
                if (self.current) |node| {
                    const next_node = node.next();
                    if (next_node) |node2| {
                        self.current = next_node;
                        return node2;
                    }
                    return null;
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

        pub fn pop(self: *Self) ?*Node {
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

pub fn StackManaged(comptime T: type) type {
    return struct {
        const Self = @This();
        inner: DoublyLinkedList(T),
        allocator: Allocator,

        pub const Slice = []T;
        pub const InnerType = DoublyLinkedList(T);
        const Node = InnerType.Node;
        const Iterator = InnerType.Iterator;

        pub fn init(gpa: Allocator) Self {
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

        pub fn append(self: *Self, item: T) Allocator.Error!void {
            const node_ptr: *Node = try self.allocator.create(Node);
            node_ptr.value = item;
            self.inner.append(node_ptr);
        }

        pub fn prepend(self: *Self, item: T) Allocator.Error!void {
            const item_ptr: *T = try self.allocator.create(T);
            item_ptr.* = item;
            self.inner.prepend(item_ptr);
        }

        pub fn appendSlice(self: *Self, items: []const T) Allocator.Error!void {
            for (items) |item| {
                try self.append(item);
            }
        }

        pub fn popLast(self: *Self) ?T {
            const result = self.inner.pop();

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
