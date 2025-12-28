const std = @import("std");
const utils = @import("utils");

const LightState = enum(u8) {
    off,
    on,

    pub fn fromChar(char: u8) utils.SolveErrors!LightState {
        switch (char) {
            '#' => return .on,
            '.' => return .off,
            else => return utils.SolveErrors.ParseError,
        }
    }
};

const JoltageNum = u64;

const Indicator = struct {
    light: LightState,
    joltage: JoltageNum,
};

const ButtonWiringNum = u16;

const ButtonWiring = struct {
    wirings: utils.ListManaged(ButtonWiringNum),

    pub fn deinit(self: *const ButtonWiring) void {
        self.wirings.deinit();
    }
};

fn UIntFromBits(comptime bits: comptime_int) type {
    return std.meta.Int(.unsigned, bits);
}

pub fn JoltageState(comptime MAX_JOLTAGE_STATE_SIZE: comptime_int) type {
    return struct {
        const Self = @This();

        const T = UIntFromBits(MAX_JOLTAGE_STATE_SIZE);

        items: []T,
        alloc: utils.Allocator,

        pub fn init(allocator: utils.Allocator, size: usize) std.mem.Allocator.Error!Self {
            return .{
                .items = try allocator.alignedAlloc(T, std.mem.Alignment.of(T), size),
                .alloc = allocator,
            };
        }

        pub fn deinit(self: *Self) void {
            self.alloc.free(self.items);
        }

        pub fn dupe(self: *const Self) std.mem.Allocator.Error!Self {
            var result = try Self.init(self.alloc, self.items.len);

            for (0..self.items.len) |i| {
                result.items[i] = self.items[i];
            }

            return result;
        }

        pub fn len(self: *const Self) usize {
            return self.items.len;
        }
    };
}

const Machine = struct {
    indicators: utils.ListManaged(Indicator),
    button_wirings: utils.ListManaged(ButtonWiring),

    pub fn deinit(self: *const Machine) void {
        for (self.button_wirings.items) |button_wiring| {
            button_wiring.deinit();
        }
        self.button_wirings.deinit();

        self.indicators.deinit();
    }

    pub fn compactButtons(self: *const Machine, allocator: utils.Allocator, comptime MAX_STATE: comptime_int) utils.SolveErrors![]UIntFromBits(MAX_STATE) {
        var buttons: []UIntFromBits(MAX_STATE) = try allocator.alloc(UIntFromBits(MAX_STATE), self.button_wirings.items.len);

        for (self.button_wirings.items, 0..) |b, i| {
            std.debug.assert(self.button_wirings.items.len <= MAX_STATE);
            var result: UIntFromBits(MAX_STATE) = @as(UIntFromBits(MAX_STATE), 0);

            for (b.wirings.items) |wiring| {
                result = result | @as(UIntFromBits(MAX_STATE), 1) << @intCast(wiring);
            }
            buttons[i] = result;
        }

        return buttons;
    }

    pub fn compactLights(self: *const Machine, comptime MAX_STATE: comptime_int) UIntFromBits(MAX_STATE) {
        std.debug.assert(self.indicators.items.len <= MAX_STATE);

        var result: UIntFromBits(MAX_STATE) = @as(UIntFromBits(MAX_STATE), 0);

        for (self.indicators.items, 0..) |indicator, i| {
            if (indicator.light == .on) {
                result = result | @as(UIntFromBits(MAX_STATE), 1) << @intCast(i);
            }
        }

        return result;
    }

    pub fn getJoltages(self: *const Machine, comptime MAX_STATE: comptime_int, allocator: utils.Allocator) utils.SolveErrors!JoltageState(MAX_STATE) {
        var result: JoltageState(MAX_STATE) = try JoltageState(MAX_STATE).init(allocator, self.indicators.items.len);

        for (self.indicators.items, 0..) |indicator, i| {
            const value: UIntFromBits(MAX_STATE) = std.math.cast(UIntFromBits(MAX_STATE), indicator.joltage) orelse return utils.SolveErrors.PredicateNotMet;
            result.items[i] = value;
        }

        return result;
    }
};

const Machines = struct {
    underlying: utils.ListManaged(Machine),

    pub fn deinit(self: *const Machines) void {
        for (self.underlying.items) |machine| {
            machine.deinit();
        }
        self.underlying.deinit();
    }
};

fn parseMachines(allocator: utils.Allocator, input: utils.Str) utils.SolveErrors!Machines {
    var machines = Machines{ .underlying = utils.ListManaged(Machine).init(allocator) };

    var iter = utils.splitSca(u8, input, '\n');

    while (iter.next()) |line| {
        if (line.len == 0) {
            continue;
        }

        var lights: utils.ListManaged(LightState) = utils.ListManaged(LightState).init(allocator);
        var joltages: utils.ListManaged(JoltageNum) = utils.ListManaged(JoltageNum).init(allocator);
        var button_wirings: utils.ListManaged(ButtonWiring) = utils.ListManaged(ButtonWiring).init(allocator);

        var line_iter = utils.splitSca(u8, line, ' ');

        while (line_iter.next()) |decl| {
            if (decl.len == 0) {
                continue;
            }

            switch (decl[0]) {
                '[' => {
                    std.debug.assert(decl[decl.len - 1] == ']');

                    std.debug.assert(lights.items.len == 0);

                    const lights_buf = decl[1 .. decl.len - 1];

                    try lights.ensureTotalCapacity(lights_buf.len);

                    for (lights_buf) |lights_cont| {
                        const light = try LightState.fromChar(lights_cont);
                        try lights.append(light);
                    }
                },
                '(' => {
                    std.debug.assert(decl[decl.len - 1] == ')');

                    var wirings: utils.ListManaged(ButtonWiringNum) = utils.ListManaged(ButtonWiringNum).init(allocator);

                    var wirings_iter = utils.splitSca(u8, decl[1 .. decl.len - 1], ',');

                    while (wirings_iter.next()) |wirings_cont| {
                        const wiring = utils.parseInt(ButtonWiringNum, wirings_cont, 10) catch {
                            return error.ParseError;
                        };

                        try wirings.append(wiring);
                    }

                    try button_wirings.append(ButtonWiring{ .wirings = wirings });
                },
                '{' => {
                    std.debug.assert(decl[decl.len - 1] == '}');

                    std.debug.assert(joltages.items.len == 0);

                    var joltage_iter = utils.splitSca(u8, decl[1 .. decl.len - 1], ',');

                    while (joltage_iter.next()) |joltages_cont| {
                        const joltage = utils.parseInt(JoltageNum, joltages_cont, 10) catch {
                            return error.ParseError;
                        };

                        try joltages.append(joltage);
                    }
                },
                else => {
                    std.debug.panic("Not expected machine declaration start: {}", .{decl[0]});
                },
            }
        }

        if (lights.items.len != joltages.items.len) {
            std.debug.print("Expected to have the same amount of lights and joltages, but got {} and {}\n", .{ lights.items.len, joltages.items.len });
            return utils.SolveErrors.ParseError;
        }

        var indicators: utils.ListManaged(Indicator) = utils.ListManaged(Indicator).init(allocator);

        const indicators_len = lights.items.len;

        try indicators.ensureTotalCapacity(indicators_len);

        for (0..indicators_len) |i| {
            try indicators.append(Indicator{ .light = lights.items[i], .joltage = joltages.items[i] });
        }

        lights.deinit();
        joltages.deinit();

        const machine = Machine{ .button_wirings = button_wirings, .indicators = indicators };

        try machines.underlying.append(machine);
    }

    return machines;
}

fn BfsOne(comptime MAX_STATE: comptime_int, comptime MAX_DEPTH: comptime_int) type {
    const BitType: type = UIntFromBits(MAX_STATE);
    const DepthType: type = UIntFromBits(MAX_DEPTH);

    return struct {
        const Self = @This();

        alloc: utils.Allocator,
        buttons: []const BitType,
        items: utils.DoublyLinkedListManaged(Self.State),

        const State = struct {
            depth: DepthType,
            light_state: BitType,
            used_buttons_mask: BitType,
        };

        pub fn init(allocator: utils.Allocator, buttons: []const BitType) Self {
            return .{
                .alloc = allocator,
                .buttons = buttons,
                .items = utils.DoublyLinkedListManaged(Self.State).init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.items.deinit();
        }

        pub fn push(self: *Self, state: Self.State) std.mem.Allocator.Error!void {
            try self.items.append(state);
        }

        fn pushButton(button: BitType, state: BitType) BitType {
            return button ^ state;
        }

        fn isStateEmpty(state: BitType) bool {
            return state == 0;
        }

        fn alreadyPressed(button_mask: BitType, index: usize) bool {
            return button_mask & (@as(BitType, 1) << @intCast(index)) != 0;
        }

        pub fn step(self: *Self) utils.SolveErrors!?DepthType {
            const maybe_state = self.items.popFirst();

            if (maybe_state) |state| {
                for (self.buttons, 0..) |button, i| {
                    if (alreadyPressed(state.used_buttons_mask, i)) {
                        continue;
                    }

                    const next_state = pushButton(button, state.light_state);
                    const next_depth = state.depth + 1;

                    if (isStateEmpty(next_state)) {
                        return next_depth;
                    }

                    const new_used_button_mask = state.used_buttons_mask | @as(UIntFromBits(MAX_STATE), 1) << @intCast(i);

                    const new_state = Self.State{
                        .depth = next_depth,
                        .light_state = next_state,
                        .used_buttons_mask = new_used_button_mask,
                    };

                    try self.push(new_state);
                }

                return null;
            }
            return utils.SolveErrors.NotSolved;
        }
    };
}

const MAX_BUTTON_SIZE: comptime_int = 16;
const MAX_BFS_DEPTH_SIZE: comptime_int = 16;
const MAX_JOLTAGE_SIZE: comptime_int = MAX_BUTTON_SIZE;

fn solveForFewestLightPresses(allocator: utils.Allocator, machine: Machine) utils.SolveErrors!u64 {
    const compact_buttons = try machine.compactButtons(allocator, MAX_BUTTON_SIZE);
    defer allocator.free(compact_buttons);

    const compact_lights_state = machine.compactLights(MAX_BUTTON_SIZE);

    const BFS = BfsOne(MAX_BUTTON_SIZE, MAX_BFS_DEPTH_SIZE);

    var bfs: BFS = BFS.init(allocator, compact_buttons);
    defer bfs.deinit();

    try bfs.push(BFS.State{ .depth = 0, .light_state = compact_lights_state, .used_buttons_mask = 0 });

    while (true) {
        if (try bfs.step()) |result| {
            return result;
        }
    }

    return utils.SolveErrors.NotSolved;
}

fn solveFirst(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    var machines = try parseMachines(allocator, input);
    defer machines.deinit();

    var sum: u64 = 0;

    for (machines.underlying.items) |machine| {
        const result = try solveForFewestLightPresses(allocator, machine);

        sum += result;
    }

    return utils.Solution{ .u64 = sum };
}

fn BfsTwo(comptime MAX_STATE: comptime_int, comptime MAX_DEPTH: comptime_int, comptime MAX_JOLTAGE_STATE_SIZE: comptime_int) type {
    const BitType: type = UIntFromBits(MAX_STATE);
    const DepthType: type = UIntFromBits(MAX_DEPTH);

    return struct {
        const Self = @This();

        const JoltageStateT = JoltageState(MAX_JOLTAGE_STATE_SIZE);

        alloc: utils.Allocator,
        buttons: []const BitType,
        items: utils.DoublyLinkedListManaged(Self.State),

        const State = struct {
            depth: DepthType,
            joltage_state: JoltageStateT,
            invalid_buttons_mask: BitType,

            pub fn deinit(self: *State) void {
                self.joltage_state.deinit();
            }
        };

        pub fn init(allocator: utils.Allocator, buttons: []const BitType) Self {
            return .{
                .alloc = allocator,
                .buttons = buttons,
                .items = utils.DoublyLinkedListManaged(Self.State).init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            var it = self.items.iter();

            while (it.next_node()) |node| {
                node.value.deinit();
            }
            self.items.deinit();
        }

        pub fn push(self: *Self, state: Self.State) std.mem.Allocator.Error!void {
            try self.items.append(state);
        }

        pub fn pop(self: *Self) ?Self.State {
            return self.items.popFirst();
        }

        const ButtonResult = union(enum) {
            not_pressable_again,
            finished,
            next_state: JoltageStateT,
        };

        pub fn is_button_finished(state: JoltageStateT, button: BitType) bool {
            for (state.items, 0..) |current_state, i| {
                if (shouldPress(button, i)) {
                    if (current_state == 0) {
                        return true;
                    }
                }
            }

            return false;
        }

        pub fn get_invalid_buttons_mask(self: *const Self, start: BitType, state: JoltageStateT) BitType {
            var result: BitType = start;

            for (self.buttons, 0..) |button, i| {
                const is_finished = is_button_finished(state, button);
                if (is_finished) {
                    result = result | @as(BitType, 1) << @intCast(i);
                }
            }
            return result;
        }

        fn pushButton(self: *const Self, button: BitType, state: JoltageStateT) std.mem.Allocator.Error!ButtonResult {
            const state_len = state.len();
            std.debug.assert(state_len != 0);

            var next_state: JoltageStateT = try JoltageStateT.init(self.alloc, state_len);

            var all_zeros: bool = true;

            for (state.items, 0..) |current_state, i| {
                if (shouldPress(button, i)) {
                    if (current_state == 0) {
                        next_state.deinit();
                        return .not_pressable_again;
                    } else if (current_state != 1) {
                        all_zeros = false;
                    }

                    next_state.items[i] = current_state - 1;
                } else {
                    if (current_state != 0) {
                        all_zeros = false;
                    }

                    next_state.items[i] = current_state;
                }
            }

            if (all_zeros) {
                next_state.deinit();
                return .finished;
            }

            return .{ .next_state = next_state };
        }

        fn shouldPress(button_mask: BitType, index: usize) bool {
            return button_mask & (@as(BitType, 1) << @intCast(index)) != 0;
        }

        fn notPressableAgain(button_mask: BitType, index: usize) bool {
            return button_mask & (@as(BitType, 1) << @intCast(index)) != 0;
        }

        pub fn step(self: *Self) utils.SolveErrors!?DepthType {
            var maybe_state = self.pop();

            if (maybe_state) |*state| {
                defer state.deinit();

                for (self.buttons, 0..) |button, i| {
                    if (notPressableAgain(state.invalid_buttons_mask, i)) {
                        continue;
                    }

                    const button_result = try self.pushButton(button, state.joltage_state);
                    const next_depth = state.depth + 1;

                    switch (button_result) {
                        .finished => return next_depth,
                        .next_state => |next_state| {
                            const new_state = Self.State{
                                .depth = next_depth,
                                .joltage_state = next_state,
                                .invalid_buttons_mask = self.get_invalid_buttons_mask(state.invalid_buttons_mask, next_state),
                            };

                            try self.push(new_state);
                        },
                        .not_pressable_again => {
                            std.debug.panic("SHOULDN'T be reachable in the first place\n", .{});
                            unreachable;
                        },
                    }
                }

                return null;
            }
            return utils.SolveErrors.NotSolved;
        }
    };
}

fn solveForFewestJoltagePresses(allocator: utils.Allocator, machine: Machine) utils.SolveErrors!u64 {
    const compact_buttons = try machine.compactButtons(allocator, MAX_BUTTON_SIZE);
    defer allocator.free(compact_buttons);

    const joltage_state = try machine.getJoltages(MAX_JOLTAGE_SIZE, allocator);

    const BFS = BfsTwo(MAX_BUTTON_SIZE, MAX_BFS_DEPTH_SIZE, MAX_JOLTAGE_SIZE);

    var bfs: BFS = BFS.init(allocator, compact_buttons);
    defer bfs.deinit();

    try bfs.push(BFS.State{ .depth = 0, .joltage_state = joltage_state, .invalid_buttons_mask = bfs.get_invalid_buttons_mask(0, joltage_state) });

    while (true) {
        if (try bfs.step()) |result| {
            return result;
        }
    }

    return utils.SolveErrors.NotSolved;
}

fn solveSecond(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    var machines = try parseMachines(allocator, input);
    defer machines.deinit();

    var sum: u64 = 0;

    std.debug.print("{s}\n", .{input});

    for (machines.underlying.items) |machine| {
        const result = try solveForFewestJoltagePresses(allocator, machine);
        std.debug.print("GOT RESULT: {}\n", .{result});
        sum += result;
    }

    return utils.Solution{ .u64 = sum };
}

const generated = @import("generated");

pub const day = utils.Day{
    .solver = utils.Solver{ .individual = .{ .first = solveFirst, .second = solveSecond } },
    .solutions = .{
        .first = .{ .implemented = .{
            .solution = .{ .u64 = 7 },
            .real_value = .{ .u64 = 417 },
        } },
        .second = .{ .implemented = .{
            .solution = .{ .u64 = 33 },
            .real_value = null,
        } },
    },
    .inputs = .both_same,
    .root = generated.root,
    .num = generated.num,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try day.run(gpa.allocator());
}

test "day 10" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try day.@"test"(gpa.allocator());
}
