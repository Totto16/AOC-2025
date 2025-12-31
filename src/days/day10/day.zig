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

fn cmpButtonWirings(ctx: void, buttons1: ButtonWiring, buttons2: ButtonWiring) bool {
    return utils.asc(usize)(ctx, buttons1.wirings.items.len, buttons2.wirings.items.len);
}

const Machine = struct {
    lights: utils.ListManaged(LightState),
    joltages: utils.ListManaged(JoltageNum),

    button_wirings: utils.ListManaged(ButtonWiring),

    pub fn deinit(self: *const Machine) void {
        for (self.button_wirings.items) |button_wiring| {
            button_wiring.deinit();
        }
        self.button_wirings.deinit();

        self.lights.deinit();
        self.joltages.deinit();
    }

    pub fn compactButtons(self: *const Machine, allocator: utils.Allocator, comptime MAX_STATE: comptime_int) utils.SolveErrors![]UIntFromBits(MAX_STATE) {
        var buttons: []UIntFromBits(MAX_STATE) = try allocator.alignedAlloc(UIntFromBits(MAX_STATE), std.mem.Alignment.of(UIntFromBits(MAX_STATE)), self.button_wirings.items.len);

        for (self.button_wirings.items, 0..) |b, i| {
            std.debug.assert(b.wirings.items.len <= MAX_STATE);
            var result: UIntFromBits(MAX_STATE) = @as(UIntFromBits(MAX_STATE), 0);

            for (b.wirings.items) |wiring| {
                result = result | @as(UIntFromBits(MAX_STATE), 1) << @intCast(wiring);
            }
            buttons[i] = result;
        }

        return buttons;
    }

    pub fn compactLights(self: *const Machine, comptime MAX_STATE: comptime_int) UIntFromBits(MAX_STATE) {
        std.debug.assert(self.lights.items.len <= MAX_STATE);

        var result: UIntFromBits(MAX_STATE) = @as(UIntFromBits(MAX_STATE), 0);

        for (self.lights.items, 0..) |light, i| {
            if (light == .on) {
                result = result | @as(UIntFromBits(MAX_STATE), 1) << @intCast(i);
            }
        }

        return result;
    }

    pub fn getJoltages(self: *const Machine, comptime MAX_STATE: comptime_int, allocator: utils.Allocator) utils.SolveErrors!JoltageState(MAX_STATE) {
        var result: JoltageState(MAX_STATE) = try JoltageState(MAX_STATE).init(allocator, self.joltages.items.len);

        for (self.joltages.items, 0..) |joltage, i| {
            const value: UIntFromBits(MAX_STATE) = std.math.cast(UIntFromBits(MAX_STATE), joltage) orelse return utils.SolveErrors.PredicateNotMet;
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

        // this helps, as "better" buttons get pressed first in the BFS and this helps in part 02!
        utils.sort(ButtonWiring, button_wirings.items, {}, cmpButtonWirings);

        const machine = Machine{ .button_wirings = button_wirings, .lights = lights, .joltages = joltages };

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

fn DfsTwo(comptime MAX_STATE: comptime_int, comptime MatrixType: type) type {
    const BitType: type = UIntFromBits(MAX_STATE);
    const MatrixT = Matrix(MatrixType);
    const Eqs = MatrixT.Equations;
    const SolveP = MatrixT.SolvePath;

    return struct {
        const Self = @This();

        alloc: utils.Allocator,
        buttons: []const BitType,
        equations: *const Eqs,
        solve_path: *const SolveP,
        max_joltage_value: JoltageNum,

        const State = struct {
            free_variables: []const MatrixType,
            min: u64,

            pub fn init(free_variables: []const MatrixType, min: u64) State {
                return .{
                    .free_variables = free_variables,
                    .min = min,
                };
            }

            pub fn get_new_value(self: *const State, allocator: utils.Allocator, value: MatrixType, min: u64) std.mem.Error!State {
                var new_free_variables = try allocator.alignedAlloc(MatrixType, std.mem.Alignment.of(MatrixType), self.free_variables.len + 1);

                for (0..self.free_variables.len) |i| {
                    new_free_variables[i] = self.free_variables[i];
                }

                new_free_variables[self.free_variables.len] = value;

                return State.init(new_free_variables, min);
            }

            pub fn deinit(self: *State, allocator: utils.Allocator) void {
                allocator.free(self.free_variables);
            }
        };

        pub fn init(allocator: utils.Allocator, buttons: []const BitType, equations: *const Eqs, solve_path: *const SolveP, max_joltage_value: JoltageNum) Self {
            return .{
                .alloc = allocator,
                .buttons = buttons,
                .equations = equations,
                .solve_path = solve_path,
                .max_joltage_value = max_joltage_value,
            };
        }

        const SolvedVariable = union(enum) {
            unknown,
            value: MatrixType,
        };

        pub fn solve_equations(self: *const Self, state: *const State) utils.SolveErrors!?u64 {
            var variables = try self.alloc.alignedAlloc(SolvedVariable, std.mem.Alignment.of(SolvedVariable), self.equations.variables.len);
            defer self.alloc.free(variables);

            for (0..self.equations.variables.len) |i| {
                variables[i] = .unknown;
            }

            std.debug.assert(state.free_variables.len == self.solve_path.free_variables.len);
            for (self.solve_path.free_variables, state.free_variables) |free_var_idx, free_var_value| {
                variables[free_var_idx] = .{ .value = free_var_value };
            }

            var sum: u64 = utils.sum(MatrixType, state.free_variables);

            for (self.solve_path.solve_order) |solveable| {
                const eq = self.equations.equations[solveable.eq_idx];

                var divide_after: ?MatrixType = null;

                const rhs: MatrixType = -eq.result;

                for (eq.depends) |dep| {
                    if (dep.idx == solveable.var_idx) {
                        switch (variables[dep.idx]) {
                            .unknown => {
                                if (divide_after != null) {
                                    std.debug.print("Solve path was wrong, needed to solve one value per eq, but tried multiple!");
                                    return utils.SolveErrors.OtherError;
                                }

                                divide_after = dep.multiplier;
                            },
                            .value => {
                                std.debug.print("Solve path was wrong, variable that needed solving was already solved!");
                                return utils.SolveErrors.OtherError;
                            },
                        }
                    } else {
                        switch (variables[dep.idx]) {
                            .unknown => {
                                std.debug.print("Solve path was wrong, variable that should be already solved was not solved yet!");
                                return utils.SolveErrors.OtherError;
                            },
                            .value => |v| {
                                rhs += v * dep.multiplier;
                            },
                        }
                    }
                }

                if (divide_after) |div_val| {
                    //TODO: check if int and float are handled correctly

                    const result = result_blk: {
                        switch (@typeInfo(MatrixType)) {
                            .int => |_| {
                                if (isZero(MatrixType, div_val)) {
                                    std.debug.print("divide should never be 0 here!");
                                    return utils.SolveErrors.OtherError;
                                }

                                const rem = std.math.rem(
                                    MatrixType,
                                    rhs,
                                    div_val,
                                ) catch {
                                    std.debug.panic("div wrong, this is an implementation error", .{});
                                    unreachable;
                                };

                                if (rem != 0) {
                                    // not a whole solution, return null to indicate that!
                                    return null;
                                }

                                const result = rhs / div_val;

                                break :result_blk result;
                            },
                            .float => |_| {
                                if (isZero(MatrixType, div_val)) {
                                    std.debug.print("divide should never be 0 here!");
                                    return utils.SolveErrors.OtherError;
                                }

                                const result = rhs / div_val;

                                if (floatIsNearInt(MatrixType, result)) {
                                    // not a whole solution, return null to indicate that!
                                    return null;
                                }

                                break :result_blk result;
                            },
                            else => {
                                @compileError("Not supported type for Matrix: " ++ @typeInfo(MatrixType));
                            },
                        }
                    };

                    variables[solveable.var_idx] = .{ .value = result };
                    sum += result;

                    if (sum >= state.min) {
                        return state.min;
                    }
                } else {
                    std.debug.print("Solve path was wrong, needed to solve one value per eq, but got none!");
                    return utils.SolveErrors.OtherError;
                }
            }

            return sum;
        }

        pub fn solve_rec(self: *Self, state: *const State) utils.SolveErrors!?u64 {
            std.debug.assert(self.solve_path.free_variables.len >= state.free_variables.len);

            if (self.solve_path.free_variables.len == state.free_variables.len) {
                return self.solve_equations(state);
            }

            var min = state.min;

            const previous_sum = utils.sum(state.free_variables);

            for (0..self.max_joltage_value + 1) |free_variable_value| {

                // short circuit, the min can't get any better!
                if (previous_sum + free_variable_value >= min) {
                    break;
                }

                const next_state: State = state.get_new_value(self.alloc, free_variable_value, min);
                defer next_state.deinit(self.alloc);

                // note null means, this combination is no valid result, so just ignore it
                const result = try self.solve_rec(next_state);

                if (result) |res| {
                    if (res < min) {
                        min = res;
                    }
                }
            }

            return min;
        }

        pub fn solve(self: *Self) utils.SolveErrors!?u64 {
            var free_variables_value = [_]MatrixType{};

            const state: State = State.init(&free_variables_value, std.math.maxInt(u64));

            const result = try self.solve_rec(&state);

            return result;
        }
    };
}

fn isZero(comptime Type: type, value: Type) bool {
    switch (@typeInfo(Type)) {
        .int => |_| {
            return value == @as(Type, 0);
        },
        .float => |_| {
            return @abs(value) <= std.math.floatEps(Type);
        },
        else => {
            @compileError("Not supported type for isZero: " ++ @typeInfo(Type));
        },
    }
}

fn pivotValue(comptime Type: type) Type {
    switch (@typeInfo(Type)) {
        .int => |_| {
            return @as(Type, 1);
        },
        .float => |_| {
            return @as(Type, 1.0);
        },
        else => {
            @compileError("Not supported type for pivotValue: " ++ @typeInfo(Type));
        },
    }
}

fn floatIsNearInt(comptime Type: type, value: Type) ?Type {
    switch (@typeInfo(Type)) {
        .int => |_| {
            @compileError("Not supported type for floatIsNearInt: " ++ @typeInfo(Type));
        },
        .float => |_| {
            const rounded = std.math.round(value);
            const is_an_int = @abs(rounded - value) <= std.math.floatEpsAt(Type, value);
            if (is_an_int) {
                return rounded;
            }
            return null;
        },
        else => {
            @compileError("Not supported type for floatIsNearInt: " ++ @typeInfo(Type));
        },
    }
}

fn isPivotValue(comptime Type: type, value: Type) bool {
    switch (@typeInfo(Type)) {
        .int => |_| {
            return value == pivotValue(Type);
        },
        .float => |_| {
            return @abs(value - pivotValue(Type)) <= std.math.floatEpsAt(Type, pivotValue(Type));
        },
        else => {
            @compileError("Not supported type for isZero: " ++ @typeInfo(Type));
        },
    }
}

fn Matrix(comptime Type: type) type {
    switch (@typeInfo(Type)) {
        .int => |info| {
            if (info.signedness == .unsigned) {
                @compileError("Only Signed ints are supported for the matrix, as gaussian op 3 needs subtraction to not underflow!");
            }
        },
        .float => |_| {
            //
        },
        else => {
            @compileError("Not supported type for Matrix: " ++ @typeInfo(Type));
        },
    }

    return struct {
        const Self = @This();
        pub const T = Type;

        // 2 d array with first index being the cols and than the rows, so it is easier to perform gauss on this!
        content: [][]T,
        row_len: usize,

        const EquationVariable = struct {
            idx: usize,
            multiplier: T,
        };

        const Equation = struct {
            depends: []EquationVariable,
            result: T,

            fn init(depends: []EquationVariable, result: T) Equation {
                return .{
                    .depends = depends,
                    .result = result,
                };
            }

            pub fn fromRow(allocator: utils.Allocator, row: []Type) std.mem.Allocator.Error!Equation {
                std.debug.assert(row.len > 1);

                const eq_len = row.len - 1;

                const result = row[eq_len];

                const depends = try allocator.alignedAlloc(EquationVariable, std.mem.Alignment.of(EquationVariable), eq_len);

                for (0..eq_len) |r| {
                    depends[r] = EquationVariable{ .idx = r, .multiplier = row[r] };
                }

                return Equation.init(depends, result);
            }

            pub fn deinit(self: *Equation, allocator: utils.Allocator) void {
                allocator.free(self.depends);
            }
        };

        const VariableType = enum(u8) {
            free,
            bound,
        };

        const SolvedState = enum(u8) {
            solved,
            unknown,
        };

        const Solveable = struct {
            eq_idx: usize,
            var_idx: usize,
        };

        const SolvePath = struct {
            free_variables: []usize,
            solve_order: []Solveable,

            pub fn init(free_variables: []usize, solve_order: []Solveable) SolvePath {
                return .{
                    .free_variables = free_variables,
                    .solve_order = solve_order,
                };
            }

            pub fn deinit(self: *Equations, allocator: utils.Allocator) void {
                allocator.free(self.free_variables);
                allocator.free(self.solve_order);
            }
        };

        const Equations = struct {
            variables: []VariableType, // map from idx to type
            equations: []Equation,

            pub fn init(variables: []VariableType, equations: []Equation) Equations {
                return .{
                    .variables = variables,
                    .equations = equations,
                };
            }

            const IsEqSolveable = union(enum) {
                no,
                yes: usize,
            };

            pub fn get_solve_path(self: *Equations, allocator: utils.Allocator) utils.SolveErrors!?SolvePath {
                var free_variables_list: utils.ListManaged(usize) = utils.ListManaged(usize).init(allocator);
                defer free_variables_list.deinit();

                var solve_order_list: utils.ListManaged(Solveable) = utils.ListManaged(Solveable).init(allocator);
                defer solve_order_list.deinit();

                const state: []SolvedState = try allocator.alignedAlloc(SolvedState, std.mem.Alignment.of(SolvedState), self.variables.len);
                defer allocator.free(state);

                for (self.variables, 0..) |variable, i| {
                    if (variable == .free) {
                        try free_variables_list.append(i);
                        state[i] = .solved;
                    } else {
                        state[i] = .unknown;
                    }
                }

                var equations_to_solve_idx: utils.ListManaged(usize) = utils.ListManaged(usize).init(allocator);
                defer equations_to_solve_idx.deinit();

                try equations_to_solve_idx.ensureTotalCapacityPrecise(self.equations.len);
                equations_to_solve_idx.expandToCapacity();

                for (0..self.equations.len) |i| {
                    equations_to_solve_idx.items[i] = i;
                }

                while (equations_to_solve_idx.items.len != 0) {
                    var made_progress = false;

                    for_loop: for (equations_to_solve_idx.items) |eq_idx| {
                        const eq = self.equations[eq_idx];

                        const is_solveable: IsEqSolveable = blk: {
                            var which_one: ?usize = null;

                            for (eq.depends) |dep| {
                                if (state[dep.idx] == .unknown) {
                                    if (which_one == null) {
                                        which_one = dep.idx;
                                    } else {
                                        break :blk .no;
                                    }
                                }
                            }

                            if (which_one) |which_idx| {
                                break :blk .{ .yes = which_idx };
                            } else {
                                break :blk .no;
                            }
                        };

                        switch (is_solveable) {
                            .no => {},
                            .yes => |var_idx| {
                                state[eq_idx] = .solved;
                                _ = equations_to_solve_idx.swapRemove(eq_idx);
                                made_progress = true;

                                //TODO: does every equation need to solve some variable, this data structure is correct if so, otherwise not!
                                try solve_order_list.append(.{ .eq_idx = eq_idx, .var_idx = var_idx });
                                break :for_loop;
                            },
                        }
                    }

                    // not solveable
                    if (!made_progress) {
                        return null;
                    }
                }

                const free_variables = try free_variables_list.toOwnedSlice();
                const solve_order = try solve_order_list.toOwnedSlice();

                return SolvePath.init(free_variables, solve_order);
            }

            pub fn deinit(self: *Equations, allocator: utils.Allocator) void {
                for (self.equations) |*eq| {
                    eq.deinit(allocator);
                }
                allocator.free(self.equations);
                allocator.free(self.variables);
            }
        };

        const CompatibleRow = struct {
            idx: usize,
            scalar: T,
        };

        pub fn init(content: [][]T) Self {
            std.debug.assert(content.len > 0);

            const row_len = content[0].len;
            for (content) |c| {
                std.debug.assert(c.len == row_len);
            }

            return .{ .content = content, .row_len = row_len };
        }

        pub fn deinit(self: *Self, allocator: utils.Allocator) void {
            for (self.content) |c| {
                allocator.free(c);
            }
            allocator.free(self.content);
        }

        //formatter
        pub fn format(self: *const Self, writer: *std.Io.Writer) !void {
            try writer.print("Size: {}x{}\n", .{ self.row_len, self.content.len });

            for (self.content) |c| {
                try writer.writeByte('[');
                for (c, 0..) |val, i| {
                    if (i != 0) {
                        try writer.writeAll(", ");
                    }
                    try writer.print("{}", .{val});
                }
                try writer.writeAll("]\n");
            }
        }

        // Gauss operation 1, Interchanging two rows
        fn gauss_op_1(self: *Self, row1: usize, row2: usize) void {
            std.debug.assert(row1 != row2);

            const temp = self.content[row1];
            self.content[row1] = self.content[row2];
            self.content[row2] = temp;
        }

        // Gauss operation 3, Adding a scalar multiple of one row to another
        fn gauss_op_3(self: *Self, row1: usize, row2: usize, scalar: T) void {
            std.debug.assert(!isZero(T, scalar));

            const row2_cont = self.content[row2];

            for (0..self.row_len) |i| {
                self.content[row1][i] += row2_cont[i] * scalar;
            }
        }

        fn rowIsUpperEchelonForm(self: *const Self, row: usize, pivotOffset: *usize) bool {
            for (0..self.row_len) |r| {
                const cont = self.content[row][r];
                if (row + pivotOffset.* == r) { // this is a pivot

                    // if we reached the pivot we are always in upper echelon form, if it is 0, we have a pivot offset here, but the next non zero value is the pivot, so all fine, we just need to increment the pivot Offset correctly

                    if (isZero(T, cont)) {
                        return true;
                    } else {
                        // but we need to check until the pivot value, to set the pivot offset correctly
                        pivotOffset.* += 1;
                    }
                } else if (r < row) {
                    // before the pivot all needs to be zero
                    if (!isZero(T, cont)) {
                        return false;
                    }
                } else {
                    // after the pivot!
                    // doesn't matter

                    // so just return
                    return true;
                }
            }

            return true;
        }

        fn isUpperEchelonForm(self: *const Self) bool {
            var pivotOffset: usize = 0;

            for (0..self.content.len) |c| {
                if (!self.rowIsUpperEchelonForm(c, &pivotOffset)) {
                    return false;
                }
            }

            return true;
        }

        const ScoreVal: type = i64;

        // this is the basis for a good solving, as we try to score good options > 0 and bad options < 0, the better ones are higher, even the bas ones
        fn getScoreForRow(self: *const Self, pivot: usize, current_row: usize) ScoreVal {
            std.debug.assert(self.row_len > 1);
            const row = self.content[current_row][0 .. self.row_len - 1];

            var result: ScoreVal = 0; // neutral

            for (row, 0..) |r, i| {
                if (i == pivot) {
                    if (isZero(Type, r)) {
                        // really bad, return immediately the worst value
                        return std.math.minInt(ScoreVal);
                    } else {
                        result += 10 * std.math.pow(u63, 10, @as(u63, @intCast(i)) + 1);
                    }
                } else if (i < pivot) {
                    // before pivot, 0 means GOOD, non zero means bad
                    if (isZero(Type, r)) {
                        result += 10 * std.math.pow(u63, 10, @as(u63, @intCast(i)) + 1);
                    } else {
                        // bad, so reset to minimum, so that it cant get better than a perfect one
                        result = std.math.minInt(ScoreVal);
                    }
                } else {
                    // after pivot, not that important, but still, more zeroes is better
                    if (isZero(Type, r)) {
                        result += 10 * std.math.pow(u63, 3, @as(u63, @intCast(i)) + 1);
                    } else {
                        result -= 10 * std.math.pow(u63, 3, @as(u63, @intCast(i)) + 1);
                    }
                }
            }

            return result;
        }

        pub fn solve(self: *Self, allocator: utils.Allocator) utils.SolveErrors!Equations {
            std.debug.print("SOLVE INIT\n", .{});
            std.debug.print("Matrix: {f}\n", .{self});

            // solving the equations using  Gaussian_elimination:
            // see: https://en.wikipedia.org/wiki/Gaussian_elimination

            // perform operation 1 until we have the best matrix

            for (0..self.content.len) |r1| {
                var best_row: ?usize = null;

                // "score" of best option so far, as options get a value based on their usefullness in this row
                var best_score: ScoreVal = std.math.minInt(ScoreVal);

                // search every row r2 for the best row for r1
                for (r1..self.content.len) |r2| {
                    const score = self.getScoreForRow(r1, r2);

                    if (score > best_score) {
                        best_score = score;
                        best_row = r2;
                    }
                }

                // found best row for this and now swap it, if it's possible

                if (best_row) |b_row| {
                    if (r1 != b_row) {
                        self.gauss_op_1(r1, b_row);
                    }
                }
            }

            std.debug.print("SOLVE OPER 1 finished\n", .{});
            std.debug.print("Matrix: {f}\n", .{self});

            // perform op 3 until we have upper echelon form, triangular form
            // skip op 2, as we already have 1s in the places, given by the fact, that we have 0 or 1 before each prefix!
            { // part 1, only zeros under the triangle
                const lower_upper_echelon_form_result = blk_result: {
                    var column_selected: usize = 0;
                    var row_selected: usize = 0;

                    var pivotOffset: usize = 0;

                    while (true) {
                        if (self.isUpperEchelonForm()) {
                            break :blk_result true;
                        }

                        // if we are trying to process out of the matrix, we failed!
                        if (column_selected >= self.content.len) {
                            break :blk_result false;
                        }

                        if (column_selected == 0) {
                            // special handling for row 0, as this can't change from other rows, but the pivot offset can change here

                            if (row_selected > column_selected + pivotOffset) {
                                column_selected = 1;
                                row_selected = 0;
                                continue;
                            }

                            const isPivot = row_selected == column_selected + pivotOffset;
                            // the row 0 has always a pivot, as we move it one along an the row in that case too, other cases don#t make sense and are a programming error
                            std.debug.assert(isPivot);

                            const cont = self.content[column_selected][row_selected];

                            if (isZero(Type, cont)) {
                                // we need to check for another pivot
                                pivotOffset += 1;
                            }

                            // we are not done, the next iteration decides that
                            row_selected += 1;
                            continue;
                        }

                        // if we are past the pivot for this line, ignore this and go to the next column
                        if (row_selected > column_selected + pivotOffset) {
                            column_selected += 1;
                            row_selected = 0;

                            { // assert that this row is in upper echelon form
                                var pivotOffsetTest = pivotOffset;
                                const result = self.rowIsUpperEchelonForm(column_selected - 1, &pivotOffsetTest);
                                std.debug.assert(result);
                                // assert, that we didn't need a pivot offset increment, as that should be done here in this loop, and this if should onl trigger AFTER every pivot + offset!
                                std.debug.assert(pivotOffset == pivotOffsetTest);
                            }
                            continue;
                        }

                        // if we are at or before the pivot, try to make it 1 / 0, this only works for columns after the first one, as we only can use previous columns!
                        if (row_selected <= column_selected + pivotOffset) {
                            std.debug.assert(column_selected != 0);

                            const isPivot = row_selected == column_selected + pivotOffset;

                            const target: Type = if (isPivot) pivotValue(Type) else @as(Type, 0);

                            const current_row_value = self.content[column_selected][row_selected];

                            std.debug.print("c sel: {} row sel: {} isPiv: {}\n", .{ column_selected, row_selected, isPivot });

                            blk_pivot_check: {
                                if (isPivot) {
                                    if (isPivotValue(Type, current_row_value)) {
                                        break :blk_pivot_check;
                                    }

                                    if (isZero(Type, current_row_value)) {
                                        // the pivots are offset from now on!
                                        pivotOffset += 1;
                                        break :blk_pivot_check;
                                    }
                                } else {
                                    if (isZero(Type, current_row_value)) {
                                        break :blk_pivot_check;
                                    }
                                }

                                var compatible_row: ?CompatibleRow = null;

                                switch (@typeInfo(Type)) {
                                    .int => |_| {
                                        loop1: for (0..column_selected) |c| {
                                            const value_to_check = self.content[c][row_selected];

                                            if (isZero(Type, value_to_check)) {
                                                // not feasable, as 0 * scalar can't possible be the same as the desired value
                                                continue;
                                            }

                                            const divResult = std.math.divFloor(
                                                Type,
                                                current_row_value - target,
                                                value_to_check,
                                            ) catch {
                                                std.debug.panic("div wrong, this is an implementation error", .{});
                                                unreachable;
                                            };

                                            std.debug.assert(divResult >= 0);

                                            if (divResult == 0) {
                                                // not really doable, go to the next one
                                            } else {
                                                compatible_row = CompatibleRow{
                                                    .idx = c,
                                                    .scalar = divResult,
                                                };
                                                break :loop1;
                                            }
                                        }
                                    },
                                    .float => |_| {
                                        loop1: for (0..column_selected) |c| {
                                            const value_to_check = self.content[c][row_selected];

                                            if (isZero(Type, value_to_check)) {
                                                // not feasible, as 0 * scalar can't possible be the same as the desired value
                                                continue;
                                            }

                                            const divResult = std.math.divFloor(
                                                Type,
                                                current_row_value - target,
                                                value_to_check,
                                            ) catch {
                                                std.debug.panic("div wrong, this is an implementation error", .{});
                                                unreachable;
                                            };

                                            std.debug.assert(divResult >= 0);

                                            if (isZero(T, divResult)) {
                                                // not really doable, go to the next one
                                            } else {
                                                // only mark this compatible, if this float is near to an int, it would be doable all the time, but that is not rellay helpfull here
                                                if (floatIsNearInt(T, divResult)) |int| {
                                                    compatible_row = CompatibleRow{
                                                        .idx = c,
                                                        .scalar = int,
                                                    };
                                                }
                                                break :loop1;
                                            }
                                        }
                                    },
                                    else => {
                                        @compileError("Not supported type for Matrix: " ++ @typeInfo(Type));
                                    },
                                }

                                if (compatible_row) |c_row| {
                                    self.gauss_op_3(column_selected, c_row.idx, -c_row.scalar);
                                    std.debug.assert(self.content[column_selected][row_selected] == 1);
                                } else {
                                    std.debug.print("Matrix: {f}\n", .{self});

                                    std.debug.print("No row found to make the resulting value {} with gauss operation 3\n", .{target});
                                    if (isPivot) {
                                        std.debug.panic("Although the strict requirement for upper echelon form doesn't need a 1 pivot, we need it here!", .{});
                                    } else {
                                        std.debug.panic("Not possible to reach 0", .{});
                                    }
                                    unreachable;
                                }
                            }

                            // even if this goes past the pivot, that is caught in the next iteration of this
                            row_selected += 1;
                            continue;
                        }
                    }
                };

                if (!lower_upper_echelon_form_result) {
                    std.debug.print("Failed to bring the matrix into upper echelon form: {any}\n", .{self.content});
                    return utils.SolveErrors.NotSolved;
                }
            }

            std.debug.print("SOLVE PART 1 finished\n", .{});
            std.debug.print("Matrix: {f}\n", .{self});

            blk_done: { // part 2, try to get zeros in the upper triangle

                var column_selected: usize = 0;
                var row_selected: usize = 0;

                var pivotOffset: usize = 0;

                while (true) {
                    // if we are trying to process out of the matrix, we are done
                    if (column_selected >= self.content.len) {
                        break :blk_done;
                    }

                    // if we are at the end of the row, (excluding the last values), go to the next line
                    if (row_selected >= self.row_len - 1) {
                        column_selected += 1;
                        row_selected = 0;
                        continue;
                    }

                    // if we are before the pivot for this line,
                    if (row_selected < column_selected + pivotOffset) {
                        row_selected += 1;
                        continue;
                    }

                    // if we are at the pivot for this line, check if the pivot offset needs to be incremented
                    if (row_selected == column_selected + pivotOffset) {
                        const current_row_value = self.content[column_selected][row_selected];

                        if (isZero(Type, current_row_value)) {
                            // the pivots are offset from now on!
                            pivotOffset += 1;
                        }

                        row_selected += 1;
                        continue;
                    }

                    // if we are after the pivot for this line, try to get the value to 0
                    if (row_selected > column_selected + pivotOffset) {
                        const current_row_value = self.content[column_selected][row_selected];

                        blk_zero_check: {
                            if (isZero(Type, current_row_value)) {
                                break :blk_zero_check;
                            }

                            var compatible_row: ?CompatibleRow = null;

                            switch (@typeInfo(Type)) {
                                .int => |_| {
                                    loop1: for (column_selected..self.content.len) |c| {

                                        // check if this line can be used, as the values until now are all zeros, so that nothing before this changes
                                        for (0..row_selected) |row_check_idx| {
                                            const value_at_idx = self.content[c][row_check_idx];
                                            if (!isZero(Type, value_at_idx)) {
                                                // not usable
                                                continue :loop1;
                                            }
                                        }

                                        const value_to_check = self.content[c][row_selected];

                                        if (isZero(Type, value_to_check)) {
                                            // not feasible, as 0 * scalar can't possible be the same as the desired value
                                            continue;
                                        }

                                        const divResult = std.math.divFloor(
                                            Type,
                                            current_row_value,
                                            value_to_check,
                                        ) catch {
                                            std.debug.panic("div wrong, this is an implementation error", .{});
                                            unreachable;
                                        };

                                        std.debug.assert(divResult >= 0);

                                        if (divResult == 0) {
                                            // not really doable, go to the next one
                                        } else {
                                            compatible_row = CompatibleRow{
                                                .idx = c,
                                                .scalar = divResult,
                                            };
                                            break :loop1;
                                        }
                                    }
                                },
                                .float => |_| {
                                    loop1: for (column_selected..self.content.len) |c| {

                                        // check if this line can be used, as the values until now are all zeros, so that nothing before this changes
                                        for (0..row_selected) |row_check_idx| {
                                            const value_at_idx = self.content[c][row_check_idx];
                                            if (!isZero(Type, value_at_idx)) {
                                                // not usable
                                                continue :loop1;
                                            }
                                        }

                                        const value_to_check = self.content[c][row_selected];

                                        if (isZero(Type, value_to_check)) {
                                            // not feasible, as 0 * scalar can't possible be the same as the desired value
                                            continue;
                                        }

                                        const divResult = std.math.divFloor(
                                            Type,
                                            current_row_value,
                                            value_to_check,
                                        ) catch {
                                            std.debug.panic("div wrong, this is an implementation error", .{});
                                            unreachable;
                                        };

                                        std.debug.assert(divResult >= 0);

                                        if (isZero(T, divResult)) {
                                            // not really doable, go to the next one
                                        } else {
                                            // only mark this compatible, if this float is near to an int, it would be doable all the time, but that is not rellay helpfull here
                                            if (floatIsNearInt(T, divResult)) |int| {
                                                compatible_row = CompatibleRow{
                                                    .idx = c,
                                                    .scalar = int,
                                                };
                                            }
                                            break :loop1;
                                        }
                                    }
                                },
                                else => {
                                    @compileError("Not supported type for Matrix: " ++ @typeInfo(Type));
                                },
                            }

                            if (compatible_row) |c_row| {
                                self.gauss_op_3(column_selected, c_row.idx, -c_row.scalar);
                                std.debug.assert(self.content[column_selected][row_selected] == 0);
                            } else {
                                // ignore otherwise, the resulting equations will be harder, but it unfortunately is like this

                            }
                        }

                        // even if this goes past the pivot, that is caught in the next iteration of this
                        row_selected += 1;
                        continue;
                    }
                }
            }

            std.debug.print("SOLVE PART 2 finished\n", .{});
            std.debug.print("Matrix: {f}\n", .{self});

            const variable_len = self.row_len - 1;
            std.debug.assert(variable_len > 0);

            const variables = try allocator.alignedAlloc(VariableType, std.mem.Alignment.of(VariableType), variable_len);

            {
                var current_variable: usize = 0;

                var pivotOffset: usize = 0;

                col_loop: for (0..self.content.len) |c| {
                    for (0..self.row_len - 1) |r| {
                        if (c + pivotOffset == r) {
                            const value = self.content[c][r];

                            if (isZero(Type, value)) {
                                pivotOffset += 1;

                                variables[current_variable] = .free;
                                current_variable += 1;
                            } else {
                                variables[current_variable] = .bound;
                                current_variable += 1;

                                continue :col_loop;
                            }
                        }
                    }
                }

                while (current_variable < variable_len) {
                    variables[current_variable] = .free;
                    current_variable += 1;
                }

                std.debug.assert(current_variable == variable_len);
            }

            if (@import("builtin").mode == .Debug) {
                var bound_variables: usize = 0;
                for (0..variable_len) |row| {
                    if (variables[row] == .bound) {
                        bound_variables += 1;
                    }
                }

                std.debug.assert(bound_variables == self.content.len);
            }

            const equation_len = self.content.len;
            std.debug.assert(equation_len > 0);

            const equations = try allocator.alignedAlloc(Equation, std.mem.Alignment.of(Equation), equation_len);

            for (0..equation_len) |col| {
                equations[col] = try Equation.fromRow(allocator, self.content[col]);
            }

            return Equations.init(variables, equations);
        }
    };
}

pub fn get_bit_at(button: UIntFromBits(MAX_BUTTON_SIZE), index: usize) bool {
    return button & (@as(UIntFromBits(MAX_BUTTON_SIZE), 1) << @intCast(index)) != 0;
}

pub fn castType(comptime Type: type, value: anytype) ?Type {
    switch (@typeInfo(Type)) {
        .int => |_| {
            return std.math.cast(Type, value);
        },
        .float => |_| {
            switch (@typeInfo(@TypeOf(value))) {
                .int => |_| {
                    return @as(Type, @floatFromInt(value));
                },
                .float => |_| {
                    return @as(Type, value);
                },
                else => {
                    @compileError("Not supported type for castType:" ++ @typeInfo(Type) ++ " to " ++ @typeInfo(@TypeOf(value)));
                },
            }
        },
        else => {
            @compileError("Not supported type for castType: " ++ @typeInfo(Type));
        },
    }
}

pub fn MatrixfromMachine(comptime MatrixType: type, allocator: utils.Allocator, buttons: []const UIntFromBits(MAX_BUTTON_SIZE), joltages: utils.ListManaged(JoltageNum)) utils.SolveErrors!Matrix(MatrixType) {
    const row_len = buttons.len + 1;
    const col_len = joltages.items.len;

    var content = try allocator.alignedAlloc([]MatrixType, std.mem.Alignment.of([]MatrixType), col_len);

    for (0..col_len) |c| {
        content[c] = try allocator.alignedAlloc(MatrixType, std.mem.Alignment.of(MatrixType), row_len);

        for (0..row_len) |r| {
            if (r + 1 == row_len) {
                content[c][r] = castType(MatrixType, joltages.items[c]) orelse return utils.SolveErrors.PredicateNotMet;
            } else {
                content[c][r] = if (get_bit_at(buttons[r], c)) 1 else 0;
            }
        }
    }

    return Matrix(MatrixType).init(content);
}

fn solveForFewestJoltagePresses(comptime MatrixType: type, allocator: utils.Allocator, machine: Machine) utils.SolveErrors!u64 {
    const compact_buttons = try machine.compactButtons(allocator, MAX_BUTTON_SIZE);
    defer allocator.free(compact_buttons);

    var matrix = try MatrixfromMachine(MatrixType, allocator, compact_buttons, machine.joltages);
    defer matrix.deinit(allocator);

    std.debug.print("MATRIX INIT\n", .{});

    var equations = try matrix.solve(allocator);
    defer equations.deinit(allocator);

    const maybe_solve_path = try equations.get_solve_path(allocator);

    if (maybe_solve_path == null) {
        return utils.SolveErrors.NotSolved;
    }
    const solve_path = maybe_solve_path.?;
    defer solve_path.deinit();

    const max_joltage_value = std.mem.max(JoltageNum, machine.joltages.items);

    const DFS = DfsTwo(MAX_BUTTON_SIZE, MatrixType);

    var dfs: DFS = DFS.init(allocator, compact_buttons, &equations, &solve_path, max_joltage_value);
    defer dfs.deinit();

    const result = try dfs.solve();

    if (result) |res| {
        return utils.Solution{ .u64 = res };
    }

    return utils.SolveErrors.NotSolved;
}

fn solveSecondImpl(comptime MatrixType: type, allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    var machines = try parseMachines(allocator, input);
    defer machines.deinit();

    var sum: u64 = 0;

    std.debug.print("{s}\n", .{input});

    for (machines.underlying.items) |machine| {
        const result = try solveForFewestJoltagePresses(MatrixType, allocator, machine);
        std.debug.print("GOT RESULT: {}\n", .{result});
        sum += result;
    }

    return utils.Solution{ .u64 = sum };
}

// I challenged myself to only use integers for the matrix operations, but also made the matrix type compatible with using floats (the comparisons need an epsilon there)
const MatrixTypeUsed: type = i64;

fn solveSecond(allocator: utils.Allocator, input: utils.Str) utils.SolveResult {
    return solveSecondImpl(MatrixTypeUsed, allocator, input);
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
