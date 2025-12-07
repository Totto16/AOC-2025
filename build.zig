const std = @import("std");
const CompileStep = std.Build.Step.Compile;

const required_zig_version = std.SemanticVersion.parse("0.15.0") catch unreachable;

const ModuleKV = struct { name: []const u8, module: *std.Build.Module };

fn linkModules(obj: *CompileStep, modules: []const ModuleKV) void {
    for (modules) |module| {
        obj.root_module.addImport(module.name, module.module);
    }
}

fn getFileRoot(alloc: std.mem.Allocator, file: []const u8) !([]const u8) {
    const cwd = try std.fs.cwd().realpathAlloc(alloc, ".");
    defer alloc.free(cwd);

    const abs_file = try std.fs.path.join(alloc, &[_][]const u8{ cwd, file });
    defer alloc.free(abs_file);

    const dirname = std.fs.path.dirname(abs_file);

    if (dirname == null) {
        return error.EmptyFilePath;
    }

    return alloc.dupe(u8, dirname.?);
}

fn fileIsPresent(alloc: std.mem.Allocator, file: []const u8) !bool {
    const cwd = try std.fs.cwd().realpathAlloc(alloc, ".");
    defer alloc.free(cwd);

    const abs_file = try std.fs.path.join(alloc, &[_][]const u8{ cwd, file });
    defer alloc.free(abs_file);

    const opened_file = std.fs.openFileAbsolute(abs_file, .{ .mode = .read_only }) catch |err| {
        if (err == error.FileNotFound) {
            return false;
        }
        return err;
    };

    opened_file.close();

    return true;
}

fn generateFile(b: *std.Build, alloc: std.mem.Allocator, file_content: []const u8, generatedName: []const u8) !std.Build.LazyPath {
    const generate_file_src = try b.cache_root.join(alloc, &[_][]const u8{generatedName});
    defer alloc.free(generate_file_src);

    {
        const dirname = std.fs.path.dirname(generate_file_src);

        if (dirname) |d| {
            std.fs.cwd().makeDir(d) catch |err| blk: {
                if (err != error.PathAlreadyExists) {
                    return err;
                }
                break :blk;
            };
        }

        const opened_file = std.fs.cwd().createFile(generate_file_src, .{}) catch |err| blk: {
            if (err != error.FileAlreadyExists) {
                return err;
            }
            break :blk null;
        };

        if (opened_file) |f| {
            defer f.close();
            try f.writeAll(file_content);
        }
    }

    return b.path(generate_file_src).dupe(b);
}

const DayObj = struct {
    num: u32,
    module_kv: ModuleKV,
};

// taken and modified from: https://github.com/SpexGuy/Zig-AoC-Template/
pub fn build(b: *std.Build) !void {
    if (comptime @import("builtin").zig_version.order(required_zig_version) == .lt) {
        std.debug.print("Warning: Your version of Zig too old. You will need to download a newer build\n", .{});
        std.os.exit(1);
    }

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const install_all = b.step("install_all", "Install all days");
    const run_all = b.step("run_all", "Run all days");
    const test_all = b.step("test_all", "Run all tests");

    const ansi_term_dep = b.dependency("ansi_term", .{ .target = target, .optimize = optimize });

    const ansi_term_kv = ModuleKV{ .module = ansi_term_dep.module("ansi_term"), .name = "ansi_term" };

    const terminal_progress_dep = b.dependency("terminal_progress", .{ .target = target, .optimize = optimize });

    const terminal_progress_kv = ModuleKV{ .module = terminal_progress_dep.module("terminal_progress"), .name = "terminal_progress" };

    const tty_mod = b.createModule(.{
        .root_source_file = b.path("src/utils/tty.zig"),
        .target = target,
        .optimize = optimize,
    });

    tty_mod.addImport(ansi_term_kv.name, ansi_term_kv.module);

    const tty_mod_kv = ModuleKV{ .module = tty_mod, .name = "tty" };

    const utils_mod = b.createModule(.{
        .root_source_file = b.path("src/utils/utils.zig"),
        .target = target,
        .optimize = optimize,
    });

    utils_mod.addImport(tty_mod_kv.name, tty_mod_kv.module);
    utils_mod.addImport(terminal_progress_kv.name, terminal_progress_kv.module);

    const utils_mod_kv = ModuleKV{ .module = utils_mod, .name = "utils" };

    const test_runner: std.Build.Step.Compile.TestRunner = std.Build.Step.Compile.TestRunner{ .path = b.path("src/utils/test_runner.zig"), .mode = .simple };

    var days: std.array_list.AlignedManaged(DayObj, null) = std.array_list.AlignedManaged(DayObj, null).init(alloc);
    defer days.deinit();

    // Set up a compile target for each day
    var day: u32 = 1;
    while (day <= 25) : (day += 1) {
        const dayString = b.fmt("day{:0>2}", .{day});
        const zigFile = b.fmt("src/days/{s}/day.zig", .{dayString});

        if (try fileIsPresent(alloc, zigFile)) {
            const zigFileRoot = try getFileRoot(alloc, zigFile);

            const generatedName = b.fmt("day{:0>2}/generated.zig", .{day});

            const file_content = b.fmt(
                \\pub const root: []const u8 = "{s}";
                \\pub const num: u32 = {d};
                \\
            , .{ zigFileRoot, day });
            alloc.free(zigFileRoot);

            const file_generated_src = try generateFile(b, alloc, file_content, generatedName);

            const generated_module = b.createModule(.{
                .root_source_file = file_generated_src,
                .target = target,
                .optimize = optimize,
            });

            const generated_module_kv = ModuleKV{ .module = generated_module, .name = "generated" };

            const day_exe = b.addExecutable(.{
                .name = dayString,
                .root_module = b.createModule(.{
                    .root_source_file = b.path(zigFile),
                    .target = target,
                    .optimize = optimize,
                }),
            });

            linkModules(day_exe, &[_]ModuleKV{ utils_mod_kv, generated_module_kv });

            try days.append(DayObj{ .num = day, .module_kv = ModuleKV{ .module = day_exe.root_module, .name = dayString } });

            const install_cmd = b.addInstallArtifact(day_exe, .{});

            const build_test = b.addTest(.{ .root_module = b.createModule(.{
                .root_source_file = b.path(zigFile),
                .target = target,
                .optimize = optimize,
            }), .test_runner = test_runner });

            linkModules(build_test, &[_]ModuleKV{ utils_mod_kv, tty_mod_kv, terminal_progress_kv, generated_module_kv });

            const run_test = b.addRunArtifact(build_test);

            {
                const step_key = b.fmt("install_{s}", .{dayString});
                const step_desc = b.fmt("Install {s}.exe", .{dayString});
                const install_step = b.step(step_key, step_desc);
                install_step.dependOn(&install_cmd.step);
                install_all.dependOn(&install_cmd.step);
            }

            {
                const step_key = b.fmt("test_{s}", .{dayString});
                const step_desc = b.fmt("Run tests in {s}", .{zigFile});
                const step = b.step(step_key, step_desc);
                step.dependOn(&run_test.step);
            }

            test_all.dependOn(&run_test.step);

            const run_cmd = b.addRunArtifact(day_exe);
            b.installArtifact(day_exe);
            if (b.args) |args| {
                run_cmd.addArgs(args);
            }

            const run_key = b.fmt("run_{s}", .{dayString});
            const run_desc = b.fmt("Run {s}", .{dayString});
            const run_step = b.step(run_key, run_desc);
            run_step.dependOn(&run_cmd.step);

            run_all.dependOn(&run_cmd.step);

            const all_key = dayString;
            const all_desc = b.fmt("Do all For {s}", .{dayString});
            const all_step = b.step(all_key, all_desc);
            all_step.dependOn(&run_cmd.step);
            all_step.dependOn(&run_test.step);
        }
    }

    // Set up tests for utils.zig
    {
        const test_utils = b.step("test_utils", "Run tests in utils.zig");
        const test_cmd_utils = b.addTest(.{ .root_module = utils_mod, .test_runner = test_runner });

        linkModules(test_cmd_utils, &[_]ModuleKV{ utils_mod_kv, tty_mod_kv, terminal_progress_kv });

        const run_test_utils = b.addRunArtifact(test_cmd_utils);
        test_utils.dependOn(&run_test_utils.step);

        test_all.dependOn(&run_test_utils.step);
    }

    // Set up tests for tty.zig
    {
        const test_tty = b.step("test_tty", "Run tests in tty.zig");
        const test_cmd_tty = b.addTest(.{ .root_module = tty_mod, .test_runner = test_runner });

        linkModules(test_cmd_tty, &[_]ModuleKV{ utils_mod_kv, tty_mod_kv, terminal_progress_kv });

        const run_test_tty = b.addRunArtifact(test_cmd_tty);
        test_tty.dependOn(&run_test_tty.step);

        test_all.dependOn(&run_test_tty.step);
    }

    { // main file

        const generatedName = "main/helper.zig";

        var daysArr: std.array_list.AlignedManaged(u8, null) = std.array_list.AlignedManaged(u8, null).init(alloc);
        defer daysArr.deinit();

        for (days.items) |dayObj| {
            if (daysArr.items.len != 0) {
                try daysArr.appendSlice("\n");
            }

            try daysArr.appendSlice(b.fmt("    try array.append(@import(\"day{:0>2}\").day);", .{dayObj.num}));
        }

        const daysStr: []u8 = try daysArr.toOwnedSlice();
        defer alloc.free(daysStr);

        const file_content = b.fmt(
            \\const std = @import("std");
            \\const utils = @import("utils");
            \\
            \\pub fn getDays(alloc: std.mem.Allocator) !std.array_list.AlignedManaged(utils.Day, null) {{
            \\    var array: std.array_list.AlignedManaged(utils.Day, null) = std.array_list.AlignedManaged(utils.Day, null).init(alloc);
            \\
            \\{s}
            \\
            \\    return array;
            \\}}
            \\
        , .{daysStr});

        const file_generated_src = try generateFile(b, alloc, file_content, generatedName);

        const generated_module = b.createModule(.{
            .root_source_file = file_generated_src,
            .target = target,
            .optimize = optimize,
        });

        generated_module.addImport(utils_mod_kv.name, utils_mod_kv.module);
        for (days.items) |dayObj| {
            generated_module.addImport(dayObj.module_kv.name, dayObj.module_kv.module);
        }

        const generated_module_kv = ModuleKV{ .module = generated_module, .name = "main_helper" };

        const main_exe = b.addExecutable(.{
            .name = "main",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/main.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });

        linkModules(main_exe, &[_]ModuleKV{ utils_mod_kv, tty_mod_kv, terminal_progress_kv, generated_module_kv });

        const install_main = b.addInstallArtifact(main_exe, .{});

        const build_test = b.addTest(.{ .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }), .test_runner = test_runner });

        linkModules(build_test, &[_]ModuleKV{ utils_mod_kv, tty_mod_kv, terminal_progress_kv, generated_module_kv });

        const run_test = b.addRunArtifact(build_test);

        {
            const install_step = b.step("install_main", "Install main");
            install_step.dependOn(&install_main.step);
            install_all.dependOn(&install_main.step);
        }

        {
            const step = b.step("test_main", "Run tests in main");
            step.dependOn(&run_test.step);
        }

        test_all.dependOn(&run_test.step);

        const run_cmd = b.addRunArtifact(main_exe);
        b.installArtifact(main_exe);
        if (b.args) |args| {
            run_cmd.addArgs(args);
        }

        const run_step = b.step("run_main", "Run main");
        run_step.dependOn(&run_cmd.step);

        run_all.dependOn(&run_cmd.step);
    }

    { // test file

        const generatedName = "test/helper.zig";

        var daysArr: std.array_list.AlignedManaged(u8, null) = std.array_list.AlignedManaged(u8, null).init(alloc);
        defer daysArr.deinit();

        for (days.items) |dayObj| {
            if (daysArr.items.len != 0) {
                try daysArr.appendSlice("\n");
            }

            try daysArr.appendSlice(b.fmt(
                \\
                \\test "day {:0>2}" {{
                \\    var gpa = std.heap.GeneralPurposeAllocator(.{{}}){{}};
                \\    defer _ = gpa.deinit();
                \\
                \\    const day = @import("day{:0>2}").day;
                \\
                \\    try day.@"test"(gpa.allocator());
                \\}}
                \\
            , .{ dayObj.num, dayObj.num }));
        }

        const daysStr: []u8 = try daysArr.toOwnedSlice();
        defer alloc.free(daysStr);

        const file_content = b.fmt(
            \\const std = @import("std");
            \\const builtin = @import("builtin");
            \\
            \\pub const _ = if (!builtin.is_test) {{
            \\    @compileError("Cannot run this outside of tests");
            \\}} else 0;
            \\
            \\{s}
            \\
        , .{daysStr});

        const file_generated_src = try generateFile(b, alloc, file_content, generatedName);

        const generated_module = b.createModule(.{
            .root_source_file = file_generated_src,
            .target = target,
            .optimize = optimize,
        });

        for (days.items) |dayObj| {
            generated_module.addImport(dayObj.module_kv.name, dayObj.module_kv.module);
        }

        generated_module.addImport(tty_mod_kv.name, tty_mod_kv.module);
        generated_module.addImport(terminal_progress_kv.name, terminal_progress_kv.module);

        const tests_of_days = b.addTest(.{
            .name = "tests",
            .root_module = generated_module,
            .test_runner = test_runner,
        });

        const install_tests = b.addInstallArtifact(tests_of_days, .{});

        {
            const install_step = b.step("install_tests", "Install tests");
            install_step.dependOn(&install_tests.step);
            install_all.dependOn(&install_tests.step);
        }

        const run_cmd = b.addRunArtifact(tests_of_days);
        b.installArtifact(tests_of_days);
        if (b.args) |args| {
            run_cmd.addArgs(args);
        }

        const run_step = b.step("run_tests", "Run tests");
        run_step.dependOn(&run_cmd.step);

        run_all.dependOn(&run_cmd.step);
    }
}
