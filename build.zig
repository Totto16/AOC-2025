const std = @import("std");
const CompileStep = std.Build.Step.Compile;

const required_zig_version = std.SemanticVersion.parse("0.15.2") catch unreachable;

/// set this to true to link libc
const should_link_libc = false;

const ModuleKV = struct { name: []const u8, module: *std.Build.Module };

fn linkObject(b: *std.Build, obj: *CompileStep, modules: []const ModuleKV) void {
    if (should_link_libc) obj.root_module.linkLibC();

    // Add linking for packages or third party libraries here

    for (modules) |module| {
        obj.root_module.addImport(module.name, module.module);
    }

    _ = b;
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

    const ansi_term_dep = b.dependency("ansi_term", .{ .target = target, .optimize = optimize });

    const ansi_term_mod_kv = ModuleKV{ .module = ansi_term_dep.module("ansi_term"), .name = "ansi_term" };

    const utils_mod = b.createModule(.{
        .root_source_file = b.path("src/utils.zig"),
    });

    utils_mod.addImport(ansi_term_mod_kv.name, ansi_term_mod_kv.module);

    const utils_mod_kv = ModuleKV{ .module = utils_mod, .name = "utils" };

    // Set up a compile target for each day
    var day: u32 = 1;
    while (day <= 25) : (day += 1) {
        const dayString = b.fmt("day{:0>2}", .{day});
        const zigFile = b.fmt("src/days/{s}/day.zig", .{dayString});

        if (try fileIsPresent(alloc, zigFile)) {
            const zigFileRoot = try getFileRoot(alloc, zigFile);

            const generatedName = b.fmt("day{:0>2}/generated.zig", .{day});

            const generate_file_src = try b.cache_root.join(alloc, &[_][]const u8{generatedName});
            defer alloc.free(generate_file_src);

            const file_content = b.fmt("pub const root = \"{s}\";", .{zigFileRoot});
            alloc.free(zigFileRoot);

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

            const generated_module = b.createModule(.{
                .root_source_file = b.path(generate_file_src),
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

            linkObject(b, day_exe, &[_]ModuleKV{ utils_mod_kv, ansi_term_mod_kv, generated_module_kv });

            const install_cmd = b.addInstallArtifact(day_exe, .{});

            const build_test = b.addTest(.{
                .root_module = b.createModule(.{
                    .root_source_file = b.path(zigFile),
                    .target = target,
                    .optimize = optimize,
                }),
            });

            linkObject(b, build_test, &[_]ModuleKV{ utils_mod_kv, ansi_term_mod_kv, generated_module_kv });

            b.installArtifact(build_test);

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
        const test_cmd = b.addTest(.{
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/utils.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });

        linkObject(b, test_cmd, &[_]ModuleKV{ utils_mod_kv, ansi_term_mod_kv });

        test_utils.dependOn(&test_cmd.step);
        b.installArtifact(test_cmd);
    }
}
