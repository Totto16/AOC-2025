const std = @import("std");
const CompileStep = std.Build.Step.Compile;

const required_zig_version = std.SemanticVersion.parse("0.15.2") catch unreachable;

/// set this to true to link libc
const should_link_libc = false;

fn linkObject(b: *std.Build, obj: *CompileStep) void {
    if (should_link_libc) obj.linkLibC();
    _ = b;

    // Add linking for packages or third party libraries here
}

// taken and modified from: https://github.com/SpexGuy/Zig-AoC-Template/
pub fn build(b: *std.Build) void {
    if (comptime @import("builtin").zig_version.order(required_zig_version) == .lt) {
        std.debug.print("Warning: Your version of Zig too old. You will need to download a newer build\n", .{});
        std.os.exit(1);
    }

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const install_all = b.step("install_all", "Install all days");
    const run_all = b.step("run_all", "Run all days");

    // Set up a compile target for each day
    var day: u32 = 1;
    while (day <= 25) : (day += 1) {
        const dayString = b.fmt("day{:0>2}", .{day});
        const zigFile = b.fmt("src/days/{s}/day.zig", .{dayString});

        const exe = b.addExecutable(.{
            .name = dayString,
            .root_module = b.createModule(.{
                .root_source_file = b.path(zigFile),
                .target = target,
                .optimize = optimize,
            }),
        });
        linkObject(b, exe);

        const install_cmd = b.addInstallArtifact(exe, .{});

        const build_test = b.addTest(.{
            .root_module = b.createModule(.{
                .root_source_file = b.path(zigFile),
                .target = target,
                .optimize = optimize,
            }),
        });
        linkObject(b, build_test);

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

        const run_cmd = b.addRunArtifact(exe);
        if (b.args) |args| {
            run_cmd.addArgs(args);
        }

        const run_desc = b.fmt("Run {s}", .{dayString});
        const run_step = b.step(dayString, run_desc);
        run_step.dependOn(&run_cmd.step);
        run_all.dependOn(&run_cmd.step);
    }

    // Set up tests for util.zig
    {
        const test_util = b.step("test_util", "Run tests in util.zig");
        const test_cmd = b.addTest(.{
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/util.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        linkObject(b, test_cmd);
        test_util.dependOn(&test_cmd.step);
    }

    // Set up all tests contained in test_all.zig
    const test_all = b.step("test", "Run all tests");
    const all_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/test_all.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    const run_all_tests = b.addRunArtifact(all_tests);
    test_all.dependOn(&run_all_tests.step);
}
