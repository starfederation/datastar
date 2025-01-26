// This is a sample middleware

// Generally, using a custom Handler with a dispatch method provides the most
// flexibility and should be your first approach (see the dispatcher.zig example).

// Middleware provide an alternative way to manipulate the request/response and
// is well suited if you need different middleware (or middleware configurations)
// for different routes.

const std = @import("std");
const httpz = @import("httpz");

const Logger = @This();

// Must define an `init` method, which will accept your Config
// Alternatively, you can define a init(config: Config, mc: httpz.MiddlewareConfig)
// here mc will give you access to the server's allocator and arena
pub fn init(_: Config) !Logger {
    return .{};
}

// optionally you can define an "deinit" method
// pub fn deinit(self: *Logger) void {

// }

// Must define an `execute` method. `self` doesn't have to be `const`, but
// you're responsible for making your middleware thread-safe.
pub fn execute(_: *const Logger, req: *httpz.Request, res: *httpz.Response, executor: anytype) !void {
    std.log.info(
        "{} {s} {d}",
        .{
            req.method,
            req.url.path,
            res.status,
        },
    );

    // If you don't call executor.next(), there will be no further processing of
    // the request and we'll go straight to writing the response.
    return executor.next();
}

// Must defined a pub config structure, even if it's empty
pub const Config = struct {};
