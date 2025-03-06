const config = @import("config");

pub const consts = @import("consts.zig");
pub const ServerSentEventGenerator = @import("ServerSentEventGenerator.zig");
pub const httpz = switch (config.framework) {
    .httpz, .all => @import("httpz/root.zig"),
    else => undefined,
};
pub const tk = switch (config.framework) {
    .tokamak, .all => @import("tokamak/root.zig"),
    else => undefined,
};

test {
    @import("std").testing.refAllDecls(@This());
}
