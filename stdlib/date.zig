const std = @import("std");

pub const Date = struct {
    month: Month,
    day: i64,
    year: i64,
};

pub const Month = enum {
    January,
    February,
    March,
    April,
    May,
    June,
    July,
    August,
    September,
    October,
    November,
    December,
};

export fn zig_date_make(month: Month, day: i64, year: i64) Date {
    return .{
        .month = month,
        .day = day,
        .year = year,
    };
}

export fn zig_date_today() Date {
    const timestamp = std.time.timestamp();
    const epoch_seconds = std.time.epoch.EpochSeconds{ .secs = timestamp };
    const dt = epoch_seconds.getDatetime(std.time.epoch.epoch_day_seconds_1970);

    return Date{
        .month = @as(Month, @enumFromInt(dt.month - 1)), // Convert 1-based month to 0-based enum
        .day = dt.day,
        .year = dt.year,
    };
}
