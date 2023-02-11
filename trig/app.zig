const std = @import("std");
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const print = std.debug.print;

const BUF_SIZE = 1024;

fn extractColsFromCSV (allocator: std.mem.Allocator,
                       csv_path: []const u8,
                       cols: []const u8) ![][][:0]const u8 {
    const file = try std.fs.cwd().openFile(csv_path, .{});
    defer file.close();

    var br = std.io.bufferedReader(file.reader());
    var buffer: [BUF_SIZE]u8 = undefined;

    var rows = ArrayList([][:0]const u8).init(allocator);
    errdefer rows.deinit();

    var lix: u32 = 0;
    while (try br.reader().readUntilDelimiterOrEof(&buffer, '\n')) |line| : (lix += 1) {
        if (lix == 0) {
            continue;
        }
        var it = std.mem.split(u8, line, ",");
        var cix:u8 = 0;
        var ix:u8 = 0;
        var parts = try allocator.alloc([:0]const u8, cols.len);
        while (it.next()) |c| : (ix += 1) {
            if (cix >= cols.len) {
                break;
            }
            if (cols[cix] == ix) {
                var v = ArrayList(u8).init(allocator);
                try v.appendSlice(c);
                parts[cix] = try v.toOwnedSliceSentinel(0);
                cix += 1;
            }
        }
        try rows.append(parts);
    }
    return try rows.toOwnedSlice();
}

// test "csv to table (trip)" {
//     const file_name = "../MBTA_GTFS/trips.txt";
//     var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     defer arena.deinit();

//     var resp = try extractColsFromCSV(arena.allocator(), file_name, &[_]u8{0, 1, 2});
//     print("{d}, {d}, {s} {s} {s}\n", .{resp.len, resp[72534].len, resp[72534][0], resp[72534][1], resp[72534][2]});
// }

// test "csv to table (stoptime)" {
//     const file_name = "../MBTA_GTFS/stop_times.txt";
//     var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     defer arena.deinit();

//     var resp = try extractColsFromCSV(arena.allocator(), file_name, &[_]u8{0, 1, 2, 3});
//     print("{d}, {d}, {s} {s} {s} {s}\n", .{resp.len, resp[1795479].len, resp[1795479][0], resp[1795479][1], resp[1795479][2], resp[1795479][2]});
// }

const Trip = struct {
    routeid: [:0]const u8,
    serviceid: [:0]const u8,
    tripid: [:0]const u8,
};


const StopTime = struct {
    tripid: [:0]const u8,
    arrival: [:0]const u8,
    departure: [:0]const u8,
    stopid: [:0]const u8,
};

fn createTripsFromTable (allocator: std.mem.Allocator, table: [][][:0]const u8) !struct {[]Trip, StringHashMap(ArrayList(usize))} {
    var newTable = ArrayList(Trip).init(allocator);
    errdefer newTable.deinit();
    var routeMap = StringHashMap(ArrayList(usize)).init(allocator);
    errdefer routeMap.deinit();
    
    for (table) |row, i| {
        try newTable.append(Trip { .routeid = row[0], .serviceid = row[1], .tripid = row[2] }); 
        if (!routeMap.contains(row[0])) {
            try routeMap.put(row[0], ArrayList(usize).init(allocator));
        }
        var v = routeMap.get(row[0]).?;
        try v.append(i);
    }
    return .{ try newTable.toOwnedSlice(), routeMap};
}

test "Trip table" {
    const file_name = "../MBTA_GTFS/trips.txt";
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator: std.mem.Allocator = arena.allocator();
    var colsTable = try extractColsFromCSV(allocator, file_name, &[_]u8{0, 1, 2});
    var tripsResp = try createTripsFromTable(allocator, colsTable);
    print("{d}, {d}\n", .{tripsResp[0].len, tripsResp[1].count()});
}

fn createStoptimesFromTable (allocator: std.mem.Allocator, table: [][][:0]const u8) !struct {[]StopTime, StringHashMap(ArrayList(usize))} {
    var newTable = ArrayList(StopTime).init(allocator);
    errdefer newTable.deinit();
    var tripMap = StringHashMap(ArrayList(usize)).init(allocator);
    errdefer tripMap.deinit();
    
    for (table) |row, i| {
        try newTable.append(StopTime { .tripid = row[0], .arrival = row[1], .departure = row[2], .stopid = row[3] }); 
        if (!tripMap.contains(row[0])) {
            try tripMap.put(row[0], ArrayList(usize).init(allocator));
        }
        var v = tripMap.get(row[0]).?;
        try v.append(i);
    }
    return .{ try newTable.toOwnedSlice(), tripMap};
}

test "Stoptime table" {
    const file_name = "../MBTA_GTFS/stop_times.txt";
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator: std.mem.Allocator = arena.allocator();
    var colsTable = try extractColsFromCSV(allocator, file_name, &[_]u8{0, 1, 2, 3});
    var stoptimesResp = try createStoptimesFromTable(allocator, colsTable);
    print("{d}, {d}\n", .{stoptimesResp[0].len, stoptimesResp[1].count()});
}
