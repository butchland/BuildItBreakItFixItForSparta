@0xd1889a9de24f88ec;

using Go = import "go.capnp";
$Go.package("capnp");
$Go.import("bibifi/logfile/capnp");

struct Event {
	timestamp @0 :Int64;
	room @1 :Text;
	arrive @2 :Bool;
}

struct LogEntry {
	name @0 :Text;
	events @1 :List(Event);
}

struct Log {
	lastTimestamp @0 :Int64;
	employees @1 :List(LogEntry);
	guests @2 :List(LogEntry);
}
