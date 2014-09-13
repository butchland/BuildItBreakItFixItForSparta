package logfile

import (
	"bibifi/cmdline"
)

func (log *Log) readTime(cmd cmdline.ReadCmd) (int64, bool) {
	if cmd.Employee != nil {
		return readTimeHelper(log.EmployeeEvents[cmd.Employee[0]], log.LastTimestamp)
	}
	if cmd.Guest != nil {
		return readTimeHelper(log.GuestEvents[cmd.Guest[0]], log.LastTimestamp)
	}
	return 0, false
}

func readTimeHelper(entry *LogEntry, lastTS int64) (int64, bool) {
	if entry == nil {
		return 0, false
	}
	var count int64 = 0
	var start int64 = 0
	for _, event := range entry.Events {
		if event.Room != "" {
			continue
		}
		if event.Arrive {
			start = event.Timestamp
		} else {
			inc := event.Timestamp - start
			count += inc
			start = 0
		}
	}
	if start != 0 {
		count += lastTS - start
	}
	return count, true
}
