package logfile

import (
	"bibifi/cmdline"
)

func (log *Log) readRooms(cmd cmdline.ReadCmd) []string {
	var res []string
	if cmd.Employee != nil {
		res = readRoomsHelper(log.EmployeeEvents[cmd.Employee[0]])
	}
	if cmd.Guest != nil {
		res = readRoomsHelper(log.GuestEvents[cmd.Guest[0]])
	}
	return res
}

func readRoomsHelper(entry *LogEntry) []string {
	if entry == nil {
		return nil
	}
	var res []string
	for _, event := range entry.Events {
		if !event.Arrive || event.Room == "" {
			continue
		}
		res = append(res, event.Room)
	}
	return res
}
