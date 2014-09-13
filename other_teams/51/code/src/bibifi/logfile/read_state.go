package logfile

import (
	"bibifi/cmdline"
)

type stateResult struct {
	Employees []string
	Guests    []string

	RoomOrder []string
	Rooms     map[string][]string
}

func (log *Log) readState(cmd cmdline.ReadCmd) *stateResult {
	res := &stateResult{
		Rooms: make(map[string][]string),
	}

	for name, entry := range log.EmployeeEvents {
		gallery, room := readStateEvent(entry)
		if !gallery {
			continue
		}
		res.Employees = append(res.Employees, name)
		if room != "" {
			res.Rooms[room] = append(res.Rooms[room], name)
		}
	}

	for name, entry := range log.GuestEvents {
		gallery, room := readStateEvent(entry)
		if !gallery {
			continue
		}
		res.Guests = append(res.Guests, name)
		if room != "" {
			res.Rooms[room] = append(res.Rooms[room], name)
		}
	}

	for room, _ := range res.Rooms {
		res.RoomOrder = append(res.RoomOrder, room)
	}
	return res
}

func readStateEvent(entry *LogEntry) (gallery bool, room string) {
	eventsLen := len(entry.Events)
	if eventsLen == 0 {
		// TODO: this shouldn't be possible, but is it a valid error condition?
		return
	}
	lastEvent := entry.Events[eventsLen-1]
	if lastEvent.Arrive {
		// in the gallery
		gallery = true
		room = lastEvent.Room
	} else {
		if lastEvent.Room != "" {
			// still in the gallery
			gallery = true
		}
	}
	return
}
