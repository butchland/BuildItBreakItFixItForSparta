package logfile

import (
	"bibifi/cmdline"
)

func (log *Log) Read(cmd cmdline.ReadCmd) bool {
	if cmd.State {
		state := log.readState(cmd)
		sortState(state)
		if cmd.HTML {
			printStateHTML(state)
		} else {
			printState(state)
		}
	}
	if cmd.Rooms {
		rooms := log.readRooms(cmd)
		if cmd.HTML {
			printRoomsHTML(rooms)
		} else if rooms != nil {
			printRooms(rooms)
		}
	}
	if cmd.Time {
		time, ok := log.readTime(cmd)
		if !ok {
			return true
		}
		printTime(time)
	}
	if cmd.BoundaryA {
		employees := log.readBoundaryA(cmd)
		if cmd.HTML {
			printBoundaryAHTML(employees)
		} else if employees != nil {
			printBoundaryA(employees)
		}
	}
	if cmd.BoundaryB {
		employees := log.readBoundaryB(cmd)
		if cmd.HTML {
			printBoundaryAHTML(employees)
		} else if employees != nil {
			printBoundaryA(employees)
		}
	}
	if cmd.Intersect {
		rooms := log.readIntersect(cmd)
		if cmd.HTML {
			printRoomsHTML(rooms)
		} else if rooms != nil {
			printRooms(rooms)
		}
	}
	return true
}
