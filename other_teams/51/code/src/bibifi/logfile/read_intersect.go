package logfile

import (
	"sort"

	"bibifi/cmdline"
)

func (log *Log) readIntersect(cmd cmdline.ReadCmd) []string {
	var entries []*LogEntry

	// Step 0: collect entries
	for _, name := range cmd.Employee {
		employee, ok := log.EmployeeEvents[name]
		if !ok {
			continue
		}
		entries = append(entries, employee)
	}

	for _, name := range cmd.Guest {
		guest, ok := log.GuestEvents[name]
		if !ok {
			continue
		}
		entries = append(entries, guest)
	}

	n := len(entries)
	rooms := make([]string, n)
	index := make([]int, n)
	match := make(map[string]bool)

OUTER:
	for {
		// Step 1: Find lowest TS
		var minTS int64 = -1
		var minPos int = -1
		for i, entry := range entries {
			if index[i] >= len(entry.Events) {
				continue
			}
			event := entry.Events[index[i]]
			if event.Timestamp < minTS || minPos == -1 {
				minTS = event.Timestamp
				minPos = i
			}
		}
		if minPos == -1 {
			break
		}

		// Step 2: Update room
		event := entries[minPos].Events[index[minPos]]
		if event.Arrive {
			rooms[minPos] = event.Room
		} else {
			rooms[minPos] = ""
		}
		index[minPos]++

		// Step 3: Check if all rooms match
		check := ""
		for _, room := range rooms {
			if check == "" {
				check = room
				continue
			}
			if check != room {
				continue OUTER
			}
		}

		if check != "" {
			// Found match!
			match[check] = true
		}
	}
	var res []string
	for room, _ := range match {
		res = append(res, room)
	}
	sort.Sort(StrIntSlice(res))
	return res
}
