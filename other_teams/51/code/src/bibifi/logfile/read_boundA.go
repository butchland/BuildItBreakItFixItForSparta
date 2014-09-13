package logfile

import (
	"sort"

	"bibifi/cmdline"
)

func (log *Log) readBoundaryA(cmd cmdline.ReadCmd) []string {
	if cmd.Lower[0] > log.LastTimestamp {
		// Lower boundary is after the last event in the log.
		// Specification does not mention this case.
		return nil
	}
	var res []string
	for name, entry := range log.EmployeeEvents {
		if inGalleryWithBounds(cmd.Lower[0], cmd.Upper[0], entry.Events) {
			cmdline.Dbg(name)
			res = append(res, name)
		}
	}
	sort.Sort(sort.StringSlice(res))
	return res
}

func (log *Log) readBoundaryB(cmd cmdline.ReadCmd) []string {
	if cmd.Lower[0] > log.LastTimestamp {
		// Lower boundary is after the last event in the log.
		// Specification does not mention this case.
		return nil
	}
	var res []string
	for name, entry := range log.EmployeeEvents {
		if inGalleryWithBounds(cmd.Lower[1], cmd.Upper[1], entry.Events) {
			continue
		}
		if inGalleryWithBounds(cmd.Lower[0], cmd.Upper[0], entry.Events) {
			res = append(res, name)
		}
	}
	sort.Sort(sort.StringSlice(res))
	return res
}

func inGalleryWithBounds(low int64, up int64, events []Event) bool {
	var enter int64 = -1
	dbg := cmdline.Dbg

	for _, ev := range events {
		if ev.Room != "" {
			continue
		}
		if ev.Arrive {
			enter = ev.Timestamp
			if enter < low {
				continue
			} else if enter <= up {
				dbg("A:1")
				return true
			} else {
				return false
			}
		} else {
			leave := ev.Timestamp
			if leave < low {
				enter = -1
				continue
			} else if leave <= up {
				dbg("A:2")
				return true
			} else if enter < low {
				dbg("A:3")
				return true
			} else {
				return false
			}
		}
	}
	// Employee still in gallery
	if enter >= 0 && enter < low {
		dbg("A:4")
		return true
	}
	return false
}
