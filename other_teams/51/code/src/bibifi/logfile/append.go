package logfile

import (
	"errors"

	"bibifi/cmdline"
)

func (log *Log) Append(cmd cmdline.AppendCmd) error {
	// Check timestamp
	if cmd.Timestamp < log.LastTimestamp {
		return errors.New("cmd.Timestamp < log.LastTimestamp")
	}
	log.LastTimestamp = cmd.Timestamp

	var entry *LogEntry
	var ok bool

	if cmd.Employee != "" {
		entry, ok = log.EmployeeEvents[cmd.Employee]
		if !ok {
			entry = &LogEntry{}
			log.EmployeeEvents[cmd.Employee] = entry
		}
	}

	if cmd.Guest != "" {
		entry, ok = log.GuestEvents[cmd.Guest]
		if !ok {
			entry = &LogEntry{}
			log.GuestEvents[cmd.Guest] = entry
		}
	}

	return log.appendToEntry(entry, cmd)
}

func (log *Log) appendToEntry(entry *LogEntry, cmd cmdline.AppendCmd) error {
	var lastEvent *Event
	eventsLen := len(entry.Events)
	if eventsLen > 0 {
		lastEvent = &entry.Events[eventsLen-1]
	}

	if cmd.Arrive {
		if err := checkArrival(cmd, lastEvent); err != nil {
			return err
		}
	} else {
		if err := checkDeparture(cmd, lastEvent); err != nil {
			return err
		}
	}

	event := Event{
		Timestamp: cmd.Timestamp,
		Room:      cmd.Room,
		Arrive:    cmd.Arrive,
	}
	entry.Events = append(entry.Events, event)
	return nil
}

func checkArrival(cmd cmdline.AppendCmd, lastEvent *Event) error {
	if cmd.Room == "" {
		// this is a gallery arrival
		if lastEvent != nil {
			// ok only if prev even is gallery departure
			if lastEvent.Arrive || lastEvent.Room != "" {
				return errors.New("invalid gallery arrival: already in")
			}
		} //else ok (first event)
	} else {
		// this is a room arrival
		if lastEvent == nil {
			// not ok as first event
			return errors.New("invalid room arrival: not in gallery 1")
		}
		if lastEvent.Arrive {
			if lastEvent.Room != "" {
				// arrive in a room while in a room not ok
				return errors.New("invalid room arrival: already in a room")
			}
		} else {
			if lastEvent.Room == "" {
				// arrive in a room while not in the gallery not ok
				return errors.New("invalid room arrival: not in gallery 2")
			}
		}
	}
	return nil
}

func checkDeparture(cmd cmdline.AppendCmd, lastEvent *Event) error {
	if lastEvent == nil {
		// not ok as first event
		return errors.New("invalid departure: first event")
	}
	if cmd.Room == "" {
		// this is a gallery departure
		if lastEvent.Arrive {
			if lastEvent.Room != "" {
				// departing gallery when in a room not ok
				return errors.New("invalid gallery departure: in room")
			}
		} else {
			if lastEvent.Room == "" {
				// departing gallery when not in the gallery not ok
				return errors.New("invalid gallery departure: already out")
			}
		}
	} else {
		// this is a room departure, must have previously entered the same room
		if !lastEvent.Arrive {
			return errors.New("invalid room departure: not in a room")
		}
		if lastEvent.Room != cmd.Room {
			return errors.New("invalid room departure: not in same room")
		}
	}
	return nil
}
