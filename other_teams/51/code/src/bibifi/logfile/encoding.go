package logfile

import "bibifi/cmdline"

// Gob
/*
func encodeGob(log *Log) []byte {
	if log == nil {
		return nil
	}
	var buf bytes.Buffer
	enc := gob.NewEncoder(&buf)
	if err := enc.Encode(log); err != nil {
		panic(err)
	}
	return buf.Bytes()
}

func decodeGob(data []byte) *Log {
	buf := bytes.NewBuffer(data)
	dec := gob.NewDecoder(buf)
	var res Log
	if err := dec.Decode(&res); err != nil {
		cmdline.Dbg(err)
		return nil
	}
	return &res
}
*/

// Cap'n'proto
/*
func encodeCapn(log *Log) []byte {
	s := C.NewBuffer(nil)
	root := capnp.NewRootLog(s)
	root.SetLastTimestamp(log.LastTimestamp)

	// Employees
	employees := capnp.NewLogEntryList(s, len(log.EmployeeEvents))
	root.SetEmployees(employees)
	guests := capnp.NewLogEntryList(s, len(log.GuestEvents))
	root.SetGuests(guests)

	array := employees.ToArray()
	i := 0
	for name, src := range log.EmployeeEvents {
		capnUpdateEntry(s, name, src, array[i])
		i++
	}

	array = guests.ToArray()
	i = 0
	for name, src := range log.GuestEvents {
		capnUpdateEntry(s, name, src, array[i])
		i++
	}

	buf := bytes.Buffer{}
	s.WriteTo(&buf)

	return buf.Bytes()
}

func capnUpdateEntry(s *C.Segment, name string, src *LogEntry, dest capnp.LogEntry) {
	dest.SetName(name)
	events := capnp.NewEventList(s, len(src.Events))
	dest.SetEvents(events)
	array := events.ToArray()
	for i, event := range array {
		srcev := src.Events[i]
		event.SetTimestamp(srcev.Timestamp)
		if srcev.Room != "" {
			event.SetRoom(srcev.Room)
		}
		event.SetArrive(srcev.Arrive)
	}
}

func decodeCapn(data []byte) *Log {
	s, _, err := C.ReadFromMemoryZeroCopy(data)
	if err != nil {
		return nil
	}
	root := capnp.ReadRootLog(s)

	log := &Log{
		LastTimestamp:  root.LastTimestamp(),
		EmployeeEvents: make(map[string]*LogEntry),
		GuestEvents:    make(map[string]*LogEntry),
	}

	for _, entry := range root.Guests().ToArray() {
		capnReadEntry(entry, log.GuestEvents)
	}
	for _, entry := range root.Employees().ToArray() {
		capnReadEntry(entry, log.EmployeeEvents)
	}
	return log
}

func capnReadEntry(src capnp.LogEntry, dest map[string]*LogEntry) {
	var events []Event
	for _, ev := range src.Events().ToArray() {
		events = append(events, Event{
			Timestamp: ev.Timestamp(),
			Room:      ev.Room(),
			Arrive:    ev.Arrive(),
		})
	}
	dest[src.Name()] = &LogEntry{
		Events: events,
	}
}
*/

/*
var (
	bh codec.BincHandle
	mh codec.MsgpackHandle
)

// Codec
func encodeCodec(log *Log) []byte {
	h := &bh
	h.AsSymbols = codec.AsSymbolNone
	var buff bytes.Buffer
	enc := codec.NewEncoder(&buff, h)
	if err := enc.Encode(log); err != nil {
		cmdline.Dbg(err)
		return nil
	}
	return buff.Bytes()
}

func decodeCodec(src []byte) *Log {
	h := &bh
	h.AsSymbols = codec.AsSymbolNone
	dec := codec.NewDecoderBytes(src, h)
	var dest Log
	if err := dec.Decode(&dest); err != nil {
		cmdline.Dbg(err)
		return nil
	}
	return &dest
}
*/

// Manual serialization
func encodeMan(log *Log) []byte {
	if log == nil {
		return nil
	}
	s := NewWriteStream()
	log.manSer(s)
	res := s.Data()
	//fmt.Println("Result:", res)
	return res
}

func decodeMan(src []byte, wantLazy *cmdline.AppendCmd) *Log {
	if src == nil {
		return nil
	}
	s := NewReadStream(src)
	res := &Log{}
	res.manDeser(s, wantLazy)
	return res
}

func (log *Log) manSer(dest *WriteStream) {
	if log.sym == nil {
		log.sym = NewSymbols()
	}
	log.preprocStrings(log.sym)
	log.sym.WriteTo(dest)

	dest.WriteInt64(log.LastTimestamp)
	dest.WriteInt(len(log.EmployeeEvents))
	dest.WriteInt(len(log.GuestEvents))
	for name, entry := range log.EmployeeEvents {
		dest.WriteSym(name, log.sym)
		entry.manSer(dest, log.sym)
	}
	for name, entry := range log.GuestEvents {
		dest.WriteSym(name, log.sym)
		entry.manSer(dest, log.sym)
	}
}

func (log *Log) manDeser(src *ReadStream, wantLazy *cmdline.AppendCmd) {
	log.sym = NewSymbols()
	log.sym.ReadFrom(src)

	log.LastTimestamp = src.ReadInt64()
	ee := src.ReadInt()
	ge := src.ReadInt()

	log.EmployeeEvents = make(map[string]*LogEntry, ee*2)
	for i := 0; i < ee; i++ {
		name := src.ReadSym(log.sym)
		ev := &LogEntry{}
		lazy := false
		if wantLazy != nil {
			lazy = true
			if name == wantLazy.Employee {
				lazy = false
			}
		}
		ev.manDeser(src, lazy, log.sym)
		log.EmployeeEvents[name] = ev
	}

	log.GuestEvents = make(map[string]*LogEntry, ge*2)
	for i := 0; i < ge; i++ {
		name := src.ReadSym(log.sym)
		ev := &LogEntry{}
		lazy := false
		if wantLazy != nil {
			lazy = true
			if name == wantLazy.Guest {
				lazy = false
			}
		}
		ev.manDeser(src, lazy, log.sym)
		log.GuestEvents[name] = ev
	}
}

func (e *LogEntry) manSer(dest *WriteStream, syms *StrSymbols) {
	if e.bytes != nil {
		dest.CopyFrame(e.bytes)
		return
	}

	dest.StartFrame()
	defer dest.EndFrame()
	n := len(e.Events)
	dest.WriteInt(n)

	// Compact bool array
	m := (n + 7) / 8
	set := make([]byte, m)
	for idx, ev := range e.Events {
		i := idx / 8
		flag := byte(1 << byte(idx%8))
		if ev.Arrive {
			set[i] |= flag
		}
	}
	dest.Write(set)

	lastTS := uint64(0)
	for _, ev := range e.Events {
		dest.WriteSym(ev.Room, syms)

		ts := uint64(ev.Timestamp)
		delta := ts - lastTS
		lastTS = ts
		dest.WriteUInt64(delta)
		//dest.WriteBool(ev.Arrive)
	}

}

func (e *LogEntry) manDeser(src *ReadStream, lazy bool, syms *StrSymbols) {
	if lazy {
		e.bytes = src.SkipFrame()
		return
	}
	src.Frame()

	n := src.ReadInt()
	e.Events = make([]Event, n)

	// Read Compact bool array
	m := (n + 7) / 8
	set := make([]byte, m)
	for i := 0; i < m; i++ {
		set[i], _ = src.ReadByte()
	}

	lastTS := uint64(0)
	for idx := 0; idx < n; idx++ {
		p := &e.Events[idx]
		p.Room = src.ReadSym(syms)

		delta := src.ReadUInt64()
		ts := lastTS + delta
		p.Timestamp = int64(ts)
		lastTS = ts
		//p.Timestamp = src.ReadInt64()
		//p.Arrive = src.ReadBool()

		i := idx / 8
		flag := byte(1 << byte(idx%8))
		if set[i]&flag == flag {
			p.Arrive = true
		} else {
			p.Arrive = false
		}
	}
}

func (log *Log) preprocStrings(sym *StrSymbols) {
	for name, entry := range log.EmployeeEvents {
		sym.Put(name)
		entry.preprocStrings(sym)
	}
	for name, entry := range log.GuestEvents {
		sym.Put(name)
		entry.preprocStrings(sym)
	}
}

func (e *LogEntry) preprocStrings(sym *StrSymbols) {
	for _, ev := range e.Events {
		sym.Put(ev.Room)
	}
}
