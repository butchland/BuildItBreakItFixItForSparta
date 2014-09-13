package capnp

// AUTO GENERATED - DO NOT EDIT

import (
	"bufio"
	"bytes"
	"encoding/json"
	C "github.com/glycerine/go-capnproto"
	"io"
	"unsafe"
)

type Event C.Struct

func NewEvent(s *C.Segment) Event      { return Event(s.NewStruct(16, 1)) }
func NewRootEvent(s *C.Segment) Event  { return Event(s.NewRootStruct(16, 1)) }
func ReadRootEvent(s *C.Segment) Event { return Event(s.Root(0).ToStruct()) }
func (s Event) Timestamp() int64       { return int64(C.Struct(s).Get64(0)) }
func (s Event) SetTimestamp(v int64)   { C.Struct(s).Set64(0, uint64(v)) }
func (s Event) Room() string           { return C.Struct(s).GetObject(0).ToText() }
func (s Event) SetRoom(v string)       { C.Struct(s).SetObject(0, s.Segment.NewText(v)) }
func (s Event) Arrive() bool           { return C.Struct(s).Get1(64) }
func (s Event) SetArrive(v bool)       { C.Struct(s).Set1(64, v) }
func (s Event) WriteJSON(w io.Writer) error {
	b := bufio.NewWriter(w)
	var err error
	var buf []byte
	_ = buf
	err = b.WriteByte('{')
	if err != nil {
		return err
	}
	_, err = b.WriteString("\"timestamp\":")
	if err != nil {
		return err
	}
	{
		s := s.Timestamp()
		buf, err = json.Marshal(s)
		if err != nil {
			return err
		}
		_, err = b.Write(buf)
		if err != nil {
			return err
		}
	}
	err = b.WriteByte(',')
	if err != nil {
		return err
	}
	_, err = b.WriteString("\"room\":")
	if err != nil {
		return err
	}
	{
		s := s.Room()
		buf, err = json.Marshal(s)
		if err != nil {
			return err
		}
		_, err = b.Write(buf)
		if err != nil {
			return err
		}
	}
	err = b.WriteByte(',')
	if err != nil {
		return err
	}
	_, err = b.WriteString("\"arrive\":")
	if err != nil {
		return err
	}
	{
		s := s.Arrive()
		buf, err = json.Marshal(s)
		if err != nil {
			return err
		}
		_, err = b.Write(buf)
		if err != nil {
			return err
		}
	}
	err = b.WriteByte('}')
	if err != nil {
		return err
	}
	err = b.Flush()
	return err
}
func (s Event) MarshalJSON() ([]byte, error) {
	b := bytes.Buffer{}
	err := s.WriteJSON(&b)
	return b.Bytes(), err
}

type Event_List C.PointerList

func NewEventList(s *C.Segment, sz int) Event_List { return Event_List(s.NewCompositeList(16, 1, sz)) }
func (s Event_List) Len() int                      { return C.PointerList(s).Len() }
func (s Event_List) At(i int) Event                { return Event(C.PointerList(s).At(i).ToStruct()) }
func (s Event_List) ToArray() []Event              { return *(*[]Event)(unsafe.Pointer(C.PointerList(s).ToArray())) }

type LogEntry C.Struct

func NewLogEntry(s *C.Segment) LogEntry      { return LogEntry(s.NewStruct(0, 2)) }
func NewRootLogEntry(s *C.Segment) LogEntry  { return LogEntry(s.NewRootStruct(0, 2)) }
func ReadRootLogEntry(s *C.Segment) LogEntry { return LogEntry(s.Root(0).ToStruct()) }
func (s LogEntry) Name() string              { return C.Struct(s).GetObject(0).ToText() }
func (s LogEntry) SetName(v string)          { C.Struct(s).SetObject(0, s.Segment.NewText(v)) }
func (s LogEntry) Events() Event_List        { return Event_List(C.Struct(s).GetObject(1)) }
func (s LogEntry) SetEvents(v Event_List)    { C.Struct(s).SetObject(1, C.Object(v)) }
func (s LogEntry) WriteJSON(w io.Writer) error {
	b := bufio.NewWriter(w)
	var err error
	var buf []byte
	_ = buf
	err = b.WriteByte('{')
	if err != nil {
		return err
	}
	_, err = b.WriteString("\"name\":")
	if err != nil {
		return err
	}
	{
		s := s.Name()
		buf, err = json.Marshal(s)
		if err != nil {
			return err
		}
		_, err = b.Write(buf)
		if err != nil {
			return err
		}
	}
	err = b.WriteByte(',')
	if err != nil {
		return err
	}
	_, err = b.WriteString("\"events\":")
	if err != nil {
		return err
	}
	{
		s := s.Events()
		{
			err = b.WriteByte('[')
			if err != nil {
				return err
			}
			for i, s := range s.ToArray() {
				if i != 0 {
					_, err = b.WriteString(", ")
				}
				if err != nil {
					return err
				}
				err = s.WriteJSON(b)
				if err != nil {
					return err
				}
			}
			err = b.WriteByte(']')
		}
		if err != nil {
			return err
		}
	}
	err = b.WriteByte('}')
	if err != nil {
		return err
	}
	err = b.Flush()
	return err
}
func (s LogEntry) MarshalJSON() ([]byte, error) {
	b := bytes.Buffer{}
	err := s.WriteJSON(&b)
	return b.Bytes(), err
}

type LogEntry_List C.PointerList

func NewLogEntryList(s *C.Segment, sz int) LogEntry_List {
	return LogEntry_List(s.NewCompositeList(0, 2, sz))
}
func (s LogEntry_List) Len() int          { return C.PointerList(s).Len() }
func (s LogEntry_List) At(i int) LogEntry { return LogEntry(C.PointerList(s).At(i).ToStruct()) }
func (s LogEntry_List) ToArray() []LogEntry {
	return *(*[]LogEntry)(unsafe.Pointer(C.PointerList(s).ToArray()))
}

type Log C.Struct

func NewLog(s *C.Segment) Log              { return Log(s.NewStruct(8, 2)) }
func NewRootLog(s *C.Segment) Log          { return Log(s.NewRootStruct(8, 2)) }
func ReadRootLog(s *C.Segment) Log         { return Log(s.Root(0).ToStruct()) }
func (s Log) LastTimestamp() int64         { return int64(C.Struct(s).Get64(0)) }
func (s Log) SetLastTimestamp(v int64)     { C.Struct(s).Set64(0, uint64(v)) }
func (s Log) Employees() LogEntry_List     { return LogEntry_List(C.Struct(s).GetObject(0)) }
func (s Log) SetEmployees(v LogEntry_List) { C.Struct(s).SetObject(0, C.Object(v)) }
func (s Log) Guests() LogEntry_List        { return LogEntry_List(C.Struct(s).GetObject(1)) }
func (s Log) SetGuests(v LogEntry_List)    { C.Struct(s).SetObject(1, C.Object(v)) }
func (s Log) WriteJSON(w io.Writer) error {
	b := bufio.NewWriter(w)
	var err error
	var buf []byte
	_ = buf
	err = b.WriteByte('{')
	if err != nil {
		return err
	}
	_, err = b.WriteString("\"lastTimestamp\":")
	if err != nil {
		return err
	}
	{
		s := s.LastTimestamp()
		buf, err = json.Marshal(s)
		if err != nil {
			return err
		}
		_, err = b.Write(buf)
		if err != nil {
			return err
		}
	}
	err = b.WriteByte(',')
	if err != nil {
		return err
	}
	_, err = b.WriteString("\"employees\":")
	if err != nil {
		return err
	}
	{
		s := s.Employees()
		{
			err = b.WriteByte('[')
			if err != nil {
				return err
			}
			for i, s := range s.ToArray() {
				if i != 0 {
					_, err = b.WriteString(", ")
				}
				if err != nil {
					return err
				}
				err = s.WriteJSON(b)
				if err != nil {
					return err
				}
			}
			err = b.WriteByte(']')
		}
		if err != nil {
			return err
		}
	}
	err = b.WriteByte(',')
	if err != nil {
		return err
	}
	_, err = b.WriteString("\"guests\":")
	if err != nil {
		return err
	}
	{
		s := s.Guests()
		{
			err = b.WriteByte('[')
			if err != nil {
				return err
			}
			for i, s := range s.ToArray() {
				if i != 0 {
					_, err = b.WriteString(", ")
				}
				if err != nil {
					return err
				}
				err = s.WriteJSON(b)
				if err != nil {
					return err
				}
			}
			err = b.WriteByte(']')
		}
		if err != nil {
			return err
		}
	}
	err = b.WriteByte('}')
	if err != nil {
		return err
	}
	err = b.Flush()
	return err
}
func (s Log) MarshalJSON() ([]byte, error) {
	b := bytes.Buffer{}
	err := s.WriteJSON(&b)
	return b.Bytes(), err
}

type Log_List C.PointerList

func NewLogList(s *C.Segment, sz int) Log_List { return Log_List(s.NewCompositeList(8, 2, sz)) }
func (s Log_List) Len() int                    { return C.PointerList(s).Len() }
func (s Log_List) At(i int) Log                { return Log(C.PointerList(s).At(i).ToStruct()) }
func (s Log_List) ToArray() []Log              { return *(*[]Log)(unsafe.Pointer(C.PointerList(s).ToArray())) }
