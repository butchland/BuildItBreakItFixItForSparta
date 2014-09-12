package logfile

import "encoding/binary"

func NewReadStream(src []byte) *ReadStream {
	return &ReadStream{
		buf: src,
	}
}

func NewWriteStream() *WriteStream {
	return &WriteStream{
		buf: make([]byte, 16*1024),
	}
}

type ReadStream struct {
	buf []byte
	pos int
	err error
}

type WriteStream struct {
	buf    []byte
	pos    int
	marker int
}

func (s *WriteStream) WriteInt64(x int64) {
	s.grow(8)
	n := binary.PutVarint(s.buf[s.pos:], x)
	s.pos += n
	//fmt.Printf("Wrote %v\n", x)
}

func (s *WriteStream) WriteUInt64(x uint64) {
	s.grow(8)
	n := binary.PutUvarint(s.buf[s.pos:], x)
	s.pos += n
	//fmt.Printf("Wrote %v\n", x)
}

func (s *WriteStream) WriteInt(x int) {
	s.WriteInt64(int64(x))
}

func (s *WriteStream) WriteUInt(x uint) {
	s.WriteUInt64(uint64(x))
}

func (s *WriteStream) WriteByte(x byte) {
	s.grow(1)
	s.buf[s.pos] = x
	s.pos++
	//fmt.Printf("Wrote %v\n", int(x))
}

func (s *WriteStream) WriteString(str string) {
	l := len(str)
	s.grow(l + 1)
	s.WriteInt(l)
	copy(s.buf[s.pos:], str)
	s.pos += l
	//fmt.Printf("Wrote %v\n", str)
}

func (s *WriteStream) WriteBool(b bool) {
	if b {
		s.WriteByte(1)
	} else {
		s.WriteByte(0)
	}
}

func (s *WriteStream) Write(data []byte) {
	l := len(data)
	s.grow(l)
	copy(s.buf[s.pos:], data)
	s.pos += l
}

func (s *WriteStream) WriteSym(str string, syms *StrSymbols) {
	idx := syms.Idx(str)
	s.WriteInt(idx)
}

func (s *WriteStream) StartFrame() {
	s.grow(4)
	s.marker = s.pos
	s.pos += 4
}

func (s *WriteStream) EndFrame() {
	l := s.pos - s.marker - 4
	binary.PutVarint(s.buf[s.marker:s.marker+4], int64(l))
}

func (s *WriteStream) CopyFrame(data []byte) {
	s.grow(4)
	p0 := s.pos
	s.WriteInt(len(data))
	s.pos = p0 + 4
	s.Write(data)
}

func (s *WriteStream) Data() []byte {
	return s.buf[:s.pos]
}

func (s *WriteStream) grow(n int) {
	if s.pos+n > cap(s.buf) {
		buf := make([]byte, 2*cap(s.buf)+n)
		copy(buf, s.buf)
		s.buf = buf
	}
}

func (s *ReadStream) ReadByte() (byte, error) {
	c := s.buf[s.pos]
	s.pos++
	return c, nil
}

func (s *ReadStream) ReadInt64() int64 {
	x, err := binary.ReadVarint(s)
	if err != nil {
		s.err = err
	}
	//fmt.Printf("Read %v\n", x)
	return x
}

func (s *ReadStream) ReadUInt64() uint64 {
	x, err := binary.ReadUvarint(s)
	if err != nil {
		s.err = err
	}
	//fmt.Printf("Read %v\n", x)
	return x
}

func (s *ReadStream) ReadInt() int {
	return int(s.ReadInt64())
}

func (s *ReadStream) ReadUInt() uint {
	return uint(s.ReadUInt64())
}

func (s *ReadStream) ReadString() string {
	l := s.ReadInt()
	pos := s.pos
	//fmt.Printf("Reading str s.buf=%v pos=%v len=%v\n", s.buf, pos, l)
	res := s.buf[pos : pos+l]
	//fmt.Printf("Read str %v\n", string(res))
	s.pos += l
	return string(res)
}

func (s *ReadStream) ReadBool() bool {
	b, _ := s.ReadByte()
	if b == 1 {
		return true
	} else {
		return false
	}
}

func (s *ReadStream) ReadSym(syms *StrSymbols) string {
	idx := s.ReadInt()
	return syms.Get(idx)
}

func (s *ReadStream) Frame() {
	s.pos += 4
}

func (s *ReadStream) SkipFrame() []byte {
	p0 := s.pos
	fsize := s.ReadInt()
	p1 := p0 + 4
	p2 := p1 + fsize
	data := s.buf[p1:p2]
	s.pos = p2
	return data
}
