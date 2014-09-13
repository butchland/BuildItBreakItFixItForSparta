package logfile

/*
type ManStream interface {
	WriteInt64(x int64)
	WriteUInt64(x uint64)
	WriteInt(x int)
	WriteUInt(x uint)
	WriteByte(x byte)
	WriteString(s string)
	WriteBool(b bool)

	ReadInt64() int64
	ReadUInt64() uint64
	ReadInt() int
	ReadUInt() uint
	ReadByte() byte
	ReadString() string
	ReadBool() bool

	Data() ([]byte, error)
}

func NewManStream(src []byte) *ManStreamImpl {
	if src == nil {
		src = make([]byte, 0, 1024*16)
	}
	return &ManStreamImpl{
		buf: bytes.NewBuffer(src),
	}
}

type ManStreamImpl struct {
	buf *bytes.Buffer
	err error
}

func (s *ManStreamImpl) WriteInt64(x int64) {
	var data [8]byte
	n := binary.PutVarint(data[:], x)
	s.buf.Write(data[:n])
	//fmt.Printf("Wrote %v\n", x)
}

func (s *ManStreamImpl) WriteUInt64(x uint64) {
	var data [8]byte
	n := binary.PutUvarint(data[:], x)
	s.buf.Write(data[:n])
	//fmt.Printf("Wrote %v\n", x)
}

func (s *ManStreamImpl) WriteInt(x int) {
	s.WriteInt64(int64(x))
}

func (s *ManStreamImpl) WriteUInt(x uint) {
	s.WriteUInt64(uint64(x))
}

func (s *ManStreamImpl) WriteByte(x byte) {
	s.buf.WriteByte(x)
	//fmt.Printf("Wrote %v\n", int(x))
}

func (s *ManStreamImpl) WriteString(str string) {
	s.WriteInt(len(str))
	s.buf.WriteString(str)
	//fmt.Printf("Wrote %v\n", str)
}

func (s *ManStreamImpl) WriteBool(b bool) {
	if b {
		s.WriteByte(1)
	} else {
		s.WriteByte(0)
	}
}

func (s *ManStreamImpl) Write(data []byte) {
	s.buf.Write(data)
}

func (s *ManStreamImpl) ReadInt64() int64 {
	x, err := binary.ReadVarint(s.buf)
	if err != nil {
		s.err = err
	}
	//fmt.Printf("Read %v\n", x)
	return x
}

func (s *ManStreamImpl) ReadUInt64() uint64 {
	x, err := binary.ReadUvarint(s.buf)
	if err != nil {
		s.err = err
	}
	//fmt.Printf("Read %v\n", x)
	return x
}

func (s *ManStreamImpl) ReadInt() int {
	return int(s.ReadInt64())
}

func (s *ManStreamImpl) ReadUInt() uint {
	return uint(s.ReadUInt64())
}

func (s *ManStreamImpl) ReadByte() byte {
	b, err := s.buf.ReadByte()
	if err != nil {
		s.err = err
	}
	//fmt.Printf("Read %v\n", int(b))
	return b
}

func (s *ManStreamImpl) ReadString() string {
	var tmp [256]byte
	var dest []byte

	l := s.ReadInt()
	if l >= 256 {
		dest = make([]byte, l)
	} else {
		dest = tmp[:l]
	}
	_, err := s.buf.Read(dest)
	if err != nil {
		s.err = err
	}
	//fmt.Printf("Read %v\n", string(dest))
	return string(dest)
}

func (s *ManStreamImpl) ReadBool() bool {
	if s.ReadByte() == 1 {
		return true
	} else {
		return false
	}
}

func (s *ManStreamImpl) Data() ([]byte, error) {
	return s.buf.Bytes(), s.err
}

*/
