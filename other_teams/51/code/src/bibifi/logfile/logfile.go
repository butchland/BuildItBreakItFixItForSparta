package logfile

import (
	"bytes"
	"errors"
	"io"
	"os"

	"bibifi/cmdline"
)

type Log struct {
	salt   []byte
	nonce  []byte
	key    []byte
	file   *os.File
	oldLen int
	sym    *StrSymbols

	EmployeeEvents map[string]*LogEntry
	GuestEvents    map[string]*LogEntry
	LastTimestamp  int64
}

type LogEntry struct {
	bytes []byte

	Events []Event
}

type Event struct {
	Timestamp int64
	Room      string
	Arrive    bool
}

func Load(fn, passphrase string, cachef bool, wantLazy *cmdline.AppendCmd) (*Log, error) {
	//t0 := time.Now()

	data, f, oldLen, err := readFile(fn)
	if err != nil || data == nil {
		res := newLog()
		res.salt = randBytes(saltSize)
		res.nonce = randBytes(nonceSize)
		return res, nil
	}

	//t1 := time.Now()
	//fmt.Println("readFile", t1.Sub(t0))
	//t0 = t1

	data, salt, key, nonce := decrypt(data, passphrase)

	//t1 = time.Now()
	//fmt.Println("decrypt", t1.Sub(t0))
	//t0 = t1

	data = decompressZappy(data)

	//t1 = time.Now()
	//fmt.Println("decompress", t1.Sub(t0))
	//t0 = t1

	res := decodeMan(data, wantLazy)

	//t1 = time.Now()
	//fmt.Println("decode", t1.Sub(t0))
	//t0 = t1

	if res == nil {
		return nil, errors.New("bad file")
	}
	res.salt = salt
	res.key = key
	res.nonce = nonce
	if cachef {
		res.file = f
		res.oldLen = oldLen
	} else {
		f.Close()
	}
	return res, nil
}

func (log *Log) Store(fn, passphrase string) error {
	//t0 := time.Now()

	data := encodeMan(log)

	//t1 := time.Now()
	//fmt.Println("encode", t1.Sub(t0))
	//t0 = t1

	data = compressZappy(data)

	//t1 = time.Now()
	//fmt.Println("compress", t1.Sub(t0))
	//t0 = t1

	data = encrypt(data, passphrase, log.key, log.salt, log.nonce)

	//t1 = time.Now()
	//fmt.Println("encrypt", t1.Sub(t0))
	//t0 = t1

	if data == nil {
		return errors.New("no data")
	}
	if err := writeFile(fn, data, log.file, log.oldLen); err != nil {
		return err
	}

	//t1 = time.Now()
	//fmt.Println("writeFile", t1.Sub(t0))
	//t0 = t1

	return nil
}

func newLog() *Log {
	return &Log{
		EmployeeEvents: make(map[string]*LogEntry),
		GuestEvents:    make(map[string]*LogEntry),
	}
}

// from ioutil.ReadFile
func readFile(fn string) ([]byte, *os.File, int, error) {
	f, err := os.OpenFile(fn, os.O_RDWR, 0666)
	if err != nil {
		return nil, nil, 0, err
	}
	var n int64

	if fi, err := f.Stat(); err == nil {
		if size := fi.Size(); size < 1e9 {
			n = size
		}
	}
	dest := make([]byte, n+bytes.MinRead)
	n2, err := io.ReadFull(f, dest)
	if err != nil && err != io.ErrUnexpectedEOF {
		f.Close()
		return nil, nil, 0, err
	}
	//cmdline.Dbg(fmt.Sprintf("read %v", n2))
	return dest[:n2], f, n2, nil
}

func writeFile(filename string, data []byte, f *os.File, oldLen int) error {
	var err error
	//cmdline.Dbg(f)
	if f == nil {
		f, err = os.OpenFile(filename, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0666)
	} else {
		_, err = f.Seek(0, os.SEEK_SET)
		if oldLen > len(data) {
			if err2 := f.Truncate(0); err == nil {
				err = err2
			}
		}
	}
	if err != nil {
		return err
	}
	n, err := f.Write(data)
	if err == nil && n < len(data) {
		err = io.ErrShortWrite
	}
	if err1 := f.Close(); err == nil {
		err = err1
	}
	//cmdline.Dbg(fmt.Sprintf("wrote %v", n))
	return err
}
