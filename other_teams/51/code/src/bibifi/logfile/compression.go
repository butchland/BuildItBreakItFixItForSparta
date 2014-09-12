package logfile

import "github.com/cznic/zappy"

/*
func compressGzip(data []byte) []byte {
	if data == nil {
		return nil
	}
	var buf bytes.Buffer
	comp := gzip.NewWriter(&buf)
	_, err := comp.Write(data)
	if err != nil {
		cmdline.Dbg(err)
		return nil
	}
	if err = comp.Close(); err != nil {
		cmdline.Dbg(err)
		return nil
	}
	return buf.Bytes()
}

func decompressGzip(data []byte) []byte {
	if data == nil {
		return nil
	}
	buf := bytes.NewBuffer(data)
	decomp, err := gzip.NewReader(buf)
	if err != nil {
		cmdline.Dbg(err)
		return nil
	}
	res, err := ioutil.ReadAll(decomp)
	if err != nil {
		cmdline.Dbg(err)
		return nil
	}
	return res
}
*/

/*
func compressSnappy(data []byte) []byte {
	res, err := snappy.Encode(nil, data)
	if err != nil {
		return nil
	}
	return res
}

func decompressSnappy(data []byte) []byte {
	res, err := snappy.Decode(nil, data)
	if err != nil {
		return nil
	}
	return res
}
*/

func compressZappy(data []byte) []byte {
	res, err := zappy.Encode(nil, data)
	if err != nil {
		return nil
	}
	return res
}

func decompressZappy(data []byte) []byte {
	res, err := zappy.Decode(nil, data)
	if err != nil {
		return nil
	}
	return res
}

/*
func compressLZ4(data []byte) []byte {
	res, err := lz4.Encode(nil, data)
	if err != nil {
		return nil
	}
	return res
}

func decompressLZ4(data []byte) []byte {
	res, err := lz4.Decode(nil, data)
	if err != nil {
		return nil
	}
	return res
}
*/

/*
func compressLZMA(data []byte) []byte {
	var b bytes.Buffer
	w := lzma.NewWriterLevel(&b, lzma.BestSpeed)

	if _, err := w.Write(data); err != nil {
		cmdline.Dbg(err)
		return nil
	}
	w.Close()
	return b.Bytes()
}

func decompressLZMA(data []byte) []byte {
	b := bytes.NewBuffer(data)
	r := lzma.NewReader(b)
	defer r.Close()

	res, err := ioutil.ReadAll(r)
	if err != nil {
		return nil
	}
	return res
}
*/
