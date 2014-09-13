package logfile

type StrSymbols struct {
	index map[string]int
	count int
	data  []string
}

func NewSymbols() *StrSymbols {
	return &StrSymbols{
		index: make(map[string]int),
	}
}

func (w *StrSymbols) Put(s string) int {
	if idx, ok := w.index[s]; ok {
		return idx
	}
	w.index[s] = w.count
	w.data = append(w.data, s)
	res := w.count
	w.count++
	return res
}

func (w *StrSymbols) Idx(s string) int {
	idx, ok := w.index[s]
	if !ok {
		return 0 // this is bad!
	}
	return idx
}

func (w *StrSymbols) WriteTo(dest *WriteStream) {
	dest.WriteInt(w.count)
	for _, s := range w.data {
		dest.WriteString(s)
	}
}

func (r *StrSymbols) ReadFrom(src *ReadStream) {
	r.count = src.ReadInt()
	r.data = make([]string, r.count)
	for i := 0; i < r.count; i++ {
		s := src.ReadString()
		r.data[i] = s
		r.index[s] = i
	}
}

func (r *StrSymbols) Get(idx int) string {
	if idx >= len(r.data) {
		return ""
	}
	return r.data[idx]
}
