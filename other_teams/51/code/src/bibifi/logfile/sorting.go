package logfile

import "sort"

func sortState(state *stateResult) {
	sort.Sort(sort.StringSlice(state.Employees))
	sort.Sort(sort.StringSlice(state.Guests))
	for room, people := range state.Rooms {
		sort.Sort(sort.StringSlice(people))
		state.Rooms[room] = people
	}
	sort.Sort(StrIntSlice(state.RoomOrder))
}

type StrIntSlice []string

func (p StrIntSlice) Len() int      { return len(p) }
func (p StrIntSlice) Swap(i, j int) { p[i], p[j] = p[j], p[i] }
func (p StrIntSlice) Less(i, j int) bool {
	x, y := p[i], p[j]
	if len(x) == len(y) {
		return x < y
	}
	return len(x) < len(y)
}
