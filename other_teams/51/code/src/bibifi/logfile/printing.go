package logfile

import (
	"fmt"
	"os"
	"strings"
	"text/template"
)

func printState(state *stateResult) {
	fmt.Println(strings.Join(state.Employees, ","))
	fmt.Println(strings.Join(state.Guests, ","))
	for _, room := range state.RoomOrder {
		fmt.Printf("%s: %s\n", room, strings.Join(state.Rooms[room], ","))
	}
}

func printRooms(rooms []string) {
	fmt.Println(strings.Join(rooms, ","))
}

const stateTemplate = `
<html>
<body>
<table>
<tr>
<th>Employee</th>
<th>Guest</th>
</tr>
{{range .Persons}}
<tr>
<td>{{index . 0}}</td>
<td>{{index . 1}}</td>
</tr>
{{end}}
</table>
<table>
<tr>
<th>Room ID</th>
<th>Occupants</th>
</tr>
{{range .RoomOrder}}
<tr>
<td>{{.}}</td>
<td>{{index $.Rooms .}}</td>
</tr>
{{end}}
</table>
</body>
</html>
`

type stateResultHTML struct {
	Persons   [][]string
	RoomOrder []string
	Rooms     map[string]string
}

func printStateHTML(state *stateResult) {
	t, err := template.New("state").Parse(stateTemplate)
	if err != nil {
		panic(err)
	}

	maxLen := len(state.Employees)
	if len(state.Guests) > maxLen {
		maxLen = len(state.Guests)
	}
	persons := make([][]string, maxLen)
	for i := 0; i < maxLen; i++ {
		persons[i] = make([]string, 2)
		if i < len(state.Employees) {
			persons[i][0] = state.Employees[i]
		}
		if i < len(state.Guests) {
			persons[i][1] = state.Guests[i]
		}
	}

	rooms := make(map[string]string)
	for key, list := range state.Rooms {
		rooms[key] = strings.Join(list, ",")
	}

	err = t.Execute(os.Stdout, stateResultHTML{
		RoomOrder: state.RoomOrder,
		Rooms:     rooms,
		Persons:   persons,
	})
	if err != nil {
		panic(err)
	}
}

const roomsTemplate = `
<html>
<body>
<table>
<tr>
<th>Rooms</th>
</tr>
{{range .}}
<tr>
<td>{{.}}</td>
</tr>
{{end}}
</table>
</body>
</html>
`

func printRoomsHTML(rooms []string) {
	t, err := template.New("rooms").Parse(roomsTemplate)
	if err != nil {
		panic(err)
	}
	err = t.Execute(os.Stdout, rooms)
	if err != nil {
		panic(err)
	}
}

func printTime(time int64) {
	fmt.Println(time)
}

const boundATemplate = `
<html>
<body>
<table>
<tr>
<th>Employees</th>
</tr>
{{range .}}
<tr>
<td>{{.}}</td>
</tr>
{{end}}
</table>
</body>
</html>
`

func printBoundaryA(employees []string) {
	fmt.Println(strings.Join(employees, ","))
}

func printBoundaryAHTML(employees []string) {
	t, err := template.New("boundA").Parse(boundATemplate)
	if err != nil {
		panic(err)
	}
	err = t.Execute(os.Stdout, employees)
	if err != nil {
		panic(err)
	}
}
