package cmdline

import (
	"os"
	"strconv"
	"unicode"
)

func dbg(s interface{}) {
	//fmt.Println(s)
}

func Dbg(s interface{}) {
	//dbg(s)
}

//var personRegex *regexp.Regexp
//var roomRegex *regexp.Regexp
//var tokenRegex *regexp.Regexp

func init() {
	//personRegex = regexp.MustCompile("^[a-zA-Z]+$")
	//roomRegex = regexp.MustCompile("^[0-9]+$")
	//tokenRegex = regexp.MustCompile("^[a-zA-Z0-9]+$")
}

func checkPerson(s string) bool {
	//personRegex.MatchString(s)
	if len(s) == 0 {
		return false
	}
	for _, c := range s {
		if c > 127 {
			return false
		}
		if !unicode.IsLetter(c) {
			return false
		}
	}
	return true
}

func checkToken(s string) bool {
	//tokenRegex.MatchString(s)
	if len(s) == 0 {
		return false
	}
	for _, c := range s {
		if c > 127 {
			return false
		}
		if !unicode.IsLetter(c) && !unicode.IsDigit(c) {
			return false
		}
	}
	return true
}

func checkRoom(s string) bool {
	//roomRegex.MatchString(s)
	if len(s) == 0 {
		return false
	}
	for _, c := range s {
		if c > 127 {
			return false
		}
		if !unicode.IsDigit(c) {
			return false
		}
	}
	return true
}

func checkOnlyValidOpts(opts ...string) bool {
	allow := make(map[string]bool)
	allow["-K"] = true
	for _, opt := range opts {
		allow[opt] = true
	}
	args := os.Args[1:]
	for _, arg := range args {
		if len(arg) == 0 || arg[0] != '-' {
			continue
		}
		if !allow[arg] {
			return false
		}
	}
	return true
}

func checkLU() bool {
	// -U must come after -L
	Lseen := false
	for _, arg := range os.Args[1:] {
		if arg == "-L" {
			Lseen = true
			continue
		}
		if len(arg) > 0 && arg[0] == '-' {
			if Lseen {
				if arg != "-U" {
					return false
				} else {
					Lseen = false
					continue
				}
			} else if arg == "-U" {
				return false
			}
		}
	}
	if Lseen {
		return false
	}
	return true
}

type StrSlice []string

func (v *StrSlice) String() string {
	return "x" //fmt.Sprint(*v)
}

func (v *StrSlice) Set(s string) error {
	*v = append(*v, s)
	return nil
}

type Int64Slice []int64

func (v *Int64Slice) String() string {
	return "y" //fmt.Sprint(*v)
}

func (v *Int64Slice) Set(s string) error {
	n, err := strconv.ParseInt(s, 0, 64)
	if err != nil {
		return err
	}
	*v = append(*v, n)
	return nil
}
