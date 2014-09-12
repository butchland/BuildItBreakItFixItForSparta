package cmdline

import (
	"errors"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
)

type ReadCmd struct {
	Token string
	Log   string
	HTML  bool

	State     bool
	Rooms     bool
	Time      bool
	Intersect bool
	BoundaryA bool
	BoundaryB bool

	Lower Int64Slice
	Upper Int64Slice

	Employee StrSlice
	Guest    StrSlice
}

func ParseRead() (res ReadCmd, err error) {
	// Disable stderr during flag parsing
	//stderr := os.Stderr
	//os.Stderr = discard
	defer func() {
		if r := recover(); r != nil {
			res = ReadCmd{}
			err = fmt.Errorf("panic in ParseRead: %v", r)
		}
		//os.Stderr = stderr
	}()

	res = ReadCmd{}
	fs := flag.NewFlagSet("", flag.PanicOnError)
	fs.SetOutput(ioutil.Discard)

	fs.StringVar(&res.Token, "K", "", "")
	fs.BoolVar(&res.HTML, "H", false, "")

	fs.BoolVar(&res.State, "S", false, "")
	fs.BoolVar(&res.Rooms, "R", false, "")
	fs.BoolVar(&res.Time, "T", false, "")
	fs.BoolVar(&res.BoundaryA, "A", false, "")
	fs.BoolVar(&res.BoundaryB, "B", false, "")
	fs.BoolVar(&res.Intersect, "I", false, "")

	fs.Var(&res.Employee, "E", "")
	fs.Var(&res.Guest, "G", "")

	fs.Var(&res.Lower, "L", "")
	fs.Var(&res.Upper, "U", "")

	if err := fs.Parse(os.Args[1:]); err != nil {
		return ReadCmd{}, err
	}

	log := fs.Args()
	if len(log) != 1 {
		return ReadCmd{}, errors.New("extra non-flag args")
	}
	res.Log = log[0]

	if err := validateRead(res); err != nil {
		return ReadCmd{}, err
	}

	return res, nil
}

func validateRead(cmd ReadCmd) error {
	if cmd.Token == "" {
		return errors.New("cmd.Token == nil")
	}
	if !isOnlyOneCmd(cmd) {
		return errors.New("More than one command given.")
	}

	if cmd.State {
		// Check invalid options
		if !checkOnlyValidOpts("-S", "-H") {
			return errors.New("options incompatible with -S")
		}
	}

	if cmd.Rooms {
		// Check invalid options
		if !checkOnlyValidOpts("-R", "-H", "-E", "-G") {
			return errors.New("options incompatible with -R")
		}

		// Check my options
		if len(cmd.Employee)+len(cmd.Guest) != 1 {
			return errors.New("must be exactly one guest/employee")
		}
	}

	if cmd.Time {
		if !checkOnlyValidOpts("-T", "-E", "-G") {
			return errors.New("options incompatible with -T")
		}
		// Check my options
		if len(cmd.Employee)+len(cmd.Guest) != 1 {
			return errors.New("must be exactly one guest/employee")
		}
	}

	if cmd.BoundaryA {
		if !checkOnlyValidOpts("-H", "-A", "-L", "-U") {
			return errors.New("options incompatible with -A")
		}
		if len(cmd.Lower) != 1 || len(cmd.Upper) != 1 {
			return errors.New("-A needs both -L and -U exactly once")
		}
		if cmd.Lower[0] > cmd.Upper[0] {
			return errors.New("invalid limits")
		}
		if !checkLU() {
			return errors.New("wrong ordering -L -U")
		}
	}

	if cmd.BoundaryB {
		if !checkOnlyValidOpts("-H", "-B", "-L", "-U") {
			return errors.New("options incompatible with -B")
		}
		if len(cmd.Lower) != 2 || len(cmd.Upper) != 2 {
			return errors.New("-A needs both -L and -U exactly twice")
		}
		if cmd.Lower[0] > cmd.Upper[0] {
			return errors.New("invalid limits 0")
		}
		if cmd.Lower[1] > cmd.Upper[1] {
			return errors.New("invalid limits 1")
		}
		if !checkLU() {
			return errors.New("wrong ordering -L -U")
		}
	}

	if cmd.Intersect {
		if !checkOnlyValidOpts("-H", "-I", "-E", "-G") {
			return errors.New("options incompatible with -E")
		}
		// Check my options
		if len(cmd.Employee)+len(cmd.Guest) < 1 {
			return errors.New("must have at least one guest/employee")
		}
	}

	// validate values
	for _, name := range cmd.Employee {
		if !checkPerson(name) {
			return errors.New("Employee regex no match")
		}
	}
	for _, name := range cmd.Guest {
		if !checkPerson(name) {
			return errors.New("Guest regex no match")
		}
	}
	if !checkToken(cmd.Token) {
		return errors.New("Token regex no match")
	}
	return nil
}

func isOnlyOneCmd(cmd ReadCmd) bool {
	count := 0
	if cmd.State {
		count++
	}
	if cmd.Rooms {
		count++
	}
	if cmd.Time {
		count++
	}
	if cmd.Intersect {
		count++
	}
	if cmd.BoundaryA {
		count++
	}
	if cmd.BoundaryB {
		count++
	}
	return count == 1
}
