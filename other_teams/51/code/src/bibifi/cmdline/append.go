package cmdline

import (
	"bufio"
	"errors"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"strings"
)

type AppendCmd struct {
	Timestamp int64
	Token     string
	Log       string

	Employee string
	Guest    string

	Arrive bool
	Leave  bool
	Room   string
}

func ParseAppend() ([]AppendCmd, bool, error) {
	// Disable stderr during flag parsing
	//stderr := os.Stderr
	//os.Stderr = discard
	//defer func() {
	//	os.Stderr = stderr
	//}()

	if len(os.Args) < 3 {
		return nil, false, errors.New("not enough args")
	}

	if os.Args[1] == "-B" {
		// Batch file
		fn := os.Args[2]
		res, err := parseAppendBatch(fn)
		return res, true, err
	}

	// Single command
	cmd, err := parseAppendCmd(os.Args[1:])
	if err != nil {
		return nil, false, err
	}
	return []AppendCmd{cmd}, false, nil

}

func parseAppendBatch(fn string) ([]AppendCmd, error) {
	f, err := os.Open(fn)
	if err != nil {
		return nil, errors.New("I/O error")
	}
	defer f.Close()

	var res []AppendCmd
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		txt := scanner.Text()
		if len(txt) == 0 {
			continue
		}
		args := strings.Split(scanner.Text(), " ")
		cmd, err := parseAppendCmd(args)
		if err != nil {
			// Spec changed: -B continues even if one command is invalid.
			dbg(err)
			fmt.Println("invalid")
			continue
		}
		res = append(res, cmd)
	}

	// Spec changed: -B always returns 0.
	//if len(res) == 0 {
	//	return nil, false
	//}

	return res, nil
}

func parseAppendCmd(args []string) (out AppendCmd, err error) {
	defer func() {
		if r := recover(); r != nil {
			out = AppendCmd{}
			err = fmt.Errorf("panic in parseAppendCmd: %v", r)
		}
	}()
	res := AppendCmd{}
	fs := flag.NewFlagSet("", flag.PanicOnError)
	fs.SetOutput(ioutil.Discard)
	fs.Int64Var(&res.Timestamp, "T", -1, "")
	fs.StringVar(&res.Token, "K", "", "")
	fs.StringVar(&res.Employee, "E", "", "")
	fs.StringVar(&res.Guest, "G", "", "")
	fs.BoolVar(&res.Leave, "L", false, "")
	fs.BoolVar(&res.Arrive, "A", false, "")
	fs.StringVar(&res.Room, "R", "", "")

	if err := fs.Parse(args); err != nil {
		return AppendCmd{}, err
	}

	log := fs.Args()
	if len(log) != 1 {
		return AppendCmd{}, errors.New("extra non-flag args")
	}
	res.Log = log[0]
	for len(res.Room) > 1 && res.Room[0] == '0' {
		res.Room = res.Room[1:]
	}

	if err := validateAppend(res); err != nil {
		return AppendCmd{}, err
	}

	return res, nil
}

func validateAppend(cmd AppendCmd) error {
	if cmd.Timestamp < 0 {
		return errors.New("cmd.Timestamp < 0")
	}
	if cmd.Token == "" {
		return errors.New("cmd.Token == nil")
	}
	if cmd.Arrive && cmd.Leave {
		return errors.New("cmd.Arrive && cmd.Leave")
	}
	if !cmd.Arrive && !cmd.Leave {
		return errors.New("!cmd.Arrive && !cmd.Leave")
	}
	if cmd.Employee == "" && cmd.Guest == "" {
		return errors.New("cmd.Employee == nil && cmd.Guest == nil")
	}
	if cmd.Employee != "" && cmd.Guest != "" {
		return errors.New("cmd.Employee != nil && cmd.Guest != nil")
	}
	if cmd.Log == "" {
		return errors.New("cmd.Log == nil")
	}
	if cmd.Employee != "" {
		if !checkPerson(cmd.Employee) {
			return errors.New("Employee regex mismatch")
		}
	} else {
		if !checkPerson(cmd.Guest) {
			return errors.New("Guest regex mismatch")
		}
	}
	if cmd.Room != "" {
		if !checkRoom(cmd.Room) {
			return errors.New("Room regex mismatch")
		}
	}
	if !checkToken(cmd.Token) {
		return errors.New("Token regex mismatch")
	}
	return nil
}
