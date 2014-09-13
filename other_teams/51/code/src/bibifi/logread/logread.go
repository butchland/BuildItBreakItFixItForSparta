package main

import (
	"fmt"
	"os"
	//"runtime/debug"

	"bibifi/cmdline"
	"bibifi/logfile"
)

func main() {
	dbg := cmdline.Dbg
	cmd, err := cmdline.ParseRead()
	if err != nil {
		dbg("main ParseRead: " + err.Error())
		fmt.Println("invalid")
		os.Exit(-1)
	}
	log, err := logfile.Load(cmd.Log, cmd.Token, false, nil)
	if err != nil {
		dbg("main Load: " + err.Error())
		fmt.Fprintln(os.Stderr, "integrity violation")
		os.Exit(-1)
	}
	if !log.Read(cmd) {
		dbg("main read")
		fmt.Println("invalid")
		os.Exit(-1)
	}
}
