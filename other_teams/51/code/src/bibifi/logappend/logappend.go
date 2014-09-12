package main

import (
	"fmt"
	"os"
	"runtime/debug"

	"bibifi/cmdline"
	"bibifi/logfile"
)

var dbg func(interface{})

func init() {
	dbg = cmdline.Dbg
}

func main() {
	// Disable GC when not processing a batch file.
	debug.SetGCPercent(-1)
	//go func() {
	//	// Open the crypto/rand source in the background, so it's available without blocking when needed.
	//	tmp := make([]byte, 4)
	//	rand.Read(tmp)
	//}()
	//runtime.Gosched()
	cmds, batch, err := cmdline.ParseAppend()
	if batch {
		debug.SetGCPercent(100)
	}
	if err != nil {
		dbg("main parse: " + err.Error())
		fmt.Println("invalid")
		os.Exit(-1)
	}
	doAppend(cmds, batch)

}

func doAppend(cmds []cmdline.AppendCmd, batch bool) {
	// Load log files
	var err error
	logs := make(map[string]*logfile.Log)
	keys := make(map[string]string)
	cachef := !batch
	for _, cmd := range cmds {
		log, ok := logs[cmd.Log]
		if ok {
			continue
		}
		var lazy *cmdline.AppendCmd
		if !batch {
			lazy = &cmd
		}
		log, err = logfile.Load(cmd.Log, cmd.Token, cachef, lazy)
		if err != nil {
			dbg("main load: " + err.Error())
			fmt.Fprintln(os.Stderr, "security error")
			if !batch {
				os.Exit(-1)
			}
		}
		logs[cmd.Log] = log
		keys[cmd.Log] = cmd.Token
	}

	// Process commands
	for _, cmd := range cmds {
		log, _ := logs[cmd.Log]
		if err := log.Append(cmd); err != nil {
			dbg("main append: " + err.Error())
			fmt.Println("invalid")
			if !batch {
				os.Exit(-1)
			}
		}
	}

	// Update log file
	for name, log := range logs {
		if err := log.Store(name, keys[name]); err != nil {
			dbg("main store: " + err.Error())
			fmt.Println("invalid")
			if !batch {
				os.Exit(-1)
			}
		}
	}
}
