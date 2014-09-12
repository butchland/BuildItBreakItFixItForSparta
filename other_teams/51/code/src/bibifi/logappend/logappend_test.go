package main

import (
	"testing"

	"bibifi/cmdline"
	"bibifi/logfile"
)

func BenchmarkLogReadWrite(b *testing.B) {
	cmd := cmdline.AppendCmd{
		Token: "GROAASJS",
		Log:   "FILENAME",
	}
	for i := 0; i < b.N; i++ {
		log, err := logfile.Load(cmd.Log, cmd.Token, true)
		if err != nil {
			b.Fatal(err)
		}
		err = log.Store(cmd.Log, cmd.Token)
		if err != nil {
			b.Fatal(err)
		}
	}
}

func BenchmarkLogRead(b *testing.B) {
	cmd := cmdline.AppendCmd{
		Token: "GROAASJS",
		Log:   "FILENAME",
	}
	for i := 0; i < b.N; i++ {
		_, err := logfile.Load(cmd.Log, cmd.Token, false)
		if err != nil {
			b.Fatal(err)
		}
	}
}

func BenchmarkLogWrite(b *testing.B) {
	cmd := cmdline.AppendCmd{
		Token: "GROAASJS",
		Log:   "FILENAME",
	}
	log, err := logfile.Load(cmd.Log, cmd.Token, false)
	if err != nil {
		b.Fatal(err)
	}
	for i := 0; i < b.N; i++ {
		err = log.Store(cmd.Log, cmd.Token)
		if err != nil {
			b.Fatal(err)
		}
	}
}
