
CRTPATH=$(shell pwd)
export GOPATH=$(CRTPATH)/../
export CGO_ENABLED=1

all:
	@echo "GOPATH=$(GOPATH)"
	go build --ldflags "-w -s"  bibifi/logappend
	go build --ldflags "-w -s" bibifi/logread

clean:
	rm logread logappend
