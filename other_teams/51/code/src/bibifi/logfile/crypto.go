package logfile

import (
	"crypto/rand"
	"crypto/sha256"
	"io"
	wrand "math/rand"
	"time"

	"code.google.com/p/go.crypto/nacl/secretbox"
	"code.google.com/p/go.crypto/pbkdf2"

	"bibifi/cmdline"
)

// Much of this file are bits copied/adapted from github.com/kisom/cryptutils

// Encrypting
const (
	KeySize   = 32
	saltSize  = 8
	nonceSize = 24
)

func init() {
	wrand.Seed(time.Now().UnixNano())
}
func encrypt(input []byte, passphrase string, key0, salt []byte, nonce0 []byte) []byte {
	if salt == nil {
		cmdline.Dbg("nil salt")
		return nil
	}
	var key *[KeySize]byte
	if key0 == nil {
		key = deriveKey(passphrase, salt)
	} else {
		var tmp [KeySize]byte
		copy(tmp[:], key0)
		key = &tmp
	}
	if key == nil {
		cmdline.Dbg("nil key")
		return nil
	}

	data, ok := encrypt0(key, input, nonce0)
	if !ok {
		cmdline.Dbg("encrypt fail")
		data = nil
		return nil
	}

	data = append(salt, data...)
	return data
}

// DecryptFile recovers a secured blob from a file, returning a byte
// slice for parsing by the caller.
func decrypt(data []byte, passphrase string) ([]byte, []byte, []byte, []byte) {
	salt := data[:saltSize]
	data = data[saltSize:]

	key := deriveKey(passphrase, salt)
	if key == nil {
		cmdline.Dbg("nil key")
		return nil, nil, nil, nil
	}

	data, nonce, ok := decrypt0(key, data)
	if !ok {
		cmdline.Dbg("decrypt fail")
		return nil, nil, nil, nil
	}
	return data, salt, key[:], nonce
}

// Encrypt generates a random nonce and encrypts the input using
// NaCl's secretbox package. The nonce is prepended to the ciphertext.
func encrypt0(key *[KeySize]byte, in []byte, nonce0 []byte) ([]byte, bool) {
	var out = make([]byte, nonceSize)
	nonce := newNonce(nonce0)
	if nonce == nil {
		return nil, false
	}

	copy(out, nonce[:])
	out = secretbox.Seal(out, in, nonce, key)
	return out, true
}

// Decrypt extracts the nonce from the ciphertext, and attempts to
// decrypt with NaCl's secretbox.
func decrypt0(key *[KeySize]byte, in []byte) ([]byte, []byte, bool) {
	if len(in) < nonceSize {
		return nil, nil, false
	}
	var nonce [nonceSize]byte
	copy(nonce[:], in)
	out, ok := secretbox.Open(nil, in[nonceSize:], &nonce, key)
	return out, nonce[:], ok
}

// DeriveKey applies Scrypt with very strong parameters to generate an
// encryption key from a passphrase and salt.
func deriveKey(passphrase string, salt []byte) *[KeySize]byte {
	//rawKey, err := scrypt.Key([]byte(passphrase), salt, 8, 8, 4, KeySize)
	rawKey := pbkdf2.Key([]byte(passphrase), salt, 8, KeySize, sha256.New)
	//if err != nil {
	//	return nil
	//}
	var key [KeySize]byte
	copy(key[:], rawKey)
	zero(rawKey)
	return &key
}

// RandBytes is a wrapper for retrieving a buffer of the requested
// size, filled with random data. On failure, it returns nil.
func randBytes(size int) []byte {
	p := make([]byte, size)
	_, err := io.ReadFull(rand.Reader, p)
	if err != nil {
		p = nil
	}
	return p
}

func randBytesWeak(size int) []byte {
	p := make([]byte, size)
	for i := 0; i < size; i++ {
		p[i] = byte(wrand.Intn(256))
	}
	return p
}

// NewNonce generates a new random nonce for use with NaCl. This is a
// 192-bit random number. In this set of utilities, only one nonce is
// ever actually used with a key in most cases.
func newNonce(nonce0 []byte) *[nonceSize]byte {
	var nonce [nonceSize]byte
	var p []byte
	if nonce0 == nil {
		p = randBytes(nonceSize)
	} else {
		p = randBytesWeak(nonceSize)
		for i := 0; i < nonceSize; i++ {
			p[i] ^= nonce0[i]
		}
	}
	if p == nil {
		return nil
	}
	copy(nonce[:], p)
	return &nonce
}

func zero(in []byte) {
	for i := range in {
		in[i] ^= in[i]
	}
}
