package logfile

import (
	"testing"
)

func BenchmarkDeriveKey(b *testing.B) {
	passphrase := "ABCDEFGH"
	salt := randBytes(saltSize)
	for i := 0; i < b.N; i++ {
		deriveKey(passphrase, salt)
	}
}

func BenchmarkSimpleEncrypt(b *testing.B) {
	passphrase := "ABCDEFGH"
	salt := randBytes(saltSize)
	key := deriveKey(passphrase, salt)
	data := randBytes(1025 * 5)
	for i := 0; i < b.N; i++ {
		_, ok := encrypt0(key, data, nil)
		if !ok {
			b.Fatal("encrypt failed")
		}
	}
}

func BenchmarkSimpleDecrypt(b *testing.B) {
	passphrase := "ABCDEFGH"
	salt := randBytes(saltSize)
	key := deriveKey(passphrase, salt)
	data := randBytes(1025 * 5)
	enc, ok := encrypt0(key, data, nil)
	if !ok {
		b.Fatal("encrypt failed")
	}
	for i := 0; i < b.N; i++ {
		_, _, ok := decrypt0(key, enc)
		if !ok {
			b.Fatal("decrypt failed")
		}
	}
}
