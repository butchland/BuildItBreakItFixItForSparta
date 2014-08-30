#!/usr/bin/env python

from optparse import OptionParser
import sys
import os.path
import hashlib
from Crypto.Cipher import AES
from Crypto import Random

HARDCODED_STRING = "SWAG"

def main():
    parser = OptionParser()
    parser.add_option("-T", dest="timestamp", metavar="timestamp")
    parser.add_option("-K", dest="token", action="store", type="string", metavar="token")

    (options, args) = parser.parse_args()

    log_path = sys.argv[-1]

    if log_path == None:
        # we need the last argument to be the path to log
        sys.stdout.write('invalid')
        return -1

    if os.path.isfile(log_path):
        # appending to an existing log
        f=open(log_path, 'r')
        lines = f.readlines()
        nonce = lines[0].rstrip() # remove new line

        ciphertext = lines[1].rstrip()

        # decrypted under the -K option passed in as a 256 hash
        key = hashlib.sha256(options.token).digest()
        obj = AES.new(key, AES.MODE_CFB, nonce)

        message = obj.decrypt(ciphertext)
        print message
    else:
        f=open(log_path, 'w')
        # this is a new log
        # first line is nonce, 2nd line is encrypted hardcoded string
        # rest of file is encrypted data
        nonce = Random.new().read(AES.block_size)

        # TODO actually respond to the input
        data = 'blank'


        message = HARDCODED_STRING + "\n" + data

        # encrypted under the -K option passed in as a 256 hash
        key = hashlib.sha256(options.token).digest()

        obj = AES.new(key, AES.MODE_CFB, nonce)
        ciphertext = obj.encrypt(message)

        f.write(nonce + "\n" + ciphertext)



if __name__ == "__main__":
    main()

