#!/usr/bin/env python

from optparse import OptionParser
import sys

parser = OptionParser()
parser.add_option("-T", dest="timestamp", metavar="timestamp")
parser.add_option("-K", dest="token")

(options, args) = parser.parse_args()

log = sys.argv[1]

if log == None:
    sys.stdout.write('invalid')
    return -1