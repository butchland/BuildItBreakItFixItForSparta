#!/usr/bin/python
import sys
import random
import zlib
import base64

logfile=open("BGVYKCNH", "a+b")
log=open("decoded.log", "a+b")
encrypted = logfile.read().split()
i = 1 
for e in encrypted:
  e = base64.b64decode(e)
  if i == 1:
    rseed = (-sys.maxint - 1)
    while (1):
      if(rseed % 1000000) == 0: 
        print(rseed)
      rand = random.Random(rseed).randrange
      e = ''.join([chr(ord(elem)^rand(256)) for elem in e])
      try:
        d = zlib.decompress(e)
        log.write(d)
        log.write('\n')
        print('CRACKED: rseed == ', rseed)
        i = i + 1
        break
      except:
        rseed = rseed + 1
  else:
   rand = random.Random(rseed).randrange
   e = ''.join([chr(ord(elem)^rand(256)) for elem in e])
   d = zlib.decompress(e)
   log.write(d)
   log.write('\n')
log.close()
logfile.close()
