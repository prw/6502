#!/usr/bin/python2
import sys
import serial
import time

def readBinary(filename, chunksize=1024):
	with open(filename, "rb") as f:
		while True:
			chunk = f.read(chunksize)
			if chunk:
				for b in chunk:
					yield b
			else:
				break


prog = [("%02X"%ord(i)).lower() for i in readBinary("ROM.bin")]

if len(sys.argv) <= 1:
	prog = "RA0000D"+"D".join(prog)+"Q"
	print prog
	
	serport = '/dev/ttyUSB0'
	baud = 300
	ser = serial.Serial(serport, baud, timeout=1)
	
	print ser.read(100)
	ser.write("P")
	print ser.read(100)
	for i in prog:
		ser.write(i)
		sys.stdout.write(i)
		sys.stdout.flush()
		#ser.write(i)
		time.sleep(0.05)
	print "DONE!"
else:
	prog = "A6000D"+"D".join(prog)+"A6000E"
	print "LOCAL:", prog
	#sends executable via serial to 6502
	serport = '/dev/ttyUSB0'
	baud = 300

	startsymbol = '\x53'

	ser = serial.Serial(serport, baud)
	print "LOCAL:", ser.name
	print "LOCAL: waiting for start symbol"
	
	inchar = None
	while inchar != startsymbol:
		inchar = ser.read(1)
		sys.stdout.write(inchar)
		sys.stdout.flush()
	
	print "\nLOCAL: starting transfer"
	for i in range(len(prog)):
		ser.write(prog[i])
		inchar = ser.read(1)
		print "{} of {}: i=[{}]: {}".format(i,len(prog),prog[i],inchar)
	print "\nLOCAL: transfer done"
		
