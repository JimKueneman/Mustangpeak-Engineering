#!/usr/bin/env python
'''
Drive an OpenLCB via USB serial

Note: At 230400, requires CAN2USBino version 2 or later.

@author: Bob Jacobsen
'''
import serial
import time

class SerialOlcbLink :
    def __init__(self) :
        
        # defaults (generally overridden by system-wide defaults elsewhere)
        self.port = "/dev/tty.usbserial-A7007AOK"
        self.speed = 115200
        self.timeout = 0.1     # try to keep operations fast
        self.verbose = False
        self.parallel = False
        self.startdelay = 0    # set to 12 if your hardware resets on connection
        self.ser = None
        return
    
    def connect(self) :
        # if verbose, print
        if (self.verbose) : print "   connect to ",self.port," at ",self.speed
        
        self.ser = serial.Serial(self.port, self.speed)
        self.ser.parity = serial.PARITY_NONE
        self.ser.bytesize = serial.EIGHTBITS
        self.ser.stopbits = serial.STOPBITS_TWO
        self.ser.setXonXoff(True)
        self.ser.rtscts = False
        self.ser.dsrdtr = False
        self.ser.setDTR(True)
        self.ser.setRTS(True)
        
        # from http://bytes.com/topic/python/answers/170478-uart-parity-setting-mark-space-using-pyserial
        if self.speed == 230400 and not self.parallel :
            self.ser.parity = serial.PARITY_EVEN
            self.ser.stopbits = serial.STOPBITS_TWO
            import termios
            iflag, oflag, cflag, lflag, ispeed, ospeed, cc = termios.tcgetattr(self.ser)
            cflag |= 0x40000000 # CMSPAR to select MARK parity
            termios.tcsetattr(self.ser, termios.TCSANOW, [iflag, oflag, cflag, lflag,ispeed, ospeed, cc])
        
                
        # wait default time for Arduino startup
        # after (possible) reset due to serial startup
        if self.startdelay > 0 :
            if self.verbose : print "   waiting", self.startdelay, "seconds for adapter restart"
            time.sleep(self.startdelay)
            # dump all messages
            while self.ser.inWaiting() > 0 :
                self.ser.readline()
        return
        
    def send(self, frame) :
        if self.ser == None : self.connect()
        
        # if verbose, print
        if self.verbose : print "   send    ",frame

        # double-output format needed if operating at  230400
        tframe = frame+'\n'
        if self.speed == 230400 :
            tframe = "!!"
            for c in frame[1:len(frame)-1] :
                tframe = tframe+c+c
            tframe = tframe+";;"
        # send
        self.ser.write(tframe)
        
        return
        
    def receive(self) : # returns frame
        if (self.ser == None) : self.connect()
        
        # if verbose, print
        if (self.verbose) : print "   receive ",
            
        self.ser.timeout = self.timeout
        line = "";
        r = self.ser.readline()
        # remove Xoff/Xon characters if present
        r = r.replace("\x11", "")
        r = r.replace("\x13", "")
        # timeout returns ""
        if r == "" : 
            if (self.verbose) : print "<none>" # blank line to show delay?
            return None
        # if verbose, display what's received 
        if (self.verbose) : print r.replace("\x0A", "").replace("\x0D", "")
        return r       

    def close(self) :
        return

import getopt, sys

def main():
    global frame
    
    # create connection object
    network = SerialOlcbLink()

    # get defaults
    port = network.port 
    speed = network.speed
    verbose = network.verbose
    
    frame = ':X180A7000N;'

    # process arguments
    (port, speed, frame, verbose) = args(port, speed, frame, verbose)
        
    # load new defaults
    network.port = port
    network.speed = speed
    network.verbose = verbose
    
    # send the frame
    network.send(frame)
    while True :
        network.receive()
    
    return  # done with example

def usage() :
    print ""
    print "Python module for connecting to an OpenLCB via an serial connection."
    print "Called standalone, will send one CAN frame."
    print ""
    print "valid options:"
    print "  -v for verbose; also displays any responses"
    print "  -p, --port for serial port to USB connection"
    print "  -s, --speed for baud rate"
    print ""
    print "valid usages (default values):"
    print "  python serialolcblink.py --port=/dev/tty.usbserial-A7007AOK"
    print "  python serialolcblink.py --port=/dev/tty.usbserial-A7007AOK --speed=115200"
    print "  python serialolcblink.py --port=/dev/tty.usbserial-A7007AOK --speed=115200 :X180A7000N;\;"
    print ""
    print "Note: Most shells require escaping the semicolon at the end of the frame."
    
def args(port, speed, frame, verbose) :
    # argument processing
    try:
        opts, remainder = getopt.getopt(sys.argv[1:], "s:p:v", ["speed=", "port="])
    except getopt.GetoptError, err:
        # print help information and exit:
        print str(err) # will print something like "option -a not recognized"
        usage()
        sys.exit(2)
    for opt, arg in opts:
        if opt == "-v":
            verbose = True
        elif opt in ("-p", "--port"):
            port = arg
        elif opt in ("-s", "--speed"):
            speed = int(arg)
        else:
            assert False, "unhandled option"
    if (len(remainder) > 0) : 
        frame = remainder[0]
    return (port, speed, frame, verbose)
    
if __name__ == '__main__':
    main()
    
