#!/usr/bin/env python2
import pickle
import argparse
#import hashlib
import string
from os import path
from sys import exc_info
from securelog import *
import base64
import shlex

def parse_args(batch_args=None):
    parser = ArgumentParser(description='Parse arguments for logappend', usage=argparse.SUPPRESS)
    # Timestamp sanitized here and verified in __init__
    parser.add_argument('-T', dest='timestamp', type=int, required=True)
    # Token sanitized in __init__
    parser.add_argument('-K', dest='token', required=True)
    name_group = parser.add_mutually_exclusive_group()
    name_group.add_argument('-E', dest='employee_name')
    name_group.add_argument('-G', dest='guest_name')
    arrive_depart = parser.add_mutually_exclusive_group()
    arrive_depart.add_argument('-A', action='store_true')
    arrive_depart.add_argument('-L', action='store_true')
    parser.add_argument('-R', dest='room_id', type=int)
    parser.add_argument('log')
    batch_parser = ArgumentParser(description='Parse arguments for batch logappend', usage=argparse.SUPPRESS)
    batch_parser.add_argument('-B', dest='batch_file', required=True)
    try:
        if (batch_args):
            #debug_print('Batch run setting true')
            batch_run = True
            batch_args = shlex.split(batch_args.rstrip().lstrip())
            if '-B' in batch_args:
                exit_error('-B used in batch file')
            args = parser.parse_args(batch_args)
            #debug_print(args)
            return args
        else:
            args = parser.parse_args()
            #debug_print(args)
            return args
    except:
        try:
            args = batch_parser.parse_args()
            #debug_print(args)
            return args
        except:
            exit_error(exc_info())

def write_to_log(log, data, token):
    #key = hashlib.sha256(token)
    pickled_data = pickle.dumps(data, -1)
    e = encrypt(token, pickled_data)
    e = base64.b64encode(e)
    log.write(e)
    log.write('\n')

def logappend(args, batch_run=False):
    try:
        assert(args.log.isalnum())
    except:
        exit_error('Invalid log file')
    new_log = open_log(args.log)
    name, person_type = get_person(args)
    #Timestamp
    try:
        timestamp = int(args.timestamp)
        assert(timestamp >= 0)
    except:
        exit_error('Timestamp not an integer', batch_run)
    #Token
    if (args.token.isalnum()):
        token = args.token
        #debug_print(token)
    else:
        security_error('Token is not alnum')
    #Guest or Employee name
    if (name.isalpha()):
        assert(person_type == 'E' or person_type == 'G')
    else:
        exit_error('Person name is not alpha', batch_run)
    #A or L
    if (args.A == True):
        arrive_depart = 'A'
    elif (args.L == True):
        arrive_depart = 'L'
    else:
        arrive_depart = None
    #Room-id
    if (args.room_id != None):
        try:
            room_id = int(args.room_id)
            assert(room_id >= 0)
        except:
            exit_error('Room id not an integer', batch_run)
    else:
        room_id = None

    gal = read_log(args)
    gal = parse_gallery(gal)
    new_event = event(timestamp, token, name, person_type, arrive_depart, room_id)
    person = (new_event.name, new_event.person_type)

    try:
        if new_event.timestamp <= gal.latest:
            exit_error('Older timestamp '+str(new_event.timestamp)+' detected latest: '+str(gal.latest), batch_run)
        if new_event.token != gal.token:
            security_error('Tokens not identical')
        if new_event.arrive_depart == 'L':
            if person not in gal.people_seen:
                exit_error(str(new_event.timestamp)+': Person leaving '+new_event.name+' not in people seen '+ batch_run)
            if new_event.room != None:
                if person not in gal.people_in_rooms[new_event.room]:
                    exit_error('Person leaving not in room '+new_event.room, batch_run)
        if new_event.arrive_depart == 'A':
            if new_event.room != None:
                if person not in gal.people_seen:
                    exit_error('Person entering room without entering gallery', batch_run)
                for room in gal.rooms_seen:
                    if person in gal.people_in_rooms[room]:
                        exit_error('Person entering a room without leaving another room', batch_run)
            else:
                if person in gal.people_seen:
                    exit_error('Person already arrived at gallery, cant reenter', batch_run)
        write_to_log(new_log, new_event, new_event.token)
        new_log.close()
    except:
        pass

if __name__ == '__main__':
    try:
        args = parse_args(None)
    except:
        exit_error('Failed to parse arguments')
    #debug_print(args)
    try:
        if args.batch_file:
            batch_file = open_log(args.batch_file, 'r')
            for batch in batch_file.readlines():
                if batch.rsplit():
                    args = parse_args(batch)
                    logappend(args, True)
    except:
        try:
            logappend(args)
        except:
            exit_error('Failed to append logs'+str(exc_info()))

