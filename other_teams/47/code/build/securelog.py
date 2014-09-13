#!/usr/bin/env python2
import pickle
#import hashlib
from os import path, _exit
from sys import stderr, stdout
import random
import zlib
import base64
import argparse

debug = False

#def debug_print(message):
#    if debug:
#        print(message)

def exit_error(error, batch_run=False):
    #debug_print(error)
    stdout.write('invalid\n')
    if batch_run:
        #debug_print('batch run detected')
        raise batch_run
    else:
        stdout.flush()
        _exit(-1)

def security_error(error):
    #debug_print(error)
    stderr.write('security error\n')
    stderr.flush()
    _exit(-1)

def encrypt(key, text, reverse=False):
    try:
        rand = random.Random(key).randrange
        if not reverse:
            text = zlib.compress(text)
        text = ''.join([chr(ord(elem)^rand(256)) for elem in text])
        if reverse:
            text = zlib.decompress(text)
        return text
    except:
        security_error('failed encrypt/decrypt')

class gallery(object):
    def __init__(self, token):
        self.people_seen = set()
        self.rooms_seen = set()
        self.start_times = dict()
        self.end_times = dict()
        self.peoples_locations = dict()
        self.events = list()
        self.people_in_rooms = dict()
        self.latest = -1
        self.token = token

    def add_person(self, name, person_type, time):
        person = (name, person_type)
        if person not in self.people_seen:
            self.people_seen.add(person)
            self.start_times[person] = time
            self.end_times[person] = 0

    def find_current_room(self, name, person_type, room_num):
        for room in self.rooms_seen:
            if (name, person_type) in self.people_in_rooms[room]:
                return room
        return None

    def remove_person(self, name, person_type, room_num, time):
        person = (name, person_type)
        if self.find_current_room(name, person_type, room_num) == None:
            self.people_seen.remove(person)
            self.end_times[person] = time
            #debug_print('Person '+name+' departed gallery')
        try:
            room = self.people_in_rooms[room_num]
            room.remove(person)
            #debug_print(self.peoples_locations)
            #print(self.peoples_locations)
            for room in self.peoples_locations[person]:
                if room[0] == room_num:
                    room[2] = time
            #print(self.peoples_locations[person])
            #debug_print('Person '+name+' departed room '+str(room_num))
        except:
            pass

    def add_person_to_room(self, name, person_type, room_num, time):
        location = self.peoples_locations.get((name, person_type), list())
        location.append((room_num, time, time))
        self.peoples_locations[(name, person_type)] = location
        #debug_print(self.peoples_locations)
        default = set()
        room = self.people_in_rooms.get(room_num, default)
        room.add((name, person_type))
        self.people_in_rooms[room_num] = room
        #debug_print(self.people_in_rooms)

    def add_event(self, event):
        self.events.append(event)


class event(object):
    def __init__(self, timestamp, token, name, person_type, arrive_depart, room):
        self.timestamp = timestamp
        self.token = token
        self.name = name
        self.person_type = person_type
        self.room = room
        self.arrive_depart = arrive_depart

def get_person(args):
    if (args.employee_name):
        name = ''.join(map(str,args.employee_name))
        person_type = 'E'
    elif (args.guest_name):
        name = ''.join(map(str,args.guest_name))
        person_type = 'G'
    else:
        name, person = (None, None)
    return name, person_type

def get_employees(gallery_state):
    people = sorted(gallery_state.people_seen)
    employees = list()
    if people:
        for person in people:
            if person[1] == 'E':
                employees.append(person[0])
    return employees

def get_guests(gallery_state):
    people = sorted(gallery_state.people_seen)
    guests = list()
    if people:
        for person in people:
            if person[1] == 'G':
                guests.append(person[0])
    return guests

def html_header():
    return '<html>\n<body>\n'

def html_end(html_data):
    return html_data+'</body>\n</html>'

def open_log(log, openas='a+b'):
    if (log.isalnum()):
        if (path.isfile(log)):
            try:
                logfile = open(log, openas)
            except:
                exit_error('unable to open file')
        else:
            logfile = open(log, openas)
    else:
        exit_error('Unable to open file')
    return logfile

def read_log(args):
    #key = hashlib.sha256(args.token)
    #key = key.digest()
    key = args.token
    old_log = open_log(args.log, 'r')
    new_gallery = gallery(args.token)
    encrypted = old_log.read().split()
    for e in encrypted:
        e = base64.b64decode(e)
        d = encrypt(key, e, reverse=True)
        new_event = pickle.loads(d)
        new_gallery.add_event(new_event)
    if old_log:
        old_log.close()
    return new_gallery

def parse_gallery(gal):
    #Assume everything up to now is valid
    if gal.events:
        for ev in gal.events:
            gal.latest = ev.timestamp
            if ev.arrive_depart == 'A':
                #debug_print('Person '+ev.name+' arrived')
                gal.add_person(ev.name, ev.person_type, ev.timestamp)
                if ev.room != None:
                    #debug_print('Person '+ev.name+' entered room '+str(ev.room))
                    gal.rooms_seen.add(ev.room)
                    gal.add_person_to_room(ev.name, ev.person_type, ev.room, ev.timestamp)
            elif ev.arrive_depart == 'L':
                #debug_print('Person '+ev.name+' departed')
                gal.remove_person(ev.name, ev.person_type, ev.room, ev.timestamp)

    return gal

class ArgumentParser(argparse.ArgumentParser):

    def error(self, message):
        raise ""
        #super(ArgumentParser, self).error()
