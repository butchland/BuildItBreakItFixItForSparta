#!/usr/bin/env python2
import argparse
import string
from securelog import *

def parse_args():
    parser = ArgumentParser(description='Parse arguments for logappend', usage=argparse.SUPPRESS)
    # Token sanitized in __init__
    parser.add_argument('-K', dest='token', required=True)
    parser.add_argument('-H', dest='html', action='store_true')
    parser.add_argument('-S', dest='show_state', action='store_true')
    parser.add_argument('-R', dest='rooms_by_guest', action='store_true')
    # Timestamp sanitized here and verified in __init__
    parser.add_argument('-T', dest='show_time', action='store_true')
    parser.add_argument('-I', dest='show_rooms', action='store_true')
    parser.add_argument('-A', dest='list_employees_inc', action='store_true')
    parser.add_argument('-B', dest='list_employees_ex', action='store_true')
    parser.add_argument('-L', dest='lower_bounds', type=int, action='append', default=[])
    parser.add_argument('-U', dest='upper_bounds', type=int, action='append', default=[])
    parser.add_argument('-E', dest='employee_name', action='append', default=[])
    parser.add_argument('-G', dest='guest_name', action='append', default=[])
    parser.add_argument('log')
    try:
        args = parser.parse_args()
        return args
    except:
        exit_error('Invalid arguments')

def sort_and_delimit(people_list, delim=','):
    retlist = (delim.join(map(str, sorted([x[0] for x in people_list]))))
    return retlist

def html_state(gallery_state):
    html = html_header()
    if gallery_state.people_seen:
        html += '<table>\n<tr>\n\t<th>Employee</th>\n\t<th>Guest</th>\n'
        employees = get_employees(gallery_state)
        guests = get_guests(gallery_state)
        while(employees or guests):
            html += '<tr>\n'
            if employees:
                html += '\t<td>'+employees.pop(0)+'</td>\n'
            if guests:
                html += '\t<td>'+guests.pop(0)+'</td>\n'
            html += '</tr>\n'
        html += '</table>\n'
    rooms = sorted(gallery_state.rooms_seen)
    if rooms:
        html += '<table>\n<tr>\n\t<th>Room ID</th>\n\t<th>Occupants</th>\n</tr>'
        for room in rooms:
            people_in_room = sort_and_delimit(gallery_state.people_in_rooms[room])
            html += '<tr>\n\t<td>'+room+'</td>\n\t<td>'+people_in_room+'</td>\n'
        html += '</tr>\n'

    return html_end(html)

def print_state(gallery_state):
    employees = get_employees(gallery_state)
    guests = get_guests(gallery_state)
    if employees:
        print(','.join(map(str, employees)))
    else:
        print
    if guests:
        print(','.join(map(str, guests)))
    else:
        print
    for room in sorted(gallery_state.people_in_rooms):
        if gallery_state.people_in_rooms[room]:
            print(str(room)+":"),
            print(sort_and_delimit(gallery_state.people_in_rooms[room]))

def print_rooms_by_guest(gallery_state, person):
    try:
        #print(gallery_state.peoples_locations[person])
        person_list = gallery_state.peoples_locations[person]
        print(','.join([str(x[0]) for x in person_list]))
    except:
        exit_error('Person not found in locations')

def html_rooms_by_guest(gallery_state, person):
    html = html_header()
    try:
        person_list = gallery_state.peoples_locations[person]
    except:
        exit_error('Person not found in locations')
    if person_list:
        html += '<table>\n<tr>\n\t<th>Rooms</th>\n'
        for room in person_list:
            html += '<tr>\n\t<td>'+str(room[0])+'</td>\n</tr>\n'
        html += '</table>\n'
    return html_end(html)

def print_time_by_guest(gallery_state, person):
    try:
        if gallery_state.end_times[person]:
            print(str(gallery_state.end_times[person] - gallery_state.start_times[person]))
        else:
            print(str(gallery_state.end_times[person] - gallery_state.latest))
    except:
        pass

def print_rooms(gallery_state, people):
    rooms_seen = dict()
    rooms_in_common = set()
    #print(people)
    #print(gallery_state.peoples_locations)
    if len(people) == 1:
        print(','.join(str(x[0]) for x in gallery_state.peoples_locations.get(people[0], None)))
    else:
        for person in people:
            for times in gallery_state.peoples_locations.get(person, list()):
                print(times)


def html_rooms(gallery_state, people):
    html = html_header()
    for person in people:
        pass
    return html

if __name__ == '__main__':
    args = parse_args()
    #try:
    #    assert(args.log.isalnum())
    #    if args.employee_name:
    #        args.employee_name[:] = [str(name) for name in args.employee_name]
    #    if args.guest_name:
    #        args.guest_name[:] = [str(name) for name in args.guest_name]
    #except:
    #    exit_error('Invalid arguments')
    if (args.token.isalnum()):
        token = args.token
    else:
        security_error('Token is not alnum')
    try:
        gal = read_log(args)
    except:
        exit_error('Error reading log')
    gal = parse_gallery(gal)
    if args.show_state:
        if args.html:
            print(html_state(gal))
        else:
            print_state(gal)
    elif args.rooms_by_guest:
        name, person_type = get_person(args)
        if args.html:
            print(html_rooms_by_guest(gal, (name, person_type)))
        else:
            print_rooms_by_guest(gal, (name, person_type))
    elif args.show_time:
        if args.html:
            exit_error('Time and HTML invalid combination')
        else:
            name, person_type = get_person(args)
            print_time_by_guest(gal, (name, person_type))
    elif args.show_rooms:
        people_list = list()
        if args.employee_name:
            for employee in args.employee_name:
                people_list.append((employee, 'E'))
        if args.guest_name:
            for guest in args.guest_name:
                people_list.append((guest, 'G'))
        if not people_list:
            exit_error('Need people to show list')
        if args.html:
            print(html_rooms(gal, people_list))
        else:
            print_rooms(gal, people_list)
    else:
        exit_error('Error what to display not chosen')
