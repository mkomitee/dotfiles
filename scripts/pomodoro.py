#!/usr/bin/env python
"""Usage:
    pomodoro start
    pomodoro cancel
    pomodoro status
    pomodoro [--date <date>] report
    pomodoro --version
    pomodoro [-h | --help]

Options:
    --date <date>  Date to report
    -v, --verbose  Print verbose information
    --version      Print version and exit
    -h, --help     Print help and exit"""


import docopt
import os
import sys
import dateutil.parser
import datetime
import pickle

POMODORO_FILE = os.path.expanduser("~/.pomodoro")

START = 'start'
CANCEL = 'cancel'
STATUS = 'status'
REPORT = 'report'
DATE = '--date'

POMODORO = datetime.timedelta(minutes=25)
BREAK = datetime.timedelta(minutes=5)
EXTENDEDBREAK = datetime.timedelta(minutes=20)

def total_seconds(td):
    return (td.microseconds + (td.seconds + td.days * 24 * 3600) * 10**6) / float(10**6)

class History(object):
    def __init__(self):
        self.members = []

    def serialize(self):
        pickle.dump(self, open(POMODORO_FILE, 'w'))

    @property
    def next_duration(self):
        if self.last is not None and self.last.duration == POMODORO:
            last = self.last_seven_durations
            if len(last) < 7:
                return BREAK
            elif EXTENDEDBREAK in self.last_seven_durations:
                return BREAK
            else:
                return EXTENDEDBREAK
        else:
            return POMODORO

    @property
    def last_seven_durations(self):
        return [p.duration for p in self.members[-7:]]

    @staticmethod
    def deserialize():
        try:
            return pickle.load(open(POMODORO_FILE))
        except IOError:
            history = History()
            history.serialize()
            return history

    def append(self, pomodoro):
        self.members.append(pomodoro)
        self.serialize()

    def cancel(self):
        if len(self.members) > 0 and not self.last.finished:
            self.members.pop()
            self.serialize()

    @property
    def last(self):
        try:
            return self.members[-1]
        except IndexError:
            return None


class Pomodoro(object):
    def __init__(self, duration, start_time):
        self.duration = duration
        self.start_time = start_time
        self.end_time = start_time + duration

    def __repr__(self):
        return "Pomodoro(%s, %s)" % (repr(self.duration), repr(self.start_time))

    def __str__(self):
        total = total_seconds(self.remaining_time)
        minutes = total / 60
        seconds = total % 60
        return "%.2d:%.2d" % (minutes, seconds)

    @property
    def remaining_time(self):
        now = datetime.datetime.now()
        if now < self.end_time:
            return self.end_time - now
        else:
            return datetime.timedelta(0)

    @property
    def finished(self):
        return self.remaining_time == datetime.timedelta(0)


def start():
    history = History.deserialize()
    last = history.last
    if last is None or last.finished:
        pom = Pomodoro(history.next_duration, datetime.datetime.now())
        history.append(pom)
        return 0
    else:
        return 1


def cancel():
    history = History.deserialize()
    history.cancel()


def status():
    history = History.deserialize()
    last = history.last
    if last:
        sys.stdout.write(str(history.last))
        sys.stdout.flush()
        if history.last.finished:
            return 1
        else:
            return 0
    else:
        sys.stdout.write('--:--')
        sys.stdout.flush()
        return 1


def report(date):
    history = History.deserialize()
    for pom in history.members:
        if date.month not in (pom.start_time.month, pom.end_time.month):
            continue
        if date.day not in (pom.start_time.day, pom.end_time.day):
            continue
        start = pom.start_time.strftime("%H:%M")
        end = pom.end_time.strftime("%H:%M")
        total = total_seconds(pom.duration)
        minutes = total / 60
        seconds = total % 60
        if pom is history.last and not pom.finished:
            print "%s-%s (%.2d:%.2d)*" % (start, end, minutes, seconds)
        else:
            print "%s-%s (%.2d:%.2d)" % (start, end, minutes, seconds)


def main():
    opts = docopt.docopt(__doc__, version='1.0.0')
    if opts[START]:
        return start()
    elif opts[CANCEL]:
        return cancel()
    elif opts[STATUS]:
        return status()
    elif opts[REPORT]:
        if opts[DATE]:
            opts[DATE] = dateutil.parser.parse(opts[DATE])
        else:
            opts[DATE] = datetime.date.today()
        return report(opts[DATE])


if __name__ == '__main__':
    try:
        exit(main())
    except KeyboardInterrupt:
        exit(1)

