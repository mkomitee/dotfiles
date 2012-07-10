#!/usr/bin/env python
"""Usage:
    pomodoro [-f | --force] start
    pomodoro cancel
    pomodoro [-l | --long ] status
    pomodoro [--date <date>] report
    pomodoro --version
    pomodoro [-h | --help]

Options:
    -l, --long     Long output format
    -f, --force    Force start
    --date <date>  Date to report
    --version      Print version and exit
    -h, --help     Print help and exit

Formats:
    <date>  MM/DD"""

import os
import sys
import dateutil.parser
import datetime
import pickle

try:
    import docopt
except ImportError:
    sys.stdout.write('--:--')
    sys.stdout.flush()
    sys.exit(2)


START = 'start'
CANCEL = 'cancel'
STATUS = 'status'
REPORT = 'report'
DATE = '--date'
FORCE = '--force'
LONG = '--long'

def _env_get(name, default):
    return os.environ.get('POMODORO_%s' % name, default)

FILE = os.path.expanduser(_env_get('FILE', "~/.pomodoro"))

POMODORO = datetime.timedelta(minutes=int(_env_get('POMODORO', 25)))
BREAK = datetime.timedelta(minutes=int(_env_get('BREAK', 5)))
EXTENDEDBREAK = datetime.timedelta(minutes=int(_env_get('EXTENDED_BREAK', 20)))

def total_seconds(td):
    return td.seconds + td.days * 24 * 3600

class History(object):
    def __init__(self):
        self.members = []

    def __iter__(self):
        return iter(self.members)

    def serialize(self):
        pickle.dump(self, open(FILE, 'w'))

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
            return pickle.load(open(FILE))
        except IOError:
            history = History()
            history.serialize()
            return history
        except Exception:
            sys.stdout.write('--:--')
            sys.stdout.flush()
            sys.exit(2)

    def append(self, pomodoro):
        self.members.append(pomodoro)
        self.serialize()

    def cancel(self):
        if len(self.members) > 0 and not self.last.finished:
            self.members.pop()
            self.serialize()
            return 0
        else:
            return 1

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

    @property
    def type(self):
        if self.duration == POMODORO:
            return 'pomodoro'
        elif self.duration == BREAK:
            return 'break'
        else:
            return 'extended break'

    def __str__(self):
        total = total_seconds(self.remaining_time)
        minutes = total / 60
        seconds = total % 60
        return "%.2d:%.2d" % (minutes, seconds)

    @property
    def report(self):
        start = self.start_time.strftime("%H:%M")
        end = self.end_time.strftime("%H:%M")
        total = total_seconds(self.duration)
        _type = self.type
        minutes = total / 60
        return "%s-%s (%2d) %s" % (start, end, minutes, _type)

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


def start(force=False):
    history = History.deserialize()
    last = history.last
    if force or last is None or last.finished:
        pom = Pomodoro(history.next_duration, datetime.datetime.now())
        history.append(pom)
        return 0
    else:
        return 1


def cancel():
    history = History.deserialize()
    return history.cancel()


def status(long=False):
    history = History.deserialize()
    last = history.last
    if last:
        if long:
            output = "%s %s" % (history.last, history.last.type)
        else:
            output = str(history.last)
        sys.stdout.write(output)
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
        if pom is history.last and not pom.finished:
            print "%s %s" % (pom.report, history.last)
        else:
            print pom.report


def main():
    opts = docopt.docopt(__doc__, version='1.0.0')
    if opts[START]:
        return start(opts[FORCE])
    elif opts[CANCEL]:
        return cancel()
    elif opts[STATUS]:
        return status(opts[LONG])
    elif opts[REPORT]:
        if opts[DATE]:
            try:
                opts[DATE] = dateutil.parser.parse(opts[DATE])
            except (ValueError):
                print >> sys.stderr, "invalid date format"
                return 2
        else:
            opts[DATE] = datetime.date.today()
        return report(opts[DATE])


if __name__ == '__main__':
    try:
        exit(main())
    except KeyboardInterrupt:
        exit(2)

