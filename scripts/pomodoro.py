#!/usr/bin/env python
"""Usage: pomodoro [-f | --force] start
       pomodoro [-l | --long ] status
       pomodoro [--date <date>] report
       pomodoro cancel
       pomodoro --version
       pomodoro [-h | --help]

Commands:
    start   start the next block of tracked time
    status  report how much time is left in the current block
    report  list the days blocks
    cancel  cancel a running block of time

Options:
    -l, --long     Long output format
    -f, --force    Force start
    --date <date>  Date to report
    --version      Print version and exit
    -h, --help     Print help and exit

Formats:
    <date>  MM/DD

You'll alternate between pomodoros and minute breaks.
After every 4 pomodoros, the break will be extended.

A pomodoro is 25 minutes, a break is 5 minutes, and an extended break is 20
minutes. These can all be overridden using the POMODORO_POMODORO,
POMODORO_BREAK, and POMODORO_EXTENDED_BREAK environment variables. State will
be stored in ~/.pomodoro by default, and this can be overridden with the
POMODORO_FILE environment variable.
"""

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

__version__ = '1.0.0'

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
    """Takes a datetime.timedelta, returns total number of seconds, ignoring
    microseconds"""
    return td.seconds + td.days * 24 * 3600


class History(object):
    """Track pomodoro history"""
    def __init__(self):
        self.members = []

    def __iter__(self):
        return iter(self.members)

    def serialize(self):
        """Serialize entire history to disk"""
        pickle.dump(self, open(FILE, 'w'))

    @property
    def next_duration(self):
        """Returns the intended duration of the next block of time"""
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
        """Returns the durations of each of the previous 7 blocks of time"""
        return [p.duration for p in self.members[-7:]]

    @staticmethod
    def deserialize():
        """Deserialize entire history from disk"""
        try:
            return pickle.load(open(FILE))
        except IOError:
            history = History()
            history.serialize()
            return history

    def append(self, pomodoro):
        """Add another block of time to the history"""
        self.members.append(pomodoro)
        self.serialize()

    def cancel(self):
        """Cancel the running block of time"""
        if len(self.members) > 0 and not self.last.finished:
            self.members.pop()
            self.serialize()
            return 0
        else:
            return 1

    @property
    def last(self):
        """Returns the most recent block of time if one is available, or None
        otherwise"""
        try:
            return self.members[-1]
        except IndexError:
            return None


class Pomodoro(object):
    """A tracked block of time"""
    def __init__(self, duration, start_time):
        self.duration = duration
        self.start_time = start_time
        self.end_time = start_time + duration

    def __repr__(self):
        return "Pomodoro(%s, %s)" % (repr(self.duration),
                                     repr(self.start_time))

    @property
    def type(self):
        """A string representation of the type of block of time this is"""
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
        """A rich description of this block of time"""
        start = self.start_time.strftime("%H:%M")
        end = self.end_time.strftime("%H:%M")
        total = total_seconds(self.duration)
        _type = self.type
        minutes = total / 60
        return "%s-%s (%2d) %s" % (start, end, minutes, _type)

    @property
    def remaining_time(self):
        """The amount of time remaining in this block"""
        now = datetime.datetime.now()
        if now < self.end_time:
            return self.end_time - now
        else:
            return datetime.timedelta(0)

    @property
    def finished(self):
        """Whether or not this block of time is finished"""
        return self.remaining_time == datetime.timedelta(0)


def start(force=False):
    """Start and track a new block of time, if appropriate"""
    history = History.deserialize()
    last = history.last
    if force or last is None or last.finished:
        pom = Pomodoro(history.next_duration, datetime.datetime.now())
        history.append(pom)
        return 0
    else:
        return 1


def cancel():
    """Cancel the running block of time"""
    history = History.deserialize()
    return history.cancel()


def status(long=False):
    """Report on the status of the current block of time"""
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
    """Report all tracked blocks of time for the given day"""
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
    """Main program logic"""
    try:
        opts = docopt.docopt(__doc__, version=__version__)
        if opts[START]:
            exit(start(opts[FORCE]))
        elif opts[CANCEL]:
            exit(cancel())
        elif opts[STATUS]:
            exit(status(opts[LONG]))
        elif opts[REPORT]:
            if opts[DATE]:
                try:
                    opts[DATE] = dateutil.parser.parse(opts[DATE])
                except (ValueError):
                    raise docopt.DocoptExit("invalid date format")
            else:
                opts[DATE] = datetime.date.today()
            exit(report(opts[DATE]))
        else:
            raise docopt.DocoptExit()
    except KeyboardInterrupt:
        exit(2)
    except docopt.DocoptExit:
        raise
    except Exception as exc:
        print >> sys.stderr, "Error: %s" % exc
        sys.exit(2)


if __name__ == '__main__':
    main()
