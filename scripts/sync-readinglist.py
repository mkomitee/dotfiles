#!/usr/bin/env python
"""
Synchronizes your reading list to instapaper. Keeps track of what has been
synchronized by placing the timestamp of the last synchronized item in a file
in ~/.rl2instapaper.

Retreives the users instapaper password from the keychain item of named with
the string in the SERVER variable an an account named with the string in the
USER variable.

Requires 'instapaperlib' to be installed and available.
"""

import subprocess
import plistlib
import os
import re
import instapaperlib

SERVER    = 'instapaper.com'
USER      = 'mkomitee@gmail.com'
KEYCHAIN  = os.path.expanduser("~/Library/Keychains/login.keychain")
BOOKMARKS = os.path.expanduser("~/Library/Safari/Bookmarks.plist")
STATE     = os.path.expanduser("~/.rl2instapaper")

_last_sync_time = None
_times = []


def bookmark_plist():
    args = ['plutil', '-convert', 'xml1', '-o', '-', BOOKMARKS]
    proc = subprocess.Popen(args, stdout=subprocess.PIPE)
    xml = plistlib.readPlist(proc.stdout)
    return xml


def reading_list():
    for x in bookmark_plist()['Children']:
        if x['Title'] == 'com.apple.ReadingList':
            for child in x['Children']:
                yield(child)


def articles():
    for i in reading_list():
        article = dict()
        article['preview'] = i['ReadingList']['PreviewText'].encode('utf-8')
        article['title'] = i['URIDictionary']['title'].encode('utf-8')
        article['url'] = i['URLString'].encode('utf-8')
        article['date'] = int(i['ReadingList']['DateAdded'].strftime("%s"))
        yield(article)


def last_sync_time():
    global _last_sync_time
    if _last_sync_time is None:
        try:
            with open(STATE) as fh:
                _last_sync_time = int(fh.read().strip())
        except IOError:
            _last_sync_time = 0
    return _last_sync_time


def is_new(article):
    return article['date'] > last_sync_time()


def new_articles():
    for article in articles():
        if is_new(article):
            yield(article)


def sync(article, user, password):
    global _times
    _times.append(article['date'])
    instapaperlib.add_item(user, password, article['url'], article['title'], article['preview'])


def update_state():
    if len(_times) > 0:
        with open(STATE, 'w') as fh:
            print >> fh, sorted(_times, reverse=True)[0]


def get_keychain_pass(account=None, server=None):
    params = {
        'security': '/usr/bin/security',
        'command': 'find-internet-password',
        'account': account,
        'server': server,
        'keychain': KEYCHAIN,
    }
    command = "%(security)s -v %(command)s -g -a %(account)s -l %(server)s %(keychain)s" % params
    output = subprocess.check_output(command, shell=True, stderr=subprocess.STDOUT)
    outtext = [l for l in output.splitlines() if l.startswith('password: ')][0]

    return re.match(r'password: "(.*)"', outtext).group(1)


def main():
    password = get_keychain_pass(USER, SERVER)
    for article in new_articles():
        sync(article, user=USER, password=password)
    update_state()


if __name__ == '__main__':
    main()
