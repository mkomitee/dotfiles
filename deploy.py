#!/usr/bin/python -tt
#
#   Last edited by:  $Author$
#               on:  $Date$
#         Source  :  $Source$
#         Revision:  $Revision$
#
#               ID:  $Id$

import sys
import os
import re
import subprocess
import csv

class Subtree(object):
    def __init__(self, data):
        self.prefix = data[0]
        self.repository = data[1]
        m = re.search('/([^/]*)$', self.repository)
        base = re.sub('.git$', '', m.group(1))
        if self.prefix == '':
            self.path = base
        else:
            self.path = '%s/%s' % (self.prefix, base)

    def __repr__(self):
        return('''Subtree(path='%s', repository='%s')''' % (self.path, 
                                                              self.repository))

    def is_deployed(self):
        return(os.path.exists(self.path))

    def deploy(self):
        command = ['git', 'subtree']
        if self.is_deployed():
            command.append('pull')
        else:
            command.append('add')
        null = open('/dev/null', 'a')
        out  = open('/dev/stdout', 'a')
        err  = open('/dev/stdout', 'a')
        command += ['--prefix', self.path, '--squash', self.repository, 'master']
        print(' '.join(command))
        try:
            subprocess.check_call(command, stdin=null, stdout=out, stderr=err)
        except subprocess.CalledProcessError:
            sys.exit(1)
        out.close()
        err.close()
        null.close()


def git_status():
    null = open('/dev/null', 'a')
    command = ['git', 'status']
    try:
        subprocess.check_call(command, stdin=null, stdout=null, stderr=null)
    except subprocess.CalledProcessError:
        sys.stderr.write("Problem checking git status\n")
        sys.exit(1)
    null.close()


def read_subtrees(config_file):
    config = csv.reader(open(config_file), delimiter=',')
    subtrees = [Subtree(line) for line in config]
    return subtrees


if __name__ == '__main__':
    git_status()
    for subtree in read_subtrees('repositories.csv'):
        subtree.deploy()

# vim: set ft=python ts=4 sw=4 et:

