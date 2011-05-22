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

class Subtree(object):
    def __init__(self, line):
        data = line.strip().split()
        self.prefix = data[0]
        self.repository = data[1]
        m = re.search('/([^/]*)$', self.repository)
        base = re.sub('.git$', '', m.group(1))
        self.path = '%s/%s' % (self.prefix, base)

    def __repr__(self):
        return('''Subtree(prefix='%s', repository='%s')''' % (self.prefix, 
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


def read_subtrees(config_file):
    f = open(config_file)
    subtrees = [Subtree(l.strip()) for l in f.readlines()]
    f.close()
    return subtrees


if __name__ == '__main__':
    for subtree in read_subtrees('repositories'):
        subtree.deploy()

# vim: set ft=python ts=4 sw=4 et:

