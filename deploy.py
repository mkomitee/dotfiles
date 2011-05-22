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
from collections import namedtuple
import pprint

class Subtree(object):
    def __init__(self, line):
        data = line.strip().split()
        self.prefix = data[0]
        self.repository = data[1]
        m = re.search('/([^/]*)$', self.repository)
        base = re.sub('.git$', '', m.group(1))
        self.path = '%s/%s' % (self.prefix, base)

    def __repr__(self):
        return('''Subtree(prefix='%s', repository='%s')''' % (self.prefix, self.repository))

    def is_deployed(self):
        return(os.path.exists(self.path))

    def deploy(self):
        if self.is_deployed():
            command = ['git', 'subtree', 'pull', '--prefix', self.path, '--squash', self.repository]
        else:
            command = ['git', 'subtree', 'add', '--prefix', self.path, '--squash', self.repository]
        pprint.pprint(command)


def read_subtrees(config_file):
    f = open(config_file)
    subtrees = [Subtree(l.strip()) for l in f.readlines()]
    f.close()
    return subtrees


if __name__ == '__main__':
    for subtree in read_subtrees('repositories'):
        subtree.deploy()

# vim: set ft=python ts=4 sw=4 et:

