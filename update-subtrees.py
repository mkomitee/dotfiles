#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright 2011 Michael Komitee. All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
# this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY MICHAEL KOMITEE ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
# NO EVENT SHALL MICHAEl KOMITEE OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
# INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.
#
# The views and conclusions contained in the software and documentation are
# those of the authors and should not be interpreted as representing official
# policies, either expressed or implied, of Michael Komitee.

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
        return '''Subtree(path='%s', repository='%s')''' % (self.path, self.repository)

    def is_deployed(self):
        return os.path.exists(self.path)

    def deploy(self):
        command = ['git', 'subtree']
        if self.is_deployed():
            command.append('pull')
        else:
            command.append('add')
        with open('/dev/null', 'a') as null:
            with open('/dev/stdout', 'a') as out:
                command += ['--prefix', self.path, '--squash', self.repository, 'master']
                print ' '.join(command)
                try:
                    subprocess.check_call(command, stdin=null, stdout=out, stderr=out)
                except subprocess.CalledProcessError:
                    sys.exit(1)


def git_status():
    with open('/dev/null', 'a') as null:
        command = ['git', 'status']
        try:
            subprocess.check_call(command, stdin=null, stdout=null, stderr=null)
        except subprocess.CalledProcessError:
            sys.stderr.write('Problem checking git status\n')
            sys.exit(1)


def read_subtrees(config_file):
    config = csv.reader(open(config_file), delimiter=',')
    subtrees = [Subtree(line) for line in config if not line[0].startswith('#')]
    return subtrees


if __name__ == '__main__':
    git_status()

    # repositories.csv should be a csv file with two fields per line. The
    # first field is where in your repository (relative path) you want the
    # subtree installed, and the second should be the git repositories url
    # e.g:
    # vim/bundle,git://github.com/vim-scripts/Gist.vim.git

    for subtree in read_subtrees('repositories.csv'):
        subtree.deploy()

# vim: set ft=python ts=4 sw=4 et:
