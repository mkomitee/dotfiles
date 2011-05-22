#!/usr/local/bin/python -tt
#
#   Last edited by:  $Author$
#               on:  $Date$
#         Source  :  $Source$
#         Revision:  $Revision$
#
#               ID:  $Id$

import sys
import os

from optparse import OptionParser

__version__ = "$Revision: 0 $"[11:-2]

def main():
    """Main Program Logic"""
    options = parse_options()


def parse_options():
    """Process Commandline Arguments"""
    p = OptionParser()
    p.add_option('-v', '--version', action='store_true', dest='version',
            default=False, help='print version')
    (options, args) = p.parse_args()
    if options.version:
        version()
    if len(args) > 0:
        p.print_help()
        sys.exit(1)
    return options


def version():
    """Display current version and exit"""
    print "%s version: %s" % (os.path.basename(__file__), __version__ )
    sys.exit(0)


if __name__ == '__main__':
    main()

# vim: set ft=python ts=4 sw=4 et:
