#!/usr/bin/python
import re, subprocess
def get_keychain_pass(account=None, server=None):
    params = {
        'security': '/usr/bin/security',
        'command': 'find-internet-password',
        'account': account,
        'server': server,
        'keychain': '/Users/mkomitee/Library/Keychains/login.keychain',
    }
    command = "sudo -u mkomitee %(security)s -v %(command)s -g -a %(account)s -s %(server)s %(keychain)s" % params
    output = subprocess.check_output(command, shell=True, stderr=subprocess.STDOUT)
    outtext = [l for l in output.splitlines()
               if l.startswith('password: ')][0]

    return re.match(r'password: "(.*)"', outtext).group(1)

if __name__ == '__main__':
    print get_keychain_pass('mkomitee@gmail.com', 'imap.gmail.com')
    print get_keychain_pass('mkomitee@gmail.com', 'smtp.gmail.com')
