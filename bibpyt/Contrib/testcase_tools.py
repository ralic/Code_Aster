# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
# THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY  
# IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY  
# THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR     
# (AT YOUR OPTION) ANY LATER VERSION.                                                  
#                                                                       
# THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT   
# WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF            
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU      
# GENERAL PUBLIC LICENSE FOR MORE DETAILS.                              
#                                                                       
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE     
# ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,         
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.        
# ======================================================================

"""
Convenient tools for the testcases
"""

import sys
import os
import os.path as osp
import re
from glob import glob


DELIMITER = '---delimiter---'

def testcase_post():
    """Post-run script"""
    change_test_resu()

def change_test_resu():
    """Fill the TEST_xxxx/VALE_CALC of the .comm file with the computed value"""
    print 'try to add new values in the .comm file...'
    howto = os.linesep.join(['', 'HOWTO:', '',
        'To extract automatically the new command files, use:',
        '',
        '   python bibpyt/Contrib/testcase_tools.py extract RESDIR NEWDIR "*.mess"',
        '',
        'Replace RESDIR by the directory containing the .mess files',
        'and NEWDIR by a directory in which the new comm files will be written',
        '("*.mess" may be omitted).'
        '', ''])
    reval = re.compile('^ *(OK|NOOK|SKIP) +NON_REGRESSION +(?P<leg>.+?) +'
        '(?P<refe>.+?) +(?P<calc>.+?) +(?P<err>.+?) +(?P<tole>.+?) *$', re.M)
    fort8 = open('fort.8', 'rb').read()
    results = reval.findall(fort8)
    fort1 = open('fort.1', 'rb').read()
    keywords = read_keyword_value('VALE_CALC(|_.)', fort1)
    for i, val in enumerate(results):
        print i, val
    for i, kw in enumerate(keywords):
        print i, kw
    assert len(results) == len(keywords)

    changed = fort1
    while len(results) > 0:
        dico = keywords.pop()
        start, end = dico['start'], dico['end']
        res = results.pop()
        newval = res[3]
        if newval == '-':   # null value skipped
            newval = '0.'
        changed = changed[:start] + dico['key'] + '=' + newval + changed[end:]
    append_to_file('fort.6', howto, stdout=True)
    append_to_file('fort.6', changed, delimiter=DELIMITER, stdout=True)

def append_to_file(fname, txt, delimiter=None, stdout=None):
    """Append a text at the end of a file"""
    if delimiter:
        txt = os.linesep.join([delimiter, txt, delimiter])
    open(fname, 'ab').write(txt)
    if stdout:
        print txt

def read_keyword_value(kw, txt):
    """Read all values of a keyword
    Return a list of positions and a list of couples (keyword, value)."""
    re_vale = re.compile('(?P<key>%s) *= *(?P<val>[^,]+)' % kw, re.M)
    found = []
    for mat in re_vale.finditer(txt):
        found.append({
            'start' : mat.start(),
            'end' : mat.end(),
            'key' : mat.group('key'),
            'val' : mat.group('val'),
        })
    return found

_re_comm = re.compile('F +comm +(.*) +D', flags=re.M)
def get_dest_filename(fname, nb):
    """Return 'nb' destination filenames
    Try to use filename found in .export"""
    # search the export files locally (./astest and ../validation/astest)
    if nb < 1:
        return []
    dname, root = osp.split(osp.splitext(fname)[0])
    lexp = glob(osp.join('astest', root + '.export')) \
         + glob(osp.join('../validation/astest', root + '.export'))
    if lexp:
        export = open(lexp[0], 'rb').read()
        lres = _re_comm.findall(export)
        i = len(lres) + 100
    else:
        lres = [root + '.comm']
        i = 1
    while len(lres) < nb:
        lres.append(root + '.com%d' % i)
        i += 1
    assert len(lres) == nb, lres
    return lres

# helper functions run manually
def extract_from(from_dir, to_dir, pattern='*.mess'):
    """Extract content from files matching 'pattern' in 'from_dir'
    and write the extracted text into 'to_dir'.
    
    Example:
    python bibpyt/Contrib/testcase_tools.py extract /path/to/resutest /path/to/changed '*.mess'
    """
    print 'searching', osp.join(from_dir, pattern), '...',
    lfiles = glob(osp.join(from_dir, pattern))
    print '%d found' % len(lfiles)
    if not osp.exists(to_dir):
        os.makedirs(to_dir)
    for fname in lfiles:
        txt = open(fname, 'rb').read()
        parts = txt.split(DELIMITER)
        if len(parts) % 2 != 1:
            print '%s: expected an odd number of delimiters' % fname
            continue
        nbfile = (len(parts) - 1) / 2
        lres = get_dest_filename(fname, nbfile)
        for i in range(nbfile):
            resname = osp.join(to_dir, lres.pop(0))
            if osp.isfile(resname):
                resname += '.' + osp.basename(fname)
            print 'write', resname
            content = parts[2 * i + 1].strip() + os.linesep
            open(resname, 'wb').write(content)

if __name__ == '__main__':
    args = sys.argv[1:]
    try:
        assert len(args) >= 1, 'usage: testcase_tools.py action [args]'
        if args[0] == 'extract':
            assert 2 <= len(args[1:]) <= 3, 'invalid arguments for "testcase_tools.py extract"'
            extract_from(*args[1:])
        else:
            assert False, 'unsupported action: %s' % args[0]
    except AssertionError, exc:
        print str(exc)
        sys.exit(1)
