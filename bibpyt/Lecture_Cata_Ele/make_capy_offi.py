# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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


'''
script à appeler à la fin de la procédure majnew
     apres la mise à jour de vers/catalo/*/*.py

ce script fabrique une version "capy" des catalogues d'éléments officiels
pour que la procédure ccat92 de surcharge des catalogues soit plus rapide en
CPU
'''

import sys
import os
import os.path as osp
import glob
import traceback
import tempfile
import shutil
import optparse


def parse_args(argv=None):
    '''Parse the command line and return (rep_cata_offi, nom_capy_offi)'''
    usage = ('This script build a pickled version of the elements catalog '
             'that speed up the ccat92 procedure that overwrite the catalog')
    parser = optparse.OptionParser(usage=usage)
    parser.add_option('--bibpyt', dest='rep_scripts', metavar='FOLDER',
                      help="path to Code_Aster python source files")
    parser.add_option('-i', '--input', dest='rep_cata_offi', metavar='FOLDER',
                      help='path to .py files')
    parser.add_option('-o', '--output', dest='nom_capy_offi', metavar='FILE',
                      default='cata_ele.pickled',
                      help='file path use as output (default: %default)')

    opts, args = parser.parse_args(argv)
    if len(args) == 4 and opts.rep_cata_offi is None:  # legacy style
        opts.rep_scripts = args[0]
        opts.rep_cata_offi = args[2]
        opts.nom_capy_offi = args[3]
    if opts.rep_scripts is not None:
        sys.path.insert(0, osp.abspath(opts.rep_scripts))
    if opts.rep_cata_offi is None or not osp.isdir(opts.rep_cata_offi):
        parser.error(
            'You must provide the top folder that contains elements catalog'
            ' declaration files. Subfolder will be deduced '
            '(compelem/  options/  typele/)')

    return opts.rep_cata_offi, opts.nom_capy_offi

#


def main(rep_cata_offi, nom_capy_offi):
    """
    Script pour construire le catalogue officiel
    (Il travaille dans un sandbox)
    """
    nom_capy_offi = osp.abspath(nom_capy_offi)
    rep_cata_offi = osp.abspath(rep_cata_offi)

    trav = tempfile.mkdtemp(prefix='make_capy_offi_')
    dirav = os.getcwd()
    os.chdir(trav)
    try:
        _main(rep_cata_offi, nom_capy_offi)
    except:
        print 60 * '-' + ' debut trace back'
        traceback.print_exc(file=sys.stdout)
        print 60 * '-' + ' fin   trace back'
        raise
    finally:
        os.chdir(dirav)
        shutil.rmtree(trav)
    print "(I/U) creation du fichier '%s' terminee." % nom_capy_offi


def _main(rep_cata_offi, nom_capy_offi):
    """Script pour construire le catalogue officiel cata_ele.picked"""
    print "AJACOT rep_cata_offi=",rep_cata_offi
    print "AJACOT nom_capy_offi=",nom_capy_offi
    import sys
    print "ajacot sys.argv=",sys.argv
    print "ajacot sys.path 1=",sys.path
    dir_src=osp.dirname(sys.path[1]) # AJACOT NON !!!
    print "ajacot dir_src=",dir_src
    sys.path.insert(0,dir_src)
    print "ajacot sys.path 2=",sys.path

    import catalo
    import catalo.Tools.imprime as imprime

    imprime.impr_cata(catalo.cel, nom_capy_offi)


if __name__ == '__main__':
    main(*parse_args())
