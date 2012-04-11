#@ MODIF make_surch_offi Lecture_Cata_Ele  DATE 11/04/2012   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
script pour surcharger les catalogues officiels

ce script fabrique un fichier .ojb  contenant l'info présente dans l'ENSEMBLE
des catalogues (+surcharge)
"""

import sys
import os
import os.path as osp
import traceback
import tempfile
import shutil
import optparse

def parse_args(argv=None):
    'Parse the command line and return surch, unigest, nom_capy_offi, resu_ojb'

    usage = ('This script build an .ojb file that contains the overall '
             'information included in all given catalogs')
    parser = optparse.OptionParser(usage=usage)
    parser.add_option('--bibpyt', dest='rep_scripts', metavar='FOLDER',
                      help='path to Code_Aster python source files')
    parser.add_option('-s', '--surch', dest='surch', metavar='FILE',
                      help='big file with overloaded element catalogs')
    parser.add_option('-u', '--unigest', dest='unigest', metavar='FILE',
                      help='unigest file (only CATSUPPR lines are used)')
    parser.add_option('-i', '--input', dest='nom_capy_offi', metavar='FILE',
                      help='pickled catalog file that shall be overwritten')
    parser.add_option('-o', '--output', dest='resu_ojb', metavar='FILE',
                      help='output object file')

    opts, args = parser.parse_args(argv)
    if len(args) == 6 and opts.nom_capy_offi is None: # legacy style
        opts.rep_scripts = args[0]
        opts.surch = args[2]
        opts.unigest = args[3]
        opts.nom_capy_offi = args[4]
        opts.resu_ojb = args[5]
    if opts.rep_scripts is not None:
        sys.path.insert(0, osp.abspath(opts.rep_scripts))
    if opts.nom_capy_offi is None or not osp.isfile(opts.nom_capy_offi):
        parser.error('You must provide the input (--help for help)')

    return opts.surch, opts.unigest, opts.nom_capy_offi, opts.resu_ojb

###############################################################################
def main(surch, unigest, nom_capy_offi, resu_ojb):
    """
    Script pour surcharger les catalogues officiels
    (Il travaille dans un sandbox)
    """
    nom_capy_offi = osp.abspath(nom_capy_offi)
    resu_ojb = osp.abspath(resu_ojb)

    trav = tempfile.mkdtemp(prefix='make_surch_offi_')
    dirav = os.getcwd()
    os.chdir(trav)
    try:
        _main(surch, unigest, nom_capy_offi, resu_ojb)
    except:
        print 60*'-'+' debut trace back'
        traceback.print_exc(file=sys.stdout)
        print 60*'-'+' fin   trace back'
        raise
    finally:
        os.chdir(dirav)
        shutil.rmtree(trav)

def _main(surch, unigest, nom_capy_offi, resu_ojb):
    """Script pour surcharger les catalogues officiels"""
    from Lecture_Cata_Ele.lecture import lire_cata
    from Lecture_Cata_Ele.imprime import impr_cata
    import Lecture_Cata_Ele.utilit as utilit

    capy_surch = None
    if surch and osp.isfile(osp.abspath(surch)):
        # pour ne pas utiliser trop de mémoire, on decoupe
        # le fichier pour la lecture :
        liste_morceaux = utilit.cata_split(osp.abspath(surch), "morceau", 5000)
        capy_surch = lire_cata(liste_morceaux[0])
        for k in range(len(liste_morceaux) - 1) :
            capy_surc2 = lire_cata(liste_morceaux[k + 1])
            utilit.concat_capy(capy_surch, capy_surc2)

    capy_offi = utilit.read_capy(nom_capy_offi)

    # prise en compte des destructions demandées via unigest :
    utilit.detruire_cata(capy_offi, unigest)

    utilit.surch_capy(capy_offi, capy_surch)

    impr_cata(capy_offi, resu_ojb, 'ojb')


if __name__ == '__main__':
    main(*parse_args())
