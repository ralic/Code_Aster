#@ MODIF make_capy_offi Lecture_Cata_Ele  DATE 14/09/2004   AUTEUR MCOURTOI M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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


####################################################################################################
# script à appeler à la fin de la procédure majnew
#      apres la mise à jour de vers/catalo/*/*.cata
#
# ce script fabrique une version "capy" des catalogues d'éléments officiels
# pour que la procédure ccat92 de surcharge des catalogues soit plus rapide en CPU
####################################################################################################
usage= '''
   usage :
     python   make_capy_offi.py  rep_scripts  rep_trav  rep_cata_offi  nom_capy_offi

     rep_scripts  : nom du répertoire principal où se trouvent les scripts python d'aster (répertoire Eficas en général)

     rep_trav     : répertoire de travail  : ce répertoire NE DOIT PAS exister au préalable

     rep_cata_offi: nom du répertoire ou se trouvent les sources .cata officiels : VERS/catalo
                    le script connait l'arborescence de catalo :  /typelem /options /compelem

     nom_capy_offi: nom du fichier offi.capy produit : /VERS/catobj/elem.capy
'''
####################################################################################################

import sys  ,  os  , glob

if len(sys.argv) !=5 : print usage ; raise StandardError


# rep_scripts :
#--------------
scripts=os.path.abspath(sys.argv[1])
if os.path.isdir(scripts) :
    sys.path[:0]=[scripts]
else:
    print scripts+" doit etre le répertoire principal des sources *.py (Eficas en général)" ; raise StandardError


# rep_cata_offi :
#----------------
cata_offi=os.path.abspath(sys.argv[3])
if not os.path.isdir(cata_offi) :
    print cata_offi+" doit etre un répertoire contenant les catalogues *.cata" ; raise StandardError

# nom_capy_offi :
#----------------
capy_offi=os.path.abspath(sys.argv[4])
capy=open(capy_offi,"w")
capy.close()

# rep_trav :
#--------------
trav=os.path.abspath(sys.argv[2])
dirav=os.getcwd()
if os.path.isdir(trav) :
    print trav+" ne doit pas exister."; raise StandardError
else:
    os.mkdir(trav)
    os.chdir(trav)


# concaténation des catalogues :
#-------------------------------
toucata=open('tou.cata','w')
for soucat in ["compelem","options","typelem"] :
    lisfic= glob.glob(os.path.join(cata_offi,soucat,"*.cata"))
    for nomfic in lisfic:
        cata= open(nomfic,'r')
        texte=cata.read()
        toucata.write(texte)
        cata.close()
toucata.close()


# lecture des catalogues :
#-------------------------------
from Lecture_Cata_Ele import utilit
from Lecture_Cata_Ele import lecture

# pour ne pas utiliser trop de mémoire, on splite le fichier pour la lecture :
liste_morceaux=utilit.cata_split('tou.cata',"morceau",5000)

capy=lecture.lire_cata(liste_morceaux[0])
for k in range(len(liste_morceaux)-1) :
   capy2 =lecture.lire_cata(liste_morceaux[k+1])
   utilit.concat_capy(capy,capy2)

utilit.write_capy(capy,capy_offi)


# ménage :
#------------------------
os.chdir(dirav)
import shutil
shutil.rmtree(trav)

print "(I/U) creation du fichier cata_ele.pickled terminee."


