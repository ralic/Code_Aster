#@ MODIF miss_fichier_interf Miss  DATE 07/02/2011   AUTEUR DEVESA G.DEVESA 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
# RESPONSABLE COURTOIS M.COURTOIS

"""Module permettant de produire les fichiers :
    - de données de Miss (.in),
    - de maillage de l'interface (.mvol),
    - des modes d'interface (.chp).
"""

import os

from Miss.miss_utils import dict_format, en_ligne


#XXX voir pour les groupes
def fichier_mvol(struct):
    """Produit le contenu du fichier de maillage mvol.
    """
    cont = ["COUPLAGE MISS ASTER",]
    cont.extend(en_ligne([struct.noeud_nb, struct.maille_nb], dict_format['sI'], 2, ""))
    fmtR_fort = "3E%s" % (dict_format['R'].replace("E", ""))
    cont.append("(%s)" % fmtR_fort)
    cont.extend(en_ligne(struct.noeud_coor, dict_format['sR'], 3, ""))
    cont.extend(en_ligne(struct.maille_connec, dict_format['sI'], 20,
                         format_ligne="%(valeurs)s     GR    1"))
    cont.append("")
    return os.linesep.join(cont)


#XXX voir pour les groupes
def fichier_chp(struct):
    """Produit le contenu du fichier chp.
    """
    cont = ["GROUPE    1   2",]
    cont.append(("MODE   " + dict_format["sI"]) % struct.mode_stat_nb)
    mult = struct.noeud_nb * 3
    for i in range(struct.mode_stat_nb):
        cont.extend(en_ligne(struct.mode_stat_vale[i*mult:(i+1)*mult], dict_format['sR'], 3,
                             format_ligne="%(index_1)6d%(valeurs)s"))
        cont.append("FIN")
    cont.append("GROUPE    2")
    cont.append(("MODE   " + dict_format["sI"]) % struct.mode_dyna_nb)
    mult = struct.noeud_nb * 3
    for i in range(struct.mode_dyna_nb):
        cont.extend(en_ligne(struct.mode_dyna_vale[i*mult:(i+1)*mult], dict_format['sR'], 3,
                             format_ligne="%(index_1)6d%(valeurs)s"))
        cont.append("FIN")
    cont.append("FINC")
    cont.append("EOF")
    return os.linesep.join(cont)


def fichier_cmde(param, struct, *nom_fichier):
    """Produit le fichier de commandes Miss (in).
    """
    dict_info = {
        "projet" : param["PROJET"],
        "titre"  : struct.titre,
        "fich_mvol" : nom_fichier[0],
        "fich_chp" : nom_fichier[1],
        "fich_sol" : nom_fichier[2],
        "fich_impe" : nom_fichier[3],
        "fich_forc" : nom_fichier[4],
        "freq_min" : param["FREQ_MIN"],
        "freq_max" : param["FREQ_MAX"],
        "freq_pas" : param["FREQ_PAS"],
        "binaire"  : "",
        "z0" : param["Z0"],
        "surf"  : "",
        "rfic1"  : "",
        "rfic2"  : "",
    }
    if param["TYPE"] == "BINAIRE":
        dict_info["binaire"] = "BINA"
    if param["SURF"] == "OUI":
        dict_info["surf"] = "SURF"
    if param['RFIC'] != 0.:
        dict_info["rfic1"] = "RFIC"
        dict_info["rfic2"] = str(param['RFIC'])
    content = template_miss_in % dict_info
    return content


template_miss_in = """*
* Nom generique des fichiers MISS
* --------------------------------
GENER %%(projet)s
*
* Debut du menu DATA
* ------------------
DATA
*
* Titre de l etude
*-----------------
TITRE
%%(titre)s
*
* Lecture du maillage
*--------------------
MVOL %%(fich_mvol)s
*
* Definition du groupe lie a la structure
*----------------------------------------
GROUP
    2 VOLUME
FIN
FING
*
* Definition des modes
*---------------------
CHAMP
LIRE %%(fich_chp)s
*
* Parametres d integration
*-------------------------
*
INTEGRATION RECT 6 8 TRIANGLE 12 12
*
* Plage de frequence MISS
*-------------------------
*
FREQUENCE DE %%(freq_min)%(R)s A %%(freq_max)%(R)s PAS %%(freq_pas)%(R)s
*
* Definition du sous-domaine    1
*----------------------------
*
SDOMAINE    1 GROUPE    1   2
KCM
FINS
*
* Definition du sous-domaine    2
*----------------------------
*
SDOMAINE    2 GROUPE   -1
STRAtifie
FINS
*
* Fin du menu DATA
*-----------------
*
FIND
********************************************************************************
*
* Debut de l execution
*---------------------
*
*
* Chargement du domaine    2
* ---------------------------
DOMAINE    2
*
* Chargement des fonctions de Green
* ----------------------------------
DOS2M Z0 %%(z0)%(R)s  %%(surf)s
LIRE %%(fich_sol)s
*
* Definition des champs incidents
* --------------------------------
INCI 3
DPLANE SV 1. Z0 %%(z0)%(R)s
0. 0. 1.
DPLANE SH 1. Z0 %%(z0)%(R)s
0. 0. 1.
DPLANE P 1. Z0 %%(z0)%(R)s
0. 0. 1.
*
* Calcul des champs incidents
* --------------------------------
EXEC INCI
*
*
* Chargement du domaine    2
* ---------------------------
DOMAINE    2
*
* Chargement des fonctions de Green
* ----------------------------------
DOS2M Z0 %%(z0)%(R)s  %%(surf)s
LIRE %%(fich_sol)s
*
* Calcul dans le sol
* -------------------
* Calcul des fonctions de Green
*
EXEC SPFR
*
* Calcul des impedances
*
EXEC UGTG IMPEDANCE FORCE %%(rfic1)s %%(rfic2)s %%(rfic2)s 
*
*
* Post-traitement
* ----------------
POST
FICH %%(fich_impe)s  %%(binaire)s
IMPDC
FREQ TOUTES
CHPU TOUS
CHPT TOUS
FICH %%(fich_forc)s
FORCE
FREQ TOUTES
DDL TOUS
UI TOUS
FINP
*
* Fin de l execution
* -------------------
FIN
""" % dict_format



