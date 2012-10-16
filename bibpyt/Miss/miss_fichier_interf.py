#@ MODIF miss_fichier_interf Miss  DATE 16/10/2012   AUTEUR DEVESA G.DEVESA 
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
    if len(struct.maille_connec) != 0:
      cont.extend(en_ligne(struct.maille_connec, dict_format['sI'], 20,
                           format_ligne="%(valeurs)s     GR    1"))
    else:
      cont.extend(en_ligne(struct.maille_connec1, dict_format['sI'], 20,
                           format_ligne="%(valeurs)s     GR    1"))
      cont.extend(en_ligne(struct.maille_connec2, dict_format['sI'], 20,
                           format_ligne="%(valeurs)s     GR    2"))
      cont.extend(en_ligne(struct.maille_connec3, dict_format['sI'], 20,
                           format_ligne="%(valeurs)s     GR    3"))
      cont.extend(en_ligne(struct.maille_connec4, dict_format['sI'], 20,
                           format_ligne="%(valeurs)s     GR    4"))
    cont.append("")
    return os.linesep.join(cont)


#XXX voir pour les groupes
def fichier_chp(param,struct):
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
    if param["ISSF"] == "OUI":
       cont.append("GROUPE    3")
       if param['ALLU'] != 0.:
         refl = param['ALLU']
         kimp = refl/(2.-refl)
         simp=str(kimp)
         print 'simp ',simp
         cont.append("FLUI "+ simp + " BEM")
       else: 
         cont.append("DEPN  BEM") 
       cont.append("GROUPE    4")    
       cont.append("LIBRE")    
    cont.append("FINC")
    cont.append("EOF")
    return os.linesep.join(cont)


def fichier_cmde(param, struct, *nom_fichier):
    """Produit le fichier de commandes Miss (in).
    """
    print 'PARAM ',param
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
        "freq_list" : param['LIST_FREQ'],
        "freq_imag" : param['FREQ_IMAG'],
        "freq_nb" : "",
        "binaire"  : "",
        "z0" : param["Z0"],
        "surf"  : "",
        "issf"  : "",
        "rfic1"  : "",
        "rfic2"  : "",
    }
    if param["TYPE"] == "BINAIRE":
        dict_info["binaire"] = "BINA"
    if param["SURF"] == "OUI":
        dict_info["surf"] = "SURF"
    if param["ISSF"] == "OUI":
        dict_info["issf"] = "ISSF"
    if param['RFIC'] != 0.:
        dict_info["rfic1"] = "RFIC"
        dict_info["rfic2"] = str(param['RFIC'])
    # deux formats possibles pour les fréquences
    if param["FREQ_MIN"] and not param["LIST_FREQ"]:
        itmpl = "FREQUENCE DE %%(freq_min)%(R)s A %%(freq_max)%(R)s " \
                "PAS %%(freq_pas)%(R)s" % dict_format
        dict_info['_lfreq'] = itmpl % dict_info
    else:
        dict_info['freq_nb'] = len(param['LIST_FREQ'])
        itmpl = "FREQUENCE %%(freq_nb)%(I)s\n" % dict_format + \
                (dict_format['sR'] * dict_info['freq_nb']) % dict_info['freq_list']
        dict_info['_lfreq'] = itmpl % dict_info
    if param['FREQ_IMAG']:
        itmpl = "IMGO %%(freq_imag)%(R)s" % dict_format
        dict_info['_fimg'] = itmpl % dict_info
        #itmpl = template_ondes_inclinees_in % dict_format #Ne pas oublier : changer fichier_sol aussi
        itmpl = template_ondes_non_inclinees_in % dict_format
        dict_info['_fimg2'] = itmpl % dict_info
        itmpl = template_impe_seule_in % dict_format
        dict_info['_fimg3'] = itmpl % dict_info
        content = template_miss_in % dict_info
    else:
        if param["ISSF"] == "OUI":
          itmpl = "*" % dict_format
          dict_info['_fimg'] = itmpl % dict_info
          itmpl = template_ondes_non_inclinees_in % dict_format
          dict_info['_fimg2'] = itmpl % dict_info
          itmpl = template_impe_forc_issf_in % dict_format
          dict_info['_fimg3'] = itmpl % dict_info
          content = template_miss_issf_in % dict_info
        else:
          itmpl = "*" % dict_format
          dict_info['_fimg'] = itmpl % dict_info
          itmpl = template_ondes_non_inclinees_in % dict_format
          dict_info['_fimg2'] = itmpl % dict_info
          itmpl = template_impe_forc_in % dict_format
          dict_info['_fimg3'] = itmpl % dict_info
          content = template_miss_in % dict_info
    return content
                

def fichier_cmde_inci(param, struct, *nom_fichier):
    """Produit le fichier de commandes Miss (ini).
       Calcul du champ incident pour la methode Laplace-temps.
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
        "freq_nb" : "",
        "z0" : param["Z0"],
        "surf"  : "",
        "rfic1"  : "",
        "rfic2"  : "",
    }
    if param["SURF"] == "OUI":
        dict_info["surf"] = "SURF"
    # deux formats possibles pour les fréquences
    N = int(param['INST_FIN']/param['PAS_INST'])
    Fs = 1./param['PAS_INST']
    param['FREQ_MAX'] = Fs
    param['FREQ_MIN'] = Fs/N
    param['FREQ_PAS'] = Fs/N
    dict_info['freq_max'] = param['FREQ_MAX']
    dict_info['freq_pas'] = param['FREQ_PAS']
    dict_info['freq_min'] = param['FREQ_MIN']
    itmpl = "FREQUENCE DE %%(freq_min)%(R)s A %%(freq_max)%(R)s " \
                "PAS %%(freq_pas)%(R)s" % dict_format
    dict_info['_lfreq'] = itmpl % dict_info
    dict_info['freq_nb'] = dict_info['freq_max']/dict_info['freq_pas']
    #itmpl= template_ondes_inclinees_in % dict_format  #Ne pas oublier : changer fichier_sol aussi
    itmpl = template_ondes_non_inclinees_in % dict_format
    dict_info['_fimg2'] = itmpl % dict_info
    content = template_miss_in2 % dict_info
    return content


template_ondes_non_inclinees_in = """*
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
*"""

template_ondes_inclinees_in = """*
*
* Definition des champs incidents 
* -------------------------------- 
INCI 3 
PLANE SV 1. 
0. 0. 1.
PLANE SH 1. 
0. 0. 1.
PLANE P 1.  
0. 0. 1. 
* 
* Calcul des champs incidents
* --------------------------------
EXEC SPEC
*
EXEC INCI
*"""

template_impe_forc_in = """EXEC UGTG IMPEDANCE FORCE %%(rfic1)s %%(rfic2)s %%(rfic2)s
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
FINP"""

template_impe_forc_issf_in = """EXEC UGTG IMPEDANCE FORCE %%(rfic1)s %%(rfic2)s %%(rfic2)s
*
*
* Chargement du domaine    2
* ---------------------------
DOMAINE    2
*
* Calcul des impedances et des force induites
*
EXEC UGTG IMPEDANCE FORCE                      
*
* Resolution du probleme d interaction 
* -------------------------------------
EXEC GLOBAL
*
* Fin de la boucle sur les frequences 
* ------------------------------------
ENDF
*
* Post-traitement 
* ----------------
*
DOMAINE 0
POST
FICH %%(fich_impe)s  %%(binaire)s
IMPDC
FREQ TOUTES
CHPU TOUS
CHPT TOUS
FINP
*
* Post-traitement 
* ----------------
*
DOMAINE 0
POST
FICH %%(fich_forc)s
FORCE
FREQ TOUTES
TUDM TOUS
UI TOUS
FINP"""

template_impe_seule_in = """EXEC UGTG IMPEDANCE %%(rfic1)s %%(rfic2)s %%(rfic2)s
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
FINP"""

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
%%(_lfreq)s
%%(_fimg)s
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
%%(_fimg2)s
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
%%(_fimg3)s
*
* Fin de l execution
* -------------------
FIN
""" % dict_format

template_miss_issf_in = """*
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
%%(_lfreq)s
%%(_fimg)s
*
* Definition du sous-domaine    1
*----------------------------
*
SDOMAINE    1 GROUPE    -1  3  4
STRAtifie
FINS
*
* Definition du sous-domaine    2
*----------------------------
*
SDOMAINE    2 GROUPE   -2  -3
DFLUI RO 1000 CELER 1500 SURF 0.                                                
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
* Chargement du domaine    1
* ---------------------------
DOMAINE    1
*
* Chargement des fonctions de Green
* ----------------------------------
DOS2M Z0 %%(z0)%(R)s  %%(surf)s
LIRE %%(fich_sol)s
%%(_fimg2)s
*
* Chargement du domaine    2
* ---------------------------
DOMAINE    2
*
* Boucle sur les frequences 
* --------------------------
DOFR TOUTES SAVE MVFD TOT UI TUI IMPD FORCE
*
*
*
* Chargement du domaine    1
* ---------------------------
DOMAINE    1
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
%%(_fimg3)s
*
* Fin de l execution
* -------------------
FIN
""" % dict_format

template_miss_in2 = """*
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
%%(_lfreq)s
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
%%(_fimg2)s
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
EXEC UGTG FORCE %%(rfic1)s %%(rfic2)s %%(rfic2)s
*
*
* Post-traitement
* ----------------
POST
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


