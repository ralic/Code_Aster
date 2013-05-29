# coding=utf-8
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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
# person_in_charge: mathieu.courtois at edf.fr

from Miss.miss_utils import dict_format

# on applique `dict_format` sur tous les templates avant de les utiliser

# LE template du fichier global
def main_template():
    """Template du fichier global"""
    tmpl = """
*
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
%%(bloc_group)s
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
%%(bloc_lfreq)s
*
* Definition des sous-domaines
* ----------------------------
%%(bloc_sous_domaine)s
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
%%(bloc_calcul)s
*
%%(bloc_post)s
*
%%(bloc_fin)s
""" % dict_format
    return tmpl

# sous-templates
# plage des fréquences
def def_freq_min_max():
    """Définition de la plage de fréquences avec min, max et pas"""
    tmpl = "FREQUENCE DE %%(freq_min)%(R)s A %%(freq_max)%(R)s " \
           "PAS %%(freq_pas)%(R)s\n" % dict_format
    return tmpl

def def_freq_range(list_freq):
    """Définition de la plage des fréquences avec une liste"""
    nb = len(list_freq)
    tmpl = "FREQUENCE %%(nb)%(I)s\n" % dict_format + \
             (dict_format['sR'] * nb) % tuple(list_freq) + "\n"
    return tmpl % locals()

def def_freq_imag():
    """Définition de la fréquence complexe"""
    tmpl = "IMGO %%(freq_imag)%(R)s\n" % dict_format
    return tmpl

# sous-domaines
def def_sous_domaine(num_domain, groups, value):
    """Définition d'un sous-domaine"""
    list_groups = ("%4d" * len(groups)) % groups
    tmpl = """
*
* Definition du sous-domaine    %(num_domain)d
*----------------------------
*
SDOMAINE %(num_domain)4d GROUPE %(list_groups)s
%(value)s
FINS
*
""" % locals()
    return tmpl

def use_domaine(num):
    """Déclaration pour un domaine"""
    if num is None:
        return ""
    tmpl = """
DOMAINE    %(num)d
""" % locals()
    return tmpl

def def_chargement_decl(num):
    """Définition d'un chargement"""
    sdomain = use_domaine(num)
    tmpl = """
*
* Chargement du domaine    %(num)d
* ---------------------------
%(sdomain)s
*
""" % locals()
    return tmpl

# fonctions de Green
def def_chargement_green():
    """Définition du chargement des fonctions de Green"""
    tmpl = """
*
* Chargement des fonctions de Green
* ----------------------------------
DOS2M Z0 %%(z0)%(R)s  %%(surf)s
LIRE %%(fich_sol)s
*
""" % dict_format
    return tmpl

# ondes non inclinées
def def_ondes_non_inclinees():
    tmpl = """
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
""" % dict_format
    return tmpl

# ondes inclinées
def def_ondes_inclinees():
    tmpl = """
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
*
""" % dict_format
    return tmpl

# points de contrôle
def def_chargement_pc(num):
    """Définition du chargement pour les pc"""
    tmpl = def_chargement_decl(num) + """*
* Chargement du domaine structure 
* --------------------------------
* Matrice de masse, rigidite et chargement
* ----------------------------------------
*
EXTERIEUR
LIRE %%(fich_ext)s                                                          
FINE 
*
""" % dict_format
    return tmpl

def def_exec_pc():
    """Exécution pour les pc"""
    tmpl = """
*
EXEC CONTROLE UI
*
""" % dict_format
    return tmpl

def def_exec_manuel(what):
    """Exécution particulière"""
    tmpl = """
EXEC %(what)s
*""" % locals()
    return tmpl

def def_calcul_sol():
    """Calcul dans le sol"""
    tmpl = """
*
* Calcul dans le sol
* ------------------
*
* Calcul des fonctions de Green
*
EXEC SPFR
*
""" % dict_format
    return tmpl

def def_calcul_impedances():
    """Calcul des impédances"""
    tmpl = """
*
* Calcul des impedances
*
""" % dict_format
    return tmpl

def def_calcul_global():
    """Calcul global"""
    tmpl = """
*
* Resolution du probleme d interaction 
* -------------------------------------
EXEC GLOBAL
*
"""
    return tmpl % dict_format

# exec
def def_exec(what, mini=False):
    """Calcul des impédances et forces en ISSF"""
    if mini:
        tmpl = """
*
EXEC UGTG %%(champPC)s %(what)s
*
"""
    else:
        tmpl = """
*
EXEC UGTG %%(champPC)s %(what)s %%(rfic1)s %%(rfic2)s %%(rfic2)s
*
"""
    return tmpl % locals()

# post-traitements
def init_post():
    """Début post-traitement"""
    tmpl = """
*
* Post-traitement 
* ----------------
POST
"""
    return tmpl

def fin_post():
    """Début post-traitement"""
    tmpl = """
FINP
*
"""
    return tmpl

def post_impe(num):
    """Post du fichier des impédances"""
    sdomain = use_domaine(num)
    tmpl = """
%(sdomain)s
FICH %%(fich_impe)s  %%(binaire)s
IMPDC
FREQ TOUTES
CHPU TOUS
CHPT TOUS
""" % locals()
    return tmpl

def post_forc(num):
    """Post-traitement du fichier des forces"""
    sdomain = use_domaine(num)
    tmpl = """
%(sdomain)s
FICH %%(fich_forc)s
FORCE
FREQ TOUTES
DDL TOUS
UI TOUS
""" % locals()
    return tmpl

def lire_signal(filename):
    """Lecture du signal de réponse"""
    tmpl = """
SIGNAL LIRE %(filename)s
*
""" % locals()
    return tmpl

def init_boucle():
    """Début de boucle"""
    tmpl = """
*
* Boucle sur les frequences 
* --------------------------
DOFR TOUTES SAVE MVFD TOT UI TUI IMPD FORCE
*
"""
    return tmpl % dict_format

def fin_boucle():
    """Fin de boucle"""
    tmpl = """
* Fin de la boucle sur les frequences 
* ------------------------------------
ENDF
*
"""
    return tmpl % dict_format

def init_spec():
    """Début du bloc SPEC"""
    tmpl = """
SPEC NF=%%(_nbfreq)%(I)s FMAX=%%(freq_max)%(R)s
""" % dict_format
    return tmpl

def post_spec(grandeur, filename):
    """Bloc de post-traitement SPEC"""
    tmpl = """
FICH %(filename)s
CSOL LEGENDE %(grandeur)s
FREQ TOUTES
CHAMP DE    1 A    3  PAS 1
POINTS DE    1 A    %%(nbPC)d  PAS 1
DDL TOUS
""" % locals()
    return tmpl

def fin_spec():
    """Fin du bloc SPEC"""
    tmpl = """
FINS
*
"""
    return tmpl

def def_fin():
    """Fin"""
    tmpl = """
* Fin de l execution
* -------------------
FIN
""" % dict_format
    return tmpl
