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
# person_in_charge: serguei.potapov at edf.fr

"""
Fonctions utilitaire pour CALC_EUROPLEXUS
"""

from Utilitai.Utmess import UTMESS
import string
import numpy
#----------------------------- Precision -------------------------------
tst = 1.0E-10
#-----------------------------------------------------------------------


def float2str(x_float):
    """
        Imprime un float avec 6 chiffres significatifs dans le fichier EPX
    """
    return "%.6e" % x_float
#-----------------------------------------------------------------------


def norme(vect):
    """
        Calcul la norme euclidienne d'un vecteur.
    """
    return numpy.sqrt(numpy.dot(vect, vect))
#-----------------------------------------------------------------------


def vecteurs_egaux(vect1, vect2):
    """
        Renseigne si deux vecteurs sont égaux à la précision 'tst' près
        au sens de la norme infinie.
    """
    diff = vect1 - vect2
    for comp in diff:
        if abs(comp) > tst:
            return False
    return True
#-----------------------------------------------------------------------


def tolist(obj):
    """
        Transforme l'objet obj en un objet de type list sauf si obj est None
    """
    if type(obj) == list or obj is None:
        pass
    elif type(obj) == tuple or type(obj) == set:
        obj = list(obj)
    else:
        obj = [obj]
    return obj
#-----------------------------------------------------------------------


def recupere_structure(concept, mot_cle=None):
    """
        Renvoie les mots-clés de mise en données à partir desquels concept
        a été créé.
        Si mot_cle est renseigné, on renvoie seulement les informations de
        ce mot-clé.
    """
    try:
        structure = concept.etape.valeur
    except:
        UTMESS('F', 'PLEXUS_1')
    if mot_cle:
        if structure.has_key(mot_cle):
            return structure[mot_cle]
        else:
            return None
    return structure
#-----------------------------------------------------------------------


def get_motcle(fact, mot_cle, code_mess='F'):
    """
        Renvoie la valeur associée à mot_cle dans l'objet fact.
    """
    if code_mess != 'F' and code_mess != 'A' and code_mess is not None:
        raise Exception('Valeur de code_mess non autorisée : %s' % code_mess)
    try:
        out = fact[mot_cle]
    except:
        out = None
        if code_mess is not None:
            UTMESS(code_mess, 'PLEXUS_2', valk=mot_cle)
    return out
#-----------------------------------------------------------------------


def get_group_ma(fact, mot_cle='GROUP_MA'):
    """
        Renvoie la liste des groupes de mailles de l'instance 'fact'
        associée au mot-clé mot_cle.
    """
    group_ma = get_motcle(fact, mot_cle, code_mess='F')
    group_ma = tolist(group_ma)
    return group_ma
#-----------------------------------------------------------------------


def lire_fichier(fichier):
    """
        Lecture d'un fichier et renvoie des valeurs contenues.
    """
    fich = open(fichier, 'r')
    lignes = fich.readlines()
    fich.close()
    valeurs = []
    commentaires = ['#', '%', '$', '*']
    for line in lignes:
        add = 1
        for comment in commentaires:
            if string.find(line, comment) != -1:
                add = 0
                break
        if add:
            data = [val for val in line.split(None)]
            valeurs.append(data)
    return valeurs
#-----------------------------------------------------------------------


def lire_pun(fichier):
    """
        Lecture d'un fichier de type pun
    """
    data = lire_fichier(fichier)
    icourbe = 0
    courbes = {}
    for ligne in data:
        if ligne[0].startswith('VAL'):
            icourbe += 1
            courbes[icourbe] = []
        else:
            ligne_vale = [float(val) for val in ligne]
            courbes[icourbe].append(ligne_vale)
    for courbe in courbes:
        courbes[courbe] = numpy.transpose(numpy.array(courbes[courbe]))
    return courbes
#-----------------------------------------------------------------------


def ctime(fact):
    """
    Renvoie un bloc de données indiquant les instants à imprimer selon
    les différentes possibilités d'EPX (correspond à /CTIME/ de EPX)
    """
    from Calc_epx.calc_epx_struc import BLOC_DONNEES
    
    cata_inst = {
    'PAS_NBRE': 'FREQ',
    'PAS_INST': 'TFREQ',
    'INST'    : 'TIME',
    'NUME_ORDRE' : 'NUPA',
    }  
    
    blocs_ctime = []
    
    for cle in cata_inst.keys():
        val = get_motcle(fact, cle, code_mess=None)
        if val is not None:
            cle_epx = cata_inst[cle]
            
            if cle_epx == 'TIME':
                bloc_ctime = BLOC_DONNEES(cle_epx, l_group=val, cle_l_group='PROG',
                                          dispo_group='hori')
            elif cle_epx == 'NUPA':
                bloc_ctime = BLOC_DONNEES(cle_epx, l_group=val, dispo_group='hori')
            else:
                bloc_ctime = BLOC_DONNEES(cle_epx, cle=val)
            
            blocs_ctime.append(bloc_ctime)
            
    return blocs_ctime
#------------------------------------------------------------------------


def angle2vectx(alpha, beta):
    """
        A partir d'une direction de l'espace définie par deux angles,
        calcule le vecteur correspondant à cette direction.
    """
    [alpha, beta] = numpy.deg2rad([alpha, beta])

    cosa = numpy.cos(alpha)
    sina = numpy.sin(alpha)
    cosb = numpy.cos(beta)
    sinb = numpy.sin(beta)
    vect = [cosb * cosa, cosb * sina, -sinb]
    for comp in range(len(vect)):
        if abs(vect[comp]) <= tst:
            vect[comp] = 0.0
    vect = numpy.array(vect)
    vect = vect / norme(vect)
    return vect
#------------------------------------------------------------------------


def angle2vecty(angl):
    """
        A partir d'un repère orthonormé de l'espace défini par trois
        angles nautiques, calcule le deuxième vecteur (y) définissant ce
        repère.
    """
    angl = numpy.deg2rad(angl)

    cosa = numpy.cos(angl[0])
    sina = numpy.sin(angl[0])
    cosb = numpy.cos(angl[1])
    sinb = numpy.sin(angl[1])
    cosg = numpy.cos(angl[2])
    sing = numpy.sin(angl[2])
    vect = [sing * sinb * cosa - cosg * sina,
            cosg * cosa + sing * sinb * sina,
            sing * cosb]
    for comp in range(len(vect)):
        if abs(vect[comp]) <= tst:
            vect[comp] = 0.0
    vect = numpy.array(vect)
    vect = vect / norme(vect)
    return vect
#-----------------------------------------------------------------------


def get_unite_libre():
    """
        Retoune une unité de fichier libre.
    """
    _UL = INFO_EXEC_ASTER(LISTE_INFO='UNITE_LIBRE')
    unite = _UL['UNITE_LIBRE', 1]
    DETRUIRE(CONCEPT=(_F(NOM=_UL),), INFO=1)
    return(unite)
