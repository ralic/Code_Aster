#@ MODIF miss_resu_aster Miss  DATE 16/10/2012   AUTEUR DEVESA G.DEVESA 
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

"""Module permettant de lire le fichier produit par IMPR_MACR_ELEM/IMPR_MISS_3D
et de produire une structure pour écrire les fichiers en entrées de Miss.

Cette structure est potentiellement volumineuse et sera donc détruite dès que possible.
"""

import os
import traceback

import aster
from Miss.miss_utils import lire_nb_valeurs, double


def lire_resultat_aster(fich_aster):
    """Lit le fichier issu de IMPR_MACR_ELEM/IMPR_MISS_3D.
    """
    fobj = open(fich_aster, "r")
    struct = STRUCT_RESULTAT()
    # dimension des éléments (fixe ?)
    struct.maille_dime = 8
    ln = 0
    try:
        # modes dynamiques : nombre, type
        ln += 1
        ds, nb, typ = fobj.readline().split()
        struct.mode_dyna_nb = int(nb)
        struct.mode_dyna_type = typ
        # modes statiques : nombre, type
        ln += 1
        ds, nb, typ = fobj.readline().split()
        struct.mode_stat_nb = int(nb)
        struct.mode_stat_type = typ
        # titre
        ln += 1
        lab = fobj.readline()
        ln += 1
        struct.titre = fobj.readline().strip()
        # noeuds
        ln += 1
        lab, nb = fobj.readline().split()
        struct.noeud_nb = int(nb)
        # noeuds : coordonnées
        ln += lire_nb_valeurs(fobj, struct.noeud_nb * 3, struct.noeud_coor, double)
        # mailles
        ln += 1
        lab, nb = fobj.readline().split()
        struct.maille_nb = int(nb)
        # mailles : connectivité
        ln += lire_nb_valeurs(fobj, struct.maille_nb * struct.maille_dime, struct.maille_connec, int)
        # mode dynamiques
        ln += lire_nb_valeurs(fobj, struct.noeud_nb * 3, struct.mode_dyna_vale, double,
                              struct.mode_dyna_nb, 1, max_per_line=3,
                              regexp_label="MODE +DYNA")
        # modes dynamiques : fréquence
        ln += lire_nb_valeurs(fobj, struct.mode_dyna_nb, struct.mode_dyna_freq, double,
                              1, 1)
        # modes dynamiques : amortissement
        ln += lire_nb_valeurs(fobj, struct.mode_dyna_nb, struct.mode_dyna_amor, double,
                              1, 1)
        # modes dynamiques : masse
        ln += lire_nb_valeurs(fobj, struct.mode_dyna_nb, struct.mode_dyna_mass, double,
                              1, 1)
        # modes dynamiques : rigidité
        ln += lire_nb_valeurs(fobj, struct.mode_dyna_nb, struct.mode_dyna_rigi, double,
                              1, 1)
        # mode statiques
        ln += lire_nb_valeurs(fobj, struct.noeud_nb * 3, struct.mode_stat_vale, double,
                              struct.mode_stat_nb, 1, max_per_line=3,
                              regexp_label="MODE +STAT +INTER")
        # modes statiques : masse
        ln += lire_nb_valeurs(fobj, struct.mode_stat_nb ** 2, struct.mode_stat_mass, double,
                              1, 1)
        # modes statiques : rigidité
        ln += lire_nb_valeurs(fobj, struct.mode_stat_nb ** 2, struct.mode_stat_rigi, double,
                              1, 1)
        # modes statiques : amortissements (facultatifs)
        lbid = []
        unused = lire_nb_valeurs(fobj, struct.mode_stat_nb ** 2, lbid, double,
                              1, 1, regexp_label="STAT +AMOR")
        # modes couplés
        ln += 1
        lab, nbd, nbs = fobj.readline().split()
        struct.coupl_nb = (int(nbd), int(nbs))
        # modes couplés : masse
        ln += lire_nb_valeurs(fobj, struct.mode_dyna_nb * struct.mode_stat_nb,
                              struct.coupl_mass, double, 1, 1)
        # modes couplés : rigidité
        ln += lire_nb_valeurs(fobj, struct.mode_dyna_nb * struct.mode_stat_nb,
                              struct.coupl_rigi, double, 1, 1)
        # modes statiques : amortissements (facultatifs)
        lbid = []
        unused = lire_nb_valeurs(fobj, struct.mode_dyna_nb * struct.mode_stat_nb,
                                 lbid, double, 1, 1, regexp_label="COUPL +AMOR")

    except IOError, err:
        raise aster.error('MISS0_7', vali=ln, valk=str(err))
    # vérifications
    try:
        struct.check()
    except AssertionError, err:
        raise aster.error('MISS0_8', valk=traceback.format_exc(limit=2))
    # arrangements : compléter la connectivité à 20
    new = []
    add = [0,] * (20 - struct.maille_dime)
    for i in range(struct.maille_nb):
        new.extend(struct.maille_connec[i*struct.maille_dime:(i+1)*struct.maille_dime] + add)
    struct.maille_connec = new
    return struct

def lire_resultat_issf_aster(fich_aster):
    """Lit le fichier issu de IMPR_MACR_ELEM/IMPR_MISS_3D.
    """
    fobj = open(fich_aster, "r")
    struct = STRUCT_RESULTAT()
    # dimension des éléments (fixe ?)
    struct.maille_dime = 8
    ln = 0
    try:
        # modes dynamiques : nombre, type
        ln += 1
        ds, nb, typ = fobj.readline().split()
        struct.mode_dyna_nb = int(nb)
        struct.mode_dyna_type = typ
        # modes statiques : nombre, type
        ln += 1
        ds, nb, typ = fobj.readline().split()
        struct.mode_stat_nb = int(nb)
        struct.mode_stat_type = typ
        # titre
        ln += 1
        lab = fobj.readline()
        ln += 1
        struct.titre = fobj.readline().strip()
        # noeuds
        ln += 1
        lab, nb = fobj.readline().split()
        struct.noeud_nb = int(nb)
        # noeuds : coordonnées
        ln += lire_nb_valeurs(fobj, struct.noeud_nb * 3, struct.noeud_coor, double)
        # mailles1
        ln += 1
        lab, nb1 = fobj.readline().split()
        struct.maille_nb1 = int(nb1)
        # mailles : connectivité
        ln += lire_nb_valeurs(fobj, struct.maille_nb1 * struct.maille_dime, struct.maille_connec1, int)
        # mailles2
        ln += 1
        lab, nb2 = fobj.readline().split()
        struct.maille_nb2 = int(nb2)
        # mailles : connectivité
        ln += lire_nb_valeurs(fobj, struct.maille_nb2 * struct.maille_dime, struct.maille_connec2, int)
        # mailles3
        ln += 1
        lab, nb3 = fobj.readline().split()
        struct.maille_nb3 = int(nb3)
        # mailles : connectivité
        ln += lire_nb_valeurs(fobj, struct.maille_nb3 * struct.maille_dime, struct.maille_connec3, int)
        # mailles4
        ln += 1
        lab, nb4 = fobj.readline().split()
        struct.maille_nb4 = int(nb4)
        # mailles : connectivité
        ln += lire_nb_valeurs(fobj, struct.maille_nb4 * struct.maille_dime, struct.maille_connec4, int)
        struct.maille_nb = int(nb1)+int(nb2)+int(nb3)+int(nb4)
        # mode dynamiques
        ln += lire_nb_valeurs(fobj, struct.noeud_nb * 3, struct.mode_dyna_vale, double,
                              struct.mode_dyna_nb, 1, max_per_line=3,
                              regexp_label="MODE +DYNA")
        # modes dynamiques : fréquence
        ln += lire_nb_valeurs(fobj, struct.mode_dyna_nb, struct.mode_dyna_freq, double,
                              1, 1)
        # modes dynamiques : amortissement
        ln += lire_nb_valeurs(fobj, struct.mode_dyna_nb, struct.mode_dyna_amor, double,
                              1, 1)
        # modes dynamiques : masse
        ln += lire_nb_valeurs(fobj, struct.mode_dyna_nb, struct.mode_dyna_mass, double,
                              1, 1)
        # modes dynamiques : rigidité
        ln += lire_nb_valeurs(fobj, struct.mode_dyna_nb, struct.mode_dyna_rigi, double,
                              1, 1)
        # mode statiques
        ln += lire_nb_valeurs(fobj, struct.noeud_nb * 3, struct.mode_stat_vale, double,
                              struct.mode_stat_nb, 1, max_per_line=3,
                              regexp_label="MODE +STAT +INTER")
        # modes statiques : masse
        ln += lire_nb_valeurs(fobj, struct.mode_stat_nb ** 2, struct.mode_stat_mass, double,
                              1, 1)
        # modes statiques : rigidité
        ln += lire_nb_valeurs(fobj, struct.mode_stat_nb ** 2, struct.mode_stat_rigi, double,
                              1, 1)
        # modes statiques : amortissements (facultatifs)
        lbid = []
        unused = lire_nb_valeurs(fobj, struct.mode_stat_nb ** 2, lbid, double,
                              1, 1, regexp_label="STAT +AMOR")
        # modes couplés
        ln += 1
        lab, nbd, nbs = fobj.readline().split()
        struct.coupl_nb = (int(nbd), int(nbs))
        # modes couplés : masse
        ln += lire_nb_valeurs(fobj, struct.mode_dyna_nb * struct.mode_stat_nb,
                              struct.coupl_mass, double, 1, 1)
        # modes couplés : rigidité
        ln += lire_nb_valeurs(fobj, struct.mode_dyna_nb * struct.mode_stat_nb,
                              struct.coupl_rigi, double, 1, 1)
        # modes statiques : amortissements (facultatifs)
        lbid = []
        unused = lire_nb_valeurs(fobj, struct.mode_dyna_nb * struct.mode_stat_nb,
                                 lbid, double, 1, 1, regexp_label="COUPL +AMOR")

    except IOError, err:
        raise aster.error('MISS0_7', vali=ln, valk=str(err))
    # vérifications
    try:
        struct.check()
    except AssertionError, err:
        raise aster.error('MISS0_8', valk=traceback.format_exc(limit=2))
    # arrangements : compléter la connectivité à 20

    add = [0,] * (20 - struct.maille_dime)
    new = []
    for i in range(struct.maille_nb1):
        new.extend(struct.maille_connec1[i*struct.maille_dime:(i+1)*struct.maille_dime] + add)
    struct.maille_connec1 = new
    new = []
    for i in range(struct.maille_nb2):
        new.extend(struct.maille_connec2[i*struct.maille_dime:(i+1)*struct.maille_dime] + add)
    struct.maille_connec2 = new
    new = []
    for i in range(struct.maille_nb3):
        new.extend(struct.maille_connec3[i*struct.maille_dime:(i+1)*struct.maille_dime] + add)
    struct.maille_connec3 = new
    new = []
    for i in range(struct.maille_nb4):
        new.extend(struct.maille_connec4[i*struct.maille_dime:(i+1)*struct.maille_dime] + add)
    struct.maille_connec4 = new
    return struct



class STRUCT_RESULTAT:
    """Simple conteneur."""
    def __init__(self):
        self.titre = ""
        self.noeud_nb = 0
        self.noeud_coor = []
        self.maille_nb = 0
        self.maille_connec = []
        self.maille_connec1 = []
        self.maille_connec2 = []  
        self.maille_connec3 = []
        self.maille_connec4 = []  
        self.maille_dime = 0
        self.mode_dyna_nb = 0
        self.mode_dyna_type = ""
        self.mode_dyna_vale = []
        self.mode_dyna_freq = []
        self.mode_dyna_amor = []
        self.mode_dyna_mass = []
        self.mode_dyna_rigi = []
        self.mode_stat_nb = 0
        self.mode_stat_type = ""
        self.mode_stat_vale = []
        self.mode_stat_mass = []
        self.mode_stat_rigi = []
        self.coupl_nb = [0, 0]
        self.coupl_mass = []
        self.coupl_rigi = []


    def check(self):
        """Vérifications."""
        assert len(self.noeud_coor) == self.noeud_nb * 3
       # assert len(self.maille_connec) == self.maille_nb * self.maille_dime
        assert len(self.mode_dyna_vale) == 0 \
            or len(self.mode_dyna_vale) == self.mode_dyna_nb * self.noeud_nb * 3
        assert len(self.mode_dyna_freq) == self.mode_dyna_nb
        assert len(self.mode_dyna_amor) == self.mode_dyna_nb
        assert len(self.mode_dyna_mass) == self.mode_dyna_nb
        assert len(self.mode_dyna_rigi) == self.mode_dyna_nb
        assert len(self.mode_stat_vale) == 0 \
            or len(self.mode_stat_vale) == self.mode_stat_nb * self.noeud_nb * 3
        assert len(self.mode_stat_mass) == self.mode_stat_nb ** 2
        assert len(self.mode_stat_rigi) == self.mode_stat_nb ** 2
        assert self.coupl_nb == (self.mode_dyna_nb, self.mode_stat_nb)
        assert len(self.coupl_mass) == self.mode_dyna_nb * self.mode_stat_nb
        assert len(self.coupl_rigi) == self.mode_dyna_nb * self.mode_stat_nb


    def repr(self):
        """Pour deboggage"""
        txt = []
        for attr in dir(self):
            val = getattr(self, attr)
            if attr.startswith('_') or callable(val):
                continue
            if type(val) in (list, tuple):
                val = val[:8]
            txt.append("%-14s : %s" % (attr, val))
        return os.linesep.join(txt)

