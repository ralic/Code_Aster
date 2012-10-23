#@ MODIF miss_resu_aster Miss  DATE 23/10/2012   AUTEUR COURTOIS M.COURTOIS 
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


class ResuAsterReader(object):
    """Lit le fichier issu de IMPR_MACR_ELEM/IMPR_MISS_3D."""
    def __init__(self):
        """Initialisation"""
        self.fobj = None
        self.struct = STRUCT_RESULTAT()
        # dimension des éléments (fixe ?)
        self.struct.maille_dime = 8
        self.ln = 0
    
    def read(self, fich_aster):
        """Read the file line per line."""
        try:
            self.fobj = open(fich_aster, "r")
            self._read_all()
            self.fobj.close()
        except IOError, err:
            raise aster.error('MISS0_7', vali=ln, valk=str(err))
        self.check()
        self.post()
        return self.struct

    def check(self):
        """vérifications"""
        struct = self.struct
        try:
            struct.check()
        except AssertionError, err:
            raise aster.error('MISS0_8', valk=traceback.format_exc(limit=2))
    
    def post(self):
        """arrangements"""
        self.struct.post()
    
    def _read_all(self):
        """Read the file line per line."""
        self._read_mode_dyna_para()
        self._read_mode_stat_para()
        self._read_titre()
        self._read_noeuds_nb()
        self._read_noeuds_coord()
        self._read_mailles_connect()
        self._read_mode_dyna()
        self._read_mode_dyna_freq()
        self._read_mode_dyna_amor()
        self._read_mode_dyna_mass()
        self._read_mode_dyna_rigi()
        self._read_mode_stat()
        self._read_mode_stat_mass()
        self._read_mode_stat_rigi()
        self._read_mode_stat_amor()
        self._read_mode_coupl_para()
        self._read_mode_coupl_mass()
        self._read_mode_coupl_rigi()
        self._read_mode_coupl_amor()

    def _read_mode_dyna_para(self):
        """modes dynamiques : nombre, type"""
        self.ln += 1
        ds, nb, typ = self.fobj.readline().split()
        self.struct.mode_dyna_nb = int(nb)
        self.struct.mode_dyna_type = typ

    def _read_mode_stat_para(self):
        """modes statiques : nombre, type"""
        self.ln += 1
        ds, nb, typ = self.fobj.readline().split()
        self.struct.mode_stat_nb = int(nb)
        self.struct.mode_stat_type = typ

    def _read_titre(self):
        """titre"""
        self.ln += 1
        lab = self.fobj.readline()
        self.ln += 1
        self.struct.titre = self.fobj.readline().strip()

    def _read_noeuds_nb(self):
        """noeuds"""
        self.ln += 1
        lab, nb = self.fobj.readline().split()
        self.struct.noeud_nb = int(nb)

    def _read_noeuds_coord(self):
        """noeuds : coordonnées"""
        self.ln += lire_nb_valeurs(self.fobj,
                                   self.struct.noeud_nb * 3, 
                                   self.struct.noeud_coor, double)

    def _read_mailles_connect(self):
        """mailles : connectivité"""
        self.struct.init_connect(1)
        self._read_mailles_connect_idx(0)
        self.struct.maille_nb_tot = sum(self.struct.maille_nb)

    def _read_mailles_connect_idx(self, idx):
        """mailles : nb et connectivité pour un groupe"""
        self.ln += 1
        lab, nb = self.fobj.readline().split()
        nb = int(nb)
        self.ln += lire_nb_valeurs(self.fobj,
                                   nb * self.struct.maille_dime,
                                   self.struct.maille_connec[idx], int)
        self.struct.maille_nb[idx] = nb

    def _read_mode_dyna(self):
        """mode dynamiques"""
        self.ln += lire_nb_valeurs(self.fobj,
                                   self.struct.noeud_nb * 3,
                                   self.struct.mode_dyna_vale,
                                   double,
                                   self.struct.mode_dyna_nb,
                                   1, max_per_line=3, regexp_label="MODE +DYNA")

    def _read_mode_dyna_freq(self):
        """modes dynamiques : fréquence"""
        self.ln += lire_nb_valeurs(self.fobj,
                                   self.struct.mode_dyna_nb,
                                   self.struct.mode_dyna_freq,
                                   double, 1, 1)

    def _read_mode_dyna_amor(self):
        """modes dynamiques : amortissement"""
        self.ln += lire_nb_valeurs(self.fobj,
                                   self.struct.mode_dyna_nb,
                                   self.struct.mode_dyna_amor,
                                   double, 1, 1)
    def _read_mode_dyna_mass(self):
        """modes dynamiques : masse"""
        self.ln += lire_nb_valeurs(self.fobj,
                                   self.struct.mode_dyna_nb,
                                   self.struct.mode_dyna_mass,
                                   double, 1, 1)

    def _read_mode_dyna_rigi(self):
        """modes dynamiques : rigidité"""
        self.ln += lire_nb_valeurs(self.fobj,
                                   self.struct.mode_dyna_nb,
                                   self.struct.mode_dyna_rigi,
                                   double, 1, 1)

    def _read_mode_stat(self):
        """mode statiques"""
        self.ln += lire_nb_valeurs(self.fobj,
                                   self.struct.noeud_nb * 3,
                                   self.struct.mode_stat_vale,
                                   double,
                                   self.struct.mode_stat_nb,
                                   1, max_per_line=3, regexp_label="MODE +STAT +INTER")

    def _read_mode_stat_mass(self):
        """modes statiques : masse"""
        self.ln += lire_nb_valeurs(self.fobj,
                                   self.struct.mode_stat_nb ** 2,
                                   self.struct.mode_stat_mass,
                                   double, 1, 1)

    def _read_mode_stat_rigi(self):
        """modes statiques : rigidité"""
        self.ln += lire_nb_valeurs(self.fobj,
                                   self.struct.mode_stat_nb ** 2,
                                   self.struct.mode_stat_rigi,
                                   double, 1, 1)

    def _read_mode_stat_amor(self):
        """modes statiques : amortissements (facultatifs)"""
        lbid = []
        unused = lire_nb_valeurs(self.fobj,
                                 self.struct.mode_stat_nb ** 2,
                                 lbid,
                                 double, 1, 1, regexp_label="STAT +AMOR")

    def _read_mode_coupl_para(self):
        """modes couplés"""
        self.ln += 1
        lab, nbd, nbs = self.fobj.readline().split()
        self.struct.coupl_nb = (int(nbd), int(nbs))

    def _read_mode_coupl_mass(self):
        """modes couplés : masse"""
        self.ln += lire_nb_valeurs(self.fobj,
                                   self.struct.mode_dyna_nb * self.struct.mode_stat_nb,
                                   self.struct.coupl_mass,
                                   double, 1, 1)

    def _read_mode_coupl_rigi(self):
        """modes couplés : rigidité"""
        self.ln += lire_nb_valeurs(self.fobj,
                                   self.struct.mode_dyna_nb * self.struct.mode_stat_nb,
                                   self.struct.coupl_rigi,
                                   double, 1, 1)

    def _read_mode_coupl_amor(self):
        """modes couplés : amortissements (facultatifs)"""
        lbid = []
        unused = lire_nb_valeurs(self.fobj,
                                 self.struct.mode_dyna_nb * self.struct.mode_stat_nb,
                                 lbid,
                                 double, 1, 1, regexp_label="COUPL +AMOR")


class ResuAsterISSFReader(ResuAsterReader):
    """Lit le fichier issu de IMPR_MACR_ELEM/IMPR_MISS_3D avec ISSF='OUI'."""

    def _read_mailles_connect(self):
        """mailles : connectivité"""
        self.struct.init_connect(4)
        self._read_mailles_connect_idx(0)
        self._read_mailles_connect_idx(1)
        self._read_mailles_connect_idx(2)
        self._read_mailles_connect_idx(3)
        self.struct.maille_nb_tot = sum(self.struct.maille_nb)

class STRUCT_RESULTAT:
    """Simple conteneur."""
    def __init__(self):
        self.titre = ""
        self.noeud_nb = 0
        self.noeud_coor = []
        self.maille_nb = []
        self.maille_nb_tot = 0
        self.maille_connec = []
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

    def init_connect(self, nbgrp):
        """initialise le stockage pour les nbgrp groupes de mailles."""
        self.maille_nb = [0] * nbgrp
        self.maille_connec = [[] for i in range(nbgrp)]

    def check(self):
        """Vérifications."""
        assert len(self.noeud_coor) == self.noeud_nb * 3
        assert len(self.maille_nb) == len(self.maille_connec)
        for nb, connec in zip(self.maille_nb, self.maille_connec):
            assert len(connec) == nb * self.maille_dime
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

    def post(self):
        """arrangements : compléter la connectivité à 20"""
        dime = self.maille_dime
        add = [0,] * (20 - dime)
        nbgrp = len(self.maille_nb)
        for idx in range(nbgrp):
            new = []
            for i in range(self.maille_nb[idx]):
                new.extend(self.maille_connec[idx][i * dime : (i+1) * dime] + add)
            self.maille_connec[idx] = new

    def repr(self):
        """Pour deboggage"""
        txt = []
        for attr in dir(self):
            val = getattr(self, attr)
            if attr.startswith('_') or callable(val):
                continue
            if type(val) in (list, tuple):
                val = list(val[:8])
                for i in range(len(val)):
                    if type(val[i]) in (list, tuple):
                        val[i] = val[i][:8]
            txt.append("%-14s : %s" % (attr, val))
        return os.linesep.join(txt)

