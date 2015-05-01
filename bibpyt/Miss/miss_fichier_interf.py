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
# person_in_charge: mathieu.courtois at edf.fr

"""Module permettant de produire les fichiers :
    - de données de Miss (.in),
    - de maillage de l'interface (.mvol),
    - des modes d'interface (.chp).
"""

import os
from functools import partial

from Miss.miss_domain import MissDomains
from Miss.miss_utils import dict_format, en_ligne


def fichier_mvol(struct):
    """Produit le contenu du fichier de maillage mvol.
    """
    cont = ["COUPLAGE MISS ASTER", ]
    cont.extend(
        en_ligne([struct.noeud_nb, struct.maille_nb_tot], dict_format['sI'], 2, ""))
    fmtR_fort = "3E%s" % (dict_format['R'].replace("E", ""))
    cont.append("(%s)" % fmtR_fort)
    cont.extend(en_ligne(struct.noeud_coor, dict_format['sR'], 3, ""))
    for i, connec in enumerate(struct.maille_connec):
        ngr = i + 1
        cont.extend(en_ligne(connec, dict_format['sI'], 20,
                             format_ligne="%%(valeurs)s     GR    %d" % ngr))
    cont.append("")
    return os.linesep.join(cont)


def fichier_chp(param, struct):
    """Produit le contenu du fichier chp"""
    domain = MissDomains(param['_hasPC'], param["ISSF"] == "OUI")
    group = domain.group
    cont = []
    # groupes des interfaces (sol-struct + struct OU fluide-struct)
    grp = "GROUPE %4d" % group['sol-struct']
    if group.get('fluide-struct'):
        grp += "%4d" % group['fluide-struct']
        if domain.def_all_domains:
            grp += "%4d" % group['struct']
    else:
        grp += "%4d" % group['struct']
    # modes statiques
    cont.append(grp)
    cont.append(("MODE   " + dict_format["sI"]) % struct.mode_stat_nb)
    fmt_ligne = partial(en_ligne, format=dict_format['sR'], cols=3,
                        format_ligne="%(index_1)6d%(valeurs)s")
    mult = struct.noeud_nb * 3
    for i in range(struct.mode_stat_nb):
        cont.extend(fmt_ligne(struct.mode_stat_vale[i * mult:(i + 1) * mult]))
        cont.append("FIN")
    # groupes des interfaces (struct OU fluide-struct)
    # pas l'interface sol-struct car hypothèse d'encastrement
    grp = "GROUPE "
    if group.get('fluide-struct'):
        grp += "%4d" % group['fluide-struct']
        if domain.def_all_domains:
            grp += "%4d" % group['struct']
    else:
        grp += "%4d" % group['struct']
    # modes dynamiques
    cont.append(grp)
    cont.append(("MODE   " + dict_format["sI"]) % struct.mode_dyna_nb)
    mult = struct.noeud_nb * 3
    for i in range(struct.mode_dyna_nb):
        cont.extend(fmt_ligne(struct.mode_dyna_vale[i * mult:(i + 1) * mult]))
        cont.append("FIN")
    if param["ISSF"] == "OUI":
        cont.append("GROUPE %4d" % group['sol-fluide'])
        if param['ALLU'] != 0.:
            refl = param['ALLU']
            kimp = refl / (2. - refl)
            simp = str(kimp)
            cont.append("FLUI " + simp + " BEM")
        else:
            cont.append("DEPN  BEM")
        cont.append("GROUPE %4d" % group['sol libre'])
        cont.append("LIBRE")
    if param['_hasPC']:
        cont.append("GROUPE %4d" % group['pc'])
        cont.append("LIBRE")
    cont.append("FINC")
    cont.append("EOF")
    cont.append("")
    return os.linesep.join(cont)


def fichier_ext(struct):
    """Produit le contenu du fichier ext"""
    dval = struct.__dict__
    dval['_useamor'] = ''
    amosta = struct.mode_stat_amor
    if len(amosta) > 0:
        dval['_useamor'] = 'AMORTISSEMENT'
    cont = ['CRAIG  %(mode_stat_nb)5d%(mode_dyna_nb)5d %(_useamor)s' % dval]
    fmt_ligne = partial(en_ligne, format="%17.10E", cols=5)
    # stat
    dec = struct.mode_stat_nb
    massta = struct.mode_stat_mass
    for i in range(struct.mode_stat_nb):
        cont.extend(fmt_ligne(massta[i * dec:(i + 1) * dec]))
    rigsta = struct.mode_stat_rigi
    for i in range(struct.mode_stat_nb):
        cont.extend(fmt_ligne(rigsta[i * dec:(i + 1) * dec]))
    if len(amosta) > 0:
        for i in range(struct.mode_stat_nb):
            cont.extend(fmt_ligne(amosta[i * dec:(i + 1) * dec]))
    # dyn et coupl
    fmt3 = dict_format['sR'] * 3
    frqdyn = struct.mode_dyna_freq
    masdyn = struct.mode_dyna_mass
    amodyn = struct.mode_dyna_amor
    mascou = struct.coupl_mass
    rigcou = struct.coupl_rigi
    amocou = struct.coupl_amor
    dec = struct.mode_stat_nb
    for i in range(struct.mode_dyna_nb):
        cont.append(fmt3 % (frqdyn[i], masdyn[i], amodyn[i]))
        cont.extend(fmt_ligne(mascou[i * dec:(i + 1) * dec]))
        cont.extend(fmt_ligne(rigcou[i * dec:(i + 1) * dec]))
        if len(amocou) > 0:
            cont.extend(fmt_ligne(amocou[i * dec:(i + 1) * dec]))
    cont.append("")
    return os.linesep.join(cont)


def fichier_sign(param):
    """Produit le contenu du fichier .sign"""
    import numpy as NP
    if param["FREQ_MIN"] is not None:
        lfreq = list(NP.arange(param["FREQ_MIN"],
                               param["FREQ_MAX"] + param["FREQ_PAS"],
                               param["FREQ_PAS"]))
    if param["LIST_FREQ"] is not None:
        lfreq = list(param['LIST_FREQ'])
    lfreq.insert(0, 0.)
    nbfr = len(lfreq)
    cont = ["TABU %5d ACCE MULT" % nbfr]
    fmt3 = dict_format['sR'] * 3
    for freq in lfreq:
        cont.append(fmt3 % (freq, 1., 0.))
    cont.append("")
    return os.linesep.join(cont)
