#@ MODIF miss_fichier_sol Miss  DATE 17/04/2012   AUTEUR GREFFET N.GREFFET 
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

import os
import aster
from Miss.miss_utils import dict_format

sfmt = dict_format['sR']


def fichier_sol(tab, param=None):
    """Retourne le contenu du fichier de sol construit à partir de
    la table des données de sol et éventuellement des paramètres du calcul.
    """
    # vérification de la table
    for p in ("NUME_COUCHE", "NUME_MATE", "E", "NU", "RHO", "EPAIS", "AMOR_HYST", "RECEPTEUR", "SUBSTRATUM"):
        if not p in tab.para:
            raise aster.error('TABLE0_2', valk=(p, 'de sol'))
    nb_couche = len(tab)
    if max(tab.NUME_COUCHE.values()) != nb_couche:
        raise aster.error('MISS0_5')
    tsubstr = (tab.SUBSTRATUM == "OUI")
    if len(tsubstr) != 1:
        raise aster.error('MISS0_3')
    
    # complète la table
    tsol = tab.copy()
    # ... niveau récepteur
    def f_recep(v):
        res = ""
        if v.strip() == "OUI":
            res = "RECEP"
        return res
    tsol.fromfunction("s_RECEP", f_recep, "RECEPTEUR")
    # ... niveau source
    def f_force(num, v):
        res = 0
        if v.strip() == "OUI":
            res = num
        return res
    tsol.fromfunction("s_FORCE", f_force, ("NUME_COUCHE", "SOURCE"))
    # ... êta
    tsol.fromfunction("ETA", lambda x : 0., "NUME_COUCHE")

    content = []
    # titre de la table
    content.append("TITRE")
    content.append(tsol.titr)
    # materiaux
    tsol.sort(CLES=["NUME_MATE",])
    nb_mate = max(tsol.NUME_MATE.values())
    content.append("MATERIAU %8d" % nb_mate)
    content.append("RO           E            NU           BETA         ETA")
    format = "%%(RHO)%(R)s %%(E)%(R)s %%(NU)%(R)s %%(AMOR_HYST)%(R)s %%(ETA)%(R)s" \
        % dict_format
    last_id_mate = 0
    for row in tsol:
        if row['NUME_MATE'] == last_id_mate:   # déjà vu, on saute
            continue
        last_id_mate = row['NUME_MATE']
        content.append(format % row)
    # couches
    tsol.sort(CLES=["NUME_COUCHE",])
    content.append("COUCHE %8d" % (nb_couche - 1))
    format = "%%(EPAIS)%(R)s MATE %%(NUME_MATE)8d %%(s_RECEP)s" % dict_format
    for ic, row in enumerate(tsol):
        if ic == nb_couche - 1:
            continue
        content.append(format % row)
    # substratum
    substr = tsubstr.rows[0]
    content.append("SUBS   MATE %8d" % substr['NUME_MATE'])
    # sources
    nb_source = len(tsol.SOURCE == "OUI")
    content.append("SOURCE %8d 3D" % nb_source)
    # forces
    format = "FORCE HORIZ POSI %(s_FORCE)8d"
    if param['TYPE_RESU'] == 'FICHIER_TEMPS' and param['FICHIER_SOL_INCI'] == None :
        # champ incident au substratum
        content.append("FORCE PLANE POSI   %8d" % nb_couche)
    else:
        for ic, row in enumerate(tsol):
            if row["s_FORCE"] != 0:
                content.append(format % row)    
    # complément des paramètres du calcul
    if param and param.get('OFFSET_MAX'):
        # ALGO
        if param['ALGO']:
            content.append("ALGO %s" % param['ALGO'])
        elif param['SURF'] == "OUI":
            content.append("ALGO DEPL")
        else:
            content.append("ALGO REGU")
        # DREF / SPEC / OFFSET
        if param['DREF']:
            content.append(("DREF" + sfmt) % param['DREF'])
        if param['SPEC_MAX']:
            content.append(("SPEC" + sfmt + " / %d") % (param['SPEC_MAX'], param['SPEC_NB']))
        else:
            content.append("SPEC AUTO")
        content.append(("OFFSET" + sfmt + " / %d") % (param['OFFSET_MAX'], param['OFFSET_NB']))

    content.append("FIND")

    # terminer le fichier par un retour chariot
    content.append("")
    return os.linesep.join(content)


