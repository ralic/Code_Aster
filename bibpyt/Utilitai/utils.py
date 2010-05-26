#@ MODIF utils Utilitai  DATE 26/05/2010   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
Module fournissant quelques fonctions utilitaires.
"""

import os
import time


def miss_dble(list1, list2):
    """miss = elements de list1 absents de list2
      dble = elements de list2 presents dans list1, fournis plusieurs fois."""
    s_ini = set(list1)
    inter = s_ini.intersection(list2)
    miss = s_ini.copy()
    dble = set()
    for p in list2:
        try:
            miss.remove(p)
        except KeyError:
            if set(p).issubset(s_ini) and p.strip() != '':
                dble.add(p)
    return miss, inter, dble


#TODO faire à l'identique pour les commandes fortran (ou appeller cette fonction)
def get_titre_concept(co=None):
    """Retourne un titre automatique."""
    # ASTER 10.01.25 CONCEPT tab0 CALCULE LE 21/05/2010 A 17:58:50 DE TYPE TABLE_SDASTER
    import aster
    from Noyau.N_ASSD import ASSD
    if not isinstance(co, ASSD):
        co = None
    fmt = {
        "version" : "ASTER %(version)s",
        "nomco" : "CONCEPT %(nom_concept)s",
        "etatco" : "CALCULE",
        "dateheure" : "%(dateheure)s",
        "typeco" : "DE TYPE %(type_concept)s",
    }
    format = [fmt["version"],]
    df = {
        "version" : aster.__version__,
        "dateheure" : time.strftime("LE %m/%d/%Y A %H:%M:%S"),
    }
    if co:
        df["nom_concept"] = co.nom
        format.append(fmt["nomco"])
        format.append(fmt["etatco"])
    format.append(fmt["dateheure"])
    if co:
        df["type_concept"] = co.__class__.__name__.upper()
        format.append(fmt["typeco"])
    globfmt = " ".join(format)
    titre = globfmt % df
    lt = titre.split()
    lt = maximize_lines(lt, 80, " ")
    return os.linesep.join(lt)


# existe dans asrun.mystring
def maximize_lines(l_fields, maxlen, sep):
    """Construit des lignes dont la longueur est au plus de `maxlen` caractères.
    Les champs sont assemblés avec le séparateur `sep`.
    """
    newlines = []
    # ceinture
    assert max([len(f) for f in l_fields]) <= maxlen, 'lignes trop longues : %s' % l_fields
    while len(l_fields) > 0:
        cur = []
        while len(l_fields) > 0 and len(sep.join(cur + [l_fields[0],])) <= maxlen:
            cur.append(l_fields.pop(0))
        # bretelle
        assert len(cur) > 0, l_fields
        newlines.append(sep.join(cur))
    newlines = [l for l in newlines if l != '']
    return newlines



if __name__ == '__main__':
   npar = ('X', 'Y',)
   nuti = ('DX', 'DY', 'X', 'X')
   print miss_dble(npar, nuti)

