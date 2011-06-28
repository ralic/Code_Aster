#@ MODIF co_table SD  DATE 28/06/2011   AUTEUR COURTOIS M.COURTOIS 
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

import Accas
from Accas import ASSD

class table_sdaster(ASSD):
    cata_sdj = "SD.sd_table.sd_table"

    def __getitem__(self, key):
        from Utilitai.Utmess import UTMESS
        if not self.accessible():
            raise Accas.AsException("Erreur dans table.__getitem__ en PAR_LOT='OUI'")
        assert len(key) == 2
        para, numlign = key
        column = getattr(self.EXTR_TABLE(), para)
        try:
            res = column.values()[numlign - 1]
        except IndexError:
            # pour __getitem__, il est plus logique de retourner KeyError.
            raise KeyError
        return res

    def TITRE(self):
        """Retourne le titre d'une table Aster
        (Utile pour récupérer le titre et uniquement le titre d'une table dont
        on souhaite manipuler la dérivée).
        """
        if not self.accessible():
            raise Accas.AsException("Erreur dans table.TITRE en PAR_LOT='OUI'")
        #titj = aster.getvectjev('%-19s.TITR' % self.get_name())
        titj = self.sdj.TITR.get()
        if titj != None:
            titr = '\n'.join(titj)
        else:
            titr = ''
        return titr

    def EXTR_TABLE(self) :
        """Produit un objet Table à partir du contenu d'une table Aster
        """
        def Nonefy(l1,l2) :
            if l2 == 0:
                return None
            else:
                return l1
        if not self.accessible():
            raise Accas.AsException("Erreur dans table.EXTR_TABLE en PAR_LOT='OUI'")
        from Utilitai.Table import Table
        import aster
        # titre
        titr = self.TITRE()
        # récupération des paramètres
        #v_tblp = aster.getvectjev('%-19s.TBLP' % self.get_name())
        v_tblp = self.sdj.TBLP.get()
        if v_tblp == None:
            # retourne une table vide
            return Table(titr=titr)
        tabnom=list(v_tblp)
        nparam=len(tabnom)/4
        lparam=[tabnom[4*i:4*i+4] for i in range(nparam)]
        dval={}
        # liste des paramètres et des types
        lpar=[]
        ltyp=[]
        for i in lparam :
            value=list(aster.getvectjev(i[2]))
            exist=aster.getvectjev(i[3])
            dval[i[0].strip()] = map(Nonefy, value, exist)
            lpar.append(i[0].strip())
            ltyp.append(i[1].strip())
        n=len(dval[lpar[0]])
        # contenu : liste de dict
        lisdic=[]
        for i in range(n) :
            d={}
            for p in lpar:
               d[p]=dval[p][i]
            lisdic.append(d)
        return Table(lisdic, lpar, ltyp, titr)

class table_fonction(table_sdaster):
    """Table contenant en plus une colonne FONCTION et/ou FONCTION_C dont les
    valeurs des cellules sont des noms de fonction_sdaster ou fonction_c."""

class table_jeveux(table_sdaster):
    """Classe permettant d'accéder à une table jeveux qui n'a pas d'ASSD associée,
    c'est le cas des concepts résultats (table, evol_xxxx) dérivés."""
    def __init__(self, nom_jeveux):
        table_sdaster.__init__(self)
        self.set_name(nom_jeveux)
