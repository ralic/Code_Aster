#@ MODIF sd_matr_asse_com SD  DATE 11/02/2008   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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

from SD import *
from SD.sd_titre import sd_titre

from SD.sd_maillage import sd_maillage
from SD.sd_nume_ddl import sd_nume_ddl
from SD.sd_matr_cine import sd_matr_cine


class sd_matr_asse_com(sd_titre):
#-----------------------------
    nomj = SDNom(fin=19)

    REFA = AsVK24(lonmax=10,)
    VALM = AsColl(acces='NU', stockage='DISPERSE', modelong='CONSTANT', type=Parmi('C', 'R'))
    UALF = Facultatif(AsColl(acces='NU', stockage='DISPERSE', modelong='CONSTANT', type=Parmi('C', 'R')))
    VALF = Facultatif(AsColl(acces='NU', stockage='DISPERSE', modelong='VARIABLE', type=Parmi('C', 'R')))
    WALF = Facultatif(AsColl(acces='NU', stockage='DISPERSE', modelong='VARIABLE', type=Parmi('C', 'R')))
    CONL = Facultatif(OJBVect(type=Parmi('C', 'R')))
    DIGS = Facultatif(OJBVect(type=Parmi('C', 'R'))) # seulement si solveurs LDLT et MULT_FRONT
    LIME = Facultatif(AsVK8())
    cine = Facultatif(sd_matr_cine(SDNom(nomj='')))

    def exists(self):
        # retourne "vrai" si la SD semble exister (et donc qu'elle peut etre vérifiée)
        return self.REFA.exists

    def check_REFA(self, checker):
        if not self.exists() : return
        refa=self.REFA.get_stripped()
        assert refa[9] in ('NOEU','GENE') , refa
        lgene = refa[9] == 'GENE'
        # pour les matrices generalisees, on ne sait pas ce qui est stocké dans refa[0]='' :
        if not lgene :
            sd2=sd_maillage(refa[0]) ; sd2.check(checker)
            sd2=sd_nume_ddl(refa[1]) ; sd2.check(checker)
        assert refa[2] in ('ELIMF','ELIML','') , refa
        assert refa[4] in ('FETI','') , refa
        # pour les matrices generalisees, refa[7] n'est pas toujours rempli :
        if not lgene :
            # glute à résorber : j'ajoute '' à la liste permise pour le test yyyy108e :
            assert refa[7] in ('ASSE','DECT','DECP','') , refa
        assert refa[8] in ('MS','MR') , refa
        if refa[8]=='MS' :
            assert self.VALM.nmaxoc == 1
        elif refa[8]=='MR' :
            assert self.VALM.nmaxoc == 2


