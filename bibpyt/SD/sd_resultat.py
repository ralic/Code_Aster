#@ MODIF sd_resultat SD  DATE 13/02/2007   AUTEUR PELLET J.PELLET 
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

from SD.sd_l_table import sd_l_table
from SD.sd_champ import sd_champ
from SD.sd_l_charges import sd_l_charges
from sets import Set
import aster


class sd_resultat(sd_titre):
#---------------------------------------
    nomj = SDNom(fin=8)
    TAVA = Facultatif(AsColl(SDNom(debut=19), acces='NU', stockage='CONTIG', modelong='CONSTANT', type='K', ltyp=8, ))
    NOVA = AsObject(SDNom(debut=19), genr='N', xous='S', type='K', ltyp=16, )
    TACH = AsColl(SDNom(debut=19), acces='NU', stockage='CONTIG', modelong='CONSTANT', type='K', ltyp=24, )
    ORDR = AsVI(SDNom(debut=19), )
    DESC = AsObject(SDNom(debut=19), genr='N', xous='S', type='K', ltyp=16, )
    # la déclaration suivante simplifie la sd_fonction check_resultat_i_char
    CHAR = Facultatif(AsVI(SDNom(debut=19),type='K', ltyp=24,  ))

    sd_l_table = Facultatif(sd_l_table(SDNom(nomj='')))


    # indirection vers les champs de .TACH :
    def check_resultat_i_TACH(self, checker):
        lnom = self.TACH.get()
        if not lnom: return
        for k in lnom.keys():
            for nom in lnom[k] :
                if not nom.strip(): continue
                sd2 = sd_champ(nom)
                sd2.check(checker)


    # indirection vers les objets de .TAVA :
    def check_resultat_i_TAVA(self, checker):
        lnom = self.TAVA.get()
        if not lnom: return
        S1=Set()
        for k in lnom.keys():
            suffix=lnom[k][0][:5]
            if not suffix.strip(): continue
            S1.add(suffix)
        for suffix in S1 :
            nom=self.nomj()[:19]+suffix
            sd2 = AsObject(SDNom(nomj=nom,debut=0), xous='S', genr='V', type=Parmi('I','R','C','K'),
                           ltyp=Parmi(4,8,16,24),) ; sd2.check(checker)


    # indirection vers les sd_l_charges stockées comme paramètres dans l'objet .CHAR :
    def check_resultat_i_CHAR(self, checker):
        lnom = self.CHAR.get()
        if not lnom: return
        S1=Set()
        for nom in lnom:
            if not nom.strip(): continue
            S1.add(nom)
        for nom in S1 :
            sd2 = sd_l_charges(nomj=nom); sd2.check(checker)

