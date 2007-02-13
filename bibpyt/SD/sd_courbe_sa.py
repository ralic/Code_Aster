#@ MODIF sd_courbe_sa SD  DATE 13/02/2007   AUTEUR PELLET J.PELLET 
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

class sd_courbe_sa(AsBase):
    nomj = SDNom(fin=8)
    MAIL2 = AsColl(SDNom(debut=8), acces='NU', stockage='CONTIG', modelong='VARIABLE', type='I', )
    XSARC = AsVR(SDNom(debut=8), )
    MAIL1 = AsColl(SDNom(debut=8), acces='NU', stockage='CONTIG', modelong='VARIABLE', type='I', )
    PAREX = AsColl(SDNom(debut=8), acces='NU', stockage='CONTIG', modelong='VARIABLE', type='R', ltyp=8, )
    XYCARC = AsVR(SDNom(debut=8), )
    XYBSGT = AsVR(SDNom(debut=8), )
    XYASGT = AsVR(SDNom(debut=8), )
    XRARC = AsVR(SDNom(debut=8), )
    CNXOR = AsColl(SDNom(debut=8), acces='NU', stockage='CONTIG', modelong='VARIABLE', type='I', )
    PAROR = AsColl(SDNom(debut=8), acces='NU', stockage='CONTIG', modelong='VARIABLE', type='R', ltyp=8, )
    FACOR = AsColl(SDNom(debut=8), acces='NU', stockage='CONTIG', modelong='VARIABLE', type='I', )
    ORSGT = AsColl(SDNom(debut=8), acces='NU', stockage='CONTIG', modelong='VARIABLE', type='R', ltyp=8, )
    CNXEX = AsColl(SDNom(debut=8), acces='NU', stockage='CONTIG', modelong='VARIABLE', type='I', )
    FACEX = AsColl(SDNom(debut=8), acces='NU', stockage='CONTIG', modelong='VARIABLE', type='I', )
    EXSGT = AsColl(SDNom(debut=8), acces='NU', stockage='CONTIG', modelong='VARIABLE', type='R', ltyp=8, )


