#@ MODIF sd_cara_elem SD  DATE 13/02/2007   AUTEUR PELLET J.PELLET 
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

from SD.sd_cham_elem import sd_cham_elem
from SD.sd_carte import sd_carte
class sd_cara_elem(AsBase):
    nomj = SDNom(fin=8)
    cargenba = Facultatif(sd_carte(SDNom(nomj='.CARGENBA', fin=19)))
    cafibr   = Facultatif(sd_cham_elem(SDNom(nomj='.CAFIBR', fin=19)))
    carmassi = Facultatif(sd_carte(SDNom(nomj='.CARMASSI', fin=19)))
    carcable = Facultatif(sd_carte(SDNom(nomj='.CARCABLE', fin=19)))
    carcoque = Facultatif(sd_carte(SDNom(nomj='.CARCOQUE', fin=19)))
    cargeoba = Facultatif(sd_carte(SDNom(nomj='.CARGEOBA', fin=19)))
    canbsp   = Facultatif(sd_cham_elem(SDNom(nomj='.CANBSP', fin=19)))
    cardisck = Facultatif(sd_carte(SDNom(nomj='.CARDISCK', fin=19)))
    cararcpo = Facultatif(sd_carte(SDNom(nomj='.CARARCPO', fin=19)))
    cargenpo = Facultatif(sd_carte(SDNom(nomj='.CARGENPO', fin=19)))
    cardiscm = Facultatif(sd_carte(SDNom(nomj='.CARDISCM', fin=19)))
    carorien = Facultatif(sd_carte(SDNom(nomj='.CARORIEN', fin=19)))
    cardisca = Facultatif(sd_carte(SDNom(nomj='.CARDISCA', fin=19)))
    cventcxf = Facultatif(sd_carte(SDNom(nomj='.CVENTCXF', fin=19)))
    carpoufl = Facultatif(sd_carte(SDNom(nomj='.CARPOUFL', fin=19)))
    cargeopo = Facultatif(sd_carte(SDNom(nomj='.CARGEOPO', fin=19)))


