#@ MODIF sd_maillage SD  DATE 13/02/2007   AUTEUR PELLET J.PELLET 
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

from SD.sd_cham_no import sd_cham_no
from SD.sd_carte import sd_carte
from SD.sd_l_table import sd_l_table


class sd_maillage(sd_titre):
#-------------------------------
    nomj = SDNom(fin=8)

    DIME = AsVI(lonmax=6, )

    # un sd_maillage a toujours des noeuds :
    NOMNOE = AsObject(genr='N', xous='S', type='K', ltyp=8, )
    coordo = sd_cham_no(SDNom(nomj='.COORDO'))

    # normalement, un sd_maillage a toujours une "sd_l_table" mais POST_MAIL_XFEM ...
    lt = Facultatif(sd_l_table(SDNom(nomj='')))

    # si le sd_maillage a des groupes :
    GROUPENO = Facultatif(AsColl(acces='NO', stockage='DISPERSE', modelong='VARIABLE', type='I', ))
    GROUPEMA = Facultatif(AsColl(acces='NO', stockage='DISPERSE', modelong='VARIABLE', type='I', ))

    # si le sd_maillage a des mailles :
    CONNEX  = Facultatif(AsColl(acces='NU', stockage='CONTIG', modelong='VARIABLE', type='I', ))
    TYPMAIL = Facultatif(AsVI())
    NOMMAI  = Facultatif(AsObject(genr='N', xous='S', type='K', ltyp=8, ))

    # si le sd_maillage a des super-mailles :
    NOMACR  = Facultatif(AsVK8())
    SUPMAIL = Facultatif(AsColl(acces='NO', stockage='DISPERSE', modelong='VARIABLE', type='I', ))
    PARA_R  = Facultatif(AsVR())
    TYPL    = Facultatif(AsVI())

    # si le sd_maillage est linéique (tube_GV) :
    abs_curv  = Facultatif(sd_carte(SDNom(nomj='.ABS_CURV')))

    ADAPTATION = Facultatif(AsVI(lonmax=1, ))
    FORM = Facultatif(AsVK32(SDNom(debut=19), lonmax=2, ))

