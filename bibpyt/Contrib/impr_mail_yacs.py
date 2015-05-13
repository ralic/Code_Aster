# -*- coding: utf-8 -*-
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
# person_in_charge: nicolas.greffet at edf.fr
#
# RECUPERATION DES MAILLAGES IFS VENANT DE SATURNE VIA YACS
#

from Cata import cata
from Cata.cata import *


IMPR_MAIL_YACS=PROC(nom="IMPR_MAIL_YACS",op=43,
            UIinfo={"groupes":("CACHE",)},
               fr=tr("Lecture d'un maillage via YACS lors du Couplage de Code_Aster et Saturne"),
         UNITE_MAILLAGE = SIMP(statut='f',typ='I',defaut=30),
         TYPE_MAILLAGE = SIMP(statut='o',typ='TXM',into=("SOMMET","MILIEU")),
         INFO            =SIMP(statut='f',typ='I',defaut=1,into=(1,2)),
)  ;
