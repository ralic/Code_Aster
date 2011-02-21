#@ MODIF archivage Messages  DATE 21/02/2011   AUTEUR ABBAS M.ABBAS 
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
# RESPONSABLE DELMAS J.DELMAS

def _(x) : return x

cata_msg = {

1 : _("""
 Vous risquez d'écraser des données déjà stockées dans la structure de données résultat.
 Dernier instant stocké dans la structure de données résultat: %(r1)19.12e
 Premier instant du calcul: %(r2)19.12e
 Si vous êtes sûr de vous, vous pouvez autoriser l'écrasement avec le mot-clef DETR_NUME_SUIV dans ARCHIVAGE.
"""),

4 : _("""
 Archivage de l'état initial
"""),

5 : _("""
  Archivage des champs
"""),

6 : _("""
    Champ stocké <%(k1)s> à l'instant %(r1)19.12e pour le numéro d'ordre %(i1)d
"""),

7 : _("""
 Archivage des champs dérivés par rapport à %(k1)s
"""),

8 : _("""
    Mode vibratoire stocké pour le numéro d'ordre %(i1)d  
"""),

9 : _("""
    Mode de flambement stocké pour le numéro d'ordre %(i1)d  
"""),


}
