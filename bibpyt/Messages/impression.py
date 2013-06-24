# coding=utf-8
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
# person_in_charge: josselin.delmas at edf.fr


cata_msg = {

1 : _(u"""
 Il y a trop de colonnes actives (%(i1)d colonnes).
"""),

2 : _(u"""
 Il y a trop de colonnes à afficher dans le tableau de convergence.
 La largeur maximale affichable est de 256 caractères, donc 14 colonnes au maximum.
 Or vous avez <%(i1)d> caractères !
 Si vous avez des colonnes SUIVI_DDL, supprimez en.
 Vous pouvez éventuellement désactiver INFO_RESIDU ou INFO_TEMPS.
"""),

3 : _(u"""
 Il y a trop de colonnes de type SUIVI_DDL (%(i1)d colonnes).
"""),


}
