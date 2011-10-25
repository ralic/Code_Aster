#@ MODIF graph0 Messages  DATE 25/10/2011   AUTEUR COURTOIS M.COURTOIS 
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

cata_msg = {
 1 : _(u""""Val" doit etre une liste de 2 ou 3 listes de rééls de meme longueur.
"""),

 2 : _(u""""Lab" doit etre une liste de 2 ou 3 chaines.
"""),

 3 : _(u"""Le format est inconnu : %(k1)s.
"""),

 4 : _(u"""On limite la fenetre aux abscisses positives.
"""),

 5 : _(u"""On limite la fenetre aux ordonnées positives.
"""),

 6 : _(u"""Des erreurs se sont produites :
   %(k1)s
"""),

 7 : _(u"""La variable DISPLAY n'est pas définie.
"""),

 8 : _(u"""On fixe la variable DISPLAY à %(k1)s.
"""),

 9 : _(u"""Erreur lors de l'utilisation du filtre '%(k1)s'.
Le fichier retourné est le fichier '.agr'.
"""),

10 : _(u"""
   <I> Informations sur le fichier '%(k1)s' :
      Nombre de courbes    : %(i1)3d
      Bornes des abscisses : [ %(r1)13.6G , %(r2)13.6G ]
      Bornes des ordonnées : [ %(r3)13.6G , %(r4)13.6G ]
"""),

11 : _(u"""
   Le fichier '%(k1)s' ne semble pas être au format texte de xmgrace.
   On ne peut donc pas recalculer les valeurs extrêmes.
   Le pilote ne permet probablement pas d'imprimer plusieurs
   graphiques dans le même fichier.

Conseil :
   N'utilisez pas le mot-clé PILOTE et produisez l'image en
   utilisant xmgrace.
"""),

}
