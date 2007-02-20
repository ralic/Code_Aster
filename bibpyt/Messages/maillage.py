#@ MODIF maillage Messages  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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

def _(x) : return x

cata_msg={

1: _("""
   Le nom de groupe numéro %(i1)d de la famille %(k1)s
   est trop long. Il sera tronqué à 8 caractères.
   Le groupe %(k2)s est renommé en %(k3)s.
"""),

2: _("""
Le nom de groupe numéro %(i1)d de la famille %(k1)s
est vide.
"""),

3: _("""
Famille %(k1)s :
   Incohérence sur les nombres de %(k2)s, il y en a %(i1)d alors
   que la fonction MED en annonce %(i2)d.

Impossible de lire ce fichier. On peut utiliser mdump (utilitaire med)
pour voir si le problème vient du fichier MED ou de la lecture dans
Code_Aster.
"""),

4: _("""
La famille %(k1)s n'a ni groupe, ni attribut.
"""),

5: _("""
   Lecture de la famille numéro %(i1)4d de nom %(k1)s.
"""),

6: _("""
      Groupe numéro %(i1)6d : %(k1)s
"""),

7: _("""
      Groupe numéro %(i1)6d : %(k1)s
                renommé en : %(k2)s
"""),

8: _("""
Vous ne pouvez pas renommer le groupe %(k1)s en %(k2)s
car %(k2)s existe déjà dans le fichier MED.
"""),

9: _("""
Arret en raison des conflits sur les noms de groupe.
"""),

}

