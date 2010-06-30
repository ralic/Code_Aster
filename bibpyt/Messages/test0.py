#@ MODIF test0 Messages  DATE 30/06/2010   AUTEUR DELMAS J.DELMAS 
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
# RESPONSABLE DELMAS J.DELMAS

def _(x) : return x

cata_msg={
1 : _("""
Expression régulière invalide : %(k2)s

Exception retournée :
   %(k1)s
"""),

2 : _("""
Le fichier n'a pas été fermé : %(k1)s
"""),

3 : _("""
TEST_FICHIER impossible, fichier inexistant : %(k1)s
"""),

4 : _("""

     Nom du fichier   : %(k3)s

                       -----------------------------------------------------------------------
                       |             FICHIER              |            REFERENCE             |
   -------------------------------------------------------------------------------------------
   | Nombre de valeurs |            %(i1)6d                |            %(i2)6d                |
   | Somme des valeurs |      %(r1)20.13e        |      %(k4)20s        |
   | md5sum des textes | %(k1)-32s | %(k2)-32s |
   -------------------------------------------------------------------------------------------

"""),

5 : _("""

   -------------------------------------------------------------------------------------------
      Fichier de configuration                    : %(k1)s
      Identifiant pour la mesure des performances : %(k2)s
   -------------------------------------------------------------------------------------------

"""),

7 : _("""
La commande '%(k1)s' n'a pas été exécutée %(i1)d fois.
"""),

8 : _("""
- soit PRECISION contient une seule valeur, et alors, celle-ci sera utilisée
  pour toutes les machines,
- soit PRECISION contient autant de valeurs qu'il y a de MACHINEs.
"""),

9 : _("""Les temps de référence ne sont pas connus pour l'identifiant '%(k1)s'.
On utilise les valeurs de '%(k2)s'.
"""),

10 : _("""
Les listes fournies aux mots-clefs MACHINE et VALE doivent avoir le meme cardinal.
"""),

}



