#@ MODIF discretisation Messages  DATE 30/06/2010   AUTEUR DELMAS J.DELMAS 
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

cata_msg = {

1 : _("""
Certains pas de temps de la liste (LISTE_INST) sont plus petits
 que le pas de temps minimal renseigné (SUBD_PAS_MINI)
"""),

3 : _("""
 le nombre de subdivisions du pas de temps doit etre plus grand que 1 (SUBD_PAS)
"""),

4 : _("""
 Avec une gestion automatique de la liste d'instants, la notion de NIVEAU de subdivisions est ignorée.
"""),

5 : _("""
 L'adaptation du pas de temps a été désactivée. Seuls les instants définis par LIST_INST seront calculés
 (hormis les sous-découpages éventuels).
"""),

6 : _("""
 valeur de SUBD_ITER_IGNO incoherent avec ITER_GLOB_MAXI.
 augmentez ITER_GLOB_MAXI
"""),

7 : _("""
 valeur de SUBD_ITER_FIN incoherent avec ITER_GLOB_MAXI.
 augmentez ITER_GLOB_MAXI
"""),

8 : _("""
 Vous faites un calcul de thermique sans résolution stationnaire et sans
 non plus de résolution transitoire.

 Conseils :
   Renseignez la discrétisation temporelle par le mot clé INCREMENT
"""),


10 : _("""
 On ne peut définir qu'une seule occurence de ECHEC/EVENEMENT='DIVERGENCE'.
"""),

11 : _("""
 La valeur du pas de temps retenu (%(r1)s) est inférieure à PAS_MINI.
"""),

12 : _("""
 La valeur du pas de temps retenu (%(r1)s) est supérieure à PAS_MAXI.
"""),

13 : _("""
 On a depassé le nombre maximal de pas de temps autorisé.
"""),

86 : _("""
Il n'y a aucun pas de calcul temporel.
En mécanique, 'LIST_INST' est absent.
En thermique, 'LIST_INST' est absent ou un singleton.
"""),

87 : _("""
La liste d'instants n'est pas strictement croissante.
"""),

89 : _("""
Instant initial introuvable dans la liste d'instants (LIST_INST).
"""),

90 : _("""
Instant final introuvable dans la liste d'instants (LIST_INST).
"""),

92 : _("""
 NUME_INST_INIT plus grand que NUME_INST_FIN
"""),

93: _("""
 NUME_INST_INIT n'appartient pas à la liste d'instants
"""),

94 : _("""
  -> Le numto d'ordre correspondant à l'instant final de calcul NUME_INST_FIN
     n'appartient pas à la liste des numéros d'ordre.
     Dans ce cas, Aster considère pour numéro d'ordre final, le dernier de
     la liste fournie.
  -> Risque & Conseil :
     Afin d'éviter des pertes de résultats, assurez-vous que le numéro d'ordre
     associé à l'instant NUME_INST_FIN appartienne bien à la liste des numéros
     d'ordre.
"""),

99 : _("""
Le nombre de niveau de subdivisions doit etre plus grand que 1 (SUBD_NIVEAU)
"""),

}
