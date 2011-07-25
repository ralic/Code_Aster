#@ MODIF discretisation Messages  DATE 12/07/2011   AUTEUR ABBAS M.ABBAS 
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
Certains pas de temps de la liste (LISTE_INST) sont plus petits
 que le pas de temps minimal renseigné (SUBD_PAS_MINI)
"""),

2 : _("""
 L'instant initial de la liste est plus grand que le deuxième instant.
 Si vous faites une reprise de calcul (REUSE), vous pouvez utiliser le mot-clef ETAT_INIT/INST_ETAT_INIT pour corriger celà.
"""),

5 : _("""
 L'adaptation du pas de temps a été désactivée. Seuls les instants définis par LIST_INST seront calculés
 (hormis les sous-découpages éventuels).
"""),

8 : _("""
 Vous faites un calcul de thermique sans résolution stationnaire et sans
 non plus de résolution transitoire.

 Conseils :
   Renseignez la discrétisation temporelle par le mot clé INCREMENT
"""),

9 : _("""
 Attention, en cas d'erreur (contact, loi de comportement, pilotage, ...), le pas de temps
 ne sera pas re-découpé.
"""),


10 : _("""
 On ne peut définir qu'une seule occurrence de ECHEC/EVENEMENT='ERREUR'.
"""),

14 : _("""
 Attention : avec MODE_CALCUL_TPLUS = 'IMPLEX', on doit demander le calcul à tous les instants
 (EVENEMENT='TOUT_INST')
"""),

15 : _("""
 Attention : MODE_CALCUL_TPLUS = 'IMPLEX' ne permet qu'un mot clé ADAPTATION
"""),

40 : _("""
  Le solveur <%(k1)s> ne permet pas la détection de singularité.
  La découpe du pas de temps en cas d'erreur (pivot nul) n'est donc pas possible.
"""),

50 : _("""
  Déclenchement de l'événement <%(k1)s>.
"""),

51 : _("""
  Traitement de l'événement <%(k1)s>.
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

92 : _("""
 NUME_INST_INIT est plus grand que NUME_INST_FIN
"""),

94 : _("""
  -> Le numéro d'ordre correspondant à l'instant final de calcul NUME_INST_FIN
     n'appartient pas à la liste des numéros d'ordre.
     Dans ce cas, Aster considère pour numéro d'ordre final, le dernier de
     la liste fournie.
  -> Risque & Conseil :
     Afin d'éviter des pertes de résultats, assurez-vous que le numéro d'ordre
     associé à l'instant NUME_INST_FIN appartienne bien à la liste des numéros
     d'ordre.
"""),


}
