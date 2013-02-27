#@ MODIF modal Messages  DATE 26/02/2013   AUTEUR BOITEAU O.BOITEAU 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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

cata_msg={

1: _(u"""
Arrêt du calcul des modes : pas de mode propre dans la bande de fréquence demandée.
"""),
2: _(u"""
Opérateur MACRO_MODE_MECA
Pas de test de Sturm demandé (VERI_MODE=_F(STURM='NON')).
Donc, à l'issu de chaque calcul modal sur une sous-bande, on vérifie seulement que:
   - la norme de l'erreur est bien valide (via le paramètre VERI_MODE/SEUIL),
   - chaque fréquence est bien incluse dans la bande spécifiée (VERI_MODE/PREC_SHIFT).
Pas de test de Sturm local ou global.
"""),
3: _(u"""
Opérateur MACRO_MODE_MECA
Test de Sturm local demandé (VERI_MODE=_F(STURM='LOCAL')).
Donc, à l'issu de chaque calcul modal sur une sous-bande, on vérifie que:
    - la norme de l'erreur est bien valide (via le paramètre VERI_MODE/SEUIL),
    - chaque fréquence est bien incluse dans la bande spécifiée (VERI_MODE/PREC_SHIFT),
    - le test de Sturm est valide.
"""),
4: _(u"""
Opérateur MACRO_MODE_MECA
Test de Sturm global demandé (VERI_MODE=_F(STURM='GLOBAL')).
Donc, à l'issu de chaque calcul modal sur une sous-bande, on vérifie que:
    - la norme de l'erreur est bien valide (via le paramètre VERI_MODE/SEUIL),
    - chaque fréquence est bien incluse dans la bande spécifiée (VERI_MODE/PREC_SHIFT),
    - pas de test de Sturm local.
Puis, on réunit les fréquences calculées sur toutes les sous-bandes et on fait un test
de Sturm global:
  Dans l'intervalle (%(r1)f,%(r2)f), il y a théoriquement %(i1)d fréquence(s) et on
  en a bien calculé %(i2)d.
"""),
5: _(u"""
Opérateur MACRO_MODE_MECA, test de Sturm global:
  Dans l'intervalle (%(r1)f,%(r2)f), il y a théoriquement %(i1)d fréquence(s) et on
  en a calculé %(i2)d.
  On a décalé la bande de calcul des modes de PREC_SHIFT=%(r3)f %%.

  Conseils:
    * La bande retenue pour le test de Sturm englobe la bande de calcul. Elle est peut-être
      trop grande et donc elle englobe d'autres modes. Vous pouvez la réduire en diminuant la
      valeur de VERI_MODE/PREC_SHIFT.
    * Vous pouvez relancer le calcul en demandant à faire le test de Sturm de post-vérification
      sur chaque sous-bande (VERI_MODE=_F(STURM='LOCAL')). Cela peut aider à trouver le problème:
      mode multiple sauté, borne trop proche d'un mode multiple, mode de corps rigide...
    * Vous pouvez aussi relancez un INFO_MODE et/ou des MODE_ITER_SIMULT sur une sous-partie pour
      corroborer (ou non) les résultats précédent.
"""),
6: _(u"""
Opérateur MACRO_MODE_MECA, la sous-bande n %(i1)d est vide, on passe à la suivante.
"""),
7: _(u"""
Opérateur MACRO_MODE_MECA: Test de Sturm global.
  On n'a pas pu faire ce test car la bande de fréquence demandée (%(r1)f,%(r2)f) est vide !
"""),
8: _(u"""
Opérateur MACRO_MODE_MECA:
  Pas de mode propre dans la bande de fréquence demandée (%(r1)f,%(r2)f) !
  Le concept résultat sera vide.
"""),
9: _(u"""
Opérateur MACRO_MODE_MECA:
  Le nombre de processeurs, %(i1)d, et le nombre de sous-bandes fréquentielles non vides, %(i2)d,
  sont incompatibles !
  Avec le solveur linéaire MUMPS, ce nombre de processeurs peut être supérieur ou égale
  (idéalement proportionnel) au nombre de sous-bandes.
  Avec les autres solveurs linéaires ces deux paramètres doivent être rigoureusement égaux.
  Ici le solveur linéaire utilisé est %(k1)s.
  
  Conseil:
    * Ajuster le nombre de processeurs et/ou la distribution des sous-bandes et/ou
      le solveur linéaire en conséquence.
"""),
10: _(u"""
Opérateur MACRO_MODE_MECA:
  Le nombre de processeurs, %(i1)d, et le nombre de fréquences, %(i2)d, sont incompatibles !
  Avec le solveur linéaire MUMPS, ce nombre de processeurs peut être supérieur ou égale
  (idéalement proportionnel) au nombre de fréquences - 1.
  Avec les autres solveurs linéaires ces deux paramètres doivent être rigoureusement égaux.
  Ici le solveur linéaire utilisé est %(k1)s.

  Conseil:
    * Ajuster le nombre de processeurs et/ou la distribution des sous-bandes et/ou
      le solveur linéaire en conséquence.
"""),
11: _(u"""
Opérateur MACRO_MODE_MECA:
  Chacune des %(i1)d fréquences (autre que l'initiale) utilise le solveur linéaire
  MUMPS sur son propre paquet de processeurs.
  Cependant ces occurrences MUMPS sont parallélisées sur un nombre de processeurs
  différent, %(i2)d ou %(i3)d, d'où un potentiel déséquilibre de charge.
  Le temps d'exécution du calcul n'est alors probablement pas optimal !  
  
  Conseil:
    * Ajuster le nombre de processeurs et/ou la distribution des sous-bandes 
      en conséquence. Par exemple, un nombre de processeurs = %(i1)d  x 2, 4 ou 8.
"""),
12: _(u"""
Opérateur MACRO_MODE_MECA:
  Chacune des %(i1)d sous-bandes fréquentielles non vides utilise, indépendamment des autres,
  le solveur linéaire parallèle MUMPS.
  Cependant ces occurrences de solveur linéaires sont parallélisées sur un nombre de processeurs
  différent, %(i2)d ou %(i3)d, d'où un potentiel déséquilibre de charge.
  Le temps d'exécution du calcul n'est alors probablement pas optimal !  
  
  Conseil:
    * Ajuster le nombre de processeurs et/ou la distribution des sous-bandes 
      en conséquence. Par exemple, un nombre de processeurs = %(i1)d  x 2, 4 ou 8.
"""),
13: _(u"""
Opérateur MACRO_MODE_MECA:
  Chacune des 2 fréquences du test de Sturm de post-vérification utilise le solveur linéaire
  MUMPS sur son propre paquet de processeurs.
  Cependant ces occurrences MUMPS sont parallélisées sur un nombre de processeurs
  différent, %(i1)d ou %(i2)d, d'où un potentiel déséquilibre de charge.
  Le temps d'exécution du calcul n'est alors probablement pas optimal !  
  
  Conseil:
    * Idéalement, le nombre de processeurs devrait être pair.
"""),
14: _(u"""
Opérateur MACRO_MODE_MECA:
  Vous avez demandé la parallélisation, sur %(i1)d processeurs, de la partie solveur linéaire
   de votre calcul. Mais vous avez paramétré un solveur linéaire séquentiel: %(k1)s !
  
  Conseils:
    * Lancez votre calcul en séquentiel,
    * Changez votre solveur linéaire pour MUMPS (mot-clé facteur SOLVEUR + METHODE='MUMPS' + 
       plus pour de meilleures performances en modal GESTION_MEMOIRE='IN_CORE' + RENUM='QAMD'),
    * Changez le niveau de parallélisme (mot-clé NIVEAU_PARALLELISME='TOTAL').
"""),
}
