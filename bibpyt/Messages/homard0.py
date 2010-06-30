#@ MODIF homard0 Messages  DATE 30/06/2010   AUTEUR DELMAS J.DELMAS 
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
1: _("""
Cette macro commande est inconnue.
"""),

2: _("""
Erreur : %(k1)s
"""),

3: _("""
Impossible de tuer le fichier %(k1)s
"""),

4: _("""
Impossible de créer le répertoire de travail pour HOMARD : %(k1)s
"""),

5: _("""
Impossible de détruire le fichier :%(k1)s
"""),

6: _("""
La vérification de l'interpénétration peut etre très longue.
Il ne faut l'utiliser que volontairement. Voir la documentation.
"""),

7: _("""
Dès que le nombre de mailles est important, la vérification de l'interpénétration peut etre très longue.
En principe, on ne devrait l'utiliser que dans les cas suivants :
  . Informations sur un maillage avec MACR_INFO_MAIL
  . Debogage sur une adaptation avec MACR_ADAP_MAIL
Conseil :
Pour un usage courant de l'adaptation, il est recommandé de passer à NON toutes les
options de controle ; autrement dit, laisser les options par défaut.
"""),

}
