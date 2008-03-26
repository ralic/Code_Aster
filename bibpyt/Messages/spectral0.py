#@ MODIF spectral0 Messages  DATE 26/03/2008   AUTEUR BODEL C.BODEL 
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

def _(x) : return x

cata_msg={
1: _("""
Nombre de fonctions erroné pour une matrice hermitienne.
"""),

2: _("""
%(k1)s: FREQ_MAX < FREQ_MIN
"""),

3: _("""
Erreur dans les indices.
"""),

4: _("""
Le fichier %(k1)s est introuvable.
"""),

5: _("""
La dimension DIM n est pas précisée dans le fichier lu.
"""),

6: _("""
Nombre de fonctions incorrect.
"""),

7: _("""
Erreur dans les données de fonctions.
"""),

8: _("""
Pas de calcul absolu avec TRAN_GENE.
"""),

9: _("""
Le fichier IDEAS est vide ou ne contient pas le data set demande
"""),

10: _("""
Un des data sets 58 contient une donnee qui n'est pas un inter-spectre
"""),

11: _("""
On ne traite pas les cas ou les abscisses frequentielles ne sont pas reglierement espacees
"""),

12: _("""
Le mot-cle format correspond au format du fichier source, qui peut etre 'ASTER' ou 'IDEAS' (pour lire les DS58)
"""),


}
