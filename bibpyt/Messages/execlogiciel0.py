#@ MODIF execlogiciel0 Messages  DATE 22/04/2010   AUTEUR ASSIRE A.ASSIRE 
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
1 : _("""
Format Salome, l'argument 1 doit etre 
le nom du fichier med produit par le script python.
"""),

2 : _("""
On ne sait pas traiter le format %(k1)s
"""),

3 : _("""
Code retour incorrect (MAXI %(i1)d) : %(i2)d

"""),

4 : _("""
Le mot-cle logiciel n'est pas utilise avec ce format.
"""),

5 : _("""
Erreurs lors de l'execution du fichier ci-dessous :
<<<<<<<<<<<<<<< DEBUT DU FICHIER >>>>>>>>>>>>>>>
%(k1)s
<<<<<<<<<<<<<<<  FIN  DU FICHIER >>>>>>>>>>>>>>>
"""),

6 : _("""
Le fichier %(k1)s n'existe pas.
"""),

7 : _("""
Mode de lancement inconnu : %(k1)s
"""),

8 : _("""
----------------------------------------------------------------------------------
 Commande :
   %(k1)s
"""),

9 : _("""
----- Sortie standard (stdout) ---------------------------------------------------
%(k1)s
----- fin stdout -----------------------------------------------------------------
"""),

10 : _("""
----- Sortie erreur standard (stderr) --------------------------------------------
%(k1)s
----- fin stderr -----------------------------------------------------------------
"""),

11 : _("""
 Code retour = %(i1)d      (maximum toléré : %(i2)d)
"""),

#12 : _(""" """),

13 : _("""
 -> Le maillage n'a pas été produit par le logiciel externe (format "%(k1)s")
 
 -> Conseil :
    Vous devriez trouver ci-dessus des messages du logiciel en question
    expliquant les raisons de cet échec.
"""),

}
