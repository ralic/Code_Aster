#@ MODIF jeveux1 Messages  DATE 30/06/2010   AUTEUR DELMAS J.DELMAS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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

 L'attribut %(k1)s est non modifiable ou déjà défini.

"""),

2 : _("""

 L'attribut %(k1)s est non modifiable ou déjà défini pour un objet simple.

"""),

3 : _("""

 L'attribut %(k1)s n'est pas compatible avec la valeur de LONT.

"""),

4 : _("""

 L'attribut %(k1)s n'est pas accessible ou non modifiable.

"""),

5 : _("""

 Pour une collection contigüe, il faut définit %(k1)s dans l'ordre de création des objets.

"""),

6 : _("""

 L'attribut %(k1)s n'est pas modifiable ou déjà défini (attribut LONO non nul).

"""),

7 : _("""

 L'attribut %(k1)s est incompatible avec la valeur initiale de LONT.

"""),

8 : _("""

 Le premier argument %(k1)s n'est pas du bon type (différent de CHARACTER).

"""),

9 : _("""

 L'appel est invalide pour l'objet simple "%(k1)s".

"""),

10 : _("""

 Le nom de l'attribut est incompatible avec le genre %(k1)s.

"""),

}
