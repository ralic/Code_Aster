#@ MODIF xfem2 Messages  DATE 30/04/2007   AUTEUR ABBAS M.ABBAS 
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


5: _("""
Le vecteur TAU1 correspondant la première direction du frottement dans l'élément XFEM est nul.
Ceci signifie que les gradients des level sets sont surement colinéaires en ce point.
"""),

6: _("""
Multifissuration interdite avec l'opérateur PROPA_XFEM.
"""),

7: _("""
La structure de données du contact liée à MODI_MODELE_XFEM n'est pas
celle transmise en argument de MECA_NON_LINE.
"""),

44: _("""
Le champ de nom %(k1)s n'a pas été créé car aucun TYPE_ELEM du LIGREL de nom %(k2)s
ne connait le paramètre de l'option %(k3)s.
Contactez les développeurs.
"""),

51: _("""
Il n'y a aucune maille enrichie.
"""),

}
