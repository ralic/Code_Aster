#@ MODIF jeveux Messages  DATE 26/09/2006   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
     REOUVERTURE DE LA BASE                  :  %(k1)s  
     CREEE AVEC LA VERSION                   :  %(k2)s                                                                        
     NOMBRE D'ENREGISTREMENTS UTILISES       :  %(i1)d                                                                                       
     NOMBRE D'ENREGISTREMENTS MAXIMUM        :  %(i2)d                                                                                     
     LONGUEUR D'ENREGISTREMENT (OCTETS)      :  %(i3)d                                                                              
     NOMBRE D'IDENTIFICATEURS UTILISES       :  %(i4)d                                                                                 
     TAILLE MAXIMUM DU REPERTOIRE            :  %(i5)d                                                                                 
     POURCENTAGE D'UTILISATION DU REPERTOIRE :  %(i6)d %%                                                                                  
"""),

2: _(""" 
     NOM DE LA BASE                          :  %(k1)s                                                                                
     NOMBRE D'ENREGISTREMENTS UTILISES       :  %(i1)d                                                                         
     NOMBRE D'ENREGISTREMENTS MAXIMUM        :  %(i2)d                                                                         
     LONGUEUR D'ENREGISTREMENT (OCTETS)      :  %(i3)d                                                                         
     NOMBRE TOTAL D'ENTREES/SORTIES          :  %(i4)d                                                                         
     NOMBRE D'IDENTIFICATEURS UTILISES       :  %(i5)d                                                                         
     TAILLE MAXIMUM DU REPERTOIRE            :  %(i6)d                                                                         
     POURCENTAGE D'UTILISATION DU REPERTOIRE :  %(i7)d %%                                                                              
"""),

}


