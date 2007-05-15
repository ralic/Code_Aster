#@ MODIF co_vect_asse_gene SD  DATE 16/05/2007   AUTEUR COURTOIS M.COURTOIS 
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

import Accas
from SD import *
from sd_cham_gene import sd_cham_gene

import Numeric

# -----------------------------------------------------------------------------

class vect_asse_gene(ASSD, sd_cham_gene):
   def EXTR_VECT_GENE_R(self) :
      """ retourne les valeurs du vecteur generalisee
      dans un format Numerical Array
         Attributs retourne
            - self.valeurs : Numeric.array contenant les valeurs """
      if self.par_lot():
         raise Accas.AsException("Erreur dans vect_asse_gene_r.EXTR_VECT_GENE en PAR_LOT='OUI'")
      ncham=self.get_name()
      ncham=ncham+(8-len(ncham))*' '
      valeur=Numeric.array(aster.getvectjev(ncham+(19-len(ncham))*' '+'.VALE'))
      return valeur

   def RECU_VECT_GENE_R(self,vecteur) :
      """ envoie les valeurs d'un Numerical Array dans un vecteur generalise
      reel definie dans jeveux
         Attributs ne retourne rien """
      if self.par_lot():
         raise Accas.AsException("Erreur dans vect_asse_gene_r.RECU_VECT_GENE en PAR_LOT='OUI'")
      from Utilitai.Utmess import UTMESS

      # avertissement generique
      UTMESS('A','RECU_VECT_GENE',' ATTENTION, VOUS ALLEZ ECRASER DES CONCEPTS EXISTANTS')

      Numeric.asarray(vecteur)
      ncham=self.get_name()
      ncham=ncham+(8-len(ncham))*' '
      desc=Numeric.array(aster.getvectjev(ncham+(19-len(ncham))*' '+'.DESC'))
      # On teste si le DESC du vecteur existe
      if (desc==None):
         raise Accas.AsException("L'objet vecteur n'existe pas ou \
         est mal cree par Code Aster")
      # On teste si la taille du vecteur jeveux et python est identique
      if desc[1]<>Numeric.shape(vecteur)[0] :
         raise Accas.AsException("La taille du vecteur python est incorrecte")
      aster.putvectjev(ncham+(19-len(ncham))*' '+'.VALE',len(vecteur),tuple((\
      range(1,len(vecteur)+1))),tuple(vecteur),tuple(vecteur),1)
      return

   def EXTR_VECT_GENE_C(self) :
      """ retourne les valeurs du vecteur generalisee
      dans un format Numerical Array
         Attributs retourne
            - self.valeurs : Numeric.array contenant les valeurs """
      if self.par_lot():
         raise Accas.AsException("Erreur dans vect_asse_gene_c.EXTR_VECT_GENE en PAR_LOT='OUI'")

      ncham=self.get_name()
      ncham=ncham+(8-len(ncham))*' '
      valeur=Numeric.array(aster.getvectjev(ncham+(19-len(ncham))*' '+'.VALE'),Numeric.Complex)

      return valeur

   def RECU_VECT_GENE_C(self,vecteur) :
      """ envoie les valeurs d'un Numerical Array dans un vecteur generalise
      complexe definie dans jeveux
         Attributs ne retourne rien """
      if self.par_lot():
         raise Accas.AsException("Erreur dans vect_asse_gene_c.RECU_VECT_GENE en PAR_LOT='OUI'")
      from Utilitai.Utmess import UTMESS

      # avertissement generique
      UTMESS('A','RECU_VECT_GENE',' ATTENTION, VOUS ALLEZ ECRASER DES CONCEPTS EXISTANTS')

      Numeric.asarray(vecteur)
      ncham=self.get_name()
      ncham=ncham+(8-len(ncham))*' '
      desc=Numeric.array(aster.getvectjev(ncham+(19-len(ncham))*' '+'.DESC'))
      # On teste si le DESC de la matrice existe
      if (desc==None):
         raise Accas.AsException("L'objet vecteur n'existe pas ou \
         est mal cree par Code Aster")
      # On teste si la taille de la matrice jeveux et python est identique
      if desc[1]<>Numeric.shape(vecteur)[0] :
         raise Accas.AsException("La taille du vecteur python est incorrecte")
      tmpr=vecteur.real
      tmpc=vecteur.imag
      aster.putvectjev(ncham+(19-len(ncham))*' '+'.VALE',len(tmpr),tuple((
      range(1,len(tmpr)+1))),tuple(tmpr),tuple(tmpc),1)
      return

