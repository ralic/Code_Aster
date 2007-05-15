#@ MODIF co_macr_elem_dyna SD  DATE 16/05/2007   AUTEUR COURTOIS M.COURTOIS 
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
from sd_macr_elem_dyna import sd_macr_elem_dyna

import Numeric

# -----------------------------------------------------------------------------
class macr_elem_dyna(ASSD, sd_macr_elem_dyna):
   def NBRE_MODES(self) :
      """ retourne le nombre de modes total, dynamiques et d'interface """
      if self.par_lot() :
         raise Accas.AsException("Erreur dans macr_elem_dyna.NBRE_MODES en PAR_LOT='OUI'")
      nommacr=self.get_name()
      ncham=nommacr+(8-len(nommacr))*' '
      ncham=nommacr+(8-len(nommacr))*' '+'.MAEL'
      nombase=aster.getvectjev(ncham+'_REFE')[0]
      nbmode=Numeric.array(aster.getvectjev(nombase[0:8]+(19-len(nombase[0:8]))*' '+'.UTIL'))
      nbmodtot=nbmode[1]
      nbmoddyn=nbmode[2]
      nbmodint=nbmode[3]
      return [nbmodtot,nbmoddyn,nbmodint]

   def EXTR_MATR_GENE(self,typmat) :
      """ retourne les valeurs des matrices generalisees reelles
      dans un format Numerical Array
         typmat='MASS_GENE' pour obtenir la matrice de masse generalisee
         typmat='RIGI_GENE' pour obtenir la matrice de raideur generalisee
         typmat='AMOR_GENE' pour obtenir la matrice d'amortissement generalisee
         Attributs retourne
            - self.valeurs : Numeric.array contenant les valeurs """
      if self.par_lot() :
         raise Accas.AsException("Erreur dans macr_elem_dyna.EXTR_MATR_GENE en PAR_LOT='OUI'")

      nommacr=self.get_name()
      if (typmat=='MASS_GENE') :
         ext='.MAEL_MASS'
      elif (typmat=='RIGI_GENE') :
         ext='.MAEL_RAID'
      elif (typmat=='AMOR_GENE') :
         ext='.MAEL_AMOR'
      else:
         raise Accas.AsException("Le type de la matrice est incorrect")
      ncham=nommacr+(8-len(nommacr))*' '+ext
      desc=Numeric.array(aster.getvectjev(ncham+'_DESC'))

      # On teste si le DESC du vecteur existe
      if (desc==None):
         raise Accas.AsException("L'objet matrice n'existe pas ou \
         est mal cree par Code Aster")

      tmp=Numeric.array(aster.getvectjev(ncham+'_VALE'))
      matrice=Numeric.zeros([desc[1],desc[1]],Numeric.Float)
      for j in range(desc[1]+1):
         for i in range(j):
            k=j*(j-1)/2+i
            matrice[j-1,i]=tmp[k]
      matrice=(matrice+Numeric.transpose(matrice))
      for i in range(desc[1]):
         matrice[i,i]=0.5*matrice[i,i]
      return matrice

   def RECU_MATR_GENE(self,typmat,matrice) :
      """ envoie les valeurs d'un Numerical Array dans des matrices generalisees
      reelles definies dans jeveux
         typmat='MASS_GENE' pour obtenir la matrice de masse generalisee
         typmat='RIGI_GENE' pour obtenir la matrice de raideur generalisee
         typmat='AMOR_GENE' pour obtenir la matrice d'amortissement generalisee
         Attributs ne retourne rien """
      if self.par_lot() :
         raise Accas.AsException("Erreur dans macr_elem_dyna.RECU_MATR_GENE en PAR_LOT='OUI'")
      from Utilitai.Utmess import UTMESS

      # avertissement generique
      UTMESS('A','RECU_MATR_GENE',' ATTENTION, VOUS ALLEZ ECRASER DES CONCEPTS EXISTANTS')

      nommacr=self.get_name()
      if (typmat=='MASS_GENE') :
         ext='.MAEL_MASS'
      elif (typmat=='RIGI_GENE') :
         ext='.MAEL_RAID'
      elif (typmat=='AMOR_GENE') :
         ext='.MAEL_AMOR'
      else:
         raise Accas.AsException("Le type de la matrice \
                                 est incorrect")
      ncham=nommacr+(8-len(nommacr))*' '+ext
      desc=Numeric.array(aster.getvectjev(ncham+'_DESC'))

      # On teste si le DESC de la matrice jeveux existe
      if (desc==None):
         raise Accas.AsException("L'objet matrice n'existe pas ou \
                                 est mal cree par Code Aster")
      Numeric.asarray(matrice)

      # On teste si la matrice python est de dimension 2
      if (len(Numeric.shape(matrice))<>2):
         raise Accas.AsException("La dimension de la matrice \
                                 est incorrecte")

      # On teste si les tailles de la matrice jeveux et python sont identiques
      if (tuple([desc[1],desc[1]])<>Numeric.shape(matrice)) :
         raise Accas.AsException("La dimension de la matrice \
                                 est incorrecte")
      taille=desc[1]*desc[1]/2.0+desc[1]/2.0
      tmp=Numeric.zeros([int(taille)],Numeric.Float)
      for j in range(desc[1]+1):
         for i in range(j):
            k=j*(j-1)/2+i
            tmp[k]=matrice[j-1,i]
      aster.putvectjev(ncham+'_VALE',len(tmp),tuple((
      range(1,len(tmp)+1))),tuple(tmp),tuple(tmp),1)
      return

