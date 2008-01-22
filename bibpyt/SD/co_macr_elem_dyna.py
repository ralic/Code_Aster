#@ MODIF co_macr_elem_dyna SD  DATE 17/01/2008   AUTEUR ZENTNER I.ZENTNER 
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

def VALE_triang2array(vect_VALE, dim, typ):
   """Conversion (par recopie) de l'objet .VALE decrivant une matrice pleine
   par sa triangulaire sup en Numeric.array plein.
   """
   triang_sup = Numeric.array(vect_VALE)
   assert dim*(dim+1)/2 == len(triang_sup), \
         'Matrice non pleine : %d*(%d+1)/2 != %d' % (dim, dim, len(triang_sup))

   valeur = Numeric.zeros([dim, dim], typ)
   for i in range(1, dim+1):
     for j in range(1, i+1):
       k = i*(i-1)/2 + j
       valeur[j-1, i-1]=triang_sup[k-1]
   valeur = valeur + Numeric.transpose(valeur)
   for i in range(dim):
      valeur[i, i] = 0.5 * valeur[i, i]

   return valeur

# -----------------------------------------------------------------------------
class macr_elem_dyna(ASSD, sd_macr_elem_dyna):
   def NBRE_MODES(self) :
      """ retourne le nombre de modes total, dynamiques et d'interface """
      if self.par_lot() :
         raise Accas.AsException("Erreur dans macr_elem_dyna.NBRE_MODES en PAR_LOT='OUI'")
      nombase = self.MAEL_REFE.get()[0]
      nbmode=Numeric.array(aster.getvectjev('%-19s' % nombase[0:8] + '.UTIL'))
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

      if (typmat=='MASS_GENE') :
         macr_elem = self.MAEL_MASS
      elif (typmat=='RIGI_GENE') :
         macr_elem = self.MAEL_RAID
      elif (typmat=='AMOR_GENE') :
         macr_elem = self.MAEL_AMOR
      else:
         raise Accas.AsException("Le type de la matrice est incorrect")

      desc=Numeric.array(macr_elem.DESC.get())
      # On teste si le DESC du vecteur existe
      if (desc==None):
         raise Accas.AsException("L'objet matrice n'existe pas ou est mal cree par Code Aster")

      matrice = VALE_triang2array(macr_elem.VALE.get(), desc[1], Numeric.Float)
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

      nommacr=self.get_name()
      if (typmat=='MASS_GENE') :
         macr_elem = self.MAEL_MASS
      elif (typmat=='RIGI_GENE') :
         macr_elem = self.MAEL_RAID
      elif (typmat=='AMOR_GENE') :
         macr_elem = self.MAEL_AMOR
      else:
         raise Accas.AsException("Le type de la matrice est incorrect")
      nom_vale = macr_elem.VALE.nomj()
      desc=Numeric.array(macr_elem.DESC.get())

      # On teste si le DESC de la matrice jeveux existe
      if (desc==None):
         raise Accas.AsException("L'objet matrice n'existe pas ou est mal cree par Code Aster")
      Numeric.asarray(matrice)

      # On teste si la matrice python est de dimension 2
      if (len(Numeric.shape(matrice))<>2):
         raise Accas.AsException("La dimension de la matrice est incorrecte")

      # On teste si les tailles de la matrice jeveux et python sont identiques
      if (tuple([desc[1],desc[1]])<>Numeric.shape(matrice)) :
         raise Accas.AsException("La dimension de la matrice est incorrecte")
      taille=desc[1]*desc[1]/2.0+desc[1]/2.0
      tmp=Numeric.zeros([int(taille)],Numeric.Float)
      for j in range(desc[1]+1):
         for i in range(j):
            k=j*(j-1)/2+i
            tmp[k]=matrice[j-1,i]
      aster.putvectjev(nom_vale,len(tmp),tuple((
         range(1,len(tmp)+1))),tuple(tmp),tuple(tmp),1)
      return

