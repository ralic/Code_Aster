#@ MODIF co_matr_asse_gene SD  DATE 11/05/2010   AUTEUR COURTOIS M.COURTOIS 
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
from sd_matr_asse_gene import sd_matr_asse_gene

import numpy
import math

def VALM_triang2array(dict_VALM, dim, dtype=None):
   """Conversion (par recopie) de l'objet .VALM decrivant une matrice pleine
   par sa triangulaire inf (et parfois triang sup) en numpy.array plein.
   """
   # stockage symetrique ou non (triang inf+sup)
   sym = len(dict_VALM) == 1
   triang_sup = numpy.array(dict_VALM[1])
   assert dim*(dim+1)/2 == len(triang_sup), \
         'Matrice non pleine : %d*(%d+1)/2 != %d' % (dim, dim, len(triang_sup))
   if sym:
      triang_inf = triang_sup
   else:
      triang_inf = numpy.array(dict_VALM[2])
   valeur=numpy.zeros([dim, dim], dtype=dtype)
   for i in range(1, dim+1):
     for j in range(1, i+1):
       k = i*(i-1)/2 + j
       valeur[i-1, j-1]=triang_inf[k-1]
       valeur[j-1, i-1]=triang_sup[k-1]
   return valeur

def VALM_diag2array(dict_VALM, dim, dtype=None):
   """Conversion (par recopie) de l'objet .VALM decrivant une matrice
   diagonale en numpy.array plein.
   """
   diag = numpy.array(dict_VALM[1])
   assert dim == len(diag), 'Dimension incorrecte : %d != %d' % (dim, len(diag))
   valeur=numpy.zeros([dim, dim], dtype=dtype)
   for i in range(dim):
      valeur[i,i] =  diag[i]
   return valeur

# -----------------------------------------------------------------------------
class matr_asse_gene(ASSD, sd_matr_asse_gene):
   pass

# -----------------------------------------------------------------------------
class matr_asse_gene_r(matr_asse_gene):
  def EXTR_MATR_GENE(self) :
    """ retourne les valeurs de la matrice generalisee reelle
    dans un format numpyal Array
        Attributs retourne
          - self.valeurs : numpy.array contenant les valeurs """
    if not self.accessible():
       raise Accas.AsException("Erreur dans matr_asse_gene.EXTR_MATR_GENE en PAR_LOT='OUI'")

    desc=numpy.array(self.DESC.get())
    # On teste si le DESC de la matrice existe
    if (desc==None):
       raise Accas.AsException("L'objet matrice n'existe pas ou est mal cree par Code Aster")
    # Si le stockage est plein
    if desc[2]==2 :
       valeur = VALM_triang2array(self.VALM.get(), desc[1])

    # Si le stockage est diagonal
    elif desc[2]==1 :
       valeur = VALM_diag2array(self.VALM.get(), desc[1])

    # Sinon on arrete tout
    else:
      raise KeyError
    return valeur

  def RECU_MATR_GENE(self,matrice) :
    """ envoie les valeurs d'un tableau numpy dans des matrices
    generalisees reelles definies dans jeveux
        Attributs ne retourne rien """
    if not self.accessible():
       raise Accas.AsException("Erreur dans matr_asse_gene.RECU_MATR_GENE en PAR_LOT='OUI'")

    ncham=self.get_name()
    desc=numpy.array(self.DESC.get())

    # On teste si le DESC de la matrice existe
    if (desc==None):
       raise Accas.AsException("L'objet matrice n'existe pas ou est mal cree par Code Aster")
    numpy.asarray(matrice)

    # On teste si la dimension de la matrice python est 2
    if (len(numpy.shape(matrice))<>2) :
       raise Accas.AsException("La dimension de la matrice est incorrecte ")

    # On teste si les tailles des matrices jeveux et python sont identiques
    if (tuple([desc[1],desc[1]])<>numpy.shape(matrice)) :
       raise Accas.AsException("La taille de la matrice est incorrecte ")

    # Si le stockage est plein
    if desc[2]==2 :
      taille=desc[1]*desc[1]/2.0+desc[1]/2.0
      tmp=numpy.zeros([int(taille)])
      for j in range(desc[1]+1):
        for i in range(j):
          k=j*(j-1)/2+i
          tmp[k]=matrice[j-1,i]
      aster.putcolljev('%-19s.VALM' % ncham,len(tmp),tuple((\
      range(1,len(tmp)+1))),tuple(tmp),tuple(tmp),1)
    # Si le stockage est diagonal
    elif desc[2]==1 :
      tmp=numpy.zeros(desc[1])
      for j in range(desc[1]):
          tmp[j]=matrice[j,j]
      aster.putcolljev('%-19s.VALM' % ncham,len(tmp),tuple((\
      range(1,len(tmp)+1))),tuple(tmp),tuple(tmp),1)
    # Sinon on arrete tout
    else:
      raise KeyError
    return

# -----------------------------------------------------------------------------
class matr_asse_gene_c(matr_asse_gene):
  def EXTR_MATR_GENE(self) :
    """ retourne les valeurs de la matrice generalisee complexe
    dans un format numpy
        Attributs retourne
          - self.valeurs : numpy.array contenant les valeurs """
    if not self.accessible():
       raise Accas.AsException("Erreur dans matr_asse_gene_c.EXTR_MATR_GENE en PAR_LOT='OUI'")

    desc = numpy.array(self.DESC.get())
    if desc == None:
       raise Accas.AsException("L'objet matrice n'existe pas ou est mal cree par Code Aster ")
    # Si le stockage est plein
    if desc[2] == 2 :
       valeur = VALM_triang2array(self.VALM.get(), desc[1], complex)

    # Si le stockage est diagonal
    elif desc[2]==1 :
       valeur = VALM_diag2array(self.VALM.get(), desc[1], complex)

    # Sinon on arrete tout
    else:
       raise KeyError
    return valeur

  def RECU_MATR_GENE(self,matrice) :
    """ envoie les valeurs d'un tableau numpy dans des matrices
    generalisees reelles definies dans jeveux
        Attributs ne retourne rien """
    if not self.accessible():
       raise Accas.AsException("Erreur dans matr_asse_gene_c.RECU_MATR_GENE en PAR_LOT='OUI'")

    numpy.asarray(matrice)
    ncham=self.get_name()
    desc=numpy.array(self.DESC.get())

    # On teste si le DESC de la matrice existe
    if (desc==None):
       raise Accas.AsException("L'objet matrice n'existe pas ou est mal cree par Code Aster")
    numpy.asarray(matrice)

    # On teste si la dimension de la matrice python est 2
    if (len(numpy.shape(matrice))<>2) :
       raise Accas.AsException("La dimension de la matrice est incorrecte ")

    # On teste si la taille de la matrice jeveux et python est identique
    if (tuple([desc[1],desc[1]])<>numpy.shape(matrice)) :
       raise Accas.AsException("La taille de la matrice est incorrecte ")

    # Si le stockage est plein
    if desc[2]==2 :
      taille=desc[1]*desc[1]/2.0+desc[1]/2.0
      tmpr=numpy.zeros([int(taille)])
      tmpc=numpy.zeros([int(taille)])
      for j in range(desc[1]+1):
        for i in range(j):
          k=j*(j-1)/2+i
          tmpr[k]=matrice[j-1,i].real
          tmpc[k]=matrice[j-1,i].imag
      aster.putvectjev('%-19s.VALM' % ncham, len(tmpr), tuple((\
                       range(1,len(tmpr)+1))),tuple(tmpr),tuple(tmpc),1)
    # Si le stockage est diagonal
    elif desc[2]==1 :
      tmpr=numpy.zeros(desc[1])
      tmpc=numpy.zeros(desc[1])
      for j in range(desc[1]):
          tmpr[j]=matrice[j,j].real
          tmpc[j]=matrice[j,j].imag
      aster.putvectjev('%-19s.VALM' % ncham,len(tmpr),tuple((\
                       range(1,len(tmpr)+1))),tuple(tmpr),tuple(tmpc),1)
    # Sinon on arrete tout
    else:
      raise KeyError
    return
