#@ MODIF co_matr_asse_gene SD  DATE 16/10/2007   AUTEUR REZETTE C.REZETTE 
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

import Numeric

# -----------------------------------------------------------------------------
class matr_asse_gene(ASSD, sd_matr_asse_gene):
   pass

# -----------------------------------------------------------------------------
class matr_asse_gene_r(matr_asse_gene):
  def EXTR_MATR_GENE(self) :
    """ retourne les valeurs de la matrice generalisee reelle
    dans un format Numerical Array
        Attributs retourne
          - self.valeurs : Numeric.array contenant les valeurs """
    if self.par_lot():
       raise Accas.AsException("Erreur dans matr_asse_gene.EXTR_MATR_GENE en PAR_LOT='OUI'")

    ncham=self.get_name()
    ncham=ncham+(8-len(ncham))*' '
    desc=Numeric.array(aster.getvectjev(ncham+(19-len(ncham))*' '+'.DESC'))
    # On teste si le DESC de la matrice existe
    if (desc==None):
       raise Accas.AsException("L'objet matrice n'existe pas ou \
                                est mal cree par Code Aster")
    # Si le stockage est plein
    if desc[2]==2 :
      tmp=Numeric.array(aster.getcolljev(ncham+(19-len(ncham))*' '+'.VALM')[1])
      valeur=Numeric.zeros([desc[1],desc[1]],Numeric.Float)
      for j in range(desc[1]+1):
        for i in range(j):
          k=j*(j-1)/2+i
          valeur[j-1,i]=tmp[k]
      valeur=(valeur+Numeric.transpose(valeur))
      for i in range(desc[1]):
        valeur[i,i]=0.5*valeur[i,i]
    # Si le stockage est diagonal
    elif desc[2]==1 :
      tmp=Numeric.array(aster.getcolljev(ncham+(19-len(ncham))*' '+'.VALM')[1])
      valeur=Numeric.zeros([desc[1],desc[1]],Numeric.Float)
      for i in range(desc[1]):
        valeur[i,i]=tmp[i]
    # Sinon on arrete tout
    else:
      raise KeyError
    return valeur

  def RECU_MATR_GENE(self,matrice) :
    """ envoie les valeurs d'un Numerical Array dans des matrices
    generalisees reelles definies dans jeveux
        Attributs ne retourne rien """
    if self.par_lot():
       raise Accas.AsException("Erreur dans matr_asse_gene.RECU_MATR_GENE en PAR_LOT='OUI'")
    from Utilitai.Utmess import UTMESS

    # avertissement generique
    UTMESS('A','SDVERI_3')

    ncham=self.get_name()
    ncham=ncham+(8-len(ncham))*' '
    desc=Numeric.array(aster.getvectjev(ncham+(19-len(ncham))*' '+'.DESC'))

    # On teste si le DESC de la matrice existe
    if (desc==None):
       raise Accas.AsException("L'objet matrice n'existe pas ou \
                                est mal cree par Code Aster")
    Numeric.asarray(matrice)

    # On teste si la dimension de la matrice python est 2
    if (len(Numeric.shape(matrice))<>2) :
       raise Accas.AsException("La dimension de la matrice est incorrecte ")

    # On teste si les tailles des matrices jeveux et python sont identiques
    if (tuple([desc[1],desc[1]])<>Numeric.shape(matrice)) :
       raise Accas.AsException("La taille de la matrice est incorrecte ")

    # Si le stockage est plein
    if desc[2]==2 :
      taille=desc[1]*desc[1]/2.0+desc[1]/2.0
      tmp=Numeric.zeros([int(taille)],Numeric.Float)
      for j in range(desc[1]+1):
        for i in range(j):
          k=j*(j-1)/2+i
          tmp[k]=matrice[j-1,i]
      aster.putcolljev(ncham+(19-len(ncham))*' '+'.VALM',len(tmp),tuple((\
      range(1,len(tmp)+1))),tuple(tmp),tuple(tmp),1)
    # Si le stockage est diagonal
    elif desc[2]==1 :
      tmp=Numeric.zeros(desc[1],Numeric.Float)
      for j in range(desc[1]):
          tmp[j]=matrice[j,j]
      aster.putcolljev(ncham+(19-len(ncham))*' '+'.VALM',len(tmp),tuple((\
      range(1,len(tmp)+1))),tuple(tmp),tuple(tmp),1)
    # Sinon on arrete tout
    else:
      raise KeyError
    return

# -----------------------------------------------------------------------------
class matr_asse_gene_c(matr_asse_gene):
  def EXTR_MATR_GENE(self) :
    """ retourne les valeurs de la matrice generalisee complexe
    dans un format Numerical Array
        Attributs retourne
          - self.valeurs : Numeric.array contenant les valeurs """
    if self.par_lot():
       raise Accas.AsException("Erreur dans matr_asse_gene_c.EXTR_MATR_GENE en PAR_LOT='OUI'")

    ncham=self.get_name()
    ncham=ncham+(8-len(ncham))*' '
    desc=Numeric.array(aster.getvectjev(ncham+(19-len(ncham))*' '+'.DESC'))
    if (desc==None):
       raise Accas.AsException("L'objet matrice n'existe pas ou \
       est mal cree par Code Aster ")
    # Si le stockage est plein
    if desc[2]==2 :
      tmp=Numeric.array(aster.getcolljev(ncham+(19-len(ncham))*' '+'.VALM')[1])
      valeur=Numeric.zeros([desc[1],desc[1]],Numeric.Complex)
      for j in range(desc[1]+1):
        for i in range(j):
          k=j*(j-1)/2+i
          valeur[j-1,i]=tmp[k]
      valeur=(valeur+Numeric.transpose(valeur))
      for i in range(desc[1]):
        valeur[i,i]=0.5*valeur[i,i]
    # Si le stockage est diagonal
    elif desc[2]==1 :
      tmp=Numeric.array(aster.getcolljev(ncham+(19-len(ncham))*' '+'.VALM')[1])
      valeur=Numeric.zeros([desc[1],desc[1]],Numeric.Complex)
      for i in range(desc[1]):
        valeur[i,i]=tmp[i]
    # Sinon on arrete tout
    else:
      raise KeyError
    return valeur

  def RECU_MATR_GENE(self,matrice) :
    """ envoie les valeurs d'un Numerical Array dans des matrices
    generalisees reelles definies dans jeveux
        Attributs ne retourne rien """
    if self.par_lot():
       raise Accas.AsException("Erreur dans matr_asse_gene_c.RECU_MATR_GENE en PAR_LOT='OUI'")
    from Utilitai.Utmess import UTMESS

    # avertissement generique
    UTMESS('A','SDVERI_3')

    Numeric.asarray(matrice)
    ncham=self.get_name()
    ncham=ncham+(8-len(ncham))*' '
    desc=Numeric.array(aster.getvectjev(ncham+(19-len(ncham))*' '+'.DESC'))

    # On teste si le DESC de la matrice existe
    if (desc==None):
       raise Accas.AsException("L'objet matrice n'existe pas ou \
       est mal cree par Code Aster")
    Numeric.asarray(matrice)

    # On teste si la dimension de la matrice python est 2
    if (len(Numeric.shape(matrice))<>2) :
       raise Accas.AsException("La dimension de la matrice est incorrecte ")

    # On teste si la taille de la matrice jeveux et python est identique
    if (tuple([desc[1],desc[1]])<>Numeric.shape(matrice)) :
       raise Accas.AsException("La taille de la matrice est incorrecte ")

    # Si le stockage est plein
    if desc[2]==2 :
      taille=desc[1]*desc[1]/2.0+desc[1]/2.0
      tmpr=Numeric.zeros([int(taille)],Numeric.Float)
      tmpc=Numeric.zeros([int(taille)],Numeric.Float)
      for j in range(desc[1]+1):
        for i in range(j):
          k=j*(j-1)/2+i
          tmpr[k]=matrice[j-1,i].real
          tmpc[k]=matrice[j-1,i].imag
      aster.putvectjev(ncham+(19-len(ncham))*' '+'.VALM',len(tmpr),tuple((\
                       range(1,len(tmpr)+1))),tuple(tmpr),tuple(tmpc),1)
    # Si le stockage est diagonal
    elif desc[2]==1 :
      tmpr=Numeric.zeros(desc[1],Numeric.Float)
      tmpc=Numeric.zeros(desc[1],Numeric.Float)
      for j in range(desc[1]):
          tmpr[j]=matrice[j,j].real
          tmpc[j]=matrice[j,j].imag
      aster.putvectjev(ncham+(19-len(ncham))*' '+'.VALM',len(tmpr),tuple((\
                       range(1,len(tmpr)+1))),tuple(tmpr),tuple(tmpc),1)
    # Sinon on arrete tout
    else:
      raise KeyError
    return
