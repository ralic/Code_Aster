#@ MODIF co_matr_asse SD  DATE 16/05/2011   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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

from SD import *
from sd_matr_asse import sd_matr_asse
from sd_nume_ddl  import sd_nume_ddl
from SD.sd_stoc_morse import sd_stoc_morse
import numpy

# -----------------------------------------------------------------------------
class matr_asse(ASSD, sd_matr_asse):


  def EXTR_MATR(self) :
    """ retourne les valeurs de la matrice  dans un format numpy
        Attributs retourne
          - self.valeurs : numpy.array contenant les valeurs """

    if not self.accessible():
       raise Accas.AsException("Erreur dans matr_asse.EXTR_MATR en PAR_LOT='OUI'")

    refa = numpy.array(self.REFA.get())
    ma=refa[0]
    nu=refa[1]
    smos = sd_stoc_morse(nu[:14]+'.SMOS')

    valm=self.VALM.get()
    smhc=smos.SMHC.get()
    smdi=smos.SMDI.get()

    sym=len(valm)==1
    dim=len(smdi)
    nnz=smdi[dim-1]

    triang_sup = numpy.array(valm[1])
    if sym:
      triang_inf = triang_sup
    else:
      triang_inf = numpy.array(valm[2])

    if type(valm[1][0]) == type(1.j) :
       dtype=complex
    else :
       dtype=float
    valeur=numpy.zeros([dim, dim], dtype=dtype)

    jcol=1
    for kterm in range(1,nnz+1):
      ilig=smhc[kterm-1]
      if (smdi[jcol-1] < kterm) : jcol=jcol+1

      valeur[jcol-1,ilig-1]=triang_inf[kterm-1]
      valeur[ilig-1,jcol-1]=triang_sup[kterm-1]



    return valeur
