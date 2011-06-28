#@ MODIF sd_table_container SD  DATE 28/06/2011   AUTEUR COURTOIS M.COURTOIS 
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

from SD.sd_table import sd_table, Colonne
from SD.sd_vect_elem import sd_vect_elem
from SD.sd_matr_elem import sd_matr_elem
from SD.sd_cham_elem import sd_cham_elem
from SD.sd_mode_meca import sd_mode_meca
# --------------------------------------------------------------------
# sd_table contenant les colonnes nommée "NOM_OBJET","TYPE_OBJET",
# et "NOM_SD"
# --------------------------------------------------------------------



class sd_table_container(sd_table):
#-------------------------------------
    nomj = SDNom(fin=19)

    def check_table_container(self, checker):

        # vérification de l'existence de la table
        if not self.exists() :
           checker.err(self,"La sd_table_container %s ne semble"
                             +"pas exister" %(nomj))

        # on vérifie la présence des paramètres
        # 'NOM_OBJET','TYPE_OBJET','NOM_SD'
        param=['NOM_OBJET','TYPE_OBJET','NOM_SD']
        shape = self.TBNP.get()
        assert shape[0]>2 # la table à au moins 3 paramètres
        for n in param:
          col=self.get_column_name(n)
          if col == None:
             checker.err(self,"Paramètre %s manquant!" %(n))

          # on vérifie que les colonnes ne sont pas vides
          data = col.data.get()
          if data is not None:
             if col.data.lonuti != shape[1]:
                  checker.err(self,"Taille inconsitante %d!=%d" %
                                (col.data.lonuti,shape[1]))


        # on vérifie le contenu de la colonne NOM_SD
        col1=self.get_column_name('TYPE_OBJET')
        col2=self.get_column_name('NOM_SD')
        nbli=col1.data.lonuti
        lnom1=col1.data.get_stripped()
        lnom2=col2.data.get_stripped()
        for k in range(nbli):
          if lnom1[k][:9]=='VECT_ELEM':
             sd5=sd_vect_elem(lnom2[k])
             sd5.check(checker)
          elif lnom1[k][:9]=='MATR_ELEM':
             sd5=sd_matr_elem(lnom2[k])
             sd5.check(checker)
          elif lnom1[k][:9]=='CHAM_ELEM':
             sd5=sd_cham_elem(lnom2[k])
             sd5.check(checker)
          elif lnom1[k][:11]=='MODE_MECA':
             sd5=sd_mode_meca(lnom2[k])
             sd5.check(checker)
          else:
             assert 0,lnom1[k]

