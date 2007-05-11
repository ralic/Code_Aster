#@ MODIF sd_melasflu SD  DATE 09/05/2007   AUTEUR PELLET J.PELLET 
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

from SD import *

from SD.sd_l_table import sd_l_table
from SD.sd_table import sd_table
from SD.sd_cham_no import sd_cham_no
from SD.sd_matr_asse_gene import sd_matr_asse_gene
from SD.sd_type_flui_stru import sd_type_flui_stru
from SD.sd_resultat_dyn import sd_resultat_dyn
from SD.sd_util import *


class sd_melasflu(AsBase):
#-------------------------------
    nomj = SDNom(fin=8)

    MASG = AsVR(SDNom(debut=19), )
    VITE = AsVR(SDNom(debut=19), )
    REMF = AsVK8(SDNom(debut=19), lonmax=2, )
    FREQ = AsVR(SDNom(debut=19), )
    NUMO = AsVI(SDNom(debut=19))
    FACT = AsVR(SDNom(debut=19), )
    DESC = AsVK16(SDNom(debut=19), lonmax=1, )

    # Critère présence VCN et VEN ????
    VCN  = Facultatif(AsVR())
    VEN  = Facultatif(AsVR())

    sd_l_table = Facultatif(sd_l_table(SDNom(nomj=''))) # Si faisceau axial
    sd_table   = sd_table(SDNom(nomj=''))


    # indirections via .REMF :
    #----------------------------------
    def check_melasflu_i_REMF(self, checker):
        lnom=self.REMF.get()
        sd2 = sd_type_flui_stru(lnom[0]) ; sd2.check(checker)
        sd2 = sd_resultat_dyn(lnom[1]) ; sd2.check(checker)


    # Vérifications supplémentaires :
    #----------------------------------
    def check_veri1(self, checker):
        desc=self.DESC.get()
        numo=self.NUMO.get()
        fact=self.FACT.get()
        freq=self.FREQ.get()
        masg=self.MASG.get()
        vite=self.VITE.get()
        vcn=self.VCN.get()
        ven=self.VEN.get()
        l_table = sd_l_table(self.nomj) # Si faisceau axial

        # calcul de itypfl (type d'interaction fluide / structure) :
        typfl = sd_type_flui_stru(self.REMF.get()[0])
        itypfl=typfl.FSIC.get()[0]

        # calcul de nbmode (nombre de modes) :
        nbmode=len(numo)

        # calcul de nbvite (nombre de vitesses) :
        nbvite=len(vite)

        # vérification de l'objet .DESC :
        #--------------------------------
        sdu_compare(self.DESC,checker,desc[0].strip(),'==','DEPL','DESC[1]=="DEPL"')

        # vérification de l'objet .NUMO :
        #--------------------------------
        sdu_tous_compris(self.NUMO,checker,vmin=1)

        # vérification de l'objet .FACT :
        #--------------------------------
        if itypfl==3 :  # faisceau axial
            sdu_compare(self.FACT,checker,len(fact),'==',3*nbmode*nbvite,'LONMAX(FACT)==3*nbmode*nbvite')
        else :
            sdu_compare(self.FACT,checker,len(fact),'==',3*nbmode,'LONMAX(FACT)==3*nbmode')

        # vérification de l'objet .MASG :
        #--------------------------------
        if itypfl==3 :  # faisceau axial
            sdu_compare(self.MASG,checker,len(masg),'==',nbmode*nbvite,'LONMAX(MASG)==nbmode*nbvite')
        else :
            sdu_compare(self.MASG,checker,len(masg),'==',nbmode,'LONMAX(MASG)==nbmode')

        # vérification de l'objet .FREQ :
        #--------------------------------
        sdu_compare(self.FREQ,checker,len(freq),'==',2*nbmode*nbvite,'LONMAX(FREQ)==2*nbmode*nbvite')

        # vérification de l'objet .VCN :
        #--------------------------------
        if vcn :
            fsvi=typfl.FSVI.get()
            nbzex=len(fsvi)
            nbval=1
            for i in range(nbzex) :
                nbval=nbval+fsvi[nbzex+i]
            sdu_compare(self.VCN,checker,len(vcn),'==',nbmode,'LONMAX(VCN)==nbmode*nbval')

        # vérification de l'objet .VEN :
        #--------------------------------
        if ven :
            sdu_compare(self.VEN,checker,len(ven),'==',nbmode,'LONMAX(VEN)==nbmode')


        # vérification de la SD table contenant les cham_no :
        #----------------------------------------------------
        tcham=sd_table(self.nomj)
        sdu_compare(None,checker,tcham.nb_column(),'==',1,"1 seule colonne dans la table")
        col1=tcham.get_column_name('NOM_CHAM')
        sdu_assert(None, checker, col1, "Manque colonne NOM_CHAM")

        data=col1.data.get()
        mask=col1.mask.get()
        profchno=''
        for k in range(len(mask)):
            if not mask[k] : continue
            ch1=sd_cham_no(data[k])
            ch1.check(checker)

            # Tous les cham_no doivent avoir le meme prof_chno :
            profchn1=ch1.REFE.get()[1]
            if profchno == '':
                profchno=profchn1
            else:
                sdu_compare(None,checker,profchn1,'==',profchno,"Tous PROFCHNO identiques")


        # vérification de la SD l_table :
        #--------------------------------
        if itypfl==3 :  # faisceau axial
            l_table.check(checker)

            # la l_table ne contient qu'une seule table nommée 'MATR_GENE'
            sdu_compare(l_table.LTNT,checker,l_table.LTNT.lonuti,'==',1,"LONUTI(LTNT)==1")
            sdu_compare(l_table.LTNT,checker,l_table.LTNT.get()[0].strip(),'==','MATR_GENE',"LTNT[0]==MATR_GENE")

            # vérification de la table 'MATR_GENE' :
            tmatgen=sd_table(l_table.LTNS.get()[0])
            col1=tmatgen.get_column_name('NUME_VITE')
            sdu_assert(None, checker, col1, "Manque colonne NUME_VITE")
            col1=tmatgen.get_column_name('VITE_FLUI')
            sdu_assert(None, checker, col1, "Manque colonne VITE_FLUI")

            for x in 'MATR_RIGI', 'MATR_MASS', 'MATR_AMOR' :
                col1=tmatgen.get_column_name(x)
                sdu_assert(None, checker, col1, "Manque colonne : "+x)
                data=col1.data.get()
                mask=col1.mask.get()
                for k in range(len(mask)):
                    if not mask[k] : continue
                    matgen=sd_matr_asse_gene(data[k])
                    matgen.check(checker)


