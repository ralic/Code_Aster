# coding=utf-8
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

    # si FAISCEAU_TRANS + couplage fluide-structure + BASE_MODALE/AMOR_REDUIT_CONN :
    VCN  = Facultatif(AsVR())
    VEN  = Facultatif(AsVR())
    RAP  = Facultatif(AsVR())

    sd_table   = sd_table(SDNom(nomj=''))
    sd_l_table = Facultatif(sd_l_table(SDNom(nomj=''))) # Si FAISCEAU_AXIAL


    # indirections via .REMF :
    #----------------------------------
    def check_melasflu_i_REMF(self, checker):
        remf=self.REMF.get_stripped()
        sd2 = sd_type_flui_stru(remf[0]) ; sd2.check(checker)
        sd2 = sd_resultat_dyn(remf[1]) ; sd2.check(checker)


    # Vérifications supplémentaires :
    #----------------------------------
    def check_veri1(self, checker):
        remf=self.REMF.get()
        desc=self.DESC.get_stripped()

        # calcul de itypfl (type d'interaction fluide / structure) :
        typfl = sd_type_flui_stru(remf[0])
        itypfl=typfl.FSIC.get()[0]  # 1 -> FAISCEAU_TRANS
                                    # 3 -> FAISCEAU_AXIAL
        couplage=typfl.FSIC.get()[1]  # 1 -> prise en compte du couplage
        assert itypfl > 0 , remf

        # calcul de nbmode (nombre de modes) :
        nbmode=self.NUMO.lonmax
        assert nbmode > 0

        # calcul de nbvite (nombre de vitesses) :
        nbvite=self.VITE.lonmax
        assert nbvite > 0

        # vérification de l'objet .DESC :
        #--------------------------------
        assert len(desc)==1 , desc
        assert desc[0] == 'DEPL' , desc

        # vérification de l'objet .NUMO :
        #--------------------------------
        for x in self.NUMO.get() :
            assert x >= 1 , numo

        # vérification de l'objet .FACT :
        #--------------------------------
        if itypfl==3 :  # faisceau axial
            assert self.FACT.lonmax == 3*nbmode*nbvite
        else :
            assert self.FACT.lonmax == 3*nbmode

        # vérification de l'objet .MASG :
        #--------------------------------
        if itypfl==3 :  # faisceau axial
            assert self.MASG.lonmax == nbmode*nbvite
        else :
            assert self.MASG.lonmax == nbmode

        # vérification de l'objet .FREQ :
        #--------------------------------
        assert self.FREQ.lonmax == 2*nbmode*nbvite

        # vérification existence .VCN et .VEN:
        #-------------------------------------
        if self.VCN.exists : assert self.VEN.exists
        if self.VEN.exists : assert self.VCN.exists
        if self.VEN.exists : assert itypfl == 1 and couplage == 1
        if self.RAP.exists : assert (self.VEN.exists and self.VCN.exists)

        # vérification de l'objet .VCN :
        #--------------------------------
        if self.VCN.exists :
            fsvi=typfl.FSVI.get()
            nbzone=fsvi[1]
            nbval=0
            for i in range(nbzone) :
                nbval=nbval+fsvi[2+nbzone+i]
            assert self.VCN.lonmax == nbmode*nbval*2

        # vérification de l'objet .VEN :
        #--------------------------------
        if self.VEN.exists :
            assert self.VEN.lonmax == nbmode*2

        # vérification de l'objet .RAP :
        #--------------------------------
        if self.RAP.exists :
            fsvi=typfl.FSVI.get()
            nbzone=fsvi[1]
            nbval=0
            for i in range(nbzone) :
                nbval=nbval+fsvi[2+nbzone+i]
            assert self.RAP.lonmax == nbmode*nbval*2

        # vérification de la SD table contenant les cham_no :
        #----------------------------------------------------
        tcham=self.sd_table
        assert tcham.nb_column() == 1  , tcham
        col1=tcham.get_column_name('NOM_CHAM')
        assert col1, "Il manque la colonne NOM_CHAM"

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
                assert profchn1 == profchno  ,(profchn1, profchno)


        # vérification de la SD l_table :
        #--------------------------------
        if self.sd_l_table.LTNT.exists : assert itypfl == 3   # FAISCEAU_AXIAL
        if itypfl == 3  : assert self.sd_l_table.LTNT.exists

        if self.sd_l_table.LTNT.exists :
            l_table = self.sd_l_table
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
