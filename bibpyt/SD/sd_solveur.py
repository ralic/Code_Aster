#@ MODIF sd_solveur SD  DATE 11/09/2012   AUTEUR BOITEAU O.BOITEAU 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

class sd_solveur(AsBase):
    nomj = SDNom(fin=19)
    SLVK = AsVK24(SDNom(debut=19), lonmax=12, )
    SLVR = AsVR(SDNom(debut=19), lonmax=4, )
    SLVI = AsVI(SDNom(debut=19), lonmax=7, )


    def check_SLVK(self,checker):
    #---------------------------------------------
        slvk = self.SLVK.get_stripped()
        method=slvk[0]
        if method == 'MUMPS' :
            assert slvk[1] in ('AUTO', 'SANS'), slvk
            assert slvk[2] in ('NONSYM', 'SYMGEN', 'SYMDEF', 'AUTO'), slvk
            assert slvk[3] in ('AMD','AMF','PORD','METIS','QAMD','AUTO','SCOTCH'), slvk
            assert slvk[4] in ('OUI', 'NON'), slvk
            assert slvk[5] in ('OUI', 'NON'), slvk
            assert slvk[6] in ('OUI', 'NON','XXXX'), slvk
            assert slvk[7] in ('OUI', 'NON','XXXX'), slvk
            assert slvk[8] in ('IN_CORE', 'OUT_OF_CORE', 'AUTO', 'EVAL','XXXX'), slvk
            assert slvk[9] in ('OUI', 'NON','XXXX'), slvk
            assert slvk[10] in ('SANS', 'AUTO', 'FORCE','XXXX'), slvk
            assert slvk[11] in ('XXXX','4.9.2','4.10.0'), slvk
        elif method == 'MULT_FRONT' :
            assert slvk[1] in ('XXXX'), slvk
            assert slvk[2] in ('XXXX'), slvk
            assert slvk[3] in ('MD','MDA','METIS'), slvk
            assert slvk[4] in ('OUI', 'NON'), slvk
            assert slvk[5] in ('XXXX'), slvk
            assert slvk[6] in ('XXXX'), slvk
            assert slvk[7] in ('XXXX'), slvk
            assert slvk[8] in ('XXXX'), slvk
            assert slvk[9] in ('XXXX'), slvk
            assert slvk[10] in ('XXXX'), slvk
            assert slvk[11] in ('XXXX'), slvk
        elif method == 'LDLT' :
            assert slvk[1] in ('XXXX'), slvk
            assert slvk[2] in ('XXXX'), slvk
            assert slvk[3] in ('RCMK','SANS'), slvk
            assert slvk[4] in ('OUI', 'NON'), slvk
            assert slvk[5] in ('XXXX'), slvk
            assert slvk[6] in ('XXXX'), slvk
            assert slvk[7] in ('XXXX'), slvk
            assert slvk[8] in ('XXXX'), slvk
            assert slvk[9] in ('XXXX'), slvk
            assert slvk[10] in ('XXXX'), slvk
            assert slvk[11] in ('XXXX'), slvk
        elif method == 'GCPC' :
            assert slvk[1] in ('LDLT_SP','LDLT_INC','SANS'), slvk
            assert slvk[3] in ('RCMK', 'SANS'), slvk
            assert slvk[4] in ('OUI','NON'), slvk
            assert slvk[5] in ('XXXX'), slvk
            assert slvk[6] in ('XXXX'), slvk
            assert slvk[7] in ('XXXX'), slvk
            assert slvk[8] in ('XXXX'), slvk
            assert slvk[9] in ('XXXX'), slvk
            assert slvk[10] in ('XXXX'), slvk
            assert slvk[11] in ('XXXX'), slvk
        elif method == 'PETSC' :
            assert slvk[1] in ('LDLT_SP','LDLT_INC','JACOBI','SOR'), slvk
            assert slvk[3] in ('RCMK','SANS'), slvk
            assert slvk[4] in ('OUI', 'NON'), slvk
            assert slvk[5] in ('CG','BCGS','BICG','CR','GMRES','TFQMR'), slvk
            assert slvk[6] in ('XXXX'), slvk
            assert slvk[7] in ('XXXX'), slvk
            assert slvk[8] in ('XXXX'), slvk
            assert slvk[9] in ('XXXX'), slvk
            assert slvk[10] in ('XXXX'), slvk
            assert slvk[11] in ('XXXX'), slvk
        elif method == 'FETI' :
            assert slvk[1] in ('LUMPE','SANS'), slvk
            assert slvk[2] in ('OUI', 'NON'), slvk
            assert slvk[3] in ('MD','MDA','METIS'), slvk
            assert slvk[4] in ('OUI','NON'), slvk
            assert slvk[6] in ('GS','GSM','IGSM','SANS'), slvk
            assert slvk[7] in ('MULT','SANS'), slvk
            assert slvk[8] in ('CAL'/'OUI'/'NON'), slvk
            assert slvk[9] in ('NON'), slvk
            assert slvk[10] in ('OUI','NON'), slvk
            assert slvk[11] in ('XXXX'), slvk
        else :
            pass


