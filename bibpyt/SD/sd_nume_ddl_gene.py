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

from SD.sd_nume_ddl_com import sd_nume_ddl_com
from SD.sd_nume_eqge import sd_nume_eqge
from SD.sd_nume_elim import sd_nume_elim

class sd_nume_ddl_gene(sd_nume_ddl_com):
#---------------------------------------
    nomj = SDNom(fin=14)
    nume = Facultatif(sd_nume_eqge(SDNom(nomj='.NUME'))) # n'existe pas toujours : CALC_MATR_AJOU/fdlv106a
    ELIM = Facultatif(sd_nume_elim(SDNom(nomj='.ELIM'))) # n'existe pas toujours : ELIMINATION
