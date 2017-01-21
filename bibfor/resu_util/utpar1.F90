subroutine utpar1(resu_type, nb_para_maxi, para_list, nb_para)
!
implicit none
!
#include "asterfort/assert.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=*), intent(in) :: resu_type
    integer, intent(in) :: nb_para_maxi
    character(len=32), intent(out) :: para_list(nb_para_maxi)
    integer, intent(out) :: nb_para
!
! --------------------------------------------------------------------------------------------------
!
! Results datastructure - Utility
!
! Get list of parameters for results datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  resu_type        : type of results datastructure
! In  nb_para_maxi     : maximum number of parameters (size of list_para)
! Out list_para        : list of parameters
! Out nb_para          : number of parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ico
!
! --------------------------------------------------------------------------------------------------
!
    ico=0
!
!
!
    if (resu_type .eq. 'EVOL_THER') then
!     --------------------------------
        ico=ico+1
        para_list(ico)='INST#A#R'
        ico=ico+1
        para_list(ico)='MODELE#P#K8'
        ico=ico+1
        para_list(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        para_list(ico)='CARAELEM#P#K8'
        ico=ico+1
        para_list(ico)='EXCIT#P#K24'
        ico=ico+1
        para_list(ico)='ITER_GLOB#P#I'
        ico=ico+1
        para_list(ico)='ITER_LINE#P#I'
        ico=ico+1
        para_list(ico)='RESI_GLOB_RELA#P#R'
        ico=ico+1
        para_list(ico)='RESI_GLOB_MAXI#P#R'
        ico=ico+1
        para_list(ico)='RHO#P#R'
        ico=ico+1
        para_list(ico)='PARM_THETA#P#R'
        ico=ico+1
        para_list(ico)='DELTAT#P#R'
        nb_para=ico
        ASSERT(nb_para.le.nb_para_maxi)
!
!
!
    else if (resu_type.eq.'ACOU_HARMO') then
!     --------------------------------
        ico=ico+1
        para_list(ico)='CARAELEM#P#K8'
        ico=ico+1
        para_list(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        para_list(ico)='EXCIT#P#K24'
        ico=ico+1
        para_list(ico)='FREQ#A#R'
        ico=ico+1
        para_list(ico)='MODELE#P#K8'
        nb_para=ico
        ASSERT(nb_para.le.nb_para_maxi)
!
!
!
    else if (resu_type.eq.'DYNA_HARMO') then
!     --------------------------------
        ico=ico+1
        para_list(ico)='CARAELEM#P#K8'
        ico=ico+1
        para_list(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        para_list(ico)='EXCIT#P#K24'
        ico=ico+1
        para_list(ico)='FREQ#A#R'
        ico=ico+1
        para_list(ico)='MODELE#P#K8'
        nb_para=ico
        ASSERT(nb_para.le.nb_para_maxi)
!
!
!
    else if (resu_type.eq.'DYNA_TRANS') then
!     --------------------------------
        ico=ico+1
        para_list(ico)='CARAELEM#P#K8'
        ico=ico+1
        para_list(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        para_list(ico)='EXCIT#P#K24'
        ico=ico+1
        para_list(ico)='INST#A#R'
        ico=ico+1
        para_list(ico)='MODELE#P#K8'
        nb_para=ico
        ASSERT(nb_para.le.nb_para_maxi)
!
!
!
    else if (resu_type.eq.'EVOL_ELAS') then
!     --------------------------------
        ico=ico+1
        para_list(ico)='CARAELEM#P#K8'
        ico=ico+1
        para_list(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        para_list(ico)='DEFO_D_DRY_X#P#R'
        ico=ico+1
        para_list(ico)='DEFO_D_DRZ_X#P#R'
        ico=ico+1
        para_list(ico)='DEFO_D_DX_X#P#R'
        ico=ico+1
        para_list(ico)='EFFORT_D_VY_X#P#R'
        ico=ico+1
        para_list(ico)='EFFORT_D_VZ_X#P#R'
        ico=ico+1
        para_list(ico)='EFFORT_N#P#R'
        ico=ico+1
        para_list(ico)='EFFORT_VY#P#R'
        ico=ico+1
        para_list(ico)='EFFORT_VZ#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_ERRE#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_ERZ1#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_ERZ2#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_QIRE#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_QIZ1#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_QIZ2#P#R'
        ico=ico+1
        para_list(ico)='EXCIT#P#K24'
        ico=ico+1
        para_list(ico)='INST#A#R'
        ico=ico+1
        para_list(ico)='ITER_GCPC#P#I'
        ico=ico+1
        para_list(ico)='METHODE#P#K16'
        ico=ico+1
        para_list(ico)='MODELE#P#K8'
        ico=ico+1
        para_list(ico)='MOMENT_D_MT_X#P#R'
        ico=ico+1
        para_list(ico)='MOMENT_MFY#P#R'
        ico=ico+1
        para_list(ico)='MOMENT_MFZ#P#R'
        ico=ico+1
        para_list(ico)='MOMENT_MT#P#R'
        ico=ico+1
        para_list(ico)='RENUM#P#K16'
        ico=ico+1
        para_list(ico)='RESI_GCPC#P#R'
        ico=ico+1
        para_list(ico)='STOCKAGE#P#K16'
        nb_para=ico
        ASSERT(nb_para.le.nb_para_maxi)
!
!
!
    else if (resu_type.eq.'EVOL_NOLI') then
!     --------------------------------
        ico=ico+1
        para_list(ico)='INST#A#R'
        ico=ico+1
!       -- FREQ ET CHAR_CRIT NE PEUTVENT  PAS ETRE UNE VARIABLE D'ACCES
!       A CAUSE DE LRIDEA.F (TEST ZZZZ165A)
        para_list(ico)='FREQ#P#R'
        ico=ico+1
        para_list(ico)='CHAR_CRIT#P#R'
!
        ico=ico+1
        para_list(ico)='CHAR_STAB#P#R'
!
        ico=ico+1
        para_list(ico)='CARAELEM#P#K8'
        ico=ico+1
        para_list(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        para_list(ico)='MODELE#P#K8'
        ico=ico+1
        para_list(ico)='PARM_THETA#P#R'
        ico=ico+1
        para_list(ico)='EXCIT#P#K24'
!
        ico=ico+1
        para_list(ico)='ERRE_HYD_D#P#R'
        ico=ico+1
        para_list(ico)='ERRE_HYD_GLOB#P#R'
        ico=ico+1
        para_list(ico)='ERRE_HYD_LOC#P#R'
        ico=ico+1
        para_list(ico)='ERRE_HYD_S#P#R'
        ico=ico+1
        para_list(ico)='ERRE_MEC#P#R'
        ico=ico+1
        para_list(ico)='ERRE_MEC_GLOB#P#R'
        ico=ico+1
        para_list(ico)='ERRE_MEC_GLOB_D#P#R'
        ico=ico+1
        para_list(ico)='ERRE_MEC_LOC#P#R'
        ico=ico+1
        para_list(ico)='ERRE_MEC_LOC_D#P#R'
        ico=ico+1
        para_list(ico)='ERRE_TPS_GLOB#P#R'
        ico=ico+1
        para_list(ico)='ERRE_TPS_LOC#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_ERRE#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_ERZ1#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_ERZ2#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_QIRE#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_QIZ1#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_QIZ2#P#R'
        ico=ico+1
        para_list(ico)='ETA_PILOTAGE#P#R'
        ico=ico+1
        para_list(ico)='ITER_GLOB#P#I'
        ico=ico+1
        para_list(ico)='CHAR_MINI#P#R'
        ico=ico+1
        para_list(ico)='TRAN_GENE_NOLI#P#K24'
        ico=ico+1
        para_list(ico)='COEF_MULT#P#R'
        ico=ico+1
        para_list(ico)='INST_PREC#P#R'
        nb_para=ico
        ASSERT(nb_para.le.nb_para_maxi)
!
!
!
    else if (resu_type.eq.'COMB_FOURIER') then
!     --------------------------------
        ico=ico+1
        para_list(ico)='ANGLE#A#R'
        ico=ico+1
        para_list(ico)='MODELE#P#K8'
        ico=ico+1
        para_list(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        para_list(ico)='CARAELEM#P#K8'
        ico=ico+1
        para_list(ico)='EXCIT#P#K24'
        nb_para=ico
        ASSERT(nb_para.le.nb_para_maxi)
!
!
!
!
    else if (resu_type.eq.'MODE_FLAMB') then
!     --------------------------------
        ico=ico+1
        para_list(ico)='CARAELEM#P#K8'
        ico=ico+1
        para_list(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        para_list(ico)='CHAR_CRIT#P#R'
        ico=ico+1
        para_list(ico)='EXCIT#P#K24'
        ico=ico+1
        para_list(ico)='MODELE#P#K8'
        ico=ico+1
        para_list(ico)='NORME#P#K24'
        ico=ico+1
        para_list(ico)='NUME_MODE#A#I'
        nb_para=ico
        ASSERT(nb_para.le.nb_para_maxi)
!
!
!
    else if (resu_type.eq.'MODE_STAB') then
!     --------------------------------
        ico=ico+1
        para_list(ico)='CARAELEM#P#K8'
        ico=ico+1
        para_list(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        para_list(ico)='CHAR_STAB#P#R'
        ico=ico+1
        para_list(ico)='EXCIT#P#K24'
        ico=ico+1
        para_list(ico)='MODELE#P#K8'
        ico=ico+1
        para_list(ico)='NORME#P#K24'
        ico=ico+1
        para_list(ico)='NUME_MODE#A#I'
        nb_para=ico
        ASSERT(nb_para.le.nb_para_maxi)
!
!
!
    elseif ((resu_type.eq.'MODE_MECA').or.&
            (resu_type.eq.'MODE_MECA_C').or.&
            (resu_type.eq.'MODE_GENE').or.&
            (resu_type.eq.'MODE_ACOU')) then
!     --------------------------------
        ico=ico+1
        para_list(ico)='AMOR_GENE#P#R'
        ico=ico+1
        para_list(ico)='AMOR_REDUIT#P#R'
        ico=ico+1
        para_list(ico)='CARAELEM#P#K8'
        ico=ico+1
        para_list(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        para_list(ico)='COEF_X#P#R'
        ico=ico+1
        para_list(ico)='COEF_Y#P#R'
        ico=ico+1
        para_list(ico)='COEF_Z#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_ERRE#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_ERZ1#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_ERZ2#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_QIRE#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_QIZ1#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_QIZ2#P#R'
        ico=ico+1
        para_list(ico)='EXCIT#P#K24'
        ico=ico+1
        para_list(ico)='FACT_PARTICI_DX#P#R'
        ico=ico+1
        para_list(ico)='FACT_PARTICI_DY#P#R'
        ico=ico+1
        para_list(ico)='FACT_PARTICI_DZ#P#R'
        ico=ico+1
        para_list(ico)='FREQ#A#R'
        ico=ico+1
        para_list(ico)='MASS_EFFE_DX#P#R'
        ico=ico+1
        para_list(ico)='MASS_EFFE_DY#P#R'
        ico=ico+1
        para_list(ico)='MASS_EFFE_DZ#P#R'
        ico=ico+1
        para_list(ico)='MASS_EFFE_UN_DX#P#R'
        ico=ico+1
        para_list(ico)='MASS_EFFE_UN_DY#P#R'
        ico=ico+1
        para_list(ico)='MASS_EFFE_UN_DZ#P#R'
        ico=ico+1
        para_list(ico)='MASS_GENE#P#R'
        ico=ico+1
        para_list(ico)='MODELE#P#K8'
        ico=ico+1
        para_list(ico)='NORME#P#K24'
        ico=ico+1
        para_list(ico)='NUME_DDL#P#I'
        ico=ico+1
        para_list(ico)='NUME_MODE#A#I'
        ico=ico+1
        para_list(ico)='NOEUD_CMP#A#K16'
        ico=ico+1
        para_list(ico)='OMEGA2#P#R'
        ico=ico+1
        para_list(ico)='RIGI_GENE#P#R'
        ico=ico+1
        para_list(ico)='TYPE_DEFO#P#K16'
        ico=ico+1
        para_list(ico)='TYPE_MODE#P#K16'
        ico=ico+1
        para_list(ico)='ERC_EVAL_FONC#P#R'
        nb_para=ico
        ASSERT(nb_para.le.nb_para_maxi)
!
    else if (resu_type.eq.'MODE_EMPI') then
!     --------------------------------
        ico=ico+1
        para_list(ico)='FREQ#A#R'
        ico=ico+1
        para_list(ico)='MODELE#P#K8'
        ico=ico+1
        para_list(ico)='NUME_MODE#A#I'
        ico=ico+1
        para_list(ico)='NOM_CHAM#P#K24'
        ico=ico+1
        para_list(ico)='NUME_PLAN#A#I'
        nb_para=ico
        ASSERT(nb_para.le.nb_para_maxi)
!
    else if (resu_type.eq.'MULT_ELAS') then
!     --------------------------------
        ico=ico+1
        para_list(ico)='CARAELEM#P#K8'
        ico=ico+1
        para_list(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        para_list(ico)='ERREUR_ERRE#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_ERZ1#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_ERZ2#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_QIRE#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_QIZ1#P#R'
        ico=ico+1
        para_list(ico)='ERREUR_QIZ2#P#R'
        ico=ico+1
        para_list(ico)='EXCIT#P#K24'
        ico=ico+1
        para_list(ico)='MODELE#P#K8'
        ico=ico+1
        para_list(ico)='NOM_CAS#A#K16'
        nb_para=ico
        ASSERT(nb_para.le.nb_para_maxi)
!
!
!
        elseif ((resu_type.eq.'FOURIER_ELAS').or. (resu_type.eq.'FOURIER_THER'))&
    then
!     -----------------------------------------
        ico=ico+1
        para_list(ico)='NUME_MODE#A#I'
        ico=ico+1
        para_list(ico)='TYPE_MODE#P#K8'
        ico=ico+1
        para_list(ico)='MODELE#P#K8'
        ico=ico+1
        para_list(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        para_list(ico)='CARAELEM#P#K8'
        ico=ico+1
        para_list(ico)='EXCIT#P#K24'
        nb_para=ico
        ASSERT(nb_para.le.nb_para_maxi)
!
!
!
    else if (resu_type.eq.'HARM_GENE') then
!     --------------------------------
        ico=ico+1
        para_list(ico)='FREQ#A#R'
        nb_para=ico
        ASSERT(nb_para.le.nb_para_maxi)
!
!
!
    else if (resu_type.eq.'EVOL_CHAR') then
!     --------------------------------
        ico=ico+1
        para_list(ico)='INST#A#R'
        ico=ico+1
        para_list(ico)='MODELE#P#K8'
        ico=ico+1
        para_list(ico)='CARAELEM#P#K8'
        ico=ico+1
        para_list(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        para_list(ico)='EXCIT#P#K24'
        nb_para=ico
        ASSERT(nb_para.le.nb_para_maxi)
!
!
!
    else if (resu_type.eq.'EVOL_VARC') then
!     --------------------------------
        ico=ico+1
        para_list(ico)='INST#A#R'
        ico=ico+1
        para_list(ico)='MODELE#P#K8'
        ico=ico+1
        para_list(ico)='CARAELEM#P#K8'
        ico=ico+1
        para_list(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        para_list(ico)='EXCIT#P#K24'
        nb_para=ico
        ASSERT(nb_para.le.nb_para_maxi)
!
!
    else
        ASSERT(.false.)
    endif
!
!
!     -- POUR EVITER UNE LISTE VIDE :
!     -------------------------------
    if (nb_para .eq. 0) then
        nb_para=1
        para_list(1)='XXX#P#R'
    endif
!
!
!
end subroutine
