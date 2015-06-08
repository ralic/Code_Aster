subroutine deprecated_algom(algo)
!
implicit none
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 2091 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: josselin.delmas at edf.fr
!
    character(len=*), intent(in) :: algo
!
! --------------------------------------------------------------------------------------------------
!
! DEPRECATED FEATURES
!
! Warning for deprecated algorithm
!
! --------------------------------------------------------------------------------------------------
!
! In  algo : name of deprecated algorithm
!
! --------------------------------------------------------------------------------------------------
!
    integer :: vali
    character(len=32) :: valk
!
! --------------------------------------------------------------------------------------------------
!
    if (algo .eq. 'DEPL_CALCULE') then
        vali = 13
        valk    = "PREDICTION='DEPL_CALCULE'"
!
    else if (algo .eq. 'GDEF_HYPO_ELAS') then
        vali = 13
        valk    = "DEFORMATION='GDEF_HYPO_ELAS'"
!
    else if (algo .eq. 'GDEF_HYPO_ELAS') then
        vali = 13
        valk    = "DEFORMATION='GDEF_HYPO_ELAS'"
!
    else if (algo .eq. 'LAGR_CONT') then
        vali = 13
        valk    = "ALGO_CONT='LAGRANGIEN'"
!
    else if (algo .eq. 'LAGR_FROT') then
        vali = 13
        valk    = "ALGO_CONT/ALGO_FROT='LAGRANGIEN'"
!
    else if (algo .eq. 'G_BILI') then
        vali = 13
        valk    = "CALC_G/OPTION='G_BILI'"
!
    else if (algo .eq. 'G_BILI_GLOB') then
        vali = 13
        valk    = "CALC_G/OPTION='G_BILI_GLOB'"
!
    else if (algo .eq. 'G_MAX') then
        vali = 13
        valk    = "CALC_G/OPTION='G_MAX'"
!
    else if (algo .eq. 'G_MAX_GLOB') then
        vali = 13
        valk    = "CALC_G/OPTION='G_MAX_GLOB'"
!
    else if (algo .eq. 'CALC_K_MAX') then
        vali = 13
        valk    = "CALC_G/OPTION='CALC_K_MAX'"
!
    else if (algo .eq. 'CASTEM') then
        vali = 13
        valk    = "IMPR_RESU/FORMAT='CASTEM'"
!
    else
        goto 999
!
    endif
!
    call utmess('A', 'SUPERVIS_9', sk = valk, si = vali)
!
999 continue
!
end subroutine
