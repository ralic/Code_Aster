subroutine apchoi(dist        , dist_mini, elem_indx, elem_indx_mini, tau1     ,&
                  tau1_mini   , tau2     , tau2_mini, ksi1          , ksi1_mini,&
                  ksi2        , ksi2_mini, proj_stat, proj_stat_mini, vect_pm  ,&
                  vect_pm_mini)
!
implicit none
!
#include "asterc/r8prem.h"
#include "asterfort/infdbg.h"
#include "blas/dcopy.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    integer, intent(in) :: proj_stat
    integer, intent(inout) :: proj_stat_mini
    real(kind=8), intent(in) :: dist
    real(kind=8), intent(inout) :: dist_mini
    integer, intent(in) :: elem_indx
    integer, intent(inout) :: elem_indx_mini
    real(kind=8), intent(in) :: tau1(3)
    real(kind=8), intent(inout) :: tau1_mini(3)
    real(kind=8), intent(in) :: tau2(3)
    real(kind=8), intent(inout) :: tau2_mini(3)
    real(kind=8), intent(in) :: vect_pm(3)
    real(kind=8), intent(inout) :: vect_pm_mini(3)
    real(kind=8), intent(in) :: ksi1
    real(kind=8), intent(inout) :: ksi1_mini
    real(kind=8), intent(in) :: ksi2
    real(kind=8), intent(inout) :: ksi2_mini
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Select nearest element
!
! --------------------------------------------------------------------------------------------------
!
! In  dist             : distance between point and projection of the point
! In  vect_pm          : vector between point and projection of the point
! In  ksi1             : first parametric coordinate of the projection of the point
! In  ksi2             : second parametric coordinate of the projection of the point
! In  proj_stat        : status of projection
!                            0 - Inside element
!                            1 - Inside element + tole_proj_out
!                            2 - Outside element
! In  elem_indx        : index of element in contact datastructure
! In  tau1             : first tangent vector for local basis
! In  tau2             : second tangent vector for local basis
! IO  dist_mini        : distance between point and projection of the point for selection
! IO  vect_pm_mini     : vector between point and projection of the point for selection
! IO  ksi1_mini        : first parametric coordinate of the projection of the point for selection
! IO  ksi2_mini        : second parametric coordinate of the projection of the point for selection
! IO  proj_stat_mini   : status of projection
!                            0 - Inside element
!                            1 - Inside element + tole_proj_out
!                            2 - Outside element
! IO  elem_indx_mini   : index of element in contact datastructure for selection
! IO  tau1_mini        : first tangent vector for local basis for selection
! IO  tau2_mini        : second tangent vector for local basis for selection
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: ecan
!
! --------------------------------------------------------------------------------------------------
!
!
! - Difference old/new distance
!
    if ((dist_mini.eq.0.d0) .or. (dist.eq.dist_mini)) then
        ecan = 0.d0
    else
        ecan = abs((dist - dist_mini)/dist_mini)
    endif
!
    if (proj_stat_mini .eq. -1) then
! ----- First projection
        dist_mini      = dist
        elem_indx_mini = elem_indx
        proj_stat_mini = proj_stat
        ksi1_mini      = ksi1
        ksi2_mini      = ksi2
        call dcopy(3, tau1, 1, tau1_mini, 1)
        call dcopy(3, tau2, 1, tau2_mini, 1)
        call dcopy(3, vect_pm, 1, vect_pm_mini, 1)
    else
! ----- Next projections
        if (proj_stat_mini .ne. 0) then
! --------- Never projected inside element
            if (proj_stat .eq. 0) then
                dist_mini      = dist
                elem_indx_mini = elem_indx
                proj_stat_mini = proj_stat
                ksi1_mini      = ksi1
                ksi2_mini      = ksi2
                call dcopy(3, tau1, 1, tau1_mini, 1)
                call dcopy(3, tau2, 1, tau2_mini, 1)
                call dcopy(3, vect_pm, 1, vect_pm_mini, 1)
            else
                if (dist .lt. dist_mini) then
                    dist_mini      = dist
                    elem_indx_mini = elem_indx
                    proj_stat_mini = proj_stat
                    ksi1_mini      = ksi1
                    ksi2_mini      = ksi2
                    call dcopy(3, tau1, 1, tau1_mini, 1)
                    call dcopy(3, tau2, 1, tau2_mini, 1)
                    call dcopy(3, vect_pm, 1, vect_pm_mini, 1)  
                endif
            endif
        else
! --------- Already projected inside element
            if (proj_stat .eq. 0) then
                if (dist .lt. dist_mini .and. ecan .gt. r8prem()) then
                    dist_mini      = dist
                    elem_indx_mini = elem_indx
                    proj_stat_mini = proj_stat
                    ksi1_mini      = ksi1
                    ksi2_mini      = ksi2
                    call dcopy(3, tau1, 1, tau1_mini, 1)
                    call dcopy(3, tau2, 1, tau2_mini, 1)
                    call dcopy(3, vect_pm, 1, vect_pm_mini, 1)
                endif
            endif
        endif
    endif
!
end subroutine
