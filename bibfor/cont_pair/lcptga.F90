subroutine lcptga(elem_dime, tria_coor , gauss_family,&
                  nb_gauss , gauss_coor, gauss_weight)
!
implicit none
! 
#include "asterfort/assert.h"
#include "asterfort/elraga.h"
#include "asterfort/reerel.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer, intent(in) :: elem_dime
    real(kind=8), intent(in) :: tria_coor(2,3)
    character(len=8) :: gauss_family
    integer, intent(out) :: nb_gauss
    real(kind=8), intent(out) :: gauss_coor(2,12)
    real(kind=8), intent(out) :: gauss_weight(12) 
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Get integration scheme
!
! --------------------------------------------------------------------------------------------------
!
! In  elem_dime        : dimension of elements
! In  tria_coor        : coordinate of current triangle
! In  gauss_family     : name of integration scheme
! Out nb_gauss         : number of integration points
! Out gauss_coor       : coordinates of integration points
! Out gauss_weight     : weight of integration points
!
! --------------------------------------------------------------------------------------------------
!
    integer ::i_gauss, i_dime, nb_node, model_ndim
    real(kind=8) :: gauxx_coor(24), gauxx_weight(12), segm_coor(2,2)
    real(kind=8) :: area
    real(kind=8) :: xpgpa(2), xpgpr(2)
    character(len=8) :: eleref
!
! --------------------------------------------------------------------------------------------------
!
    model_ndim = elem_dime - 1
    nb_gauss   = 0
    do i_dime = 1, model_ndim
        gauss_coor(i_dime,1:12) = 0.d0
    end do
    gauss_weight(1:12) = 0.d0
!
! - Select reference geometry for auxiliary parametric space
!
    if (model_ndim.eq. 2) then
        eleref  = 'TR3'
        nb_node = 3
    elseif (model_ndim .eq. 1) then
        eleref  = 'SE2'
        nb_node = 2
        segm_coor(1,1) = tria_coor(1,1)
        segm_coor(2,1) = tria_coor(2,1)
        segm_coor(1,2) = tria_coor(1,2)
        segm_coor(2,2) = tria_coor(2,2)
    else
        ASSERT(.false.)
    endif
!
! - Get integration scheme in auxiliary parametric space
!
    call elraga(eleref      , gauss_family, model_ndim, nb_gauss, gauxx_coor,&
                gauxx_weight)
!
! - Surface of real element
!           
    if (model_ndim.eq. 2) then
        area = (tria_coor(1,1)*tria_coor(2,2)-tria_coor(1,2)*tria_coor(2,1)+&
                tria_coor(1,2)*tria_coor(2,3)-tria_coor(1,3)*tria_coor(2,2)+&
                tria_coor(1,3)*tria_coor(2,1)-tria_coor(1,1)*tria_coor(2,3))*1.d0/2.d0
        area = sqrt(area*area)
    else
        area = tria_coor(1,2)-tria_coor(1,1)
        area = sqrt(area*area)
    end if
!
! - Back in element parametric space
!
    do i_gauss = 1, nb_gauss
        do i_dime = 1,model_ndim
            xpgpa(i_dime)=gauxx_coor(model_ndim*(i_gauss-1)+i_dime)
        end do
        if (model_ndim .eq. 2) then       
            call reerel(eleref, nb_node, 2, tria_coor, xpgpa,&
                        xpgpr)
        else
            call reerel(eleref, nb_node, 2, segm_coor, xpgpa,&
                        xpgpr)
        endif
        do i_dime=1,model_ndim
            gauss_coor(i_dime,i_gauss)=xpgpr(i_dime)
        end do
        if (eleref  .eq. 'TR3') then   
            gauss_weight(i_gauss)=2*area*gauxx_weight(i_gauss)
        elseif (eleref .eq. 'SE2') then
            gauss_weight(i_gauss)=1/2.d0*area*gauxx_weight(i_gauss)
        else
            ASSERT(.false.)
        end if
    end do
!
end subroutine
