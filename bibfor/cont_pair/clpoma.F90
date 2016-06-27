subroutine clpoma(elem_dime, elem_code, elem_coor, elem_nbnode, elem_weight)
!
implicit none
!
#include "asterfort/elraga.h"
#include "asterfort/subaco.h"
#include "asterfort/mmdonf.h"
#include "asterfort/sumetr.h"
#include "asterfort/assert.h" 
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
    character(len=8), intent(in) :: elem_code
    real(kind=8), intent(in) :: elem_coor(3,9)
    integer, intent(in) :: elem_nbnode
    real(kind=8), intent(out) :: elem_weight
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Compute weight of element
!
! --------------------------------------------------------------------------------------------------
!
! In  elem_dime        : dimension of current element
! In  elem_code        : code of current element
! In  elem_coor        : coordinates of nodes for current element
! In  elem_nbnode      : number of node for current element
! Out elem_weight      : weight of current element
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_gauss, nb_gauss, ino
    character(len=8) :: gauss_family
    real(kind=8) :: gauss_weight(12), gauss_coor(12*2) 
    real(kind=8) :: dff(2, 9), dxdk, dydk, dzdk
    real(kind=8) :: coptg1, coptg2
    real(kind=8) :: cova(3, 3), metr(2, 2), jacobi
!
! --------------------------------------------------------------------------------------------------
!
    elem_weight = 0.d0
!
! - Select integration scheme
!
    if (elem_code.eq. 'SE2') then
        gauss_family = 'FPG3'
    elseif (elem_code.eq. 'SE3') then
        gauss_family = 'FPG3'
    elseif (elem_code.eq. 'TR3') then
        gauss_family = 'FPG3'
    elseif (elem_code.eq. 'TR6') then
        gauss_family = 'FPG6'
    elseif (elem_code.eq. 'QU4') then 
        gauss_family = 'FPG9'  
    elseif (elem_code.eq. 'QU8') then 
        gauss_family = 'FPG9'  
    elseif (elem_code.eq. 'QU9') then 
        gauss_family = 'FPG9'
    else
        ASSERT(.false.)
    end if
!
! - Get integration scheme
!
    call elraga(elem_code   , gauss_family, elem_dime-1, nb_gauss, gauss_coor,&
                gauss_weight)
!
! - Loop on integration points
!
    do i_gauss = 1, nb_gauss
        jacobi = 0.d0
        coptg1 = gauss_coor((elem_dime-1)*(i_gauss-1)+1)
        coptg2 = gauss_coor((elem_dime-1)*(i_gauss-1)+2)
        call mmdonf(elem_dime, elem_nbnode, elem_code, coptg1, coptg2,&
                    dff)
        if ((elem_dime-1) .eq. 2) then
            call subaco(elem_nbnode, dff, elem_coor, cova)
            call sumetr(cova, metr, jacobi)
        else if ((elem_dime-1) .eq. 1) then
            dxdk=0.d0
            dydk=0.d0
            dzdk=0.d0
            do ino = 1, elem_nbnode
                dxdk = dxdk + elem_coor(1,ino)*dff(1,ino)
                dydk = dydk + elem_coor(2,ino)*dff(1,ino)
                if (elem_dime .eq. 3) then
                    dzdk = dzdk + elem_coor(3,ino)*dff(1,ino)
                end if
            end do
            jacobi = sqrt(dxdk**2+dydk**2+dzdk**2)
        else
            ASSERT(.false.)
        end if
        elem_weight = elem_weight+gauss_weight(i_gauss)*jacobi
    end do
!
end subroutine
