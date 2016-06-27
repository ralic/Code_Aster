subroutine aprtpe(elin_dime, poin_inte, nb_poin_inte,& 
                  elem_code, elin_nume)
!
implicit none
!
#include "asterfort/reerel.h"
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
    integer, intent(in) :: elin_dime
    real(kind=8), intent(inout) :: poin_inte(elin_dime-1,16)
    integer, intent(in) :: nb_poin_inte
    character(len=8), intent(in) :: elem_code
    integer, intent(in), optional :: elin_nume
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Projection from parametric space of element into sub-element parametric space
!
! --------------------------------------------------------------------------------------------------
!
! In  elin_dime        : dimension of elements
! In  poin_inte        : list (sorted) of intersection points (parametric space)
! In  nb_poin_inte     : number of intersection points
! In  elem_code        : code of element
! In  poin_inte_real   : list (sorted) of intersection points (real space)
! In  elin_nume        : index of sub-element in elements
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: poin_inte_real(elin_dime-1,16)
    integer :: i_poin_inte
    real(kind=8) :: node_real(3), ksi(2)
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: norm(3), noor
    real(kind=8) :: node_para(3,3), node_para_2(2,2)
    character(len=8) :: elin_code
!
! --------------------------------------------------------------------------------------------------
!
    do i_poin_inte = 1, nb_poin_inte
        poin_inte_real(1,i_poin_inte) = 0.d0
        poin_inte_real(2,i_poin_inte) = 0.d0
    end do
    tau1(1:3) = 0.d0
    tau2(1:3) = 0.d0
    norm(1:3) = 0.d0
    noor      = 0.d0
!
! - Loop on integration points
!
    do i_poin_inte=1, nb_poin_inte
        tau1(1:3) = 0.d0
        tau2(1:3) = 0.d0
        norm(1:3) = 0.d0
        noor      = 0.d0
        ksi(1) = poin_inte (1,i_poin_inte)
        if (elin_dime .eq. 3) then
            ksi(2) = poin_inte (2,i_poin_inte)
        end if
!
        if ((elem_code.eq.'QU4' ) .and. present(elin_nume)) then
            if (elin_nume .eq. 1) then
                node_para(1,1) = -1.d0
                node_para(2,1) = -1.d0
                node_para(3,1) =  0.d0
                node_para(1,2) =  1.d0
                node_para(2,2) = -1.d0
                node_para(3,2) =  0.d0
                node_para(1,3) =  1.d0
                node_para(2,3) =  1.d0
                node_para(3,3) =  0.d0
                elin_code   = 'TR3'
                call reerel(elin_code, 3, 3, node_para, ksi, node_real)
                poin_inte_real(1,i_poin_inte) = node_real(1)
                poin_inte_real(2,i_poin_inte) = node_real(2)
            elseif (elin_nume .eq. 2) then
                node_para(1,1) =  1.d0
                node_para(2,1) =  1.d0
                node_para(3,1) =  0.d0
                node_para(1,2) = -1.d0
                node_para(2,2) =  1.d0
                node_para(3,2) =  0.d0
                node_para(1,3) = -1.d0
                node_para(2,3) = -1.d0
                node_para(3,3) =  0.d0
                elin_code      = 'TR3'
                call reerel(elin_code,3, 3, node_para, ksi, node_real)
                poin_inte_real(1,i_poin_inte) = node_real(1)
                poin_inte_real(2,i_poin_inte) = node_real(2)
            else
                ASSERT(.false.)
            endif
        elseif (elem_code.eq.'TR6' .and. present(elin_nume)) then
            if (elin_nume .eq. 1) then
                node_para(1,1) = 0.d0
                node_para(2,1) = 0.d0
                node_para(3,1) = 0.d0
                node_para(1,2) = 5.d-1
                node_para(2,2) = 0.d0
                node_para(3,2) = 0.d0
                node_para(1,3) = 0.d0
                node_para(2,3) = 5.d-1
                node_para(3,3) = 0.d0
                elin_code='TR3'
                call reerel(elin_code,3, 3, node_para, ksi, node_real)
                poin_inte_real(1,i_poin_inte) = node_real(1)
                poin_inte_real(2,i_poin_inte) = node_real(2)            
            elseif (elin_nume .eq. 2) then
                node_para(1,1) = 5.d-1
                node_para(2,1) = 0.d0
                node_para(3,1) = 0.d0
                node_para(1,2) = 1.d0
                node_para(2,2) = 0.d0
                node_para(3,2) = 0.d0
                node_para(1,3) = 5.d-1
                node_para(2,3) = 5.d-1
                node_para(3,3) = 0.d0
                elin_code='TR3'
                call reerel(elin_code,3, 3, node_para, ksi, node_real)
                poin_inte_real(1,i_poin_inte) = node_real(1)
                poin_inte_real(2,i_poin_inte) = node_real(2)
            elseif (elin_nume .eq. 3) then
                node_para(1,1) = 5.d-1
                node_para(2,1) = 0.d0
                node_para(3,1) = 0.d0
                node_para(1,2) = 5.d-1
                node_para(2,2) = 5.d-1
                node_para(3,2) = 0.d0
                node_para(1,3) = 0.d0
                node_para(2,3) = 5.d-1
                node_para(3,3) = 0.d0
                elin_code='TR3'
                call reerel(elin_code,3, 3, node_para, ksi, node_real)
                poin_inte_real(1,i_poin_inte) = node_real(1)
                poin_inte_real(2,i_poin_inte) = node_real(2)
            elseif (elin_nume .eq. 4) then
                node_para(1,1) = 5.d-1
                node_para(2,1) = 5.d-1
                node_para(3,1) = 0.d0
                node_para(1,2) = 0.d0
                node_para(2,2) = 1.d0
                node_para(3,2) = 0.d0
                node_para(1,3) = 0.d0
                node_para(2,3) = 5.d-1
                node_para(3,3) = 0.d0
                elin_code='TR3'
                call reerel(elin_code,3, 3, node_para, ksi, node_real)
                poin_inte_real(1,i_poin_inte) = node_real(1)
                poin_inte_real(2,i_poin_inte) = node_real(2)
            end if
        elseif (elem_code.eq.'QU8' .and. present(elin_nume)) then
            if (elin_nume .eq. 1) then
                node_para(1,1) = -1.d0
                node_para(2,1) = -1.d0
                node_para(3,1) =  0.d0
                node_para(1,2) =  0.d0
                node_para(2,2) = -1.d0
                node_para(3,2) =  0.d0
                node_para(1,3) = -1.d0
                node_para(2,3) =  0.d0
                node_para(3,3) =  0.d0
                elin_code='TR3'
                call reerel(elin_code,3, 3, node_para, ksi, node_real)
                poin_inte_real(1,i_poin_inte) = node_real(1)
                poin_inte_real(2,i_poin_inte) = node_real(2)
            elseif (elin_nume .eq. 2) then
                node_para(1,1) =  0.d0
                node_para(2,1) = -1.d0
                node_para(3,1) =  0.d0
                node_para(1,2) =  1.d0
                node_para(2,2) = -1.d0
                node_para(3,2) =  0.d0
                node_para(1,3) =  1.d0
                node_para(2,3) =  0.d0
                node_para(3,3) =  0.d0
                elin_code='TR3'
                call reerel(elin_code,3, 3, node_para, ksi, node_real)
                poin_inte_real(1,i_poin_inte) = node_real(1)
                poin_inte_real(2,i_poin_inte) = node_real(2)                      
            elseif (elin_nume .eq. 3) then
                node_para(1,1) =  1.d0
                node_para(2,1) =  0.d0
                node_para(3,1) =  0.d0
                node_para(1,2) =  1.d0
                node_para(2,2) =  1.d0
                node_para(3,2) =  0.d0
                node_para(1,3) =  0.d0
                node_para(2,3) =  1.d0
                node_para(3,3) =  0.d0
                elin_code='TR3'
                call reerel(elin_code,3, 3, node_para, ksi, node_real)
                poin_inte_real(1,i_poin_inte) = node_real(1)
                poin_inte_real(2,i_poin_inte) = node_real(2)
            elseif (elin_nume .eq. 4) then
                node_para(1,1) =  0.d0
                node_para(2,1) =  1.d0
                node_para(3,1) =  0.d0
                node_para(1,2) = -1.d0
                node_para(2,2) =  1.d0
                node_para(3,2) =  0.d0
                node_para(1,3) = -1.d0
                node_para(2,3) =  0.d0
                node_para(3,3) =  0.d0
                elin_code='TR3'
                call reerel(elin_code,3, 3, node_para, ksi, node_real)
                poin_inte_real(1,i_poin_inte) = node_real(1)
                poin_inte_real(2,i_poin_inte) = node_real(2)
            elseif (elin_nume .eq. 5) then
                node_para(1,1) =  0.d0
                node_para(2,1) = -1.d0
                node_para(3,1) =  0.d0
                node_para(1,2) =  1.d0
                node_para(2,2) =  0.d0
                node_para(3,2) =  0.d0
                node_para(1,3) =  0.d0
                node_para(2,3) =  1.d0
                node_para(3,3) =  0.d0
                elin_code='TR3'
                call reerel(elin_code,3, 3, node_para, ksi, node_real)
                poin_inte_real(1,i_poin_inte) = node_real(1)
                poin_inte_real(2,i_poin_inte) = node_real(2)
            elseif (elin_nume .eq. 6) then
                node_para(1,1) =  0.d0
                node_para(2,1) =  1.d0
                node_para(3,1) =  0.d0
                node_para(1,2) = -1.d0
                node_para(2,2) =  0.d0
                node_para(3,2) =  0.d0
                node_para(1,3) =  0.d0
                node_para(2,3) = -1.d0
                node_para(3,3) =  0.d0
                elin_code='TR3'
                call reerel(elin_code,3, 3, node_para, ksi, node_real)
                poin_inte_real(1,i_poin_inte) = node_real(1)
                poin_inte_real(2,i_poin_inte) = node_real(2)                     
            else
                ASSERT(.false.)
            end if
        else
            poin_inte_real(1,i_poin_inte) = ksi(1)
            if ((elin_dime-1) .eq. 2) then
                poin_inte_real(2,i_poin_inte) = ksi(2)
            end if
        end if
    end do
!
! - Copy
!
    do i_poin_inte = 1, nb_poin_inte
        poin_inte(1, i_poin_inte) = poin_inte_real(1, i_poin_inte)
        poin_inte(2, i_poin_inte) = poin_inte_real(2, i_poin_inte)
    end do
!
end subroutine
