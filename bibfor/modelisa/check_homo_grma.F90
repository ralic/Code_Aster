subroutine check_homo_grma(cara, nval)
    implicit none
    character(len=*), intent(in) :: cara(*)
    integer, intent(in) :: nval
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!   AFFE_CARA_ELEM
!   Vérifie la présence de R_DEBUT et R_FIN
!
#include "asterfort/utmess.h"
! ----------------------------------------------------------------------
    integer, parameter :: nk = 4
    character(len=3) :: tcar(nk)
    integer :: nv, i, j
    character(len=8) :: carpou(nk)
!
    data carpou /'R_DEBUT', 'R_FIN', 'EP_DEBUT', 'EP_FIN'/
!
    nv = min(4, nval)
    tcar = '   '
    do i = 1, nv
        do j = 1, nk
            if (cara(i) .eq. carpou(j)) then
                tcar(j) = cara(i)
                exit
            endif
        end do
    end do
!
    if (tcar(1)(1:1).eq. ' ')then
        call utmess('F', 'MODELISA5_54', sk=carpou(1))
    endif
    if (tcar(2)(1:1).eq. ' ')then
        call utmess('F', 'MODELISA5_54', sk=carpou(2))
    endif
!
end subroutine
