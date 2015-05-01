subroutine vpztr3(nn, ia, a, x)
    implicit none
!
!**********************************************************************
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
!**********************************************************************
!     ROUTINE BASEE SUR LA PROCEDURE INNERPROD
!     (CF. WILKINSON & REINSCH LINEAR ALGEBRA PP.340)
!
! --- DECLARATIONS
!
! ARGUMENTS
! ---------
    integer :: nn, ia
    real(kind=8) :: a(ia, *), x(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: i, j
!
!**********************************************************************
!                   DEBUT DU CODE EXECUTABLE
!**********************************************************************
!
    do 40 i = nn, 1, -1
        if (x(i) .ne. 0.d0) then
            do 20 j = nn, i+1, -1
                x(j) = x(j) + x(i)*a(j,i)
20          continue
        endif
40  end do
!
end subroutine
