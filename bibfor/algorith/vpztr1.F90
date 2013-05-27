subroutine vpztr1(mm, nn, ia, a, x,&
                  y, alfa)
!
!**********************************************************************
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!**********************************************************************
!                         DECLARATIONS
!**********************************************************************
!
    implicit none
!
! ARGUMENTS
! ---------
    integer :: mm, nn, ia
    real(kind=8) :: alfa
    real(kind=8) :: a(ia, *), x(*), y(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: i, j
    real(kind=8) :: temp
!
!**********************************************************************
!                   DEBUT DU CODE EXECUTABLE
!**********************************************************************
!
    do 40 i = 1, nn
        if (x(i) .ne. 0.d0) then
            temp = alfa * x(i)
            do 20 j = 1, mm
                y(j)=y(j) + temp*a(j,i)
20          continue
        endif
40  end do
!
end subroutine
