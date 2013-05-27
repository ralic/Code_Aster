subroutine trapez(x, y, npt, aire)
! **********************************************************************
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
! **********************************************************************
!
! DESCRIPTION :  INTEGRALE PAR METHODE DES TRAPEZES
! -----------    DES NPT POINT X, Y
!                RESULTAT DANS AIRE
!
!
! ARGUMENTS
! ---------
    implicit none
    integer :: npt
    real(kind=8) :: x(*), y(*), aire
!
! VARIABLES LOCALES
! -----------------
    integer :: i
!
!
    aire = 0.0d0
!
    do 100 i = 1, npt-1
!
        aire = aire + ((x(i+1)-x(i)) * (y(i+1)+y(i)) / 2.0d0)
!
100  end do
end subroutine
