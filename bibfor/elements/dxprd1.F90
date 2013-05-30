subroutine dxprd1(dfpla1, dfpla2, dc, dfpla3, dfpla4,&
                  mat)
!-----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     REALISE LE CALCUL DES MATRICES INTERVENANT DANS LE
!     CALCUL DE TANGENTE DANS LE CAS DE LA LOI DE COMPORTEMENT GLRC
! ----------------------------------------------------------------------
    implicit none
    include 'asterfort/lcprte.h'
    include 'asterfort/pmat.h'
    common /tdim/ n, nd
    real(kind=8) :: dfpla1(6), dfpla2(6), dfpla3(6), dfpla4(6)
    real(kind=8) :: mata(6, 6), matb(6, 6), matc(6, 6), mat(6, 6)
    real(kind=8) :: dc(6, 6)
    integer :: n, nd
!
    n = 6
    call lcprte(dfpla1, dfpla2, mata)
    call lcprte(dfpla3, dfpla4, matb)
    call pmat(6, mata, dc, matc)
    call pmat(6, matc, matb, mat)
!
end subroutine
