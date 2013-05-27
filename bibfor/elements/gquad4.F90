subroutine gquad4(xyzl, caraq4)
    implicit none
    real(kind=8) :: xyzl(3, *), caraq4(*)
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
!     ------------------------------------------------------------------
!     GRANDEURS GEOMETRIQUES SUR LE QUAD4
!     ------------------------------------------------------------------
    real(kind=8) :: x21, x32, x43, x14, y21, y32, y43, y14
    real(kind=8) :: x31, x42, y31, y42
!     ------------------------------------------------------------------
!     -------- PROJECTION DES COTES ------------------------------------
    x21 = xyzl(1,2) - xyzl(1,1)
    x32 = xyzl(1,3) - xyzl(1,2)
    x43 = xyzl(1,4) - xyzl(1,3)
    x14 = xyzl(1,1) - xyzl(1,4)
    y21 = xyzl(2,2) - xyzl(2,1)
    y32 = xyzl(2,3) - xyzl(2,2)
    y43 = xyzl(2,4) - xyzl(2,3)
    y14 = xyzl(2,1) - xyzl(2,4)
    caraq4(1) = x21
    caraq4(2) = x32
    caraq4(3) = x43
    caraq4(4) = x14
    caraq4(5) = y21
    caraq4(6) = y32
    caraq4(7) = y43
    caraq4(8) = y14
!     -------- PROJECTION DES DIAGONALES -------------------------------
    x31 = xyzl(1,3) - xyzl(1,1)
    x42 = xyzl(1,4) - xyzl(1,2)
    y31 = xyzl(2,3) - xyzl(2,1)
    y42 = xyzl(2,4) - xyzl(2,2)
!     --------- LONGUEURS DES COTES -----------------------------------
    caraq4( 9) = sqrt(x21*x21 + y21*y21)
    caraq4(10) = sqrt(x32*x32 + y32*y32)
    caraq4(11) = sqrt(x43*x43 + y43*y43)
    caraq4(12) = sqrt(x14*x14 + y14*y14)
!     --------- COSINUS DIRECTEURS -------------------------------------
    caraq4(13) = x21 / caraq4( 9)
    caraq4(14) = x32 / caraq4(10)
    caraq4(15) = x43 / caraq4(11)
    caraq4(16) = x14 / caraq4(12)
    caraq4(17) = y21 / caraq4( 9)
    caraq4(18) = y32 / caraq4(10)
    caraq4(19) = y43 / caraq4(11)
    caraq4(20) = y14 / caraq4(12)
!     ----------- AIRE DU QUADRANGLE ----------------------------------
    caraq4(21) = ( x31 * y42 - y31 * x42)/2.d0
!     --------- AIRE DES 4 TRIANGLES -----------------------------------
    caraq4(22) = (- x21 * y14 + y21 * x14)/2.d0
    caraq4(23) = (- x32 * y21 + y32 * x21)/2.d0
    caraq4(24) = (- x43 * y32 + y43 * x32)/2.d0
    caraq4(25) = (- x14 * y43 + y14 * x43)/2.d0
!
end subroutine
