subroutine gtria3(xyzl, carat3)
    implicit none
    real(kind=8) :: xyzl(3, *), carat3(*)
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
!     GRANDEURS GEOMETRIQUES ET JACOBIEN SUR LE TRIA3
!     ------------------------------------------------------------------
    real(kind=8) :: x21, x32, x13, y21, y32, y13
!     ------------------------------------------------------------------
    x21 = xyzl(1,2) - xyzl(1,1)
    x32 = xyzl(1,3) - xyzl(1,2)
    x13 = xyzl(1,1) - xyzl(1,3)
    y21 = xyzl(2,2) - xyzl(2,1)
    y32 = xyzl(2,3) - xyzl(2,2)
    y13 = xyzl(2,1) - xyzl(2,3)
    carat3(1) = x21
    carat3(2) = x32
    carat3(3) = x13
    carat3(4) = y21
    carat3(5) = y32
    carat3(6) = y13
!     -------------- JACOBIEN -----------------------------------------
    carat3(7) = - x21 * y13 + y21 * x13
!     ------------ AIRE DU TRIANGLE -----------------------------------
    carat3(8) = carat3(7)/2.d0
!     ------- MATRICE JACOBIENNE INVERSE ------------------------------
    carat3( 9) = - y13 / carat3(7)
    carat3(10) = - y21 / carat3(7)
    carat3(11) = x13 / carat3(7)
    carat3(12) = x21 / carat3(7)
!     --------- LONGUEURS DES COTES -----------------------------------
    carat3(13) = sqrt(x21*x21 + y21*y21)
    carat3(14) = sqrt(x32*x32 + y32*y32)
    carat3(15) = sqrt(x13*x13 + y13*y13)
!     --------- COSINUS DIRECTEURS -------------------------------------
    carat3(16) = x21 / carat3(13)
    carat3(17) = x32 / carat3(14)
    carat3(18) = x13 / carat3(15)
    carat3(19) = y21 / carat3(13)
    carat3(20) = y32 / carat3(14)
    carat3(21) = y13 / carat3(15)
end subroutine
