subroutine jquad4(xyzl, qsi, eta, jacob)
    implicit  none
    real(kind=8) :: xyzl(3, *), qsi, eta, jacob(*)
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
!     JACOBIEN ET LA MATRICE INVERSE AU POINT 'INT' SUR LE QUAD4
!     ------------------------------------------------------------------
    real(kind=8) :: x21, x32, x43, x14, y21, y32, y43, y14
    real(kind=8) :: j11, j12, j21, j22
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
!     ----------- MATRICE JACOBIENNE ----------------------------------
    j11 = (x21 - x43 - eta * (x43 + x21)) / 4.d0
    j12 = (y21 - y43 - eta * (y43 + y21)) / 4.d0
    j21 = (x32 - x14 + qsi * (x32 + x14)) / 4.d0
    j22 = (y32 - y14 + qsi * (y32 + y14)) / 4.d0
!     -------------- JACOBIEN -----------------------------------------
    jacob(1) = j11 * j22 - j12 * j21
!     ------- MATRICE JACOBIENNE INVERSE ------------------------------
    jacob(2) = j22 / jacob(1)
    jacob(3) = - j12 / jacob(1)
    jacob(4) = - j21 / jacob(1)
    jacob(5) = j11 / jacob(1)
!
end subroutine
