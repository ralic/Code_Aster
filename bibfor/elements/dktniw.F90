subroutine dktniw(qsi, eta, carat3, wkt)
    implicit  none
    real(kind=8) :: qsi, eta, carat3(*), wkt(9)
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
!     FONCTIONS D'INTERPOLATION DE LA FLECHE POUR L'ELEMENT DKT
!     ------------------------------------------------------------------
    real(kind=8) :: lbd, x4, x6, y4, y6, n(9)
!     ------------------------------------------------------------------
!
    x4 = carat3(1)
    x6 = carat3(3)
    y4 = carat3(4)
    y6 = carat3(6)
!
    lbd = 1.d0 - qsi - eta
!
! ----- FONCTIONS D'INTERPOLATION DANS LE REPERE REDUIT ------------
    n(1) = lbd * lbd * (3.d0 - 2.d0*lbd) + qsi * eta * lbd * 2.d0
    n(2) = lbd * lbd * qsi + qsi * eta * lbd / 2.d0
    n(3) = lbd * lbd * eta + qsi * eta * lbd / 2.d0
    n(4) = qsi * qsi * (3.d0 - 2.d0*qsi) + qsi * eta * lbd * 2.d0
    n(5) = qsi * qsi * (-1.d0 + qsi) - qsi * eta * lbd
    n(6) = qsi * qsi * eta + qsi * eta * lbd / 2.d0
    n(7) = eta * eta * (3.d0 - 2.d0*eta) + qsi * eta * lbd * 2.d0
    n(8) = eta * eta * qsi + qsi * eta * lbd / 2.d0
    n(9) = eta * eta * (-1.d0 + eta) - qsi * eta * lbd
! ----- FONCTIONS D'INTERPOLATION DANS LE REPERE LOCAL -------------
    wkt(1) = n(1)
    wkt(2) = - x4 * n(2) + x6 * n(3)
    wkt(3) = - y4 * n(2) + y6 * n(3)
    wkt(4) = n(4)
    wkt(5) = - x4 * n(5) + x6 * n(6)
    wkt(6) = - y4 * n(5) + y6 * n(6)
    wkt(7) = n(7)
    wkt(8) = - x4 * n(8) + x6 * n(9)
    wkt(9) = - y4 * n(8) + y6 * n(9)
!
end subroutine
