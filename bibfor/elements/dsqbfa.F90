subroutine dsqbfa(qsi, eta, jacob, caraq4, bfa)
    implicit  none
    real(kind=8) :: qsi, eta, jacob(*), caraq4(*), bfa(3, 4)
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
!     -----------------------------------------------------
!     MATRICE BFA(3,4) AU POINT QSI ETA POUR L'ELEMENT DSQ
!     -----------------------------------------------------
    real(kind=8) :: vj11, vj12, vj21, vj22
    real(kind=8) :: peta, meta, pqsi, mqsi, etac, qsic
    real(kind=8) :: c5, c6, c7, c8, s5, s6, s7, s8, px1, px2, px3, px4, py1, py2
    real(kind=8) :: py3, py4
!     ------------------------------------------------------------------
    vj11 = jacob(1)
    vj12 = jacob(2)
    vj21 = jacob(3)
    vj22 = jacob(4)
    c5 = caraq4(13)
    c6 = caraq4(14)
    c7 = caraq4(15)
    c8 = caraq4(16)
    s5 = caraq4(17)
    s6 = caraq4(18)
    s7 = caraq4(19)
    s8 = caraq4(20)
!
    peta = 1.d0 + eta
    meta = 1.d0 - eta
    pqsi = 1.d0 + qsi
    mqsi = 1.d0 - qsi
    etac = (1.d0 - eta * eta) / 2.d0
    qsic = (1.d0 - qsi * qsi) / 2.d0
!
    px1 = - qsi * meta * vj11 - qsic * vj12
    px2 = - eta * pqsi * vj12 + etac * vj11
    px3 = - qsi * peta * vj11 + qsic * vj12
    px4 = - eta * mqsi * vj12 - etac * vj11
    py1 = - qsi * meta * vj21 - qsic * vj22
    py2 = - eta * pqsi * vj22 + etac * vj21
    py3 = - qsi * peta * vj21 + qsic * vj22
    py4 = - eta * mqsi * vj22 - etac * vj21
!
    bfa(1,1) = px1 * c5
    bfa(1,2) = px2 * c6
    bfa(1,3) = px3 * c7
    bfa(1,4) = px4 * c8
    bfa(2,1) = py1 * s5
    bfa(2,2) = py2 * s6
    bfa(2,3) = py3 * s7
    bfa(2,4) = py4 * s8
    bfa(3,1) = py1 * c5 + px1 * s5
    bfa(3,2) = py2 * c6 + px2 * s6
    bfa(3,3) = py3 * c7 + px3 * s7
    bfa(3,4) = py4 * c8 + px4 * s8
!
end subroutine
