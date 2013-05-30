subroutine dkqbf(qsi, eta, jacob, caraq4, bf)
    implicit  none
    real(kind=8) :: qsi, eta, jacob(*), caraq4(*), bf(3, 12)
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
!     ---------------------------------------------------
!     MATRICE B(3,12) AU POINT QSI ETA POUR L'ELEMENT DKQ
!     ---------------------------------------------------
    integer :: i
    real(kind=8) :: vj11, vj12, vj21, vj22
    real(kind=8) :: peta, meta, pqsi, mqsi, qsic, etac
    real(kind=8) :: l5, l6, l7, l8, c5, c6, c7, c8, s5, s6, s7, s8
    real(kind=8) :: cu5, cu6, cu7, cu8, su5, su6, su7, su8, cs5, cs6, cs7, cs8
    real(kind=8) :: cl5, cl6, cl7, cl8, sl5, sl6, sl7, sl8
    real(kind=8) :: bxq(12), byq(12), bxe(12), bye(12)
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
    l5 = caraq4( 9)
    l6 = caraq4(10)
    l7 = caraq4(11)
    l8 = caraq4(12)
!
    peta = (1.d0 + eta) / 4.d0
    meta = (1.d0 - eta) / 4.d0
    pqsi = (1.d0 + qsi) / 4.d0
    mqsi = (1.d0 - qsi) / 4.d0
    etac = (1.d0 - eta * eta) / 8.d0
    qsic = (1.d0 - qsi * qsi) / 8.d0
!
    cu5 = 3.d0 * c5 * c5
    cu6 = 3.d0 * c6 * c6
    cu7 = 3.d0 * c7 * c7
    cu8 = 3.d0 * c8 * c8
    su5 = 3.d0 * s5 * s5
    su6 = 3.d0 * s6 * s6
    su7 = 3.d0 * s7 * s7
    su8 = 3.d0 * s8 * s8
    cs5 = 3.d0 * c5 * s5
    cs6 = 3.d0 * c6 * s6
    cs7 = 3.d0 * c7 * s7
    cs8 = 3.d0 * c8 * s8
    cl5 = 6.d0 * c5 / l5
    cl6 = 6.d0 * c6 / l6
    cl7 = 6.d0 * c7 / l7
    cl8 = 6.d0 * c8 / l8
    sl5 = 6.d0 * s5 / l5
    sl6 = 6.d0 * s6 / l6
    sl7 = 6.d0 * s7 / l7
    sl8 = 6.d0 * s8 / l8
!
!     --------- DERIVEE DE BETAX(I) / QSI ------------
    bxq(1) = - qsi * meta * cl5 + etac * cl8
    bxq(2) = - meta + qsi * meta * cu5 + etac * cu8
    bxq(3) = qsi * meta * cs5 + etac * cs8
    bxq(4) = qsi * meta * cl5 + etac * cl6
    bxq(5) = meta + qsi * meta * cu5 - etac * cu6
    bxq(6) = qsi * meta * cs5 - etac * cs6
    bxq(7) = - qsi * peta * cl7 - etac * cl6
    bxq(8) = peta + qsi * peta * cu7 - etac * cu6
    bxq(9) = qsi * peta * cs7 - etac * cs6
    bxq(10) = qsi * peta * cl7 - etac * cl8
    bxq(11) = - peta + qsi * peta * cu7 + etac * cu8
    bxq(12) = qsi * peta * cs7 + etac * cs8
!     --------- DERIVEE DE BETAY(I) / QSI ------------
    byq(1) = - qsi * meta * sl5 + etac * sl8
    byq(2) = bxq(3)
    byq(3) = - meta + qsi * meta * su5 + etac * su8
    byq(4) = qsi * meta * sl5 + etac * sl6
    byq(5) = bxq(6)
    byq(6) = meta + qsi * meta * su5 - etac * su6
    byq(7) = - qsi * peta * sl7 - etac * sl6
    byq(8) = bxq(9)
    byq(9) = peta + qsi * peta * su7 - etac * su6
    byq(10) = qsi * peta * sl7 - etac * sl8
    byq(11) = bxq(12)
    byq(12) = - peta + qsi * peta * su7 + etac * su8
!     --------- DERIVEE DE BETAX(I) / ETA ------------
    bxe(1) = eta * mqsi * cl8 - qsic * cl5
    bxe(2) = - mqsi + eta * mqsi * cu8 + qsic * cu5
    bxe(3) = eta * mqsi * cs8 + qsic * cs5
    bxe(4) = - eta * pqsi * cl6 + qsic * cl5
    bxe(5) = - pqsi + eta * pqsi * cu6 + qsic * cu5
    bxe(6) = eta * pqsi * cs6 + qsic * cs5
    bxe(7) = eta * pqsi * cl6 + qsic * cl7
    bxe(8) = pqsi + eta * pqsi * cu6 - qsic * cu7
    bxe(9) = eta * pqsi * cs6 - qsic * cs7
    bxe(10) = - eta * mqsi * cl8 - qsic * cl7
    bxe(11) = mqsi + eta * mqsi * cu8 - qsic * cu7
    bxe(12) = eta * mqsi * cs8 - qsic * cs7
!     --------- DERIVEE DE BETAY(I) / ETA ------------
    bye(1) = eta * mqsi * sl8 - qsic * sl5
    bye(2) = bxe(3)
    bye(3) = - mqsi + eta * mqsi * su8 + qsic * su5
    bye(4) = - eta * pqsi * sl6 + qsic * sl5
    bye(5) = bxe(6)
    bye(6) = - pqsi + eta * pqsi * su6 + qsic * su5
    bye(7) = eta * pqsi * sl6 + qsic * sl7
    bye(8) = bxe(9)
    bye(9) = pqsi + eta * pqsi * su6 - qsic * su7
    bye(10) = - eta * mqsi * sl8 - qsic * sl7
    bye(11) = bxe(12)
    bye(12) = mqsi + eta * mqsi * su8 - qsic * su7
!
!     --------------------- CALCUL DE B -------------------------------
    do 100 i = 1, 12
        bf(1,i) = vj11*bxq(i) + vj12*bxe(i)
        bf(2,i) = vj21*byq(i) + vj22*bye(i)
        bf(3,i) = vj11*byq(i) + vj12*bye(i) + vj21*bxq(i) + vj22*bxe( i)
100  end do
end subroutine
