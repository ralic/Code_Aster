subroutine dktbf(qsi, eta, carat3, bf)
    implicit  none
    real(kind=8) :: qsi, eta, carat3(*), bf(3, 9)
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
!        ORDRE DES COMPOSANTES POUR BF :
!        DEFORMATIONS : KXX, KYY, KXY   (REP. INTRINSEQUE DE L'ELEMENT)
!        DEPLACEMENTS : DZ, DRY, -DRX   (REP. INTRINSEQUE DE L'ELEMENT)
!     ------------------------------------------------------------------
!     MATRICE B(3,9) AU POINT QSI ETA POUR L'ELEMENT DKT (FLEXION)
!     ------------------------------------------------------------------
    integer :: i
    real(kind=8) :: vj11, vj12, vj21, vj22
    real(kind=8) :: lmq, lme
    real(kind=8) :: l4, l5, l6, c4, c5, c6, s4, s5, s6
    real(kind=8) :: cu4, cu5, cu6, su4, su5, su6, cs4, cs5, cs6
    real(kind=8) :: cl4, cl5, cl6, sl4, sl5, sl6
    real(kind=8) :: bxq(9), byq(9), bxe(9), bye(9)
!     ------------------------------------------------------------------
    vj11 = carat3( 9)
    vj12 = carat3(10)
    vj21 = carat3(11)
    vj22 = carat3(12)
    c4 = carat3(16)
    c5 = carat3(17)
    c6 = carat3(18)
    s4 = carat3(19)
    s5 = carat3(20)
    s6 = carat3(21)
    l4 = carat3(13)
    l5 = carat3(14)
    l6 = carat3(15)
!
    cu4 = 3.d0 * c4 * c4
    cu5 = 3.d0 * c5 * c5
    cu6 = 3.d0 * c6 * c6
    su4 = 3.d0 * s4 * s4
    su5 = 3.d0 * s5 * s5
    su6 = 3.d0 * s6 * s6
    cs4 = 3.d0 * c4 * s4
    cs5 = 3.d0 * c5 * s5
    cs6 = 3.d0 * c6 * s6
    cl4 = 6.d0 * c4 / l4
    cl5 = 6.d0 * c5 / l5
    cl6 = 6.d0 * c6 / l6
    sl4 = 6.d0 * s4 / l4
    sl5 = 6.d0 * s5 / l5
    sl6 = 6.d0 * s6 / l6
    lmq = 1.d0 - 2.d0 * qsi - eta
    lme = 1.d0 - qsi - 2.d0 * eta
!
!     ------ DERIVEE DE BETAX(I) / QSI ------
    bxq(1) = lmq * cl4 + eta * cl6
    bxq(2) = - 1.d0 - lmq * cu4 + eta * cu6
    bxq(3) = - lmq * cs4 + eta * cs6
    bxq(4) = - lmq * cl4 + eta * cl5
    bxq(5) = 1.d0 - lmq * cu4 - eta * cu5
    bxq(6) = - lmq * cs4 - eta * cs5
    bxq(7) = - eta * cl6 - eta * cl5
    bxq(8) = eta * cu6 - eta * cu5
    bxq(9) = eta * cs6 - eta * cs5
!     ------ DERIVEE DE BETAY(I) / QSI ------
    byq(1) = lmq * sl4 + eta * sl6
    byq(2) = bxq(3)
    byq(3) = - 1.d0 - lmq * su4 + eta * su6
    byq(4) = - lmq * sl4 + eta * sl5
    byq(5) = bxq(6)
    byq(6) = 1.d0 - lmq * su4 - eta * su5
    byq(7) = - eta * sl6 - eta * sl5
    byq(8) = bxq(9)
    byq(9) = eta * su6 - eta * su5
!     ------ DERIVEE DE BETAX(I) / ETA ------
    bxe(1) = - lme * cl6 - qsi * cl4
    bxe(2) = - 1.d0 - lme * cu6 + qsi * cu4
    bxe(3) = - lme * cs6 + qsi * cs4
    bxe(4) = qsi * cl4 + qsi * cl5
    bxe(5) = qsi * cu4 - qsi * cu5
    bxe(6) = qsi * cs4 - qsi * cs5
    bxe(7) = lme * cl6 - qsi * cl5
    bxe(8) = 1.d0 - lme * cu6 - qsi * cu5
    bxe(9) = - lme * cs6 - qsi * cs5
!     ------ DERIVEE DE BETAY(I) / ETA ------
    bye(1) = - lme * sl6 - qsi * sl4
    bye(2) = bxe(3)
    bye(3) = - 1.d0 - lme * su6 + qsi * su4
    bye(4) = qsi * sl4 + qsi * sl5
    bye(5) = bxe(6)
    bye(6) = qsi * su4 - qsi * su5
    bye(7) = lme * sl6 - qsi * sl5
    bye(8) = bxe(9)
    bye(9) = 1.d0 - lme * su6 - qsi * su5
!
!     --------------------- CALCUL DE B -------------------------------
    do 100 i = 1, 9
        bf(1,i) = vj11*bxq(i) + vj12*bxe(i)
        bf(2,i) = vj21*byq(i) + vj22*bye(i)
        bf(3,i) = vj11*byq(i) + vj12*bye(i) + vj21*bxq(i) + vj22*bxe( i)
100  end do
!
end subroutine
