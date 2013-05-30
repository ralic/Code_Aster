subroutine dstbfa(qsi, eta, carat3, bfa)
    implicit  none
    real(kind=8) :: qsi, eta, carat3(*), bfa(3, 3)
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
!     ----------------------------------------------------
!     MATRICE BFA(3,3) AU POINT QSI ETA POUR L'ELEMENT DST
!     ----------------------------------------------------
    real(kind=8) :: vj11, vj12, vj21, vj22
    real(kind=8) :: lmq, lme
    real(kind=8) :: c4, c5, c6, s4, s5, s6
!     ------------------------------------------------------------------
!
    lmq = 1.d0 - 2.d0 * qsi - eta
    lme = 1.d0 - qsi - 2.d0 * eta
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
!
    bfa(1,1) = 4.d0*( vj11*lmq - vj12*qsi)*c4
    bfa(1,2) = 4.d0*( vj11*eta + vj12*qsi)*c5
    bfa(1,3) = 4.d0*(-vj11*eta + vj12*lme)*c6
    bfa(2,1) = 4.d0*( vj21*lmq - vj22*qsi)*s4
    bfa(2,2) = 4.d0*( vj21*eta + vj22*qsi)*s5
    bfa(2,3) = 4.d0*(-vj21*eta + vj22*lme)*s6
    bfa(3,1) = 4.d0*(lmq*(vj21*c4 + vj11*s4)-qsi*(vj22*c4 + vj12*s4))
    bfa(3,2) = 4.d0*(eta*(vj21*c5 + vj11*s5)+qsi*(vj22*c5 + vj12*s5))
    bfa(3,3) =-4.d0*(eta*(vj21*c6 + vj11*s6)-lme*(vj22*c6 + vj12*s6))
!
end subroutine
