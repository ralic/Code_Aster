subroutine dkqbnl(qsi, eta, jacob, bnl)
    implicit  none
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    real(kind=8) :: qsi, eta, jacob(*), bnl(2, 12)
!
! AJOUT ELEMENTS
!     ------------------------------------------------------------------
!     MATRICE BNL(2,12) MEMBRANE NON-LINEAIRE AU POINT QSI ETA
!                       POUR ELEMENTS DKQ ET DSQ
!     ------------------------------------------------------------------
    real(kind=8) :: vj11, vj12, vj21, vj22
    real(kind=8) :: peta, meta, pqsi, mqsi
!     ------------------------------------------------------------------
!
    vj11 = jacob(1)
    vj12 = jacob(2)
    vj21 = jacob(3)
    vj22 = jacob(4)
!
    peta = (1.d0 + eta) / 4.d0
    meta = (1.d0 - eta) / 4.d0
    pqsi = (1.d0 + qsi) / 4.d0
    mqsi = (1.d0 - qsi) / 4.d0
!
    bnl(1,1) = - meta * vj11 - mqsi * vj12
    bnl(1,2) = 0.d0
    bnl(1,3) = 0.d0
    bnl(1,4) = meta * vj11 - pqsi * vj12
    bnl(1,5) = 0.d0
    bnl(1,6) = 0.d0
    bnl(1,7) = peta * vj11 + pqsi * vj12
    bnl(1,8) = 0.d0
    bnl(1,9) = 0.d0
    bnl(1,10) = - peta * vj11 + mqsi * vj12
    bnl(1,11) = 0.d0
    bnl(1,12) = 0.d0
!
    bnl(2,1) = - meta * vj21 - mqsi * vj22
    bnl(2,2) = 0.d0
    bnl(2,3) = 0.d0
    bnl(2,4) = meta * vj21 - pqsi * vj22
    bnl(2,5) = 0.d0
    bnl(2,6) = 0.d0
    bnl(2,7) = peta * vj21 + pqsi * vj22
    bnl(2,8) = 0.d0
    bnl(2,9) = 0.d0
    bnl(2,10) = - peta * vj21 + mqsi * vj22
    bnl(2,11) = 0.d0
    bnl(2,12) = 0.d0
!
end subroutine
