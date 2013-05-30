subroutine dsqbfb(qsi, eta, jacob, bfb)
    implicit none
    real(kind=8) :: qsi, eta, jacob(*), bfb(3, 12)
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
!     MATRICE BFB(3,12) AU POINT QSI ETA POUR L'ELEMENT DSQ
!     -----------------------------------------------------
    integer :: k, j
    real(kind=8) :: vj11, vj12, vj21, vj22, peta, meta, pqsi, mqsi
!     ------------------------------------------------------------------
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
    do 100 k = 1, 3
        do 101 j = 1, 12
            bfb(k,j) = 0.d0
101      continue
100  end do
!
    bfb(1, 2) = - meta * vj11 - mqsi * vj12
    bfb(1, 5) = meta * vj11 - pqsi * vj12
    bfb(1, 8) = peta * vj11 + pqsi * vj12
    bfb(1,11) = - peta * vj11 + mqsi * vj12
    bfb(2, 3) = - meta * vj21 - mqsi * vj22
    bfb(2, 6) = meta * vj21 - pqsi * vj22
    bfb(2, 9) = peta * vj21 + pqsi * vj22
    bfb(2,12) = - peta * vj21 + mqsi * vj22
    bfb(3, 2) = bfb(2, 3)
    bfb(3, 3) = bfb(1, 2)
    bfb(3, 5) = bfb(2, 6)
    bfb(3, 6) = bfb(1, 5)
    bfb(3, 8) = bfb(2, 9)
    bfb(3, 9) = bfb(1, 8)
    bfb(3,11) = bfb(2,12)
    bfb(3,12) = bfb(1,11)
!
end subroutine
