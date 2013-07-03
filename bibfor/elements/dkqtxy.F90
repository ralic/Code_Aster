subroutine dkqtxy(qsi, eta, hft2, depf, codi,&
                  lcot, vt)
    implicit  none
#include "asterfort/matini.h"
    real(kind=8) :: qsi, eta, hft2(2, 6), depf(12), codi(*), lcot(*), vt(2)
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
!     EFFORT TRANCHANT L'ELEMENT DE PLAQUE DKQ
!     ------------------------------------------------------------------
    integer :: i, j, k
    real(kind=8) :: pqsi, mqsi, peta, meta
    real(kind=8) :: cl(4), sl(4), cs(4), cu(4), su(4)
    real(kind=8) :: tkq(6, 12), bc(2, 12)
!     ------------------ PARAMETRAGE QUADRANGLE ------------------------
    integer :: nno, nc
    parameter (nno = 4)
    parameter (nc  = 4)
!     ------------------------------------------------------------------
!
    peta = 1.d0 + eta
    meta = 1.d0 - eta
    pqsi = 1.d0 + qsi
    mqsi = 1.d0 - qsi
    do 100 i = 1, nc
        cl(i) = 1.50d0 * codi( i) / lcot(i)
        sl(i) = 1.50d0 * codi(nc+i) / lcot(i)
        cs(i) = 0.75d0 * codi( i) * codi(nc+i)
        cu(i) = 0.75d0 * codi( i) * codi( i)
        su(i) = 0.75d0 * codi(nc+i) * codi(nc+i)
100  end do
    tkq(1,1 ) = - meta * cl(1)
    tkq(1,2 ) = meta * cu(1)
    tkq(1,3 ) = meta * cs(1)
    tkq(1,4 ) = meta * cl(1)
    tkq(1,5 ) = meta * cu(1)
    tkq(1,6 ) = meta * cs(1)
    tkq(1,7 ) = - peta * cl(3)
    tkq(1,8 ) = peta * cu(3)
    tkq(1,9 ) = peta * cs(3)
    tkq(1,10) = peta * cl(3)
    tkq(1,11) = peta * cu(3)
    tkq(1,12) = peta * cs(3)
    tkq(2,1 ) = mqsi * cl(4)
    tkq(2,2 ) = mqsi * cu(4)
    tkq(2,3 ) = mqsi * cs(4)
    tkq(2,4 ) = - pqsi * cl(2)
    tkq(2,5 ) = pqsi * cu(2)
    tkq(2,6 ) = pqsi * cs(2)
    tkq(2,7 ) = pqsi * cl(2)
    tkq(2,8 ) = pqsi * cu(2)
    tkq(2,9 ) = pqsi * cs(2)
    tkq(2,10) = - mqsi * cl(4)
    tkq(2,11) = mqsi * cu(4)
    tkq(2,12) = mqsi * cs(4)
    tkq(3,1 ) = qsi * cl(1) - eta * cl(4)
    tkq(3,2 ) = - qsi * cu(1) - eta * cu(4) + 0.25d0
    tkq(3,3 ) = - qsi * cs(1) - eta * cs(4)
    tkq(3,4 ) = - qsi * cl(1) - eta * cl(2)
    tkq(3,5 ) = - qsi * cu(1) + eta * cu(2) - 0.25d0
    tkq(3,6 ) = - qsi * cs(1) + eta * cs(2)
    tkq(3,7 ) = - qsi * cl(3) + eta * cl(2)
    tkq(3,8 ) = qsi * cu(3) + eta * cu(2) + 0.25d0
    tkq(3,9 ) = qsi * cs(3) + eta * cs(2)
    tkq(3,10) = qsi * cl(3) + eta * cl(4)
    tkq(3,11) = qsi * cu(3) - eta * cu(4) - 0.25d0
    tkq(3,12) = qsi * cs(3) - eta * cs(4)
    tkq(4,1 ) = - meta * sl(1)
    tkq(4,2 ) = meta * cs(1)
    tkq(4,3 ) = meta * su(1)
    tkq(4,4 ) = meta * sl(1)
    tkq(4,5 ) = meta * cs(1)
    tkq(4,6 ) = meta * su(1)
    tkq(4,7 ) = - peta * sl(3)
    tkq(4,8 ) = peta * cs(3)
    tkq(4,9 ) = peta * su(3)
    tkq(4,10) = peta * sl(3)
    tkq(4,11) = peta * cs(3)
    tkq(4,12) = peta * su(3)
    tkq(5,1 ) = mqsi * sl(4)
    tkq(5,2 ) = mqsi * cs(4)
    tkq(5,3 ) = mqsi * su(4)
    tkq(5,4 ) = - pqsi * sl(2)
    tkq(5,5 ) = pqsi * cs(2)
    tkq(5,6 ) = pqsi * su(2)
    tkq(5,7 ) = pqsi * sl(2)
    tkq(5,8 ) = pqsi * cs(2)
    tkq(5,9 ) = pqsi * su(2)
    tkq(5,10) = - mqsi * sl(4)
    tkq(5,11) = mqsi * cs(4)
    tkq(5,12) = mqsi * su(4)
    tkq(6,1 ) = qsi * sl(1) - eta * sl(4)
    tkq(6,2 ) = - qsi * cs(1) - eta * cs(4)
    tkq(6,3 ) = - qsi * su(1) - eta * su(4) + 0.25d0
    tkq(6,4 ) = - qsi * sl(1) - eta * sl(2)
    tkq(6,5 ) = - qsi * cs(1) + eta * cs(2)
    tkq(6,6 ) = - qsi * su(1) + eta * su(2) - 0.25d0
    tkq(6,7 ) = - qsi * sl(3) + eta * sl(2)
    tkq(6,8 ) = qsi * cs(3) + eta * cs(2)
    tkq(6,9 ) = qsi * su(3) + eta * su(2) + 0.25d0
    tkq(6,10) = qsi * sl(3) + eta * sl(4)
    tkq(6,11) = qsi * cs(3) - eta * cs(4)
    tkq(6,12) = qsi * su(3) - eta * su(4) - 0.25d0
!
!     ------ VT = HFT2.TKQ.DEPF ------------------------------------
!
    call matini(2, 12, 0.d0, bc)
!
    do 130 i = 1, 2
        do 120 j = 1, 3*nno
            do 110 k = 1, 6
                bc(i,j) = bc(i,j) + hft2(i,k) * tkq(k,j)
110          continue
120      continue
130  end do
    vt(1) = 0.d0
    vt(2) = 0.d0
    do 150 i = 1, 2
        do 140 j = 1, 3*nno
            vt(i) = vt(i) + bc(i,j) * depf(j)
140      continue
150  end do
!
end subroutine
