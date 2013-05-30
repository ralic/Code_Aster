subroutine dsqlxy(qsi, eta, hlt2, an, depf,&
                  codi, lambda)
    implicit  none
    real(kind=8) :: qsi, eta, codi(*), hlt2(4, 6), an(4, 12), depf(12)
    real(kind=8) :: lambda(4)
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     'LAMBDA' DE L'ELEMENT DE PLAQUE DSQ
!     ------------------------------------------------------------------
    integer :: i, j, k
    real(kind=8) :: pqsi, mqsi, peta, meta
    real(kind=8) :: c(4), s(4)
    real(kind=8) :: ta(6, 4), tb(6, 12)
    real(kind=8) :: blb(4, 12), bla(4, 4), bln(4, 12)
!     ------------------------------------------------------------------
!
!
    do 200 k = 1, 6
        do 201 j = 1, 12
            tb(k,j) = 0.d0
201      continue
200  end do
    tb(3,2) = 0.25d0
    tb(3,5) = -0.25d0
    tb(3,8) = 0.25d0
    tb(3,11) = -0.25d0
    tb(6,3) = 0.25d0
    tb(6,6) = -0.25d0
    tb(6,9) = 0.25d0
    tb(6,12) = -0.25d0
    c(1) = codi(1)
    c(2) = codi(2)
    c(3) = codi(3)
    c(4) = codi(4)
    s(1) = codi(5)
    s(2) = codi(6)
    s(3) = codi(7)
    s(4) = codi(8)
!
    peta = 1.d0 + eta
    meta = 1.d0 - eta
    pqsi = 1.d0 + qsi
    mqsi = 1.d0 - qsi
    do 224 k = 1, 6
        do 225 j = 1, 4
            ta(k,j) = 0.d0
225      continue
224  end do
    ta(1,1) = -meta*c(1)
    ta(1,3) = -peta*c(3)
    ta(2,2) = -pqsi*c(2)
    ta(2,4) = -mqsi*c(4)
    ta(3,1) = qsi*c(1)
    ta(3,2) = -eta*c(2)
    ta(3,3) = -qsi*c(3)
    ta(3,4) = eta*c(4)
    ta(4,1) = -meta*s(1)
    ta(4,3) = -peta*s(3)
    ta(5,2) = -pqsi*s(2)
    ta(5,4) = -mqsi*s(4)
    ta(6,1) = qsi*s(1)
    ta(6,2) = -eta*s(2)
    ta(6,3) = -qsi*s(3)
    ta(6,4) = eta*s(4)
!        -------------- BLA = HLT2.TA ----------------------------
    do 228 i = 1, 4
        do 230 j = 1, 4
            bla(i,j) = 0.d0
            do 232 k = 1, 6
                bla(i,j) = bla(i,j) + hlt2(i,k)*ta(k,j)
232          continue
230      continue
228  end do
!        -------------- BLB = HLT2.TB ----------------------------
    do 236 i = 1, 4
        do 238 j = 1, 12
            blb(i,j) = 0.d0
            do 240 k = 1, 6
                blb(i,j) = blb(i,j) + hlt2(i,k)*tb(k,j)
240          continue
238      continue
236  end do
!        -------- LAMBDA = (BLB + BLA.AN).DEPF ------------------
    do 242 i = 1, 4
        lambda(i) = 0.d0
242  end do
    do 246 i = 1, 4
        do 248 j = 1, 12
            bln(i,j) = 0.d0
            do 250 k = 1, 4
                bln(i,j) = bln(i,j) + bla(i,k)*an(k,j)
250          continue
            lambda(i) = lambda(i) + (blb(i,j)+bln(i,j))*depf(j)
248      continue
246  end do
!
end subroutine
