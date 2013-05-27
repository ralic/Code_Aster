subroutine mlfmlt(b, f, y, ldb, n,&
                  p, l, opta, optb, nb)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     B = B - F*Y PAR BLOCS
    implicit none
    include 'blas/dgemm.h'
    integer :: ldb, n, p, l, nb
    real(kind=8) :: b(ldb, l), f(n, p), y(ldb, l)
    integer :: m, npb, restp, nlb, restl
    integer :: i, j, ib, jb
    real(kind=8) :: beta, alpha
    integer :: opta, optb
    character(len=1) :: tra, trb
!
    tra='T'
    if (opta .eq. 1) tra='N'
    trb='T'
    if (optb .eq. 1) trb='N'
    alpha = -1.d0
    beta = 1.d0
    m= n - p
    npb=p/nb
    nlb=l/nb
    restp = p - (nb*npb)
    restl= l - (nb*nlb)
    if (nlb .gt. 0) then
        do 500 j = 1, nlb
            jb= nb*(j-1)+1
            do 600 i = 1, npb
                ib= nb*(i-1)+1
                call dgemm(tra, trb, nb, nb, m,&
                           alpha, f(1, ib), n, y(1, jb), ldb,&
                           beta, b(ib, jb), ldb)
600          continue
            if (restp .gt. 0) then
                ib=nb*npb+1
                call dgemm(tra, trb, restp, nb, m,&
                           alpha, f(1, ib), n, y(1, jb), ldb,&
                           beta, b(ib, jb), ldb)
            endif
500      continue
    endif
    if (restl .gt. 0) then
        jb=nb*nlb+1
        do 601 i = 1, npb
            ib= nb*(i-1)+1
            call dgemm(tra, trb, nb, restl, m,&
                       alpha, f(1, ib), n, y(1, jb), ldb,&
                       beta, b(ib, jb), ldb)
601      continue
        if (restp .gt. 0) then
            ib=nb*npb+1
            call dgemm(tra, trb, restp, restl, m,&
                       alpha, f(1, ib), n, y(1, jb), ldb,&
                       beta, b(ib, jb), ldb)
        endif
    endif
end subroutine
