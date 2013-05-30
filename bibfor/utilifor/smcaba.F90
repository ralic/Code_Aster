subroutine smcaba(ftrc, trc, nbhist, x, dz,&
                  ind)
    implicit   none
    include 'asterc/r8prem.h'
    include 'asterfort/assert.h'
    include 'asterfort/rslsvd.h'
    include 'asterfort/smcosl.h'
    integer :: ind(6), nbhist
    real(kind=8) :: ftrc((3*nbhist), 3), trc((3*nbhist), 5), x(5), dz(4)
!-----------------------------------------------------------------------
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
!
!         CALCUL DES COORDONNEES BARYCENTRIQUES ET DE DZT
!-----------------------------------------------------------------------
    integer :: ifail, i, nz
    real(kind=8) :: som, alemb(6), a(6, 6), b(6)
    real(kind=8) :: epsmac, work(96), zero, s(6), u(6, 6), v(6, 6)
!     ------------------------------------------------------
!
!     CALCUL DES COORDONNEES BARYCENTRIQUES
!
    zero = 0.d0
    call smcosl(trc, ind, a, b, x,&
                nbhist)
!
    epsmac = r8prem()
!
    call rslsvd(6, 6, 6, a(1, 1), s(1),&
                u(1, 1), v(1, 1), 1, b(1), epsmac,&
                ifail, work(1))
!
!     PROBLEME DANS LA RESOLUTION DU SYSTEME SOUS CONTRAINT VSRSRR
    call assert(ifail .eq. 0)
!
    do 10 i = 1, 6
        alemb(i) = b(i)
10  end do
    som = zero
    do 20 i = 1, 6
        if (alemb(i) .lt. zero) alemb(i)=zero
        som = som + alemb(i)
20  end do
    if (som .eq. zero) then
        do 30 nz = 1, 3
            dz(nz) = ftrc(ind(1),nz)
30      continue
    else
        do 50 nz = 1, 3
            dz(nz) = zero
            do 40 i = 1, 6
                dz(nz) = dz(nz) + alemb(i)*ftrc(ind(i),nz)/som
40          continue
50      continue
    endif
!
end subroutine
