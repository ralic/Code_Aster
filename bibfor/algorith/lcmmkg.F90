subroutine lcmmkg(zinv, nvi, vind, vinf, nmat,&
                  materf, mod, nr, dsde)
    implicit none
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    include 'asterfort/lcopli.h'
    include 'asterfort/lcprmv.h'
    include 'asterfort/lctr2m.h'
    include 'asterfort/matinv.h'
    include 'asterfort/pmat.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/tnsvec.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    include 'blas/dscal.h'
    real(kind=8) :: fep(3, 3), fem(3, 3), vinf(*), vind(*), dsde(6, 3, 3), det
    real(kind=8) :: dfedf(3, 3, 3, 3), fpp(3, 3), fppinv(3, 3), id(3, 3)
    real(kind=8) :: dr1df(3, 3, 3, 3), dr1df6(6, 3, 3), zinv(6, 6)
    real(kind=8) :: dsdf(6, 3, 3)
    real(kind=8) :: fet(3, 3), fetfe(3, 3), eel(6), hooke(6, 6), s6(6), s(3, 3)
    real(kind=8) :: dtaudf(3, 3, 3, 3), materf(*), sfet(3, 3)
    integer :: nr, ndt, ndi, i, j, k, l, m, n, nmat, ind(3, 3), nvi
    common /tdim/ ndt,ndi
    character(len=8) :: mod
!     ------------------------------------------------------------------
    data id/1.d0,0.d0,0.d0, 0.d0,1.d0,0.d0, 0.d0,0.d0,1.d0/
!
    do 10 i = 1, 3
        ind(i,i)=i
10  end do
    ind(1,2)=4
    ind(2,1)=4
    ind(1,3)=5
    ind(3,1)=5
    ind(2,3)=6
    ind(3,2)=6
!
    call dcopy(9, vind(nvi-3-18+10), 1, fem, 1)
    call daxpy(9, 1.d0, id, 1, fem,&
               1)
    call dcopy(9, vinf(nvi-3-18+10), 1, fep, 1)
    call daxpy(9, 1.d0, id, 1, fep,&
               1)
!
    call dcopy(9, vinf(nvi-3-18+1), 1, fpp, 1)
    call daxpy(9, 1.d0, id, 1, fpp,&
               1)
    call matinv('S', 3, fpp, fppinv, det)
!
! CALCUL DE DFE/DF
!
    call r8inir(81, 0.d0, dfedf, 1)
    do 1 i = 1, 3
        do 1 j = 1, 3
            do 1 k = 1, 3
                do 1 l = 1, 3
                    do 1 m = 1, 3
                        dfedf(i,j,k,l)=dfedf(i,j,k,l)+id(i,k)*fem(l,m)&
                        *fppinv(m,j)
 1                  continue
!
! CALCUL DE DR1/DF
    call r8inir(81, 0.d0, dr1df, 1)
    do 3 i = 1, 3
        do 3 j = 1, 3
            do 3 k = 1, 3
                do 3 l = 1, 3
                    do 3 m = 1, 3
                        dr1df(i,j,k,l)=dr1df(i,j,k,l) +dfedf(m,i,k,l)*&
                        fep(m,j)
 3                  continue
    do 31 i = 1, 3
        do 31 j = 1, 3
            do 31 k = 1, 3
                do 31 l = 1, 3
                    do 31 m = 1, 3
                        dr1df(i,j,k,l)=dr1df(i,j,k,l) +fep(m,i)*dfedf(&
                        m,j,k,l)
31                  continue
    call dscal(81, -0.5d0, dr1df, 1)
!
! CALCUL DE DS/DF EN UTILISANT LES SYMETRIES
    do 4 i = 1, 3
        do 4 j = 1, 3
            do 4 k = 1, 3
                do 4 l = 1, 3
                    dr1df6(ind(i,j),k,l)=dr1df(i,j,k,l)
 4              continue
!
    call r8inir(54, 0.d0, dsdf, 1)
    do 6 i = 1, 6
        do 6 j = 1, 3
            do 6 k = 1, 3
                do 6 l = 1, 6
                    dsdf(i,j,k)=dsdf(i,j,k)-zinv(i,l)*dr1df6(l,j,k)
 6              continue
!
! RECALCUL DU PK2 S
    if (materf(nmat) .eq. 0) then
        call lcopli('ISOTROPE', mod, materf(1), hooke)
    else if (materf(nmat).eq.1) then
        call lcopli('ORTHOTRO', mod, materf(1), hooke)
    endif
    call lctr2m(3, fep, fet)
    call pmat(3, fet, fep, fetfe)
    call daxpy(9, -1.d0, id, 1, fetfe,&
               1)
    call dscal(9, 0.5d0, fetfe, 1)
!
!      CONTRAINTES PK2
    call tnsvec(3, 3, fetfe, eel, 1.d0)
    call lcprmv(hooke, eel, s6)
    call tnsvec(6, 3, s, s6, 1.d0)
!
! CALCUL DE DTAU/DF EN UTILISANT LES SYMETRIES
    call r8inir(81, 0.d0, dtaudf, 1)
    call pmat(3, s, fet, sfet)
    do 71 i = 1, 3
        do 71 j = 1, 3
            do 71 k = 1, 3
                do 71 l = 1, 3
                    do 71 m = 1, 3
                        dtaudf(i,j,k,l)=dtaudf(i,j,k,l)+dfedf(i,m,k,l)&
                        *sfet(m,j)
71                  continue
!
    do 72 i = 1, 3
        do 72 j = 1, 3
            do 72 k = 1, 3
                do 72 l = 1, 3
                    do 72 m = 1, 3
                        dtaudf(i,j,k,l)=dtaudf(i,j,k,l)+sfet(m,i)*&
                        dfedf(j,m,k,l)
72                  continue
!
    do 73 i = 1, 3
        do 73 j = 1, 3
            do 73 k = 1, 3
                do 73 l = 1, 3
                    do 73 m = 1, 3
                        do 73 n = 1, 3
                            dtaudf(i,j,k,l)=dtaudf(i,j,k,l)+ fep(i,m)*&
                            dsdf(ind(m,n),k,l)*fep(j,n)
73                      continue
!
    do 8 i = 1, 3
        do 8 j = 1, 3
            do 8 k = 1, 3
                do 8 l = 1, 3
                    dsde(ind(i,j),k,l)=dtaudf(i,j,k,l)
 8              continue
!
! LES RACINE(2) ATTENDUES PAR NMCOMP  !!!
    call dscal(9, sqrt(2.d0), dsde(4, 1, 1), 6)
    call dscal(9, sqrt(2.d0), dsde(5, 1, 1), 6)
    call dscal(9, sqrt(2.d0), dsde(6, 1, 1), 6)
!
end subroutine
