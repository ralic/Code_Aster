subroutine tridia(n, a, lda, d, e,&
                  tau, w)
    implicit none
    include 'asterc/r8prem.h'
    include 'asterfort/zadder.h'
    include 'asterfort/zader2.h'
    include 'asterfort/zmvpy.h'
    include 'blas/zdotc.h'
    integer :: n, lda
    real(kind=8) :: d(*), e(*)
    complex(kind=8) :: a(lda, *), tau(*), w(*)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!      REDUCTION D'UNE MATRICE HERMITIENNE EN UNE MATRICE TRIDIAGONALE
!                   SYMETRIQUE (METHODE DE HOUSEHOLDER).
!-----------------------------------------------------------------------
! IN  : N    : DIMENSION DE LA MATRICE.
!     : A    : MATRICE HERMITIENNE.
!     : LDA  : DIMENSION DE A.
! OUT : D    : VECTEUR DE DIMENSION N CONTENANT LA DIAGONALE DE LA
!              MATRICE TRIDIAGONALE
!     : E    : VECTEUR DE DIMENSION N CONTENANT LA DIAGONALE
!              SUPERIEURE DE LA MATRICE TRIDIAGONALE DANS E(2:N),
!              E(1) = 0.
!     : TAU  : VECTEUR COMPLEXE DE DIMENSION N CONTENANT LA DIAGONALE
!              DE LA MATRICE UNITAIRE T
!     : W    : VECTEUR COMPLEXE DE DIMENSION N (VECTEUR DE TRAVAIL)
!-----------------------------------------------------------------------
    integer :: i, j, k
    real(kind=8) :: bb, delta, dgamma, ratio, rho, root, tol, vr
    complex(kind=8) :: temp1
    complex(kind=8) :: vc
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    tol = 0.0d0
    do 10 i = 1, n
        do 10 j = 1, i
            tol = max(abs(dble(a(i,j))),abs(dimag(a(i,j))),tol)
10      continue
    dgamma = r8prem()**2
!
!  --- REALISATION DE N-2 TRANSFORMATIONS SIMILAIRES ---
    do 30 k = 2, n - 1
        tau(k) = 0.0d0
        vc = zdotc(n-k+1,a(k,k-1),1,a(k,k-1),1)
        vr=max(abs(dble(vc)),abs(dimag(vc)))
        if (vr .le. dgamma*tol**2) goto 30
        if (dble(a(k,k-1)) .eq. 0.0d0 .and. dimag(a(k,k-1)) .eq. 0.0d0) then
            a(k,k-1) = sqrt(vr)
            delta = vr
            tau(1) = -a(k,k-1)
        else
            root = abs(a(k,k-1))*sqrt(vr)
            delta = vr + root
            ratio = vr/root
            tau(1) = -ratio*dconjg(a(k,k-1))
            a(k,k-1) = (ratio+1.0d0)*a(k,k-1)
        endif
!
!   --- TRANSFORMATIONS ---
        do 20 j = k, n
            tau(j) = a(j,k-1)/delta
20      continue
!
        call zmvpy('LOWER', n-k+1, (1.0d0, 0.0d0), a(k, k), lda,&
                   a(k, k-1), 1, (0.0d0, 0.0d0), w(k), 1)
!                                  RHO = U*NV
        temp1 = zdotc(n-k+1,w(k),1,tau(k),1)
        rho = dble(temp1)
        call zader2('LOWER', n-k+1, (-1.0d0, 0.0d0), tau(k), 1,&
                    w(k), 1, a(k, k), lda)
        call zadder('LOWER', n-k+1, rho*delta, tau(k), 1,&
                    a(k, k), lda)
        tau(k) = tau(1)
30  end do
!
!  --- LA MATRICE A ETE REDUITE EN UNE MATRICE HERMITIENNE
!      TRIDIAGONALE. LA DIAGONALE SUPERIEURE EST TEMPORAIREMENT
!      STOCKEE DANS LE VECTEUR TAU. LA DIAGONALE EST STOCKEE DANS D
    do 40 i = 1, n
        d(i) = dble(a(i,i))
40  end do
!
    tau(1) = 1.0d0
    if (n .gt. 1) tau(n) = dconjg(a(n,n-1))
    e(1) = 0.0d0
!
    do 50 i = 2, n
        bb = abs(tau(i))
        e(i) = bb
        a(i,i) = dcmplx(dble(a(i,i)),bb)
        if (bb .eq. 0.0d0) then
            tau(i) = 1.0d0
            bb = 1.0d0
        endif
        tau(i) = tau(i)*tau(i-1)/bb
50  end do
!
end subroutine
