subroutine lcsolz(a, b, ndim, n, nbscmb,&
                  iret)
    implicit none
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     RESOLUTION PAR LA METHODE DE GAUSS D'UN SYSTEME LINEAIRE
!     A COEFFICIENTS COMPLEXES
!     ------------------------------------------------------------------
!       VAR A      : C16: MATRICE CARREE PLEINE
!       VAR B      : C16: TABLEAU BI-INDICE DE COMPLEXES
!                       EN ENTREE : LES SECONDS MEMBRES
!                       EN SORTIE : LES SOLUTIONS
!       IN  N      : IS : ORDRE DE LA MATRICE
!       IN  NDIM   : IS : DIMENSION DECLAREE DE LA MATRICE
!       IN  NBSCMB : IS : NOMBRE DE SECONDS MEMBRES
!      OUT  IRET   : IS : 0 OK
!                         1 PIVOT NUL
!     ------------------------------------------------------------------
    include 'asterc/r8miem.h'
    include 'asterfort/dcabs2.h'
    integer :: ipivot
    real(kind=8) :: apivot, zero, rmin
    complex(kind=8) :: ak, bk
    complex(kind=8) :: a(ndim, ndim), b(ndim, nbscmb)
!
!-----------------------------------------------------------------------
    integer :: i, ic, il, iret, iscmb, j, k
    integer :: n, nbscmb, ndim
!-----------------------------------------------------------------------
    iret = 0
    zero = 0.d0
    rmin = 100.d0*r8miem()
    do 1000 i = 1, n-1
!
!        DETERMINATION DU MEILLEUR PIVOT SUR LA COLONNE
        apivot = dcabs2(a(i,i))
        ipivot = i
        do 100 k = i+1, n
            if (apivot - dcabs2(a(k,i)) .lt. zero) then
                apivot = dcabs2(a(k,i))
                ipivot = k
            endif
100      continue
        if (apivot .lt. rmin) then
            iret = 1
            goto 9999
        endif
!
!        PERMUTATION DES LIGNES DE LA MATRICE
        do 200 j = 1, n
            ak = a(i,j)
            a(i,j) = a(ipivot,j)
            a(ipivot,j) = ak
200      continue
!
!        PERMUTATION DES LIGNES DES SECONDS MEMBRES
        do 300 iscmb = 1, nbscmb
            bk = b(i,iscmb)
            b(i,iscmb) = b(ipivot,iscmb)
            b(ipivot,iscmb) = bk
300      continue
!
!        CALCUL DES NOUVEAUX TERMES DE LA MATRICE ET DES SECONDS MEMBRES
        do 600 il = i+1, n
            do 400 iscmb = 1, nbscmb
                b(il,iscmb) = b(il,iscmb) - a(il,i)*b(i,iscmb)/a(i,i)
400          continue
            do 500 ic = i+1, n
                a(il,ic) = a(il,ic) - a(il,i)*a(i,ic)/a(i,i)
500          continue
600      continue
!
1000  end do
!
!     RESOLUTION
    do 1100 iscmb = 1, nbscmb
        b(n,iscmb) = b(n,iscmb)/a(n,n)
        do 1200 i = n-1, 1, -1
            do 1300 j = i+1, n
                b(i,iscmb) = b(i,iscmb)-a(i,j)*b(j,iscmb)
1300          continue
            b(i,iscmb) = b(i,iscmb)/a(i,i)
1200      continue
1100  end do
!
9999  continue
end subroutine
