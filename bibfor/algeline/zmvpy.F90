subroutine zmvpy(uplo, n, alpha, a, lda,&
                 x, incx, beta, y, incy)
    implicit none
#include "asterfort/vecinc.h"
#include "asterfort/zmult.h"
#include "blas/zaxpy.h"
    integer :: n, lda, incx, incy
    complex(kind=8) :: alpha, beta, a(lda, *), x(*), y(*)
    character(len=*) :: uplo
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!  PRODUIT D'UNE MATRICE HERMITIENNE PAR UN VECTEUR SUIVANT OPTION
!                         'U' OU 'L'.
!-----------------------------------------------------------------------
! IN  : UPLO : CARACTERE SPECIFIANT LE MODE DE STOCKAGE.
!                 UPLO              STRUCTURE
!              'U'             MATRICE TRIANGULAIRE SUPERIEURE.
!              'L'             MATRICE TRIANGULAIRE INFERIEURE
!     : N    : DIMENSION DE LA MATRICE.
!     : ALPHA: COMPLEXE.
!     : A    : MATRICE COMPLXE DE DIMENSION M*N.
!     : LDA  : DIMENSION DE A.
!     : X    : DVECTEUR COMPLXE DE LONGUEUR (N-1)*IABS(INCX)+1.
!     : INCX : DEPLACEMENT ENTRE LES ELEMENTS DE X.
!     : BETA : COMPLEXE.LORSQUE BETA EGAL ZERO, Y N'EST PAS CALCULE.
!     : Y    : VECTEUR COMPLEXE DE LONGUEUR (N-1)*IABS(INCY)+1.
!     : INCY : DEPLACEMENT ENTRE LES ELEMENTS DE Y.
!-----------------------------------------------------------------------
    integer :: i, ix, iy, j, ky
    complex(kind=8) :: temp
    real(kind=8) :: dble
!
    if (n .eq. 0 .or. (alpha.eq.(0.0d0,0.0d0).and.beta.eq.(1.0d0,0.0d0))) goto 999
!
    ix = 1
    iy = 1
    if (incx .lt. 0) ix = (-n+1)*incx + 1
    if (incy .lt. 0) iy = (-n+1)*incy + 1
!
    if (beta .eq. (1.0d0,0.0d0)) then
    else if (incy .eq. 0) then
        if (beta .eq. (0.0d0,0.0d0)) then
            y(1) = (0.0d0,0.0d0)
        else
            y(1) = beta**n*y(1)
        endif
    else if (beta .eq. (0.0d0,0.0d0)) then
        call vecinc(n, (0.0d0, 0.0d0), y, inc=abs(incy))
    else
        call zmult(n, beta, y, abs(incy))
    endif
!
    if (alpha .eq. (0.0d0,0.0d0)) goto 999
!
    if (uplo(1:1) .eq. 'U' .or. uplo(1:1) .eq. 'u') then
        do 20 j = 1, n
            temp = alpha*x(ix)
            ky = iy + (j-2)*min(incy,0)
            call zaxpy(j-1, temp, a(1, j), 1, y(ky),&
                       incy)
            ky = iy + (j-1)*incy
            y(ky) = y(ky) + temp*dble(a(j,j))
            do 10 i = j + 1, n
                ky = ky + incy
                y(ky) = y(ky) + temp*dconjg(a(j,i))
10          continue
            ix = ix + incx
20      continue
    else
        do 40 j = 1, n
            temp = alpha*x(ix)
            ky = iy
            do 30 i = 1, j - 1
                y(ky) = y(ky) + temp*dconjg(a(j,i))
                ky = ky + incy
30          continue
            y(ky) = y(ky) + temp*dble(a(j,j))
            ky = ky + incy + (n-j-1)*min(incy,0)
            call zaxpy(n-j, temp, a(j+1, j), 1, y(ky),&
                       incy)
            ix = ix + incx
40      continue
    endif
!
999   continue
end subroutine
