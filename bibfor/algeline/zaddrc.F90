subroutine zaddrc(m, n, alpha, x, incx,&
                  y, incy, a, lda)
    implicit none
#include "blas/zaxpy.h"
    integer :: m, n, incx, incy, lda
    complex(kind=8) :: alpha, x(*), y(*)
    complex(kind=8) :: a(*)
    integer :: i1x
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
!    CALCUL DE ALPHA*CONJG(Y)'
!-----------------------------------------------------------------------
! IN  : M    : NOMBRE DE LIGNE DE A.
!     : N    : NOMBRE DE COLONNE DE A.
!     : ALPHA: COMPLEXE.
!     : X    : VECTEUR COMPLEXE DE LONGUEUR (M-1)*IABS(INCX)+1.
!     : INCX : DEPLACEMENT ENTRE LES ELEMENTS DE X.
!     : Y    : VECTEUR COMPLEXE DE LONGUEUR (N-1)*IABS(INCY)+1.
!     : INCY : DEPLACEMENT ENTRE LES ELEMENTS DE Y.
! I/O : A    : MATRICE COMPLEXE DE DIMENSION M*N.
! IN  : LDA  : DIMENSION DE A.
!-----------------------------------------------------------------------
    integer :: iy, j
    if (m .eq. 0 .or. n .eq. 0 .or. alpha .eq. (0.0d0,0.0d0)) goto 9000
!
    iy = 1
    if (incy .lt. 0) iy = (-n+1)*incy + 1
!
    i1x = 1
    do 130 j = 1, n
        call zaxpy(m, alpha*dconjg(y(iy)), x, incx, a(i1x),&
                   1)
        iy = iy + incy
        i1x = i1x + lda
130  end do
!
9000  continue
    goto 9999
9999  continue
end subroutine
