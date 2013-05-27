subroutine zader2(uplo, n, alpha, x, incx,&
                  y, incy, a, lda)
    implicit none
    include 'blas/zaxpy.h'
    integer :: n, incx, incy, lda
    complex(kind=8) :: alpha, x(*), y(*), a(lda, *)
    character(len=*) :: uplo
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
!-----------------------------------------------------------------------
!   CALCUL DE A: MATRICE HERMITIENNE
!   A = A + ALPHA*X*CONJG(Y)' + CONJG(ALPHA)*Y*CONJG(X)'
!-----------------------------------------------------------------------
! IN  : UPLO : INDIQUE LE MODE DE STOCKAGE DE LA MATRICE.
!              SI UPLO EST 'U' ALORS SEULEMENT LA PARTIE SUPERIEURE DE A
!              EST UTILISEE. SI UPLO EST 'L', ALORS LA PARTIE INFERIEURE
!              EST UTILISEE.
!     : N    : DIMENSION DE LA MATRICE A.
!     : ALPHA: SCALAIRE.
!     : X    : DVECTEURE COMPLEXE DE LONGUEUR (N-1)*IABS(INCX)+1.
!     : INCX : DEPLACEMENT ENTRE LES ELEMENTS DE X.
!     : Y    : DVECTEURE COMPLEXE DE LONGUEUR (N-1)*IABS(INCY)+1.
!     : INCY : DEPLACEMENT ENTRE LES ELEMENTS DE Y.
! I/O : A    : MATRICE COMPLEXE DE DIMENSION N.
! IN  : LDA  : DIMENSION DE A
!-----------------------------------------------------------------------
    integer :: ix, iy, j
    complex(kind=8) :: tempx, tempy, temp1
    logical :: upper
    real(kind=8) :: dble
!
    if (n .eq. 0 .or. alpha .eq. (0.0d0,0.0d0)) goto 9999
!
    ix = 1
    iy = 1
    if (incx .lt. 0) ix = 1 - (n-1)*incx
    if (incy .lt. 0) iy = 1 - (n-1)*incy
!
    upper = (uplo(1:1).eq.'U') .or. (uplo(1:1).eq.'u')
!
    do 10 j = 1, n
        tempx = dconjg(alpha*x(ix))
        tempy = alpha*dconjg(y(iy))
        if (upper) then
            if (incx .ge. 0) then
                call zaxpy(j-1, tempy, x, incx, a(1, j),&
                           1)
            else
                call zaxpy(j-1, tempy, x(ix-incx), incx, a(1, j),&
                           1)
            endif
            if (incy .ge. 0) then
                call zaxpy(j-1, tempx, y, incy, a(1, j),&
                           1)
            else
                call zaxpy(j-1, tempx, y(iy-incy), incy, a(1, j),&
                           1)
            endif
        else
            if (incx .ge. 0) then
                call zaxpy(n-j, tempy, x(ix+incx), incx, a(j+1, j),&
                           1)
            else
                call zaxpy(n-j, tempy, x, incx, a(j+1, j),&
                           1)
            endif
            if (incy .ge. 0) then
                call zaxpy(n-j, tempx, y(iy+incy), incy, a(j+1, j),&
                           1)
            else
                call zaxpy(n-j, tempx, y, incy, a(j+1, j),&
                           1)
            endif
        endif
        temp1 = a(j,j) + y(iy)*tempx + x(ix)*tempy
        a(j,j) = dble(temp1)
        ix = ix + incx
        iy = iy + incy
10  end do
!
9999  continue
end subroutine
