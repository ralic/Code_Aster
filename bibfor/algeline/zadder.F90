subroutine zadder(uplo, n, alpha, x, incx,&
                  a, lda)
    implicit none
    include 'blas/zaxpy.h'
    integer :: n, incx, lda
    real(kind=8) :: alpha
    complex(kind=8) :: x(*), a(*)
    character(len=*) :: uplo
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
!    CALCUL DE ALPHA*X*CONJG(X)'  =>    A MATRICE HERMITIENNE.
!-----------------------------------------------------------------------
! IN  : UPLO : INDIQUE LE MODE DE STOCKAGE DE LA MATRICE.
!              SI UPLO EST 'U' ALORS SEULEMENT LA PARTIE SUPERIEURE DE A
!              EST UTILISEE. SI UPLO EST 'L', ALORS LA PARTIE INFERIEURE
!              EST UTILISEE.
!     : N    : DIMENSION DE LA MATRICE A.
!     : ALPHA: SCALAIRE.
!     : X    : DVECTEURE COMPLEXE DE LONGUEUR (N-1)*IABS(INCX)+1.
!     : INCX : DEPLACEMENT ENTRE LES ELEMENTS DE X.
! I/O : A    : MATRICE COMPLEXE DE DIMENSION N.
! IN  : LDA  : DIMENSION DE A.
!-----------------------------------------------------------------------
    integer :: ix, j
    complex(kind=8) :: temp, temp1, temp2, temp3, temp4
    logical :: upper
    real(kind=8) :: dble
!
    if (n .eq. 0 .or. alpha .eq. 0.0d0) goto 9000
!
    ix = 1
    if (incx .lt. 0) ix = (-n+1)*incx + 1
!
    upper = (uplo(1:1).eq.'U') .or. (uplo(1:1).eq.'u')
!
    do 10 j = 1, n
        temp = alpha*dconjg(x(ix))
        if (upper) then
            if (incx .ge. 0) then
                call zaxpy(j-1, temp, x, incx, a(lda*(j-1)+1),&
                           1)
            else
                call zaxpy(j-1, temp, x(ix-incx), incx, a(lda*(j-1)+ 1),&
                           1)
            endif
        else
            if (incx .ge. 0) then
                call zaxpy(n-j, temp, x(ix+incx), incx, a(lda*(j-1)+ j+1),&
                           1)
            else
                call zaxpy(n-j, temp, x, incx, a(lda*(j-1)+j+1),&
                           1)
            endif
        endif
        temp1 = a(lda*(j-1)+j)
        temp2 = x(ix)*temp
        temp3 = dble(temp1)
        temp4 = dble(temp2)
        a(lda*(j-1)+j) = temp3 + temp4
        ix = ix + incx
10  end do
!
9000  continue
    goto 9999
9999  continue
end subroutine
