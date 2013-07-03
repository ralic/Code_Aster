subroutine zmulmv(trans, m, n, alpha, a,&
                  lda, x, incx, beta, y,&
                  incy)
    implicit none
#include "asterfort/zinit.h"
#include "asterfort/zmult.h"
#include "blas/zaxpy.h"
#include "blas/zdotc.h"
#include "blas/zdotu.h"
    integer :: m, n, lda, incx, incy
    complex(kind=8) :: alpha, beta, x(*), y(*)
    character(len=*) :: trans
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
!  CALCUL DU PRODUIT D'UNE MATRICE PAR UN VECTEUR (OPTION 'N' 'T' 'C').
!-----------------------------------------------------------------------
! IN  : TRANS: CARACTERE SPECIFIANT L'OPERATION A REALISER.
!                 TRANS               OPERATION
!              'N'             Y = ALPHA*A*X + BETA*Y
!              'T'             Y = ALPHA*A'*X + BETA*Y
!              'C'             Y = ALPHA*CONJG(A)'*X + BETA*Y
!     : M    : NOMBRE DE LIGNES DE A.
!     : N    : NOMBRE DE COLONNES DE A.
!     : ALPHA: SCALAIRE.
!     : A    : MATRICE COMPLEXE DE DIMENSION M*N
!     : LDA  : DIMENSION DE A
!     : X    : VECTEUR COMLEXE DE LONGUEUR (N-1)*IABS(INCX)+1 LORSQUE
!              TRANS EST EGAL A 'N' ET DE LONGUEUR (M-1)*IABS(INCX)+1
!              SINON.
!     : INCX : DEPLACEMENT ENTRE LES ELEMENTS DE X.
!     : BETA : COMPLEXE.'LORSQUE BETA EGAL ZERO, Y EST NON CALCULE.
! I/O : Y    :  (N-1)*IABS(INCY)+1
!               (M-1)*IABS(INCY)+1
! OUT : INCY : DEPLACEMENT ENTRE LES ELEMENTS DE Y.
!-----------------------------------------------------------------------
!                                  SPECIFICATIONS FOR LOCAL VARIABLES
    integer :: i, ix, iy, ky, lenx, leny
    complex(kind=8) :: a(*)
    integer :: kx
!
    if (m .eq. 0 .or. n .eq. 0 .or. alpha .eq. (0.0d0,0.0d0) .and. beta .eq. (1.0d0,0.0d0)) &
    goto 9000
!
    if (trans(1:1) .eq. 'N' .or. trans(1:1) .eq. 'n') then
        lenx = n
        leny = m
    else
        lenx = m
        leny = n
    endif
!
    ix = 1
    iy = 1
    if (incx .lt. 0) ix = (-lenx+1)*incx + 1
    if (incy .lt. 0) iy = (-leny+1)*incy + 1
!
    if (beta .eq. (1.0d0,0.0d0)) then
    else if (incy .eq. 0) then
        if (beta .eq. (0.0d0,0.0d0)) then
            y(1) = (0.0d0,0.0d0)
        else
            y(1) = beta**leny*y(1)
        endif
    else if (beta .eq. (0.0d0,0.0d0)) then
        call zinit(leny, (0.0d0, 0.0d0), y, abs(incy))
    else
        call zmult(leny, beta, y, abs(incy))
    endif
!
    if (alpha .eq. (0.0d0,0.0d0)) goto 9000
!
    if (trans(1:1) .eq. 'N' .or. trans(1:1) .eq. 'n') then
        kx = ix
        do 10 i = 1, n
            call zaxpy(m, alpha*x(kx), a(lda*(i-1)+1), 1, y,&
                       incy)
            kx = kx + incx
10      continue
    else if (trans(1:1).eq.'T' .or. trans(1:1).eq.'t') then
!
        ky = iy
        do 20 i = 1, n
            y(ky) = y(ky) + alpha*zdotu(m,a(lda*(i-1)+1),1,x,incx)
            ky = ky + incy
20      continue
!
    else
        ky = iy
        do 30 i = 1, n
            y(ky) = y(ky) + alpha*zdotc(m,a(lda*(i-1)+1),1,x,incx)
            ky = ky + incy
30      continue
    endif
!
9000  continue
end subroutine
