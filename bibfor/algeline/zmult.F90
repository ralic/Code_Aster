subroutine zmult(n, za, zx, incx)
    implicit none
    integer :: n, incx
    complex(kind=8) :: za, zx(*)
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
!    CALCUL DE Y = A*X POUR DES VECTEURS COMPLEXES
!-----------------------------------------------------------------------
! IN  : N    : LONGUEUR DU VECTEUR X.
!     : ZA   : COMPLEXE.
! I/O : ZX   : VECTEUR COMPLEXE DE LONGUEUR MAX(N*IABS(INCX),1).
!              ZMULT REMPLACE X(I) PAR ZA*X(I) POUR I = 1,...,N.
! IN  : INCX : DEPLACEMENT ENTRE LES ELEMENTS DE ZX.
!              X(I) EST DEFINI PAR ZX(1+(I-1)*INCX). INCX DOIT ETRE
!              PLUS GRAND QUE ZERO.
!-----------------------------------------------------------------------
    integer :: i, ix
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if (n .gt. 0) then
        if (incx .ne. 1) then
!
            ix = 1
            if (incx .lt. 0) ix = (-n+1)*incx + 1
            do 10 i = 1, n
                zx(ix) = za*zx(ix)
                ix = ix + incx
10          continue
        else
!
            do 20 i = 1, n
                zx(i) = za*zx(i)
20          continue
        endif
    endif
end subroutine
