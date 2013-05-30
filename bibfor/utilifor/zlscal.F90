subroutine zlscal(n, za, zx, incx)
! ======================================================================
! COPYRIGHT (C) BLAS
! COPYRIGHT (C) 2007 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
!     SCALES A VECTOR BY A CONSTANT.
!     JACK DONGARRA, 3/11/78.
!     MODIFIED 3/93 TO RETURN IF INCX .LE. 0.
!     MODIFIED 12/3/93, ARRAY(1) DECLARATIONS CHANGED TO ARRAY(*)
! ======================================================================
! REMPLACE LA BLAS ZSCAL SUR LES MACHINES OU ELLE N'EST PAS DISPONIBLE
! DANS LES LIBRAIRIES SYSTEME
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
    complex(kind=8) :: za, zx(*)
    integer :: i, incx, ix, n
!
    if (n .le. 0 .or. incx .le. 0) goto 9999
    if (incx .eq. 1) goto 20
!
!        CODE FOR INCREMENT NOT EQUAL TO 1
!
    ix = 1
    do 10 i = 1, n
        zx(ix) = za*zx(ix)
        ix = ix + incx
10  end do
    goto 9999
!
!        CODE FOR INCREMENT EQUAL TO 1
!
20  continue
    do 30 i = 1, n
        zx(i) = za*zx(i)
30  end do
9999  continue
end subroutine
