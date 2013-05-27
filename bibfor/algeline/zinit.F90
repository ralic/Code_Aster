subroutine zinit(n, za, zx, incx)
    implicit none
    integer :: n, incx
    complex(kind=8) :: za, zx(*)
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
!    INITIALISATION D'UN VECTEUR COMPLEXE A UNE VALEUR ZA.
!-----------------------------------------------------------------------
! IN  : N    : LONGUEUR DU VECTEUR X.
!     : ZA   : COMPLEXE.
! I/O : ZX   : VECTEUR COMPLEXE DE LONGUEUR N*INCX.
!              ZINIT REMPLACE X(I) PAR ZA POUR I=1,...,N.
!     : INCX : DEPLACEMENT ENTRE LES ELEMENTS DE ZX.
!              X(I) EST DEFINI PAR ZX(1+(I-1)*INCX).INCX DOIT ETRE
!              PLUS GRAND QUE ZERO.
!-----------------------------------------------------------------------
    integer :: i, nincx
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if (n .gt. 0) then
        if (incx .ne. 1) then
!
            nincx = n*incx
            do 10 i = 1, nincx, incx
                zx(i) = za
10          continue
        else
!
            do 20 i = 1, n
                zx(i) = za
20          continue
        endif
    endif
end subroutine
