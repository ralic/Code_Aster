subroutine te0343(option, nomte)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! aslint: disable=W0104
    implicit none
#include "jeveux.h"
#include "asterfort/dfdm1b.h"
#include "asterfort/elref2.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/r8inir.h"
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
!     CALCUL DES COORDONNEES DES POINTS DE GAUSS
!     POUR LES ELEMENTS CABLE GAINE
! ----------------------------------------------------------------------
!
    character(len=8) :: lielrf(10)
    integer :: jgn, nno, g, iw, ivf, igeom, ntrou
    integer :: npg, nnos, icopg, ndim, idf, i, n
    real(kind=8) :: x(3), wg, dfdx(9), wref
! DEB ------------------------------------------------------------------
!
    call elref2(nomte, 2, lielrf, ntrou)
    call elrefe_info(elrefe=lielrf(1),fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=iw,jvf=ivf,jdfde=idf,jgano=jgn)
    ndim = 3
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCOORPG', 'E', icopg)
!
    do g = 1, npg
        call r8inir(ndim, 0.d0, x, 1)
        do 10 n = 1, nno
            do 12 i = 1, ndim
                x(i) = x(i) + zr(igeom+ndim*(n-1)+i-1)*zr(ivf+(g-1)* nno+n-1)
12          continue
10      continue
!
        wref = zr(iw+g-1)
!
        call dfdm1b(nno, wref, zr(idf+(g-1)*nno), zr(igeom), dfdx,&
                    wg)
        do 15 i = 1, ndim
            zr(icopg+(ndim+1)*(g-1)+i-1) = x(i)
15      continue
        zr(icopg+(ndim+1)*(g-1)+ndim) = wg
!
    end do
!
end subroutine
