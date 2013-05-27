subroutine te0362(option, nomte)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jerome.laverne at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/dfdm1d.h'
    include 'asterfort/elref2.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/subaco.h'
    include 'asterfort/sumetr.h'
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
!     CALCUL DES COORDONNEES DES POINTS DE GAUSS
!     POUR LES ELEMENTS D'INTERFACE
! ----------------------------------------------------------------------
!
!
!
!
    character(len=8) :: lielrf(10)
    integer :: jgn, nno, g, iw, ivf, igeom, ntrou
    integer :: npg, nnos, icopg, ndim, idf, i, n
!
    real(kind=8) :: cova(3, 3), metr(2, 2), jac, x(3)
    real(kind=8) :: wg, dfdx(9), cour, cosa, sina, wref
! DEB ------------------------------------------------------------------
!
    call elref2(nomte, 2, lielrf, ntrou)
    call elref4(lielrf(2), 'RIGI', ndim, nno, nnos,&
                npg, iw, ivf, idf, jgn)
    ndim = ndim + 1
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCOORPG', 'E', icopg)
!
    do 20 g = 1, npg
        call r8inir(ndim, 0.d0, x, 1)
        do 10 n = 1, nno
            do 12 i = 1, ndim
                x(i) = x(i) + zr(igeom+ndim*(n-1)+i-1)*zr(ivf+(g-1)* nno+n-1)
12          continue
10      continue
!
        wref = zr(iw+g-1)
        if (ndim .eq. 3) then
            call subaco(nno, zr(idf+(g-1)*(ndim-1)*nno), zr(igeom), cova)
            call sumetr(cova, metr, jac)
            wg = wref*jac
        else if (ndim.eq.2) then
            call dfdm1d(nno, wref, zr(idf+(g-1)*(ndim-1)*nno), zr(igeom), dfdx,&
                        cour, wg, cosa, sina)
        endif
!
        do 15 i = 1, ndim
            zr(icopg+(ndim+1)*(g-1)+i-1) = x(i)
15      continue
        zr(icopg+(ndim+1)*(g-1)+ndim) = wg
!
20  end do
!
end subroutine
