subroutine te0462(option, nomte)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elref4.h"
#include "asterfort/fmater.h"
#include "asterfort/jevech.h"
#include "asterfort/tecach.h"
#include "asterfort/utpvlg.h"
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
!     CALCUL DES COORDONNEES DES SOUS POINTS DE GAUSS SUR LES FAMILLE
!     DE LA LISTE MATER
!     POUR LES ELEMENTS : DKT
! ----------------------------------------------------------------------
!     NOMBRE MAX DE FAMILLE DANS MATER
    integer :: nfpgmx
!     NOMBRE DE NIVEAUX PAR COUCHE
    integer :: nbniv
!     DIMENSION
    integer :: ndim
    parameter (nfpgmx=10,nbniv=3,ndim=3)
!
    integer :: ndim1, nno, nnos, npg, jgano, idfde, ipoids, ivf
    integer :: igeom, jtab(7), icopg, inbf, icoq, iret, decpo, iad
    integer :: nbsp, nbcou, nfpg, decfpg
    integer :: ifpg, ig, icou, iniv, ino
    real(kind=8) :: pgl(3, 3), xx, yy, zz
    real(kind=8) :: epais, excen, gm1(3), gm2(3), epc, bas, hh
    logical :: grille
    character(len=8) :: fami(nfpgmx)
    data gm1 / 0.d0,0.d0,1.d0/
! ----------------------------------------------------------------------
!
    if (nomte(1:4) .ne. 'MEDK' .and. nomte(1:4) .ne. 'MEGC') then
        ASSERT(.false.)
    endif
    grille= nomte(1:4).eq.'MEGC'
!
!     NOMBRE DE NOEUDS
    if (nomte(5:7) .eq. 'QU4') then
        nno = 4
    else if (nomte(5:7).eq.'TR3') then
        nno = 3
    else
        ASSERT(.false.)
    endif
!
!
    call jevech('PGEOMER', 'L', igeom)
!
!     ZR(ICOPG) : COORDONNEES DE SOUS-POINTS DE GAUSS
    call tecach('OON', 'PCOOPGM', 'E', iret, nval=7,&
                itab=jtab)
    icopg=jtab(1)
    nbsp=jtab(7)
    ASSERT(nbsp.gt.0)
!
    call jevech('PCACOQU', 'L', icoq)
!
    if (grille) then
        excen = zr(icoq+3)
    else
!       ELEMENTS A SOUS POINTS : DKT
        call jevech('PNBSP_I', 'L', inbf)
        nbcou = zi(inbf)
        epais = zr(icoq)
        excen = zr(icoq+4)
        bas=-epais/2.d0+excen
        epc=epais/nbcou
    endif
!
! ON UTILISE LE VECTEUR NORMAL DE LA PLAQUE
    if (nno .eq. 3) then
        call dxtpgl(zr(igeom), pgl)
    else if (nno.eq.4) then
        call dxqpgl(zr(igeom), pgl, 'S', iret)
    endif
!
    call utpvlg(1, 3, pgl, gm1, gm2)
!
    call fmater(nfpgmx, nfpg, fami)
    decfpg = 0
    do 200 ifpg = 1, nfpg
!
        call elref4(' ', fami(ifpg), ndim1, nno, nnos,&
                    npg, ipoids, ivf, idfde, jgano)
!
        do 100 ig = 1, npg
!
!         CALCUL DES COORDONNEES DES POINTS DE GAUSS
            xx = 0.d0
            yy = 0.d0
            zz = 0.d0
            do 10 ino = 1, nno
                xx = xx + zr(igeom+3* (ino-1)+0)*zr(ivf+ (ig-1)*nno+ ino-1)
                yy = yy + zr(igeom+3* (ino-1)+1)*zr(ivf+ (ig-1)*nno+ ino-1)
                zz = zz + zr(igeom+3* (ino-1)+2)*zr(ivf+ (ig-1)*nno+ ino-1)
10          continue
!
            if (grille) then
                decpo=ndim*(decfpg+ig-1)
                iad = icopg+decpo
                zr(iad+0) = xx + excen*gm2(1)
                zr(iad+1) = yy + excen*gm2(2)
                zr(iad+2) = zz + excen*gm2(3)
            else
                decpo=nbcou*nbniv*ndim*(decfpg+ig-1)
                do 110 icou = 1, nbcou
                    do 120 iniv = 1, nbniv
                        hh=bas+dble(icou-1)*epc+dble(iniv-1)*epc/2.d0
                        iad = icopg+decpo+(icou-1)*nbniv*ndim+(iniv-1) *ndim
                        zr(iad+0) = xx + hh*gm2(1)
                        zr(iad+1) = yy + hh*gm2(2)
                        zr(iad+2) = zz + hh*gm2(3)
120                  continue
110              continue
            endif
100      continue
        decfpg = decfpg + npg
200  end do
!
!
!
end subroutine
