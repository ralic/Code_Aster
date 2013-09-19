subroutine te0488(option, nomte)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/subaco.h"
#include "asterfort/sumetr.h"
#include "asterfort/tecach.h"
#include "asterfort/utpvlg.h"
#include "asterfort/vectan.h"
#include "asterfort/vectgt.h"
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
!     CALCUL DES COORDONNEES DES POINTS DE GAUSS
!     POUR LES ELEMENTS ISOPARAMETRIQUES 3D  ET LEURS ELEMENTS DE PEAU
!
!
!
!
    integer :: jgano, nno, kp, ipoids, ivf, igeom
    integer :: npg, nnos, icopg, ino, ndim, idfde
!
    real(kind=8) :: xx, yy, zz, rbid81(81), poids, cova(3, 3), metr(2, 2), jac
    integer :: inbf, nbc, icoq, decpo, ic, ispc, jtab(7), nbsp, iret
    real(kind=8) :: epais, excen, bas, epc, pgl(3, 3), gm1(3), gm2(3)
    integer :: lzi, lzr, nb1, nb2
    real(kind=8) :: zero, vectg(2, 3), vectt(3, 3)
    real(kind=8) :: vecta(9, 2, 3), vectn(9, 3), vectpt(9, 2, 3)
    real(kind=8) :: poidc(3), hh
    parameter(zero=0.0d0)
    logical :: coq3d, grille
    data gm1 / 0.d0,0.d0,1.d0/
    data poidc / 0.16666666666666666d0,0.66666666666666663d0,&
     &             0.16666666666666666d0/
! DEB ------------------------------------------------------------------
    coq3d=nomte(1:4).eq.'MEC3'
    grille= nomte(1:4).eq.'MEGC'
    if (coq3d) then
        call elref4(' ', 'MASS', ndim, nno, nnos,&
                    npg, ipoids, ivf, idfde, jgano)
    else
        call elref4(' ', 'RIGI', ndim, nno, nnos,&
                    npg, ipoids, ivf, idfde, jgano)
    endif
!
    call jevech('PGEOMER', 'L', igeom)
    call tecach('OON', 'PCOORPG', 'E', iret, nval=7,&
                itab=jtab)
    icopg=jtab(1)
    nbsp=jtab(7)
!
! POUR LES GRILLES, LE NOMBRE DE SOUS POINTS N'EST PAS ACTIF DANS LE
! CALCUL ON BLINDE
    if (grille) then
        nbsp=1
        call jevech('PCACOQU', 'L', icoq)
        excen = zr(icoq+3)
        if (nno .eq. 3) then
            call dxtpgl(zr(igeom), pgl)
        else if (nno.eq.4) then
            call dxqpgl(zr(igeom), pgl, 'S', iret)
        endif
        call utpvlg(1, 3, pgl, gm1, gm2)
    endif
!
! EN CAS DE SOUS POINTS (DKT,DST,COQUE_3D)
    if (nbsp .ne. 1) then
        call jevech('PNBSP_I', 'L', inbf)
        nbc = zi(inbf)
        call jevech('PCACOQU', 'L', icoq)
        epais = zr(icoq)
        if (coq3d) then
            excen = zr(icoq+5)
! COQUES 3D : ON UTILISE LE VECTEUR NORMAL A CHAQUE POINT DE GAUSS
!  ON PREPARE VECTN POUR LE CALCUL DE GM2 DANS LA BOUCLE PG PLUS BAS
            call jevete('&INEL.'//nomte(1:8)//'.DESI', ' ', lzi)
            nb1 = zi(lzi-1+1)
            nb2 = zi(lzi-1+2)
            npg = zi(lzi-1+4)
            call jevete('&INEL.'//nomte(1:8)//'.DESR', ' ', lzr)
            call vectan(nb1, nb2, zr(igeom), zr(lzr), vecta,&
                        vectn, vectpt)
        else
            excen = zr(icoq+4)
! DKT,DST : ON UTILISE LE VECTEUR NORMAL DE LA PLAQUE
!   ON CALCULE GM2 UNE SEULE FOIS
            if (nno .eq. 3) then
                call dxtpgl(zr(igeom), pgl)
            else if (nno.eq.4) then
                call dxqpgl(zr(igeom), pgl, 'S', iret)
            endif
            call utpvlg(1, 3, pgl, gm1, gm2)
        endif
        bas=-epais/2.d0+excen
        epc=epais/nbc
    endif
!
! BOUCLE POINT DE GAUSS
    do 20 kp = 1, npg
!     CALCUL DES COORDONNEES DES POINTS DE GAUSS
        xx = 0.d0
        yy = 0.d0
        zz = 0.d0
        do 10 ino = 1, nno
            xx = xx + zr(igeom+3* (ino-1)+0)*zr(ivf+ (kp-1)*nno+ino-1)
            yy = yy + zr(igeom+3* (ino-1)+1)*zr(ivf+ (kp-1)*nno+ino-1)
            zz = zz + zr(igeom+3* (ino-1)+2)*zr(ivf+ (kp-1)*nno+ino-1)
10      continue
        if (ndim .eq. 3) then
!         -- CAS DES ELEMENTS 3D
            call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                        poids)
        else if (ndim.eq.2) then
!         -- CAS DES ELEMENTS "PEAU" DE 3D
            call subaco(nno, zr(idfde+(kp-1)*ndim*nno), zr(igeom), cova)
            call sumetr(cova, metr, jac)
            poids=jac*zr(ipoids-1+kp)
        else
            ASSERT(.false.)
        endif
!
!
        if (nbsp .ne. 1) then
            decpo=4*3*nbc*(kp-1)
            if (coq3d) then
!         CALCUL DE LA NORMAL AU POINT DE GAUSS
                call vectgt(1, nb1, zr(igeom), zero, kp,&
                            zr(lzr), epais, vectn, vectg, vectt)
                gm2(1)=vectt(3,1)
                gm2(2)=vectt(3,2)
                gm2(3)=vectt(3,3)
            endif
            do 30 ic = 1, nbc
                do 32 ispc = 1, 3
                    hh=bas+dble(ic-1)*epc+dble(ispc-1)*epc/2.d0
                    zr(icopg+decpo+(ic-1)*12+(ispc-1)*4+0) = xx + hh* gm2(1)
                    zr(icopg+decpo+(ic-1)*12+(ispc-1)*4+1) = yy + hh* gm2(2)
                    zr(icopg+decpo+(ic-1)*12+(ispc-1)*4+2) = zz + hh* gm2(3)
                    zr(icopg+decpo+(ic-1)*12+(ispc-1)*4+3) = poids* epc*poidc( ispc)
32              continue
30          continue
        else if (grille) then
            zr(icopg+4*(kp-1)+0) = xx+ excen*gm2(1)
            zr(icopg+4*(kp-1)+1) = yy+ excen*gm2(2)
            zr(icopg+4*(kp-1)+2) = zz+ excen*gm2(3)
            zr(icopg+4*(kp-1)+3) = poids
        else
            zr(icopg+4*(kp-1)+0) = xx
            zr(icopg+4*(kp-1)+1) = yy
            zr(icopg+4*(kp-1)+2) = zz
            zr(icopg+4*(kp-1)+3) = poids
        endif
20  end do
!
end subroutine
