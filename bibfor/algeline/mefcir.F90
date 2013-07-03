subroutine mefcir(ndim, nbcyl, nbgrp, numgrp, som,&
                  rint, dcent, ficent, d, fi,&
                  ppxx, ppxy, ppyx, ppyy, vnxx,&
                  vnxy, vnyx, vnyy, tmp)
    implicit none
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/mefasc.h"
#include "asterfort/mtcrog.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: nbcyl, ndim(14), numgrp(*)
    real(kind=8) :: som(9), rint(*), dcent(*), ficent(*), d(*), fi(*)
    real(kind=8) :: ppxx(nbcyl, nbgrp), ppxy(nbcyl, nbgrp)
    real(kind=8) :: ppyx(nbcyl, nbgrp), ppyy(nbcyl, nbgrp)
    real(kind=8) :: vnxx(nbcyl, nbgrp), vnxy(nbcyl, nbgrp)
    real(kind=8) :: vnyx(nbcyl, nbgrp), vnyy(nbcyl, nbgrp)
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     ASSEMBLAGE ET CALCUL DES COEFFICIENTS INTERVENANT DANS
!     L EXPRESSION DES FORCES DE PRESSION PERTURBEE, ET ET DES FORCES
!     NORMALES DE FROTTEMENTS SUR CHAQUE CYLINDRE DANS LE CAS D UNE
!     ENCEINTE CIRCULAIRE
!     OPERATEUR APPELANT : OP0144 , FLUST3, MEFIST
! ----------------------------------------------------------------------
!     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE
!     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
!     DE TUBES SOUS ECOULEMENT AXIAL"
! ----------------------------------------------------------------------
! IN  : NDIM   : TABLEAU DES DIMENSIONS
! IN  : NBCYL  : NOMBRE DE CYLINDRES
! IN  : NUMGRP : INDICES DES GROUPES D EQUIVALENCE
! IN  : SOM    : XEXT,YEXT,REXT
! IN  : RINT   : RAYONS DES CYLINDRES
! IN  : DCENT  : DISTANCE DU CENTRE DES CYLINDRES AU CENTRE DE
!                L ENCEINTE
! IN  : FICENT : ANGLE POLAIRE PAR RAPPORT AU CENTRE DE L ENCEINTE
! IN  : D      : DISTANCE RELATIVE ENTRE LES CENTRES DES CYLINDRES
! IN  : FI     : ANGLE POLAIRE RELATIF PAR RAPPORT AU CENTRE DE CHAQUE
!                CYLINDRE
! OUT : PPXX   : COEFFICIENT DE MASSES AJOUTEES INTERVENANT DANS LES
!                EFFORTS DE PRESSION PERTURBES SUIVANT XX
! OUT : PPXY   : COEFFICIENT DE MASSES AJOUTEES INTERVENANT DANS LES
!                EFFORTS DE PRESSION PERTURBES SUIVANT XY
! OUT : PPYX   : COEFFICIENT DE MASSES AJOUTEES INTERVENANT DANS LES
!                EFFORTS DE PRESSION PERTURBES SUIVANT YX
! OUT : PPYY   : COEFFICIENT DE MASSES AJOUTEES INTERVENANT DANS LES
!                EFFORTS DE PRESSION PERTURBES SUIVANT YY
! OUT : VNXX   : COEFFICIENT INTERVENANT DANS L EXPRESSION DES EFFORTS
!                VISQUEUX NORMAUX SUIVANT XX
! OUT : VNXY   : COEFFICIENT INTERVENANT DANS L EXPRESSION DES EFFORTS
!                VISQUEUX NORMAUX SUIVANT XY
! OUT : VNYX   : COEFFICIENT INTERVENANT DANS L EXPRESSION DES EFFORTS
!                VISQUEUX NORMAUX SUIVANT YX
! OUT : VNYY   : COEFFICIENT INTERVENANT DANS L EXPRESSION DES EFFORTS
!                VISQUEUX NORMAUX SUIVANT YY
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: i, j, k
    integer :: ncyl
    real(kind=8) :: tmp(4, *), rayoi, rayoj
! ----------------------------------------------------------------------
!
! --- LECTURE DES DIMENSIONS
!-----------------------------------------------------------------------
    integer :: ia, ib, idir, ier, igrp, itrav, ix
    integer :: ixx, nbgrp, nbtron, nmax, nv
!-----------------------------------------------------------------------
    nbcyl = ndim(3)
    nbgrp = ndim(4)
    nbtron = ndim(5)
!
    call jemarq()
!
! --- TABLEAUX DE TRAVAIL, ALLOCATION MEMOIRE
    nv = 2 * nbtron * (nbcyl+1)
    call wkvect('&&MEFCIR.TMP.AB', 'V V R', nv*(3+2*nbgrp+nv), ia)
    ib = ia + nv * nv
    ixx = ib + nv
    ix = ixx + nv * 2 * nbgrp
    itrav = ix + nv
!
! --- INITIALISATIONS
!
    nmax = 4 * nbtron * (nbcyl+1) * nbgrp
    do 1 i = 1, nmax
        zr(ixx+i-1) = 0.d0
 1  end do
!
    nmax = 2 * nbtron * (nbcyl+1)
    do 2 igrp = 1, nbgrp
        do 21 idir = 1, 2
            do 211 i = 1, nmax*nmax
                zr(ia+i-1) = 0.d0
211          continue
            do 212 i = 1, nmax
                zr(ib+i-1) = 0.d0
                zr(ix+i-1) = 0.d0
212          continue
!
! ---       ASSEMBLAGE
            call mefasc(ndim, nbcyl, nbgrp, nbtron, numgrp,&
                        idir, igrp, som, rint, dcent,&
                        ficent, d, fi, zr(ia), zr(ib))
!
! ---       RESOLUTION DU SYSTEME A.X = B PAR LA METHODE DE CROUT
            ier = 1
            call mtcrog(zr(ia), zr(ib), nmax, nmax, 1,&
                        zr(ix), zr(itrav), ier)
!
            if (ier .eq. 1) then
                call u2mess('F', 'ALGELINE_76')
            endif
!
            do 213 i = 1, nmax
                zr(ixx+i-1+nmax*(2*igrp-idir)) = zr(ix+i-1)
213          continue
21      continue
 2  end do
!
!
! --- CALCUL DES COEFFICIENTS PPXX, PPXY, PPYX, PPYY,
!     ET VNXX, VNXY, VNYX, VNYY
    do 3 i = 1, nbgrp
        do 31 j = 1, nbcyl
            ppxx(j,i) = 2.d0*zr(ixx-1+2*nbtron*((2*i-1)*(nbcyl+1)+j)+ 1)
            ppxy(j,i) = 2.d0*zr(ixx-1+2*nbtron*((2*i-2)*(nbcyl+1)+j)+ 1)
            ppyx(j,i) = 2.d0*zr(ixx-1+2*nbtron*((2*i-1)*(nbcyl+1)+j)+ 2)
            ppyy(j,i) = 2.d0*zr(ixx-1+2*nbtron*((2*i-2)*(nbcyl+1)+j)+ 2)
!
            if (i .eq. numgrp(j)) then
                ppxx(j,i) = ppxx(j,i) + 1.d0
                ppyy(j,i) = ppyy(j,i) + 1.d0
            endif
31      continue
 3  end do
!
    do 4 i = 1, nbgrp
        do 41 j = 1, nbgrp
            tmp(1,j) = 0.d0
            tmp(2,j) = 0.d0
            tmp(3,j) = 0.d0
            tmp(4,j) = 0.d0
41      continue
        do 42 j = 1, nbcyl
            tmp(1,numgrp(j)) = tmp(1,numgrp(j)) + ppxx(j,i)
            tmp(2,numgrp(j)) = tmp(2,numgrp(j)) + ppxy(j,i)
            tmp(3,numgrp(j)) = tmp(3,numgrp(j)) + ppyx(j,i)
            tmp(4,numgrp(j)) = tmp(4,numgrp(j)) + ppyy(j,i)
42      continue
        do 43 j = 1, nbgrp
            ppxx(j,i) = tmp(1,j)
            ppxy(j,i) = tmp(2,j)
            ppyx(j,i) = tmp(3,j)
            ppyy(j,i) = tmp(4,j)
43      continue
 4  end do
!
! --- ON FORCE LA SYMETRIE DES COEFFICIENTS
!
    do 5 i = 1, nbgrp
        do 50 k = 1, nbcyl
            if (numgrp(k) .eq. i) then
                rayoi = rint(k)
            endif
50      continue
        ppxy(i,i) = ppxy(i,i) + ppyx(i,i)
        ppxy(i,i) = ppxy(i,i) / 2.d0
        ppyx(i,i) = ppxy(i,i)
!
        do 51 j = 1, i-1
            do 511 k = 1, nbcyl
                if (numgrp(k) .eq. j) then
                    rayoj = rint(k)
                endif
511          continue
            ppxx(i,j) = rayoi * rayoi * ppxx(i,j) + rayoj * rayoj * ppxx(j,i)
            ppxx(i,j) = ppxx(i,j) / 2.d0
            ppxx(j,i) = ppxx(i,j)
            ppxx(i,j) = ppxx(i,j) / rayoi / rayoi
            ppxx(j,i) = ppxx(j,i) / rayoj / rayoj
!
            ppxy(i,j) = rayoi * rayoi * ppxy(i,j) + rayoj * rayoj * ppyx(j,i)
            ppxy(i,j) = ppxy(i,j) / 2.d0
            ppyx(j,i) = ppxy(i,j)
            ppxy(i,j) = ppxy(i,j) / rayoi / rayoi
            ppyx(j,i) = ppyx(j,i) / rayoj / rayoj
!
            ppyx(i,j) = rayoi * rayoi * ppyx(i,j) + rayoj * rayoj * ppxy(j,i)
            ppyx(i,j) = ppyx(i,j) / 2.d0
            ppxy(j,i) = ppyx(i,j)
            ppyx(i,j) = ppyx(i,j) / rayoi / rayoi
            ppxy(j,i) = ppxy(j,i) / rayoj / rayoj
!
            ppyy(i,j) = rayoi * rayoi * ppyy(i,j) + rayoj * rayoj * ppyy(j,i)
            ppyy(i,j) = ppyy(i,j) / 2.d0
            ppyy(j,i) = ppyy(i,j)
            ppyy(i,j) = ppyy(i,j) / rayoi / rayoi
            ppyy(j,i) = ppyy(j,i) / rayoj / rayoj
51      continue
 5  end do
!
    do 6 i = 1, nbgrp
        do 61 j = 1, nbgrp
            vnxx(j,i) = 0.5d0 * ppxx(j,i)
            vnxy(j,i) = 0.5d0 * ppxy(j,i)
            vnyx(j,i) = 0.5d0 * ppyx(j,i)
            vnyy(j,i) = 0.5d0 * ppyy(j,i)
            if (j .eq. i) then
                ncyl = 0
                do 611 k = 1, nbcyl
                    if (numgrp(k) .eq. i) ncyl = ncyl + 1
611              continue
                vnxx(j,i) = vnxx(j,i) + 0.5d0 * ncyl
                vnyy(j,i) = vnyy(j,i) + 0.5d0 * ncyl
            endif
61      continue
 6  end do
!
! --- MENAGE
    call jedetr('&&MEFCIR.TMP.AB')
    call jedema()
end subroutine
