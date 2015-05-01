subroutine mefasc(ndim, nbcyl, nbgrp, nbtron, numgrp,&
                  idir, igrp, som, rint, dcent,&
                  ficent, d, fi, a, b)
    implicit none
!
#include "asterfort/mefac1.h"
#include "asterfort/mefac2.h"
    integer :: ndim(14), nbcyl, nbgrp, nbtron, numgrp(*), idir, igrp
    real(kind=8) :: dcent(nbcyl), ficent(nbcyl), rint(*), som(9)
    real(kind=8) :: d(nbcyl, nbcyl), fi(nbcyl, nbcyl)
    real(kind=8) :: a(2*nbtron*(nbcyl+1), *), b(*)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ASSEMBLAGE POUR L ENCEINTE CIRCULAIRE
!     OPERATEUR APPELANT : OP0144 , FLUST3, MEFIST, MEFCIR
! ----------------------------------------------------------------------
!     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE
!     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
!     DE TUBES SOUS ECOULEMENT AXIAL"
! ----------------------------------------------------------------------
! IN  : NDIM   : TABLEAU DES DIMENSIONS
! IN  : NBCYL  : NOMBRE DE CYLINDRES
! IN  : NBGRP  : NOMBRE DE GROUPES D EQUIVALENCE
! IN  : NBTRON : ORDRE DE TRONCATURE DES SERIES DE LAURENT DANS LA BASE
!                MODALE
! IN  : NUMGRP : INDICES DES GROUPES D EQUIVALENCE
! IN  : IDIR   : INDICES DE CYLINDRE
! IN  : IGRP   : INDICES DE GROUPE DE CYLINDRE
! IN  : SOM    : XEXT,YEXT,REXT
! IN  : RINT   : RAYONS DES CYLINDRES
! IN  : DCENT  : DISTANCE DU CENTRE DES CYLINDRES AU CENTRE DE
!                L ENCEINTE
! IN  : FICENT : ANGLE POLAIRE PAR RAPPORT AU CENTRE DE L ENCEINTE
! IN  : D      : DISTANCE RELATIVE ENTRE LES CENTRES DES CYLINDRES
! IN  : FI     : ANGLE POLAIRE RELATIF PAR RAPPORT AU CENTRE DE CHAQUE
!                CYLINDRE
! IN  : A      : TABLEAU DE TRAVAIL: SOUS MATRICE DU SYSTEME A.X = B
! IN  : B      : TABLEAU DE TRAVAIL: SECOND MEMBRE DU SYSTEME A.X = B
! ----------------------------------------------------------------------
    integer :: i, j, k, l, ni, nj, nk, nl
    real(kind=8) :: coef
! ----------------------------------------------------------------------
!
! --- LECTURE DES DIMENSIONS
!-----------------------------------------------------------------------
    real(kind=8) :: rext
!-----------------------------------------------------------------------
    nbcyl = ndim(3)
    nbgrp = ndim(4)
    nbtron = ndim(5)
!
!
    rext = som(3)
!
!
    do 1 j = 1, nbtron
        nj = 2*j
        do 11 k = 1, nbcyl
            nk = 2*nbtron*k
            do 111 l = 1, j
                nl = nk+2*l
                if (dcent(k) .eq. 0.d0 .and. j .eq. l) then
                    coef = mefac1(j,l)* (rint(k)**(l+1))/(rext**(j+1))
                else if (dcent(k).eq.0.d0.and.j.ne.l) then
                    coef = 0.d0
                else
                    coef = mefac1(j,l)*(dcent(k)**(j-l))* (rint(k)**( l+1))/(rext**(j+1))
                endif
                a(nj-1,nl-1) = -coef*cos((j-l)*ficent(k))
                a(nj,nl-1) = coef*sin((j-l)*ficent(k))
                a(nj-1,nl) = coef*sin((j-l)*ficent(k))
                a(nj,nl) = coef*cos((j-l)*ficent(k))
111          continue
11      continue
        a(nj-1,nj-1) = j
        a(nj,nj) = -j
!
 1  end do
!
    do 2 i = 1, nbcyl
        ni = 2*nbtron*i
        do 21 j = 1, nbtron
            nj = ni+2*j
            do 211 k = 1, nbcyl
                nk = 2*nbtron*k
                if (k .ne. i) then
                    do 2111 l = 1, nbtron
                        nl = nk+2*l
                        coef = mefac2(l,j)*(rint(i)**(j-1))* (rint(k) **(l+1))/(d(i,k)**(l+j))
                        coef = coef*((-1)**l)
                        a(nj-1,nl-1) = coef*cos((j+l)*fi(i,k))
                        a(nj,nl-1) = coef*sin((j+l)*fi(i,k))
                        a(nj-1,nl) = coef*sin((j+l)*fi(i,k))
                        a(nj,nl) = -coef*cos((j+l)*fi(i,k))
2111                  continue
                else
                    nl = nk+2*j
                    a(nj-1,nl-1) = -j
                    a(nj,nl) = -j
                endif
211          continue
!
            do 221 l = j, nbtron
                nl = 2*l
                if (dcent(i) .eq. 0.d0 .and. j .eq. l) then
                    coef = mefac1(l,j)*(rint(i)**(j-1)) /(rext**(l-1))
                else if (dcent(i).eq.0.d0.and.j.ne.l) then
                    coef = 0.d0
                else
                    coef = mefac1(l,j)*(rint(i)**(j-1))* (dcent(i)**( l-j))/(rext**(l-1))
                endif
                a(nj-1,nl-1) = coef*cos((l-j)*ficent(i))
                a(nj,nl-1) = -coef*sin((l-j)*ficent(i))
                a(nj-1,nl) = coef*sin((l-j)*ficent(i))
                a(nj,nl) = coef*cos((l-j)*ficent(i))
221          continue
!
21      continue
        if (numgrp(i) .eq. igrp) then
            b(2*nbtron*i+idir) = 1.d0
        endif
 2  end do
!
end subroutine
