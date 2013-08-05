subroutine xmilac(ndim, igeom, ptint, tabco, tabdir,&
                  jgrlsn, p, r, ptmil, milac)
! aslint: disable=W1306
    implicit none
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/elrfvf.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/milfic.h"
#include "asterfort/normev.h"
#include "asterfort/pdsca1.h"
#include "asterfort/provec.h"
#include "asterfort/reereg.h"
#include "asterfort/vecini.h"
    integer :: ndim, igeom, r, p
    integer :: jgrlsn, tabdir(4)
    real(kind=8) :: milac(ndim), ptint(*), ptmil(*), tabco(*)
!
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
!                      CALCUL ET STOCKAGE DES COORDONNEES DU MILIEU DE
!                      L'ARETE CREEE LORS DU SOUS DECOUPAGE
!
!     ENTREE
!       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
!       IGEOM   : COORDONNEES DES NOEUDS DE L'ELEMENT PARENT
!       NUMAEP  : NUMERO DES NOEUDS DE L'ELEMENT ENFANT ET LEUR
!                 EQUIVALENCE DANS L'ELEMENT PARENT
!       PTINT  : COORDONNÉES DES POINTS D'INTERSECTION
!       TABCO  : COORDONNÉES DES NOEUDS DE L'ELEMENT ENFANT
!       JTABLS  : LSN DES NOEUDS DE L'ELEMENT ENFANT
!         P     :
!         R     :
!
!     SORTIE
!       MILAC  : COORDONNEES DU POINT MILIEU DE L'ARETE CREEE
!
!     ----------------------------------------------------------------
!
    integer :: nbnomx
    parameter   (nbnomx = 27)
!
    integer :: iret, j, k
    real(kind=8) :: vab(3), vba(3), vac(3), vn101c(3), vnfi(3), vnma(3)
    real(kind=8) :: v1(3), v2(3), ksip(3), vn(3), ksif(3)
    real(kind=8) :: pscal1, pscal2, pscal3, pscal4, pscal5, norme
    real(kind=8) :: a(3), b(3), c(3), e(3), f(3), tab(8*ndim)
    real(kind=8) :: ff(9), h, pint1(3), pint2(3), tabar(9)
    integer :: ibid, nnop
    character(len=8) :: elrefp, elp
    logical :: deja
    parameter    (elp='QU8')
!
! --------------------------------------------------------------------
!
    call jemarq()
    ASSERT(ndim.eq.2)
!
! --- INITIALISATION
    call elref1(elrefp)
    call elref4(' ', 'RIGI', ibid, nnop, ibid,&
                ibid, ibid, ibid, ibid, ibid)
!
    call vecini(3, 0.d0, vab)
    call vecini(3, 0.d0, vba)
    call vecini(3, 0.d0, vac)
    call vecini(3, 0.d0, v1)
    call vecini(3, 0.d0, v2)
    call vecini(3, 0.d0, vn101c)
    call vecini(3, 0.d0, vnfi)
    call vecini(3, 0.d0, f)
    call vecini(3, 0.d0, vn)
    call vecini(ndim, 0.d0, milac)
    deja=.false.
!
! --- RECUPERATION DES COORDONNES
    do 101 k = 1, ndim
        pint1(k)=ptint(ndim*(r-2)+k)
        pint2(k)=ptint(ndim*(r-1)+k)
101  end do
!
    do 10 k = 1, ndim
        a(k)=tabco(ndim*(tabdir(1)-1)+k)
        b(k)=tabco(ndim*(tabdir(2)-1)+k)
        c(k)=tabco(ndim*(tabdir(3)-1)+k)
        e(k)=tabco(ndim*(tabdir(4)-1)+k)
10  end do
!
! --- VECTEURS AB, AC, BA
    do 102 k = 1, ndim
        vab(k) = b(k)-a(k)
        vac(k) = c(k)-a(k)
        vba(k) = -vab(k)
102  end do
!
! --- NORMALE A 101C : VN101C
    do 103 k = 1, 2
        if (abs(c(k)-pint1(k)) .lt. r8prem()) then
            vn101c(1) = 2-k
            vn101c(2) = k-1
            deja=.true.
        endif
103  end do
!
    if (.not.deja) then
        h = (c(2)-pint1(2)) / (c(1)-pint1(1))
        v1(2) = 1/sqrt(h*h+1)
        v1(1) = -h*v1(2)
        v2(2) = -1/sqrt(h*h+1)
        v2(1) = -h*v2(2)
!       ORIENTATION DE VN101C (PASCAL AVEC BA>0)
        call pdsca1(v1, vba, pscal1)
        call pdsca1(v2, vba, pscal2)
        if (pscal1 .gt. 0.d0) then
            vn101c(1) = v1(1)
            vn101c(2) = v1(2)
        else if (pscal2.gt.0.d0) then
            vn101c(1) = v2(1)
            vn101c(2) = v2(2)
        endif
    endif
!
! --- NORMALE A LA FISSURE : VNFI
!
!     CALCUL DES COORDONNEES DE REF DE 101
    call reereg('S', elrefp, nnop, zr(igeom), pint1,&
                2, ksip, iret)
!
!     CALCUL DES FONCTIONS DE FORME DE L'ELEMENT EN KSI
    call elrfvf(elrefp, ksip, nbnomx, ff, ibid)
!
    do 114 j = 1, ndim
        do 115 k = 1, nnop
            vnfi(j) = vnfi(j) + ff(k)*zr(jgrlsn-1+ndim*(k-1)+j)
115      continue
114  continue
    call normev(vnfi, norme)
!
!     ORIENTATION DE VNFI (PASCAL AVEC BA>0)
    call pdsca1(vnfi, vba, pscal3)
    if (pscal3 .lt. 0.d0) then
        do 20 k = 1, ndim
            vnfi(k) = -vnfi(k)
20      continue
    endif
!
    call pdsca1(vn101c, vnfi, pscal5)
!
! --- NORMALE AU MAILLAGE : VNMA
    call provec(vab, vac, vnma)
! --- PRODUIT VECTORIEL ENTRE NORMALEFISSURE ET NORMALEARETE : VN
    call provec(vnfi, vn101c, vn)
! --- PRODUIT SCALAIRE VN.VNMA
    call pdsca1(vn, vnma, pscal4)
!
! --- CALCUL COORDONNEES POINT MILIEU ARETE
    if (pscal4 .gt. 0.d0) then
!     ARETE COURBE
        do 105 k = 1, ndim
            tab(0*ndim+k)= b(k)
            tab(1*ndim+k)= c(k)
            tab(2*ndim+k)= pint2(k)
            tab(3*ndim+k)= pint1(k)
            tab(4*ndim+k)= e(k)
            tab(5*ndim+k)= ptmil(2*(p-1)*ndim+k)
            tab(6*ndim+k)= ptmil((2*p)*ndim+k)
            tab(7*ndim+k)= ptmil(2*(p-2)*ndim+k)
105      continue
!
!       CALCUL DES FONCTIONS DE FORME DE L'ELEMENT EN (0,0)
        ksif(1)=0.d0
        ksif(2)=0.d0
        call elrfvf(elp, ksif, nbnomx, ff, ibid)
!
        do 106 j = 1, ndim
            do 107 k = 1, 8
                f(j) = f(j) + ff(k)*tab((k-1)*ndim+j)
107          continue
106      continue
!       TABLEAU DES COORDONNEES DES NOEUDS DE L'ARETE C-101-F
        do 108 k = 1, ndim
            tabar(k)=c(k)
            tabar(ndim+k)=pint1(k)
            tabar(2*ndim+k)=f(k)
108      continue
!
        call milfic(ndim, tabar, milac)
!
    else
!     ARETE DROITE
        do 109 k = 1, ndim
            milac(k)=(pint1(k)+c(k))/2
109      continue
!
    endif
!
    call jedema()
end subroutine
