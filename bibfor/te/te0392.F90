subroutine te0392(option, nomte)
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
!
!          ELEMENTS ISOPARAMETRIQUES 3D_SI
!    FONCTION REALISEE:
!            OPTION : 'RIGI_MECA      '
!                            CALCUL DES MATRICES ELEMENTAIRES  3D
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/caatdb.h"
#include "asterfort/cast3d.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/dmatmc.h"
#include "asterfort/elraga.h"
#include "asterfort/elref4.h"
#include "asterfort/invjac.h"
#include "asterfort/jevech.h"
#include "asterfort/ortrep.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
!
!-----------------------------------------------------------------------
    integer :: idecno, idecpg, idfde2, igau, imate, imatuu, ipoid2
    integer :: ivf2, nbres, nbsig, nno, npg1
!-----------------------------------------------------------------------
    parameter (nbres=9)
    integer :: icodre(nbres)
    character(len=2) :: k2bid
    character(len=16) :: nomte, option, phenom
    real(kind=8) :: jacgau
    real(kind=8) :: repere(7), xyzgau(3), instan
    integer :: igeom, ipoids, ivf, idfde
!
    logical :: calbn
    integer :: kpg, i, ino, j, k, proj, nbpg2
    integer :: ndim, nnos, jgano, kp, idim
    real(kind=8) :: d(6, 6), s
    real(kind=8) :: poipg2(8), b(6, 81), b0(6, 3, 8)
    real(kind=8) :: jac, invja(3, 3), bi(3, 8), hx(3, 4), bary(3)
    real(kind=8) :: gam(4, 8), coopg2(24), h(8, 4), dh(4, 24)
    real(kind=8) :: bn(6, 3, 8)
    real(kind=8) :: dfdx(8), dfdy(8), dfdz(8)
    real(kind=8) :: valres(2), nu, nub
    integer :: codre(1)
    character(len=8) :: nomres(2)
    data h/ 1.d0, 1.d0, -1.d0,-1.d0,-1.d0,-1.d0, 1.d0, 1.d0,&
     &        1.d0,-1.d0, -1.d0, 1.d0,-1.d0, 1.d0, 1.d0,-1.d0,&
     &        1.d0,-1.d0,  1.d0,-1.d0, 1.d0,-1.d0, 1.d0,-1.d0,&
     &       -1.d0, 1.d0, -1.d0, 1.d0, 1.d0,-1.d0, 1.d0,-1.d0/
!
! ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
! ---- GEOMETRIE ET INTEGRATION
!      ------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
!
! --- INITIALISATIONS :
!     -----------------
    k2bid = '  '
    instan = 0.d0
!
! ---- RECUPERATION DES COORDONNEES DES CONNECTIVITES
!      ----------------------------------------------
    call jevech('PGEOMER', 'L', igeom)
!
! ---- RECUPERATION DU MATERIAU
!      ------------------------
    call jevech('PMATERC', 'L', imate)
    call rccoma(zi(imate), 'ELAS', 1, phenom, icodre(1))
!
! ---- RECUPERATION  DES DONNEEES RELATIVES AU REPERE D'ORTHOTROPIE
!      ------------------------------------------------------------
!     COORDONNEES DU BARYCENTRE ( POUR LE REPERE CYLINDRIQUE )
    bary(1) = 0.d0
    bary(2) = 0.d0
    bary(3) = 0.d0
    do 150 i = 1, nno
        do 140 idim = 1, ndim
            bary(idim) = bary(idim)+zr(igeom+idim+ndim*(i-1)-1)/nno
140      continue
150  end do
    call ortrep(zi(imate), ndim, bary, repere)
!
    call jevech('PMATUUR', 'E', imatuu)
    do 1 i = 1, 300
        zr(imatuu-1+i)=0.0d0
 1  end do
!
!    PROJ : INDICATEUR DE LA PROJECTION
!           0 AUCUNE
!           1 ADS
!           2 ASBQI
    proj= 2
!
    calbn = .false.
! - INITIALISATION HEXAS8
    call elraga('HE8', 'FPG8    ', ndim, nbpg2, coopg2,&
                poipg2)
    call elref4('HE8', 'MASS', ndim, nno, nnos,&
                nbpg2, ipoid2, ivf2, idfde2, jgano)
!
!
!  RECUP DU COEF DE POISSON POUR ASQBI
!
    nomres(1)='E'
    if (phenom .eq. 'ELAS') then
        nomres(2)='NU'
    else if (phenom.eq.'ELAS_ISTR') then
        nomres(2)='NU_LT'
    else if (phenom.eq.'ELAS_ORTH') then
        nomres(2)='NU_LT'
    else
        ASSERT(.false.)
    endif
!
!
    call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                ' ', phenom, 0, ' ', [0.d0],&
                1, nomres(2), valres(2), codre, 1)
    if (codre(1) .eq. 0) then
        nu = valres(2)
    else
        call utmess('F', 'ELEMENTS4_72')
    endif
    nub = nu/(1.d0-nu)
!
! - CALCUL DES COEFFICIENTS BI (MOYENNE DES DERIVEES DES FCTS DE FORME)
!
    do 2 kpg = 1, npg1
        call dfdm3d(nno, kpg, ipoids, idfde, zr(igeom),&
                    jac, dfdx, dfdy, dfdz)
        do 3 ino = 1, nno
            bi(1,ino) = dfdx(ino)
            bi(2,ino) = dfdy(ino)
            bi(3,ino) = dfdz(ino)
 3      continue
 2  end do
!
    do 110 i = 1, 6
        do 110 j = 1, 81
            b(i,j) = 0.d0
110      continue
!
! ---  BOUCLE SUR LES POINTS D'INTEGRATION
!      -----------------------------------
    do 50 igau = 1, npg1
!
        idecpg = nno* (igau-1) - 1
!
!  --      COORDONNEES ET TEMPERATURE/HYDRATATION/SECHAGE AU POINT
!  --      D'INTEGRATION COURANT
!          -------
        xyzgau(1) = 0.d0
        xyzgau(2) = 0.d0
        xyzgau(3) = 0.d0
!
        do 30 i = 1, nno
!
            idecno = 3* (i-1) - 1
!
            xyzgau(1) = xyzgau(1) + zr(ivf+i+idecpg)*zr(igeom+1+ idecno)
            xyzgau(2) = xyzgau(2) + zr(ivf+i+idecpg)*zr(igeom+2+ idecno)
            xyzgau(3) = xyzgau(3) + zr(ivf+i+idecpg)*zr(igeom+3+ idecno)
!
30      continue
!
        call dfdm3d(nno, igau, ipoids, idfde, zr(igeom),&
                    jacgau, dfdx, dfdy, dfdz)
!
!  --      CALCUL DE LA MATRICE B RELIANT LES DEFORMATIONS DU
!  --      PREMIER ORDRE AUX DEPLACEMENTS
        do 20 i = 1, 8
            j= 3*(i-1) + 1
            b(1,j) = bi(1,i)
            b(2,j+1) = bi(2,i)
            b(3,j+2) = bi(3,i)
            b(4,j) = bi(2,i)
            b(4,j+1) = bi(1,i)
            b(5,j) = bi(3,i)
            b(5,j+2) = bi(1,i)
            b(6,j+1) = bi(3,i)
            b(6,j+2) = bi(2,i)
20      continue
        do 22 i = 1, nno
            do 22 j = 1, 3
                do 22 k = 1, 6
                    b0(k,j,i)=b(k,(i-1)*3+j)
22              continue
!
!  --      CALCUL DE LA MATRICE DE HOOKE (LE MATERIAU POUVANT
!  --      ETRE ISOTROPE, ISOTROPE-TRANSVERSE OU ORTHOTROPE)
!          -------------------------------------------------
        call dmatmc('RIGI', k2bid, zi(imate), instan, '+',&
                    igau, 1, repere, xyzgau, nbsig,&
                    d)
!
!     CALCUL DE KC (MATRICE DE RIGIDITE AU CENTRE)
!     --------------------------------------------
        call caatdb(nno, b0, d, b0, jacgau,&
                    zr(imatuu))
!
50  end do
! - CALCUL DES COEFFICIENTS GAMMA
!
    do 6 i = 1, 4
        do 7 k = 1, 3
            hx(k,i) = 0.d0
            do 8 j = 1, nno
                hx(k,i) = hx(k,i) + h(j,i) * zr(igeom-1+3*(j-1)+k)
 8          continue
 7      continue
 6  end do
!
    do 9 i = 1, 4
        do 10 j = 1, nno
            s = 0.d0
            do 11 k = 1, 3
                s = s + hx(k,i) * bi(k,j)
11          continue
            gam(i,j) = 0.125d0 * (h(j,i) - s)
10      continue
 9  end do
!
!           CORRECTION DE LA MATRICE DE RIGIDITE
!                 CALCUL DE KSTAB
!     --------------------------------------------
!
!        CALCUL DES TERMES EVALUES AUX 8 POINTS DE GAUSS
    do 160 kpg = 1, nbpg2
        kp = 3*(kpg-1)
        call invjac(nno, kpg, ipoid2, idfde2, zr(igeom),&
                    invja, jac)
!
        do 161 i = 1, 3
            dh(1,kp+i) = coopg2(3*kpg-1) * invja(i,3) + coopg2(3*kpg) * invja(i,2)
161      continue
!
        do 162 i = 1, 3
            dh(2,kp+i) = coopg2(3*kpg-2) * invja(i,3) + coopg2(3*kpg) * invja(i,1)
162      continue
!
        do 163 i = 1, 3
            dh(3,kp+i) = coopg2(3*kpg-2) * invja(i,2) + coopg2(3*kpg- 1) * invja(i,1)
163      continue
!
        do 164 i = 1, 3
            dh(4,kp+i) = coopg2(3*kpg-2) * coopg2(3*kpg-1) * invja(i, 3) + coopg2(3*kpg-1) * coop&
                         &g2(3*kpg) * invja(i,1) + coopg2(3*kpg-2) * coopg2(3*kpg) * invja(i,2)
164      continue
!
        call cast3d(proj, gam, dh, b0, nno,&
                    kpg, nub, nu, d, calbn,&
                    bn, jac, zr( imatuu))
!
160  continue
!
end subroutine
