subroutine te0556(option, nomte)
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
    implicit none
#include "jeveux.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          CORRESPONDANT A UNE IMPEDANCE ANECHOIQUE
!          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'IMPE_ABSO'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
    integer :: ipoids, ivf, idfdx, idfdy, igeom, i, j
    integer :: ndim, nno, ipg, npg1, ino, jno
    integer :: idec, jdec, kdec, ldec, ires, imate, ivite
    integer :: ii, mater, nnos, jgano, kpg, spt
    real(kind=8) :: jac, nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9)
    real(kind=8) :: valres(3), e, nu, lambda, mu, rho
    real(kind=8) :: rhocp, rhocs
    real(kind=8) :: taux, tauy, tauz
    real(kind=8) :: nux, nuy, nuz, scal, vnx, vny, vnz
    real(kind=8) :: vtx, vty, vtz, r8b
    integer :: icodre(3)
    character(len=8) :: nomres(3), fami, poum
!     ------------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfdx, jgano)
    idfdy = idfdx + 1
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PVITPLU', 'L', ivite)
    call jevech('PVECTUR', 'E', ires)
!
    do 20 i = 1, 3*nno
        zr(ires+i-1) = 0.0d0
20  end do
!
    mater=zi(imate)
    nomres(1)='E'
    nomres(2)='NU'
    nomres(3)='RHO'
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, mater,&
                ' ', 'ELAS', 0, ' ', [r8b],&
                3, nomres, valres, icodre, 1)
    e = valres(1)
    if (e .lt. 1.d-1) goto 999
    nu = valres(2)
    rho = valres(3)
!
    lambda = e*nu/(1.d0+nu)/(1.d0-2.d0*nu)
    mu = e/2.d0/(1.d0+nu)
!
    rhocp = sqrt((lambda+2.d0*mu)*rho)
    rhocs = sqrt(mu*rho)
!
!     --- CALCUL DES PRODUITS VECTORIELS OMI X OMJ ---
!
    do 30 ino = 1, nno
        i = igeom + 3*(ino-1) -1
        do 32 jno = 1, nno
            j = igeom + 3*(jno-1) -1
            sx(ino,jno) = zr(i+2) * zr(j+3) - zr(i+3) * zr(j+2)
            sy(ino,jno) = zr(i+3) * zr(j+1) - zr(i+1) * zr(j+3)
            sz(ino,jno) = zr(i+1) * zr(j+2) - zr(i+2) * zr(j+1)
32      continue
30  end do
!
!     --- BOUCLE SUR LES POINTS DE GAUSS ---
!
    do 100 ipg = 1, npg1
        kdec = (ipg-1)*nno*ndim
        ldec = (ipg-1)*nno
!
        nx = 0.0d0
        ny = 0.0d0
        nz = 0.0d0
!
!        --- CALCUL DE LA NORMALE AU POINT DE GAUSS IPG ---
!
        do 102 i = 1, nno
            idec = (i-1)*ndim
            do 102 j = 1, nno
                jdec = (j-1)*ndim
                nx = nx + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sx(i,j)
                ny = ny + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sy(i,j)
                nz = nz + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sz(i,j)
102          continue
!
!        --- LE JACOBIEN EST EGAL A LA NORME DE LA NORMALE ---
!
        jac = sqrt (nx*nx + ny*ny + nz*nz)
!
!        --- CALCUL DE LA NORMALE UNITAIRE ---
!
        nux = nx / jac
        nuy = ny / jac
        nuz = nz / jac
!
!        --- CALCUL DE V.N ---
!
        scal = 0.d0
        do 110 i = 1, nno
            ii = 3*i-2
            scal = scal+nux*zr(ivf+ldec+i-1)*zr(ivite+ii-1)
            scal = scal+nuy*zr(ivf+ldec+i-1)*zr(ivite+ii+1-1)
            scal = scal+nuz*zr(ivf+ldec+i-1)*zr(ivite+ii+2-1)
110      continue
!
!        --- CALCUL DE LA VITESSE NORMALE ET DE LA VITESSE TANGENCIELLE
!
        vnx = nux*scal
        vny = nuy*scal
        vnz = nuz*scal
!
        vtx = 0.d0
        vty = 0.d0
        vtz = 0.d0
!
        do 120 i = 1, nno
            ii = 3*i-2
            vtx = vtx+zr(ivf+ldec+i-1)*zr(ivite+ii-1)
            vty = vty+zr(ivf+ldec+i-1)*zr(ivite+ii+1-1)
            vtz = vtz+zr(ivf+ldec+i-1)*zr(ivite+ii+2-1)
120      continue
!
        vtx = vtx - vnx
        vty = vty - vny
        vtz = vtz - vnz
!
!        --- CALCUL DU VECTEUR CONTRAINTE
!
        taux = rhocp*vnx + rhocs*vtx
        tauy = rhocp*vny + rhocs*vty
        tauz = rhocp*vnz + rhocs*vtz
!
!        --- CALCUL DU VECTEUR ELEMENTAIRE
!
        do 130 i = 1, nno
            ii = 3*i-2
            zr(ires+ii-1) = zr(ires+ii-1) + taux*zr(ivf+ldec+i-1)*jac* zr(ipoids+ipg-1)
            zr(ires+ii+1-1) = zr(ires+ii+1-1) + tauy*zr(ivf+ldec+i-1)* jac*zr(ipoids+ipg-1)
            zr(ires+ii+2-1) = zr(ires+ii+2-1) + tauz*zr(ivf+ldec+i-1)* jac*zr(ipoids+ipg-1)
130      continue
!
100  continue
!
999  continue
!
end subroutine
