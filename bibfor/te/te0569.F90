subroutine te0569(option, nomte)
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
    implicit none
#include "jeveux.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
!
    character(len=16) :: option, nomte
!
    integer :: ipoids, ivf, idfdx, idfdy, igeom, i, j
    integer :: ndim, nno, ipg, npg1, ino, jno, ij, kpg, spt
    integer :: idec, jdec, kdec, ldec, imate, imatuu
    integer :: mater, ll, k, l, nnos, jgano
    real(kind=8) :: jac, nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9)
    real(kind=8) :: valres(5), e, nu, lambda, mu, rho
    real(kind=8) :: rhocp, rhocs, l0, usl0
    real(kind=8) :: taux, tauy, tauz
    real(kind=8) :: nux, nuy, nuz, scal, vnx, vny, vnz
    real(kind=8) :: vituni(3, 3), vect(9, 3, 27)
    real(kind=8) :: matr(27, 27)
    real(kind=8) :: vtx, vty, vtz
    integer :: icodre(5)
    character(len=8) :: fami, poum
    character(len=16) :: nomres(5)
!     ------------------------------------------------------------------
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfdx,jgano=jgano)
    idfdy = idfdx + 1
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PMATUUR', 'E', imatuu)
!
    mater=zi(imate)
    nomres(1)='E'
    nomres(2)='NU'
    nomres(3) = 'RHO'
    nomres(4) = 'LONG_CARA'
    nomres(5) = 'COEF_AMOR'
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, mater,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                5, nomres, valres, icodre, 1)
    e = valres(1)
    nu = valres(2)
    rho = valres(3)
    l0 = valres(4)
    if (l0 .lt. 1.d-2) then
      usl0= 0.d0
    else
      usl0=1.d0/l0
    endif
    lambda = e*nu/ (1.d0+nu)/ (1.d0-2.d0*nu)
    mu = e/2.d0/ (1.d0+nu)
!
    if (option .eq. 'AMOR_MECA') then
      rhocp = valres(5)*sqrt((lambda+2.d0*mu)*rho)
      rhocs = valres(5)*sqrt(mu*rho)
    else
      rhocp = (lambda+2.d0*mu)*usl0
      rhocs = mu*usl0
    endif
!
!
!     VITESSE UNITAIRE DANS LES 3 DIRECTIONS
!
    vituni(1,1) = 1.d0
    vituni(1,2) = 0.d0
    vituni(1,3) = 0.d0
!
    vituni(2,1) = 0.d0
    vituni(2,2) = 1.d0
    vituni(2,3) = 0.d0
!
    vituni(3,1) = 0.d0
    vituni(3,2) = 0.d0
    vituni(3,3) = 1.d0
!
    do 310 i = 1, nno
        do 311 j = 1, 3
            do 312 k = 1, 3*nno
                vect(i,j,k) = 0.d0
312         continue
311     continue
310 continue
!
!     --- CALCUL DES PRODUITS VECTORIELS OMI X OMJ ---
!
    do 30 ino = 1, nno
        i = igeom + 3*(ino-1) -1
        do 31 jno = 1, nno
            j = igeom + 3*(jno-1) -1
            sx(ino,jno) = zr(i+2)*zr(j+3) - zr(i+3)*zr(j+2)
            sy(ino,jno) = zr(i+3)*zr(j+1) - zr(i+1)*zr(j+3)
            sz(ino,jno) = zr(i+1)*zr(j+2) - zr(i+2)*zr(j+1)
31      continue
30  continue
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
        do 101 i = 1, nno
            idec = (i-1)*ndim
            do 102 j = 1, nno
                jdec = (j-1)*ndim
                nx = nx + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sx(i,j)
                ny = ny + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sy(i,j)
                nz = nz + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sz(i,j)
102         continue
101     continue
!
!        --- LE JACOBIEN EST EGAL A LA NORME DE LA NORMALE ---
!
        jac = sqrt(nx*nx + ny*ny + nz*nz)
!
!        --- CALCUL DE LA NORMALE UNITAIRE ---
!
        nux = nx / jac
        nuy = ny / jac
        nuz = nz / jac
!
!        --- CALCUL DE V.N ---
!
        do 103 i = 1, nno
            do 104 j = 1, 3
                scal = nux*zr(ivf+ldec+i-1)*vituni(j,1)
                scal = scal+nuy*zr(ivf+ldec+i-1)*vituni(j,2)
                scal = scal+nuz*zr(ivf+ldec+i-1)*vituni(j,3)
!
!        --- CALCUL DE LA VITESSE NORMALE ET DE LA VITESSE TANGENCIELLE
!
                vnx = nux*scal
                vny = nuy*scal
                vnz = nuz*scal
!
                vtx = zr(ivf+ldec+i-1)*vituni(j,1)
                vty = zr(ivf+ldec+i-1)*vituni(j,2)
                vtz = zr(ivf+ldec+i-1)*vituni(j,3)
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
                do 105 l = 1, nno
                    ll = 3*l-2
                    vect(i,j,ll) = vect(i,j,ll) + taux*zr(ivf+ldec+l- 1)*jac*zr(ipoids+ipg-1)
                    vect(i,j,ll+1) = vect(i,j,ll+1) + tauy*zr(ivf+ ldec+l-1)*jac*zr(ipoids+ipg-1)
                    vect(i,j,ll+2) = vect(i,j,ll+2) + tauz*zr(ivf+ ldec+l-1)*jac*zr(ipoids+ipg-1)
105             continue
104         continue
103     continue
100 continue
!
    do 400 i = 1, nno
        do 401 j = 1, 3
            do 402 k = 1, 3*nno
                matr(3*(i-1)+j,k) = vect(i,j,k)
402         continue
401     continue
400 continue
!
!       --- PASSAGE AU STOCKAGE TRIANGULAIRE
!
    do 210 i = 1, 3*nno
        do 211 j = 1, i
            ij = (i-1)*i/2+j
            zr(imatuu+ij-1) = matr(i,j)
211     continue
210 continue
!
end subroutine
