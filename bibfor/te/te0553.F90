subroutine te0553(option, nomte)
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
#include "asterfort/vff2dn.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!
!     BUT: CALCUL DES MATRICES ELEMENTAIRES EN MECANIQUE
!          CORRESPONDANT A UN AMORTISSEMENT
!          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 2D
!
!          OPTION : 'AMOR_MECA'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    character(len=16) :: nomres(5)
    character(len=8) ::  fami, poum
    integer :: icodre(5), kpg, spt
    real(kind=8) :: poids, nx, ny, valres(5), e, nu, lambda, mu
    real(kind=8) :: rhocp, rhocs, l0, usl0
    real(kind=8) :: rho, taux, tauy, nux, nuy, scal, vnx, vny, vtx, vty
    real(kind=8) :: vituni(2, 2), vect(3, 2, 6), matr(6, 6), jac
    integer :: nno, kp, npg, ipoids, ivf, idfde, igeom
    integer :: k, i, l, mater
!
!-----------------------------------------------------------------------
    integer :: ij, imate, imatuu, j, jgano, ll, ndim
    integer :: nnos
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PMATUUR', 'E', imatuu)
!
!      WRITE(6,*) 'MARC KHAM ---> TE0553  OPTION=',OPTION
!
    mater = zi(imate)
    nomres(1) = 'E'
    nomres(2) = 'NU'
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
!
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
!     VITESSE UNITAIRE DANS LES 3 DIRECTIONS
!
    vituni(1,1) = 1.d0
    vituni(1,2) = 0.d0
    vituni(2,1) = 0.d0
    vituni(2,2) = 1.d0
    do 10 i = 1, nno
        do 11 j = 1, 2
            do 12 k = 1, 2*nno
                vect(i,j,k) = 0.d0
12          continue
11      continue
10  continue
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 40 kp = 1, npg
        k = (kp-1)*nno
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom), nx, ny, poids)
        jac = sqrt(nx*nx+ny*ny)
!
!        --- CALCUL DE LA NORMALE UNITAIRE ---
!
        nux = nx/jac
        nuy = ny/jac
!
!        --- CALCUL DE V.N ---
!
        scal = 0.d0
        do 41 i = 1, nno
            do 42 j = 1, 2
                scal = nux*zr(ivf+k+i-1)*vituni(j,1)
                scal = scal + nuy*zr(ivf+k+i-1)*vituni(j,2)
!
!        --- CALCUL DE LA VITESSE NORMALE ET DE LA VITESSE TANGENCIELLE
!
                vnx = nux*scal
                vny = nuy*scal
                vtx = zr(ivf+k+i-1)*vituni(j,1)
                vty = zr(ivf+k+i-1)*vituni(j,2)
                vtx = vtx - vnx
                vty = vty - vny
!
!        --- CALCUL DU VECTEUR CONTRAINTE
!
                taux = rhocp*vnx + rhocs*vtx
                tauy = rhocp*vny + rhocs*vty
!
!        --- CALCUL DU VECTEUR ELEMENTAIRE
!
                do 43 l = 1, nno
                    ll = 2*l - 1
                    vect(i,j,ll) = vect(i,j,ll) + taux*zr(ivf+k+l-1)* poids
                    vect(i,j,ll+1)=vect(i,j,ll+1)+tauy*zr(ivf+k+l-1)*&
                    poids
43              continue
42          continue
41      continue
40  continue
!
    do 80 i = 1, nno
        do 81 j = 1, 2
            do 82 k = 1, 2*nno
                matr(2* (i-1)+j,k) = vect(i,j,k)
82          continue
81      continue
80  continue
!
!       --- PASSAGE AU STOCKAGE TRIANGULAIRE
!
    do 100 i = 1, 2*nno
        do 101 j = 1, i
            ij = (i-1)*i/2 + j
            zr(imatuu+ij-1) = matr(i,j)
101     continue
100 continue
!
end subroutine
