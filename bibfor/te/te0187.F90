subroutine te0187(option, nomte)
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
!.......................................................................
!
!     BUT: CALCUL DU VECTEUR INTENSITE ACTIVE AUX NOEUDS
!          ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'INTE_ELNO'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    implicit none
!
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
!
    integer :: icodre(1)
    character(len=16) :: nomte, option
    character(len=8) :: fami, poum
    real(kind=8) :: omega
    real(kind=8) :: omerho, pi
    complex(kind=8) :: vitx(27), vity(27), vitz(27)
!
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), jac
    integer :: idfde, igeom, idino, ipino
!
    integer :: iinte, ipres, imate, ifreq
    integer :: jgano, nno, ino, i, kpg, spt
!
!
!-----------------------------------------------------------------------
    integer :: ipoids, ivf, mater, ndim, nnos, npg
    real(kind=8) :: rho(1)
!-----------------------------------------------------------------------
    call elrefe_info(fami='NOEU',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PPRESSC', 'L', ipres)
    call jevech('PMATERC', 'L', imate)
    call jevech('PINTER', 'E', iinte)
!
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    mater=zi(imate)
    call rcvalb(fami, kpg, spt, poum, mater,&
                ' ', 'FLUIDE', 0, ' ', [0.d0],&
                1, 'RHO', rho, icodre, 1)
!
    pi=r8pi()
    call jevech('PFREQR', 'L', ifreq)
    omega=2.d0*pi*zr(ifreq)
    omerho=omega*rho(1)
!
!
!    BOUCLE SUR LES NOEUDS
    do 30 ino = 1, nno
!
        idino=iinte+(ino-1)*6-1
        ipino=ipres+ino-1
        call dfdm3d(nno, ino, ipoids, idfde, zr(igeom),&
                    jac, dfdx, dfdy, dfdz)
!
        vitx(ino)=(0.0d0,0.0d0)
        vity(ino)=(0.0d0,0.0d0)
        vitz(ino)=(0.0d0,0.0d0)
!
        do 20 i = 1, nno
!
            vitx(ino)=vitx(ino)+dfdx(i)*zc(ipres+i-1)
            vity(ino)=vity(ino)+dfdy(i)*zc(ipres+i-1)
            vitz(ino)=vitz(ino)+dfdz(i)*zc(ipres+i-1)
20      continue
!
        vitx(ino)=vitx(ino)*(0.d0,1.d0)/omerho
        vity(ino)=vity(ino)*(0.d0,1.d0)/omerho
        vitz(ino)=vitz(ino)*(0.d0,1.d0)/omerho
!
        zr(idino+1)=0.5d0*dble(zc(ipino)*dconjg(vitx(ino)))
        zr(idino+2)=0.5d0*dble(zc(ipino)*dconjg(vity(ino)))
        zr(idino+3)=0.5d0*dble(zc(ipino)*dconjg(vitz(ino)))
        zr(idino+4)=0.5d0*dimag(zc(ipino)*dconjg(vitx(ino)))
        zr(idino+5)=0.5d0*dimag(zc(ipino)*dconjg(vity(ino)))
        zr(idino+6)=0.5d0*dimag(zc(ipino)*dconjg(vitz(ino)))
30  end do
!
end subroutine
