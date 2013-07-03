subroutine te0175(option, nomte)
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
!.......................................................................
!
!     BUT: CALCUL DU VECTEUR INTENSITE ACTIVE AUX NOEUDS
!          ELEMENTS ISOPARAMETRIQUES 2D
!
!          OPTION : 'INTE_ELNO'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    implicit none
#include "jeveux.h"
!
#include "asterc/r8pi.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elref4.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
    integer :: idfde, igeom, idino, kpg, spt
    integer :: iinte, ipres, imate, ifreq, npg, ipoids, ivf
    integer :: nno, ino, i, ndim, nnos, jgano, mater
    real(kind=8) :: omerho, pi, dfdx(9), dfdy(9), jac, r8b, rho
    integer :: icodre
    character(len=8) :: fami, poum
    character(len=16) :: nomte, option
    complex(kind=8) :: vitx, vity
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
!
    call elref4(' ', 'NOEU', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PPRESSC', 'L', ipres)
    call jevech('PMATERC', 'L', imate)
    call jevech('PINTER', 'E', iinte)
!
    mater=zi(imate)
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, mater,&
                ' ', 'FLUIDE', 0, '   ', r8b,&
                1, 'RHO', rho, icodre, 1)
!
    call jevech('PFREQR', 'L', ifreq)
    pi=r8pi()
    omerho=2.d0*pi*zr(ifreq)*rho
!
!    BOUCLE SUR LES NOEUDS
    do 30 ino = 1, npg
        idino=iinte+(ino-1)*4-1
        call dfdm2d(nno, ino, ipoids, idfde, zr(igeom),&
                    dfdx, dfdy, jac)
!
        vitx=(0.0d0,0.0d0)
        vity=(0.0d0,0.0d0)
        do 20 i = 1, nno
            vitx=vitx+dfdx(i)*zc(ipres+i-1)
            vity=vity+dfdy(i)*zc(ipres+i-1)
20      continue
!
        vitx=vitx*(0.d0,1.d0)/omerho
        vity=vity*(0.d0,1.d0)/omerho
!
        zr(idino+1)=0.5d0*dble(zc(ipres+ino-1)*dconjg(vitx))
        zr(idino+2)=0.5d0*dble(zc(ipres+ino-1)*dconjg(vity))
        zr(idino+3)=0.5d0*dimag(zc(ipres+ino-1)*dconjg(vitx))
        zr(idino+4)=0.5d0*dimag(zc(ipres+ino-1)*dconjg(vity))
30  end do
!
    call jedema()
end subroutine
