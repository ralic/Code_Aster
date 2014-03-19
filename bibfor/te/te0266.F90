subroutine te0266(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
    character(len=16) :: option, nomte
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
!
!     BUT:
!       CALCUL DES FLUX DE TEMPERATURE AUX POINTS DE GAUSS
!       ELEMENTS 2D AXI
!       OPTION : 'FLUX_ELGA'
!
! ---------------------------------------------------------------------
!
!
!
    integer :: icodre(1)
    integer :: nno, kp, i, k, itempe, itemp, iflux, iharm, nh
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: npg, nnos, jgano, ndim, kpg, spt, j, nbcmp
!
    real(kind=8) :: valres(1), fluxr, fluxz, fluxt
    real(kind=8) :: dfdr(9), dfdz(9), poids, xh, r
!
    character(len=8) :: fami, poum
!
!-----------------------------------------------------------------------
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PHARMON', 'L', iharm)
    nh = zi(iharm)
    xh = dble(nh)
    call jevech('PMATERC', 'L', imate)
    call jevech('PTEMPSR', 'L', itemp)
    call jevech('PTEMPER', 'L', itempe)
    call jevech('PFLUXPG', 'E', iflux)
!
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    nbcmp=3
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'THER', 1, 'INST', [zr(itemp)],&
                1, 'LAMBDA', valres, icodre, 1)
!
    do 101 kp = 1, npg
        k = (kp-1)*nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids, dfdr, dfdz)
!
        r = 0.d0
        do 102 i = 1, nno
            r = r + zr(igeom+2*i-2) * zr(ivf+k+i-1)
102      continue
!
        fluxr = 0.0d0
        fluxz = 0.0d0
        fluxt = 0.0d0
        do 110 j = 1, nno
            fluxr = fluxr + zr(itempe+j-1)*dfdr(j)
            fluxz = fluxz + zr(itempe+j-1)*dfdz(j)
            fluxt = fluxt - zr(itempe+j-1)*zr(ivf+k+j-1)*xh/r
110      continue
!
        zr(iflux+(kp-1)*nbcmp-1+1) = -valres(1)*fluxr
        zr(iflux+(kp-1)*nbcmp-1+2) = -valres(1)*fluxz
        zr(iflux+(kp-1)*nbcmp-1+3) = -valres(1)*fluxt
!
101  end do
!
end subroutine
