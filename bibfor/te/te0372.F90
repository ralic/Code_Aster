subroutine te0372(option, nomte)
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
    implicit none
!
!     BUT: CALCUL DES MATRICES ELEMENTAIRES EN MECANIQUE
!          CORRESPONDANT A UN TERME D'AMORTISSEMENT EN ONDE INCIDENTE
!           IMPOSEE SUR DES FACES 1D D'ELEMENTS ISOPARAMETRIQUES 2D
!
!!          OPTION : 'ONDE_FLUI'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
#include "jeveux.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rcvalb.h"
#include "asterfort/vff2dn.h"
!
    integer :: icodre(2), kpg, spt
    character(len=8) :: nomres(2), fami, poum
    character(len=16) :: nomte, option
    real(kind=8) :: nx, ny, poids
    real(kind=8) :: valres(2), rho, celer
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: ndi, nno, kp, npg, imatuu
    integer :: ldec, ionde
    logical :: laxi
!
!
!-----------------------------------------------------------------------
    integer :: i, ii, ij, j, jgano, jj, ndim
    integer :: nnos
    real(kind=8) :: r
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
    ndi = nno* (2*nno+1)
    laxi = .false.
    if (lteatt('AXIS','OUI')) laxi = .true.
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PONDECR', 'L', ionde)
    call jevech('PMATUUR', 'E', imatuu)
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    nomres(1) = 'RHO'
    nomres(2) = 'CELE_R'
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'FLUIDE', 0, ' ', [0.d0],&
                2, nomres, valres, icodre, 1)
    rho = valres(1)
    celer = valres(2)
!
    do 10 i = 1, ndi
        zr(imatuu+i-1) = 0.d0
10  end do
!
    if (zr(ionde) .eq. 0.d0) goto 60
!
    do 50 kp = 1, npg
        ldec = (kp-1)*nno
!
        nx = 0.0d0
        ny = 0.0d0
!
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom), nx, ny, poids)
!
        if (laxi) then
            r = 0.d0
            do 20 i = 1, nno
                r = r + zr(igeom+2* (i-1))*zr(ivf+ldec+i-1)
20          continue
            poids = poids*r
        endif
!
        do 40 i = 1, nno
            do 30 j = 1, i
                ii = 2*i
                jj = 2*j
                ij = (ii-1)*ii/2 + jj
                zr(imatuu+ij-1) = zr(imatuu+ij-1) - poids*zr(ivf+ldec+ i-1)*zr(ivf+ldec+j-1)* rho&
                                  &/celer
30          continue
40      continue
50  end do
60  continue
!
end subroutine
