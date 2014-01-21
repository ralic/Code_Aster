subroutine te0258(option, nomte)
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
!          CORRESPONDANT A UNE IMPEDANCE IMPOSEE
!          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 1D
!
!          OPTION : 'IMPE_MECA'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
#include "jeveux.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rcvalb.h"
#include "asterfort/vff2dn.h"
!
    integer :: icodre(1)
    character(len=8) :: fami, poum
    character(len=16) :: nomte, option
    real(kind=8) :: nx, ny, poids, rho(1), rho2
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: ndi, nno, kp, npg, imatuu, iimpe
    integer :: ldec, kpg, spt
    logical :: laxi
!
!
!-----------------------------------------------------------------------
    integer :: i, ii, ij, j, jgano, jj, ndim
    integer :: nnos
    real(kind=8) :: r
!-----------------------------------------------------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
    ndi = nno* (2*nno+1)
!
    laxi = .false.
    if (lteatt('AXIS','OUI')) laxi = .true.
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PIMPEDR', 'L', iimpe)
    call jevech('PMATUUR', 'E', imatuu)
!
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'FLUIDE', 0, ' ', [0.d0],&
                1, 'RHO', rho, icodre, 1)
!
! --- INITIALISATION DE LA MATRICE D'IMPEDANCE
    do 10 i = 1, ndi
        zr(imatuu+i-1) = 0.d0
10  end do
!
    if (zr(iimpe) .eq. 0.d0) then
        goto 60
    else
!
!        BOUCLE SUR LES POINTS DE GAUSS
        rho2 = -rho(1)*rho(1)
!
        do 50 kp = 1, npg
!
            ldec = (kp-1)*nno
!
            nx = 0.0d0
            ny = 0.0d0
!
            call vff2dn(ndim, nno, kp, ipoids, idfde,&
                        zr(igeom), nx, ny, poids)
!%
            if (laxi) then
                r = 0.d0
                do 20 i = 1, nno
                    r = r + zr(igeom+2* (i-1))*zr(ivf+ldec+i-1)
20              continue
                poids = poids*r
            endif
!%
            do 40 i = 1, nno
!
                do 30 j = 1, i
                    ii = 2*i
                    jj = 2*j
                    ij = (ii-1)*ii/2 + jj
!
                    zr(imatuu+ij-1) = zr(imatuu+ij-1) + poids*rho2/zr( iimpe+kp-1)* zr(ivf+ldec+i&
                                      &-1)*zr(ivf+ldec+j-1)
!
30              continue
40          continue
!
50      continue
    endif
60  continue
end subroutine
