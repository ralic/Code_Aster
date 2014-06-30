subroutine te0178(option, nomte)
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
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
    implicit none
!                          D'AMORTISSEMENT ACOUSTIQUE SUR DES ARETES
!                          D'ELEMENTS 2D
!                          OPTION : 'AMOR_ACOU'
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
#include "jeveux.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rcvalb.h"
#include "asterfort/vff2dn.h"
!
    complex(kind=8) :: rhosz
    character(len=8) :: fami, poum
    character(len=16) :: option, nomte
    integer :: icodre(1)
    real(kind=8) :: poids, r, nx, ny, rho(1)
    integer :: nno, kp, npg, ipoids, ivf, idfde, igeom
    integer :: imattt, i, j, ij, l, li, lj
    integer :: imate, iimpe, kpg, spt
    logical(kind=1) :: laxi
!
!
!-----------------------------------------------------------------------
    integer :: jgano, mater, ndim, nnos
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    laxi = .false.
    if (lteatt('AXIS','OUI')) laxi = .true.
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PIMPEDC', 'L', iimpe)
    call jevech('PMATERC', 'L', imate)
    call jevech('PMATTTC', 'E', imattt)
!
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    mater = zi(imate)
    call rcvalb(fami, kpg, spt, poum, mater,&
                ' ', 'FLUIDE', 0, ' ', [0.d0],&
                1, 'RHO', rho, icodre, 1)
!
    if (zc(iimpe) .ne. (0.d0,0.d0)) then
        rhosz = rho(1)/zc(iimpe)
    else
        goto 50
    endif
!
    do 40 kp = 1, npg
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom), nx, ny, poids)
        if (laxi) then
            r = 0.d0
            do 10 i = 1, nno
                l = (kp-1)*nno + i
                r = r + zr(igeom+2*i-2)*zr(ivf+l-1)
10          continue
            poids = poids*r
        endif
        ij = imattt - 1
        do 30 i = 1, nno
            li = ivf + (kp-1)*nno + i - 1
            do 20 j = 1, i
                lj = ivf + (kp-1)*nno + j - 1
                ij = ij + 1
                zc(ij) = zc(ij) + poids*rhosz*zr(li)*zr(lj)
20          continue
30      continue
40  end do
50  continue
end subroutine
