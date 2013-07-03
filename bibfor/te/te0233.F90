subroutine te0233(option, nomte)
    implicit none
#include "jeveux.h"
!
#include "asterfort/dfdm1d.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
    character(len=16) :: option, nomte
! ......................................................................
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
!    - FONCTION REALISEE:  CALCUL DES TERMES ELEMENTAIRES EN MECANIQUE
!                          COQUE 1D
!                          OPTION : 'CHAR_MECA_PESA_R'
!                          ELEMENT: MECXSE3,METCSE3,METDSE3
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    character(len=8) :: elrefe, fami, poum
    integer :: icodre, kpg, spt
    real(kind=8) :: dfdx(3), nx, ny, poids, cour, rx
    integer :: nno, kp, k, npg, i, ivectu, ipesa, icaco
    integer :: ipoids, ivf, idfdk, igeom, imate
!
!
!-----------------------------------------------------------------------
    integer :: jgano, ndim, nnos
    real(kind=8) :: r8b, rho
!-----------------------------------------------------------------------
    call elref1(elrefe)
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfdk, jgano)
!
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PCACOQU', 'L', icaco)
    call jevech('PPESANR', 'L', ipesa)
    call jevech('PVECTUR', 'E', ivectu)
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'ELAS', 0, ' ', r8b,&
                1, 'RHO', rho, icodre, 1)
!
    do 40 kp = 1, npg
        k = (kp-1)*nno
        call dfdm1d(nno, zr(ipoids+kp-1), zr(idfdk+k), zr(igeom), dfdx,&
                    cour, poids, nx, ny)
        poids = poids*rho*zr(ipesa)*zr(icaco)
        if (nomte .eq. 'MECXSE3') then
            rx = 0.d0
            do 10 i = 1, nno
                rx = rx + zr(igeom+2*i-2)*zr(ivf+k+i-1)
10          continue
            poids = poids*rx
            do 20 i = 1, nno
                zr(ivectu+3*i-2) = zr(ivectu+3*i-2) + poids*zr(ipesa+ 2)*zr(ivf+k+i-1)
20          continue
        else
            do 30 i = 1, nno
                zr(ivectu+3*i-3) = zr(ivectu+3*i-3) + poids*zr(ipesa+ 1)*zr(ivf+k+i-1)
                zr(ivectu+3*i-2) = zr(ivectu+3*i-2) + poids*zr(ipesa+ 2)*zr(ivf+k+i-1)
30          continue
        endif
40  end do
end subroutine
