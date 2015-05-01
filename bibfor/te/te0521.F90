subroutine te0521(option, nomte)
!
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/ntfcma.h"
#include "asterfort/rcfode.h"
!
    character(len=16) :: nomte, option
! ----------------------------------------------------------------------
!
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          OPTION : 'RIGI_THER_TRANS'
!                          ELEMENTS 3D ISO PARAMETRIQUES
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!
!
    real(kind=8) :: xkpt, alpha, tpg
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), poids
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: jgano, nno, kp, npg, i, j, l, ij, imattt, itemps, ifon(3)
    integer :: ndim, itemp, itempi, nnos
!
! DEB ------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PTEMPER', 'L', itemp)
    call jevech('PTEMPEI', 'L', itempi)
    call jevech('PMATTTR', 'E', imattt)
!
    call ntfcma(' ', zi(imate), ifon)
!
    do 40 kp = 1, npg
        l = (kp-1)*nno
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids, dfdx, dfdy, dfdz)
        tpg = 0.d0
        do 10 i = 1, nno
            tpg = tpg + zr(itempi+i-1)*zr(ivf+l+i-1)
10      continue
!
        call rcfode(ifon(2), tpg, alpha, xkpt)
!
        do 30 i = 1, nno
!
            do 20 j = 1, i
                ij = (i-1)*i/2 + j
                zr(imattt+ij-1) = zr(imattt+ij-1) + poids* (alpha* ( dfdx(i)*dfdx(j)+ dfdy(i)*dfd&
                                  &y(j)+dfdz(i)*dfdz(j)))
20          continue
30      continue
40  end do
! FIN ------------------------------------------------------------------
end subroutine
