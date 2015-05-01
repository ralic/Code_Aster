subroutine te0180(option, nomte)
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
!     BUT: CALCUL DES MATRICES DE RIGIDITE ELEMENTAIRES EN ACOUSTIQUE
!          ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'RIGI_ACOU '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
!
    character(len=16) :: nomte, option
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), poids
    integer :: ipoids, ivf, idfde, igeom
    integer :: jgano, nno, kp, npg1, i, j, imattt, ndi
!
!
!
!
!-----------------------------------------------------------------------
    integer :: ij, ndim, nnos
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
    ndi = nno* (nno+1)/2
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATTTC', 'E', imattt)
!
    do 20 i = 1, ndi
        zc(imattt+i-1) = (0.0d0,0.0d0)
20  end do
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 50 kp = 1, npg1
!
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids, dfdx, dfdy, dfdz)
!
        do 40 i = 1, nno
            do 30 j = 1, i
                ij = (i-1)*i/2 + j
                zc(imattt+ij-1) = zc(imattt+ij-1) + poids* (dfdx(i)* dfdx(j)+dfdy(i)*dfdy(j)+ dfd&
                                  &z(i)*dfdz(j))
30          continue
40      continue
!
50  end do
!
end subroutine
