subroutine te0176(option, nomte)
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
!     BUT: CALCUL DES MATRICES DE RIGIDITE ELEMENTAIRES EN ACOUSTIQUE
!          ELEMENTS ISOPARAMETRIQUES 2D
!
!          OPTION : 'RIGI_ACOU '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    implicit none
#include "jeveux.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
!
    character(len=16) :: nomte, option
    real(kind=8) :: dfdx(9), dfdy(9), poids
    integer :: ipoids, ivf, idfde, igeom, ij
    integer :: nno, kp, npg1, i, j, imattt, ndi
    integer :: ndim, nnos, jgano
!
!
!
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    ndi = nno*(nno+1)/2
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATTTC', 'E', imattt)
!
    do 111 i = 1, ndi
        zc(imattt+i-1) =(0.0d0 ,0.0d0 )
111  end do
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 101 kp = 1, npg1
!
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids, dfdx, dfdy)
!
        ij=imattt - 1
        do 106 i = 1, nno
            do 107 j = 1, i
                ij = ij + 1
                zc(ij) = zc(ij) + poids * (dfdx(i) * dfdx(j) + dfdy(i) * dfdy(j) )
107          continue
106      continue
!
101  end do
!
end subroutine
