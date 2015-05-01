subroutine te0177(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rcvalc.h"
!
    character(len=16) :: option, nomte
! ......................................................................
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
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          OPTION : 'MASS_ACOU'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
    integer :: kp, i, j, k, ij, imattt, igeom, imate
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfde, jgano
    real(kind=8) :: poids, r
    integer :: icodre(1)
    complex(kind=8) :: valres(1)
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PMATTTC', 'E', imattt)
!
    call rcvalc(zi(imate), 'FLUIDE', 1, 'CELE_C', valres,&
                icodre, 1)
!
    do kp = 1, npg
        k = (kp-1)*nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids)
        if (lteatt('AXIS','OUI')) then
            r = 0.d0
            do i = 1, nno
                r = r + zr(igeom+2*(i-1))*zr(ivf+k+i-1)
            end do
            poids = poids*r
        endif
!
        ij = imattt - 1
        do i = 1, nno
            do j = 1, i
                ij = ij + 1
                zc(ij) = zc(ij)+poids*((1.0d0,0.0d0)/(valres(1)**2))*zr(ivf+k+i-1)*zr(ivf+k+j-1)
            end do
        end do
    end do
!
end subroutine
