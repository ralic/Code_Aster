subroutine te0192(option, nomte)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/vff2dn.h"
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
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          ELEMENTS 2D AXISYMETRIQUES FOURIER
!                          OPTION : 'CHAR_MECA_PRES_R'
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
    integer :: nno, nnos, jgano, ndim, kp, npg, ipoids, ivf, idfde, igeom
    integer :: ipres, ivectu, k, i
    real(kind=8) :: poids, r, tx, ty, nx, ny, pres
!     ------------------------------------------------------------------
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PPRESSR', 'L', ipres)
    call jevech('PVECTUR', 'E', ivectu)
!
    do 30 kp = 1, npg
        k = (kp-1)*nno
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom), nx, ny, poids)
        r = 0.d0
        pres = 0.d0
        do 10 i = 1, nno
            r = r + zr(igeom+2* (i-1))*zr(ivf+k+i-1)
            pres = pres + zr(ipres+i-1)*zr(ivf+k+i-1)
10      continue
!
        poids = poids*r
        tx = -nx*pres
        ty = -ny*pres
!
        do 20 i = 1, nno
            zr(ivectu+3*i-3) = zr(ivectu+3*i-3) + tx*zr(ivf+k+i-1)* poids
            zr(ivectu+3*i-2) = zr(ivectu+3*i-2) + ty*zr(ivf+k+i-1)* poids
            zr(ivectu+3*i-1) = 0.d0
20      continue
30  end do
!
end subroutine
