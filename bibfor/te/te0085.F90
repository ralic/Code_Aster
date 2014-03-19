subroutine te0085(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "jeveux.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
!
    character(len=16) :: option, nomte, phenom
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES TERMES ELEMENTAIRES EN MECANIQUE
!                          OPTION : 'CHAR_MECA_PESA_R'
!                          2D PLAN ET AXISYMETRIQUE
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: icodre(1)
    real(kind=8) :: poids, rx
    integer :: nno, kp, k, npg, i, ivectu, ipesa
    integer :: ipoids, ivf, idfde, igeom, imate
!
!
!-----------------------------------------------------------------------
    integer :: jgano, ndim, nnos
    real(kind=8) :: rho(1)
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PPESANR', 'L', ipesa)
    call jevech('PVECTUR', 'E', ivectu)
!
    call rccoma(zi(imate), 'ELAS', 1, phenom, icodre(1))
    call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                ' ', phenom, 0, ' ', [0.d0],&
                1, 'RHO', rho, icodre(1), 1)
!
    do kp = 1, npg
        k = nno*(kp-1)
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids)
        poids = poids * rho(1) * zr(ipesa)
        if (lteatt('AXIS','OUI')) then
            rx= 0.d0
            do i = 1, nno
                rx= rx+ zr(igeom+2*i-2)*zr(ivf+k+i-1)
            end do
            poids = poids*rx
            do i = 1, nno
                zr(ivectu+2*i-1) = zr(ivectu+2*i-1) + poids*zr(ipesa+ 2)*zr(ivf+k+i-1)
            end do
        else
            do i = 1, nno
                zr(ivectu+2*i-2) = zr(ivectu+2*i-2) + poids*zr(ipesa+ 1)*zr(ivf+k+i-1)
                zr(ivectu+2*i-1) = zr(ivectu+2*i-1) + poids*zr(ipesa+ 2)*zr(ivf+k+i-1)
            end do
        endif
    end do
end subroutine
