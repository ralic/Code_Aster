subroutine te0093(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/tefrep.h"
    character(len=16) :: option, nomte
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
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_MECA_FR2D2D  '
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: ndim, nno, nnos, npg, i, k, kp, ii, iforc, ivectu
    integer :: ipoids, ivf, idfde, igeom, jgano
    real(kind=8) :: poids, r, fx, fy
!     ------------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PVECTUR', 'E', ivectu)
    call tefrep(option, nomte, 'PFR2D2D', iforc)
!
    do kp = 1, npg
        k=(kp-1)*nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids)
!
!      --- CALCUL DE LA FORCE AUX PG (A PARTIR DES NOEUDS) ---
        fx = 0.d0
        fy = 0.d0
        do i = 1, nno
            ii = 2 * (i-1)
            fx = fx + zr(ivf+k+i-1) * zr(iforc+ii )
            fy = fy + zr(ivf+k+i-1) * zr(iforc+ii+1)
        end do
!
        if (lteatt('AXIS','OUI')) then
            r = 0.d0
            do i = 1, nno
                r = r + zr(igeom+2*(i-1))*zr(ivf+k+i-1)
            end do
            poids = poids*r
        endif
        do i = 1, nno
            zr(ivectu+2*i-2) = zr(ivectu+2*i-2) + poids * fx * zr(ivf+ k+i-1)
            zr(ivectu+2*i-1) = zr(ivectu+2*i-1) + poids * fy * zr(ivf+ k+i-1)
        end do
    end do
!
end subroutine
