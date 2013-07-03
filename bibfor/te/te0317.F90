subroutine te0317(option, nomte)
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
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES DE FLUX FLUIDE EN MECANIQUE
!          ELEMENTS ISOPARAMETRIQUES 1D
!
!          OPTION : 'FLUX_FLUI_Y '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
#include "jeveux.h"
!
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/vff2dn.h"
    character(len=16) :: nomte, option
    real(kind=8) :: poids, nx, ny, norm(2)
    integer :: ipoids, ivf, idfde, igeom
    integer :: ndi, nno, kp, npg
    integer :: ldec
    logical :: laxi
!
!
!-----------------------------------------------------------------------
    integer :: i, ij, imattt, j, jgano, ndim, nnos
!
    real(kind=8) :: r
!-----------------------------------------------------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
    ndi = nno* (nno+1)/2
    laxi = .false.
    if (lteatt(' ','AXIS','OUI')) laxi = .true.
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATTTR', 'E', imattt)
    do 10 i = 1, ndi
        zr(imattt+i-1) = 0.0d0
10  end do
!
!     BOUCLE SUR LES POINTS DE GAUSS
!
    do 50 kp = 1, npg
        ldec = (kp-1)*nno
        nx = 0.0d0
        ny = 0.0d0
! ON CALCULE L ACCEL AU POINT DE GAUSS
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom), nx, ny, poids)
        norm(1) = nx
        norm(2) = ny
!
! CAS AXISYMETRIQUE
!
        if (laxi) then
            r = 0.d0
            do 20 i = 1, nno
                r = r + zr(igeom+2* (i-1))*zr(ivf+ldec+i-1)
20          continue
            poids = poids*r
        endif
!
!CDIR$ IVDEP
        do 40 i = 1, nno
            do 30 j = 1, i
                ij = (i-1)*i/2 + j
                zr(imattt+ij-1) = zr(imattt+ij-1) + poids*norm(2)*zr( ivf+ldec+i-1)* zr(ivf+ldec+&
                                  &j-1)
30          continue
40      continue
50  end do
end subroutine
