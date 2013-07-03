subroutine te0092(option, nomte)
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
    implicit none
#include "jeveux.h"
!
#include "asterfort/dfdm2d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          OPTION : 'RIGI_MECA_GEOM  '
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    real(kind=8) :: dfdx(9), dfdy(9), poids, r, zero, un, axis
    real(kind=8) :: sxx, sxy, syy
    integer :: nno, kp, k, npg, ii, jj, i, j, imatuu, kd1, kd2, ij1, ij2
    integer :: ipoids, ivf, idfde, igeom, icontr, kc
!
!
!-----------------------------------------------------------------------
    integer :: jgano, ndim, nnos
!-----------------------------------------------------------------------
    zero=0.d0
    un  =1.d0
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCONTRR', 'L', icontr)
    call jevech('PMATUUR', 'E', imatuu)
!
    axis=zero
    r   =un
    if (lteatt(' ','AXIS','OUI')) axis=un
!
    do 101 kp = 1, npg
        k=(kp-1)*nno
        kc=icontr+4*(kp-1)
        sxx=zr(kc )
        syy=zr(kc+1)
        sxy=zr(kc+3)
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdx, dfdy, poids)
        if (axis .gt. 0.5d0) then
            r = zero
            do 102 i = 1, nno
                r = r + zr(igeom+2*(i-1))*zr(ivf+k+i-1)
102          continue
            do 103 i = 1, nno
                dfdx(i)=dfdx(i)+zr(ivf+k+i-1)/r
103          continue
            poids=poids*r
        endif
!
        kd1=2
        kd2=1
        do 106 i = 1, 2*nno, 2
            kd1=kd1+2*i-3
            kd2=kd2+2*i-1
            ii = (i+1)/2
            do 107 j = 1, i, 2
                jj = (j+1)/2
                ij1=imatuu+kd1+j-2
                ij2=imatuu+kd2+j-1
                zr(ij2) = zr(ij2) +poids*( dfdx(ii)*(dfdx(jj)*sxx+ dfdy(jj)*sxy)+ dfdy(ii)*(dfdx(&
                          &jj)*sxy+dfdy(jj)*syy))
                zr(ij1) = zr(ij2)
107          continue
106      continue
!
101  end do
end subroutine
