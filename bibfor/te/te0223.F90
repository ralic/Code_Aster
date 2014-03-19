subroutine te0223(option, nomte)
    implicit none
#include "jeveux.h"
!
#include "asterfort/dfdm1d.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
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
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          COQUE 1D
!                          OPTION : 'CHAR_MECA_FRCO2D  '
!                          ELEMENT: MECXSE3,METCSE3,METDSE3
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    character(len=8) :: elrefe
    real(kind=8) :: poids, r, fx, fy, mz, f1, f3, m2, nx, ny, cour, dfdx(3)
    integer :: nno, nddl, kp, npg, ipoids, ivf, idfdk, igeom
    integer :: ivectu, k, i, l, iforc
    logical :: global
!
!
!-----------------------------------------------------------------------
    integer :: jgano, ndim, nnos
!-----------------------------------------------------------------------
    call elref1(elrefe)
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfdk,jgano=jgano)
!
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PFRCO2D', 'L', iforc)
    call jevech('PVECTUR', 'E', ivectu)
    nddl = 3
    global = abs(zr(iforc+5)) .lt. 1.d-3
!
    do 30 kp = 1, npg
        k = (kp-1)*nno
        call dfdm1d(nno, zr(ipoids+kp-1), zr(idfdk+k), zr(igeom), dfdx,&
                    cour, poids, nx, ny)
        r = 0.d0
        fx = 0.d0
        fy = 0.d0
        mz = 0.d0
        do 10 i = 1, nno
            l = (kp-1)*nno + i
            if (global) then
                fx = fx + zr(iforc-1+6* (i-1)+1)*zr(ivf+l-1)
                fy = fy + zr(iforc-1+6* (i-1)+2)*zr(ivf+l-1)
                mz = mz + zr(iforc-1+6* (i-1)+5)*zr(ivf+l-1)
            else
                f1 = zr(iforc+6* (i-1))
!-----------------------------------------------------
!  LE SIGNE AFFECTE A F3 A ETE CHANGE PAR AFFE_CHAR_MECA SI PRES
!  POUR RESPECTER LA CONVENTION
!      UNE PRESSION POSITIVE PROVOQUE UN GONFLEMENT
!-----------------------------------------------------
                f3 = zr(iforc+6* (i-1)+2)
                m2 = zr(iforc+6* (i-1)+3)
                fx = fx + (nx*f3-ny*f1)*zr(ivf+l-1)
                fy = fy + (ny*f3+nx*f1)*zr(ivf+l-1)
                mz = mz + m2*zr(ivf+l-1)
            endif
            r = r + zr(igeom+2* (i-1))*zr(ivf+l-1)
10      continue
        if (nomte .eq. 'MECXSE3') then
            poids = poids*r
        endif
        do 20 i = 1, nno
            l = (kp-1)*nno + i
            zr(ivectu+nddl* (i-1)) = zr(ivectu+nddl* (i-1)) + fx*zr( ivf+l-1 )*poids
            zr(ivectu+nddl* (i-1)+1) = zr(ivectu+nddl* (i-1)+1) + fy*zr(ivf+l-1 )*poids
            zr(ivectu+nddl* (i-1)+2) = zr(ivectu+nddl* (i-1)+2) + mz*zr(ivf+l-1 )*poids
20      continue
30  end do
end subroutine
