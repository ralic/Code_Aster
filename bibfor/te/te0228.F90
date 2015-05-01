subroutine te0228(option, nomte)
    implicit   none
#include "jeveux.h"
#include "asterfort/dfdm1d.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/ppgan2.h"
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
! .  - FONCTION REALISEE:  CALCUL  DEFORMATIONS GENERALISEES AUX NOEUDS
! .                        COQUE 1D
! .
! .                        OPTIONS : 'DEGE_ELNO  '
! .                        ELEMENT: MECXSE3,METCSE3,METDSE3
! .
! .  - ARGUMENTS:
! .      DONNEES:      OPTION       -->  OPTION DE CALCUL
! .                    NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: i, k, kp, igeom, idepl, idefor, nno, nnos, jgano, ndim, npg
    integer :: ivf, idfdk, ipoids, idefpg
    character(len=8) :: elrefe
    real(kind=8) :: dfdx(3), degepg(24)
    real(kind=8) :: cosa, sina, cour, r, zero, jac
    real(kind=8) :: eps(5), e11, e22, k11, k22
!
!
    call elref1(elrefe)
    zero = 0.0d0
!
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfdk,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PDEPLAR', 'L', idepl)
!
    if (option .eq. 'DEGE_ELNO') then
        call jevech('PDEFOGR', 'E', idefor)
    endif
!
    if (option .eq. 'DEGE_ELGA') then
        call jevech('PDEFOPG', 'E', idefpg)
    endif
!
    do 30 kp = 1, npg
        k = (kp-1)*nno
        call dfdm1d(nno, zr(ipoids+kp-1), zr(idfdk+k), zr(igeom), dfdx,&
                    cour, jac, cosa, sina)
        do 10 i = 1, 5
            eps(i) = zero
10      continue
        r = zero
        do 20 i = 1, nno
            eps(1) = eps(1) + dfdx(i)*zr(idepl+3*i-3)
            eps(2) = eps(2) + dfdx(i)*zr(idepl+3*i-2)
            eps(3) = eps(3) + dfdx(i)*zr(idepl+3*i-1)
            eps(4) = eps(4) + zr(ivf+k+i-1)*zr(idepl+3*i-3)
            eps(5) = eps(5) + zr(ivf+k+i-1)*zr(idepl+3*i-1)
            r = r + zr(ivf+k+i-1)*zr(igeom+2* (i-1))
20      continue
        e11 = eps(2)*cosa - eps(1)*sina
        k11 = eps(3)
        if (nomte .eq. 'MECXSE3') then
            e22 = eps(4)/r
            k22 = -eps(5)*sina/r
        else
            e22 = zero
            k22 = zero
        endif
!
        degepg(6* (kp-1)+1) = e11
        degepg(6* (kp-1)+2) = e22
        degepg(6* (kp-1)+3) = zero
        degepg(6* (kp-1)+4) = k11
        degepg(6* (kp-1)+5) = k22
        degepg(6* (kp-1)+6) = zero
        if (option .eq. 'DEGE_ELGA') then
            zr(idefpg-1+6*(kp-1)+1) = e11
            zr(idefpg-1+6*(kp-1)+2) = e22
            zr(idefpg-1+6*(kp-1)+3) = zero
            zr(idefpg-1+6*(kp-1)+4) = k11
            zr(idefpg-1+6*(kp-1)+5) = k22
            zr(idefpg-1+6*(kp-1)+6) = zero
        endif
30  end do
!
!
!     -- PASSAGE GAUSS -> NOEUDS :
    if (option .eq. 'DEGE_ELNO') then
        call ppgan2(jgano, 1, 6, degepg, zr(idefor))
    endif
end subroutine
