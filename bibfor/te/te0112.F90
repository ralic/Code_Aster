subroutine te0112(option, nomte)
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
    implicit none
#include "jeveux.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - BUT :  CALCUL DES MATRICES DE RAIDEUR GEOMETRIQUE ELEMENTAIRES
!                          POUR LES ELEMENTS DE FOURIER
!                          OPTION : 'RIGI_MECA_GE    '
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    real(kind=8) :: a(3, 3, 9, 9)
    real(kind=8) :: dfdr(9), dfdz(9), dfdt(9), poids, r, xh
    integer :: nno, kp, npg1, imatuu, icontr, iharmo, ndim, nnos
    integer :: ipoids, ivf, idfde, igeom, jgano
!
!
!-----------------------------------------------------------------------
    integer :: i, ic, ijkl, ik, j, k, l
    integer :: nh
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCONTRR', 'L', icontr)
    call jevech('PHARMON', 'L', iharmo)
    nh = zi(iharmo)
    xh = dble(nh)
    call jevech('PMATUUR', 'E', imatuu)
!
    do 113 k = 1, 3
        do 113 l = 1, 3
            do 113 i = 1, nno
                do 113 j = 1, i
                    a(k,l,i,j) = 0.d0
113              continue
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 101 kp = 1, npg1
!
        k=(kp-1)*nno
        ic = icontr + (kp-1)*6
!
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids, dfdr, dfdz)
!
        r = 0.d0
        do 102 i = 1, nno
            r = r + zr(igeom+2*(i-1))*zr(ivf+k+i-1)
102      continue
        poids = poids*r
!
        do 103 i = 1, nno
            dfdr(i) = dfdr(i) + zr(ivf+k+i-1)/r
            dfdt(i) = - xh * zr(ivf+k+i-1)/r
103      continue
!
        do 106 i = 1, nno
            do 107 j = 1, i
!
                a(1,1,i,j) = a(1,1,i,j) + poids * ( zr(ic) * dfdr(i) * dfdr(j) + zr(ic+1) * dfdz(&
                             &i) * dfdz(j) + zr(ic+2) * dfdt(i) * dfdt(j) + zr(ic+3) * (dfdr(i) *&
                             & dfdz(j) + dfdz(i) * dfdr(j)) + zr(ic+4) * (dfdt(i) * dfdr(j) + dfd&
                             &r(i) * dfdt(j)) + zr(ic+5) * (dfdz(i) * dfdt(j) + dfdt(i) * dfdz(j)&
                             &))
!
107          continue
106      continue
!
101  end do
!
    do 108 i = 1, nno
        do 109 j = 1, i
            a(2,2,i,j) = a(1,1,i,j)
            a(3,3,i,j) = a(1,1,i,j)
109      continue
108  continue
!
! PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
!
    do 111 k = 1, 3
        do 111 l = 1, 3
            do 111 i = 1, nno
                ik = ((3*i+k-4) * (3*i+k-3)) / 2
                do 111 j = 1, i
                    ijkl = ik + 3 * (j-1) + l
                    zr(imatuu+ijkl-1) = a(k,l,i,j)
111              continue
!
end subroutine
