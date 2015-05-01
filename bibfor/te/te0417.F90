subroutine te0417(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/dxroep.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/utmess.h"
#include "asterfort/vectan.h"
#include "asterfort/vectci.h"
!
    character(len=*) :: option, nomte
!     ------------------------------------------------------------------
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
!     OPTION : 'MASS_INER'              (ELEMENTS COQUE_3D)
!     ------------------------------------------------------------------
!
    real(kind=8) :: xi(3, 9), xg(3), ix2(3), ix1x2, ix1x3, ix2x3, matine(6)
    real(kind=8) :: vecta(9, 2, 3), vectn(9, 3), vectpt(9, 2, 3)
!
!     RECUPERATION DES OBJETS
!
!-----------------------------------------------------------------------
    integer :: i, intsn, intsx, j, jgeom, k, l1
    integer :: l2, lcastr, lzi, lzr, nb1, nb2, npgsn
!
    real(kind=8) :: epais, epais2, epais3, rho, rnormc, volume, wgt
    real(kind=8) :: xx, xy, xz, yy, yz, zz
!-----------------------------------------------------------------------
    call jevete('&INEL.'//nomte(1:8)//'.DESI', ' ', lzi)
    nb1  =zi(lzi-1+1)
    nb2  =zi(lzi-1+2)
    npgsn=zi(lzi-1+4)
    call jevete('&INEL.'//nomte(1:8)//'.DESR', ' ', lzr)
!
    call jevech('PGEOMER', 'L', jgeom)
!
    do 5 i = 1, nb2
        xi(1,i)=zr(jgeom+3*(i-1))
        xi(2,i)=zr(jgeom+3*(i-1)+1)
        xi(3,i)=zr(jgeom+3*(i-1)+2)
 5  end do
!
    call dxroep(rho, epais)
    if (rho .le. r8prem()) then
        call utmess('F', 'ELEMENTS5_45')
    endif
    epais2=epais*epais
    epais3=epais*epais2
!
    call jevech('PMASSINE', 'E', lcastr)
!
    call vectan(nb1, nb2, xi, zr(lzr), vecta,&
                vectn, vectpt)
!
    volume=0.d0
!
    do 10 k = 1, 3
        xg(k) =0.d0
        ix2(k)=0.d0
10  end do
    ix1x2 =0.d0
    ix1x3 =0.d0
    ix2x3 =0.d0
!
    do 200 intsn = 1, npgsn
!
!     RNORMC EST LE DETERMINANT DE LA SURFACE MOYENNE
!
        call vectci(intsn, nb1, xi, zr(lzr), rnormc)
!
!     WGT= ZR(9-1+INTE) * ZR(LZR+126-1+INTSN)
!        =    1.D0      * ZR(LZR+126-1+INTSN)
        wgt= zr(lzr+126-1+intsn)
!
        volume=volume+epais*wgt*rnormc
!
!     CENTRE DE GRAVITE
!
        l1=lzr-1+135
        intsx=8*(intsn-1)
        l2=l1+intsx
!
        wgt=wgt*rnormc
!
        do 20 j = 1, nb1
            do 25 k = 1, 3
                xg(k)=xg(k)+epais*wgt*zr(l2+j)*xi(k,j)
25          continue
!
!     MOMENTS ET PRODUITS D'INERTIE
!
            do 30 i = 1, nb1
                do 35 k = 1, 3
                    ix2(k)=ix2(k)+epais*wgt*zr(l2+j)*xi(k,j)*zr(l2+i)*&
                    xi(k,i) +epais3/12.d0*wgt*zr(l2+j)*vectn(j,k)*zr(&
                    l2+i)*vectn(i,k)
35              continue
!
                ix1x2=ix1x2+epais*wgt*zr(l2+j)*xi(1,j)*zr(l2+i)*xi(2,&
                i) +epais3/12.d0*wgt*zr(l2+j)*vectn(j,1)*zr(l2+i)*&
                vectn(i,2)
                ix1x3=ix1x3+epais*wgt*zr(l2+j)*xi(1,j)*zr(l2+i)*xi(3,&
                i) +epais3/12.d0*wgt*zr(l2+j)*vectn(j,1)*zr(l2+i)*&
                vectn(i,3)
                ix2x3=ix2x3+epais*wgt*zr(l2+j)*xi(2,j)*zr(l2+i)*xi(3,&
                i) +epais3/12.d0*wgt*zr(l2+j)*vectn(j,2)*zr(l2+i)*&
                vectn(i,3)
30          end do
!
20      end do
!
200  end do
!
    matine(1)=rho*(ix2(2)+ix2(3))
    matine(2)=rho*ix1x2
    matine(3)=rho*(ix2(1)+ix2(3))
    matine(4)=rho*ix1x3
    matine(5)=rho*ix2x3
    matine(6)=rho*(ix2(1)+ix2(2))
!
    zr(lcastr)  =rho*volume
    zr(lcastr+1)=xg(1)/volume
    zr(lcastr+2)=xg(2)/volume
    zr(lcastr+3)=xg(3)/volume
!
    xx=zr(lcastr+1)*zr(lcastr+1)
    yy=zr(lcastr+2)*zr(lcastr+2)
    zz=zr(lcastr+3)*zr(lcastr+3)
    xy=zr(lcastr+1)*zr(lcastr+2)
    xz=zr(lcastr+1)*zr(lcastr+3)
    yz=zr(lcastr+2)*zr(lcastr+3)
!
    zr(lcastr+4)=matine(1)-zr(lcastr)*(yy+zz)
    zr(lcastr+5)=matine(3)-zr(lcastr)*(xx+zz)
    zr(lcastr+6)=matine(6)-zr(lcastr)*(xx+yy)
    zr(lcastr+7)=matine(2)-zr(lcastr)*xy
    zr(lcastr+8)=matine(4)-zr(lcastr)*xz
    zr(lcastr+9)=matine(5)-zr(lcastr)*yz
!
end subroutine
