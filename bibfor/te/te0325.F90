subroutine te0325(option, nomte)
    implicit none
!
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
!
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES DE FLUX FLUIDE EN MECANIQUE
!          ELEMENTS ISOPARAMETRIQUES 2D
!
!          OPTION : 'CHAR_THER_ACCE_R 'OU 'CHAR_THER_ACCE_X'
!                    OU 'CHAR_THER_ACCE_Y'OU 'CHAR_THER_ACCE_Z'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
#include "jeveux.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
!
    integer :: icodre(1), kpg, spt
    character(len=8) :: fami, poum
    character(len=16) :: nomte, option
    real(kind=8) :: jac, nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9)
    real(kind=8) :: norm(3)
    real(kind=8) :: acloc(3, 9), acc(3, 9), flufn(9)
    integer :: ipoids, ivf, idfdx, idfdy, igeom
    integer :: ndim, nno, ipg, npg1, ivectt, imate
    integer :: idec, jdec, kdec, ldec, nnos, jgano
!
!
!-----------------------------------------------------------------------
    integer :: i, iacce, idim, ino, itemp, j, jno
    integer :: k, mater
    real(kind=8) :: rho(1)
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfdx,jgano=jgano)
    idfdy = idfdx + 1
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PVECTTR', 'E', ivectt)
!
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    mater = zi(imate)
    call rcvalb(fami, kpg, spt, poum, mater,&
                ' ', 'THER', 0, ' ', [0.d0],&
                1, 'RHO_CP', rho, icodre, 1)
!
    if (option(16:16) .eq. 'R') then
        call jevech('PACCELR', 'L', iacce)
        elseif ( option(16:16).eq.'X' .or. option(16:16).eq.'Y' .or.&
    option(16:16).eq.'Z' ) then
        call jevech('PTEMPER', 'L', itemp)
    endif
!
! ON RECUPERE LE CHAMNO DE DEPL (MODAL)
!
    k = 0
    do 10 i = 1, nno
        if (option(16:16) .eq. 'R') then
            do 20 idim = 1, 3
                k=k+1
                acloc(idim,i) = zr(iacce+k-1)
20          continue
        else if (option(16:16) .eq. 'X') then
            k=k+1
            acloc(1,i) = zr(itemp+k-1)
            acloc(2,i) = 0.d0
            acloc(3,i) = 0.d0
        else if (option(16:16).eq.'Y') then
            k=k+1
            acloc(1,i) = 0.d0
            acloc(2,i) = zr(itemp+k-1)
            acloc(3,i) = 0.d0
        else if (option(16:16).eq.'Z') then
            k=k+1
            acloc(1,i) = 0.d0
            acloc(2,i) = 0.d0
            acloc(3,i) = zr(itemp+k-1)
        endif
10  end do
!
    do 11 i = 1, nno
        zr(ivectt+i-1) = 0.d0
11  end do
!
!     CALCUL DES PRODUITS VECTORIELS OMI X OMJ
!
    do 21 ino = 1, nno
        i = igeom + 3*(ino-1) -1
        do 22 jno = 1, nno
            j = igeom + 3*(jno-1) -1
            sx(ino,jno) = zr(i+2) * zr(j+3) - zr(i+3) * zr(j+2)
            sy(ino,jno) = zr(i+3) * zr(j+1) - zr(i+1) * zr(j+3)
            sz(ino,jno) = zr(i+1) * zr(j+2) - zr(i+2) * zr(j+1)
22      continue
21  end do
!
!     BOUCLE SUR LES POINTS DE GAUSS
!
    do 101 ipg = 1, npg1
        kdec=(ipg-1)*nno*ndim
        ldec=(ipg-1)*nno
!
        nx = 0.0d0
        ny = 0.0d0
        nz = 0.0d0
!        --- ON CALCULE L ACCEL AU POINT DE GAUSS
        do 102 i = 1, nno
            idec = (i-1)*ndim
            do 104 j = 1, nno
                jdec = (j-1)*ndim
!
                nx = nx + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sx(i,j)
                ny = ny + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sy(i,j)
                nz = nz + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sz(i,j)
!
104          continue
102      continue
!
        acc(1,ipg)=0.0d0
        acc(2,ipg)=0.0d0
        acc(3,ipg)=0.0d0
        do 105 i = 1, nno
            acc(1,ipg) = acc(1,ipg) + acloc(1,i)*zr(ivf+ldec+i-1)
            acc(2,ipg) = acc(2,ipg) + acloc(2,i)*zr(ivf+ldec+i-1)
            acc(3,ipg) = acc(3,ipg) + acloc(3,i)*zr(ivf+ldec+i-1)
105      continue
!
!        CALCUL DU JACOBIEN AU POINT DE GAUSS IPG
!
        jac = sqrt (nx*nx + ny*ny + nz*nz)
        norm(1) = nx/jac
        norm(2) = ny/jac
        norm(3) = nz/jac
        flufn(ipg) = 0.d0
! CALCUL DU FLUX FLUIDE NORMAL AU POINT DE GAUSS
        flufn(ipg) = acc(1,ipg)*norm(1) + acc(2,ipg)*norm(2) +acc(3, ipg)*norm(3)
!
        do 103 i = 1, nno
            zr(ivectt+i-1) = zr(ivectt+i-1) + jac*zr(ipoids+ipg-1)* flufn(ipg) * rho(1) *&
                             zr(ivf+ldec+i-1)
103      continue
101  end do
!
end subroutine
