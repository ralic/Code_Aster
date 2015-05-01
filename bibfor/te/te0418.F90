subroutine te0418(option, nomte)
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
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/r8inir.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_ME_FR1D3D  '
!                                   'CHAR_ME_FF1D3D  '
!                        ELEMENT  : 'MEBOCQ3'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    character(len=8) :: elrefe
    real(kind=8) :: fx, fy, fz, mx, my, mz, jac, jacp, effglb(18)
    integer :: i
!
    real(kind=8) :: valpar(4)
    character(len=8) :: nompar(4)
!
!-----------------------------------------------------------------------
    integer :: i1, icod1, icod2, icod3, icod4, icod5, icod6
    integer :: idfdk, iforc, igeom, ino, ipoids, itpsr, ivectu
    integer :: ivf, jgano, k, kp, ndim, nno, nnos
    integer :: npg
    real(kind=8) :: dxdk, dydk, dzdk, x, y, z, zero
!
!-----------------------------------------------------------------------
    call elref1(elrefe)
    zero = 0.d0
!
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfdk,jgano=jgano)
!
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PVECTUR', 'E', ivectu)
!
    call r8inir(18, 0.d0, effglb, 1)
!
!     -- CALCUL DE LA FORCE MOYENNE :
    if (option(11:16) .eq. 'FR1D3D') then
        call jevech('PFR1D3D', 'L', iforc)
        fx = zr(iforc-1+1)
        fy = zr(iforc-1+2)
        fz = zr(iforc-1+3)
        mx = zr(iforc-1+4)
        my = zr(iforc-1+5)
        mz = zr(iforc-1+6)
    else if (option(11:16).eq.'FF1D3D') then
        call jevech('PFF1D3D', 'L', iforc)
        call jevech('PTEMPSR', 'L', itpsr)
!
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'Z'
        nompar(4) = 'INST'
        valpar(4) = zr(itpsr)
    else
        call utmess('F', 'ELEMENTS2_77', sk=option)
    endif
!
    do 40 kp = 1, npg
        k = (kp-1)*nno
!       CALL DFDM1D( NNO,ZR(IPOIDS+KP-1),ZR(IDFDK+K),
!    &               ZR(IGEOM),DFDX,COUR,JACP,COSA,SINA)
!
        dxdk = zero
        dydk = zero
        dzdk = zero
!
        do 10 i = 1, nno
            dxdk = dxdk + zr(igeom+3* (i-1))*zr(idfdk+k+i-1)
            dydk = dydk + zr(igeom+3* (i-1)+1)*zr(idfdk+k+i-1)
            dzdk = dzdk + zr(igeom+3* (i-1)+2)*zr(idfdk+k+i-1)
10      continue
        jac = sqrt(dxdk**2+dydk**2+dzdk**2)
        jacp = jac*zr(ipoids+kp-1)
        if (option(11:16) .eq. 'FF1D3D') then
            x = zero
            y = zero
            z = zero
            do 20 i = 1, nno
                x = x + zr(igeom+3* (i-1))*zr(ivf+k+i-1)
                y = y + zr(igeom+3* (i-1)+1)*zr(ivf+k+i-1)
                z = z + zr(igeom+3* (i-1)+2)*zr(ivf+k+i-1)
20          continue
            valpar(1) = x
            valpar(2) = y
            valpar(3) = z
            call fointe('FM', zk8(iforc-1+1), 4, nompar, valpar,&
                        fx, icod1)
            call fointe('FM', zk8(iforc-1+2), 4, nompar, valpar,&
                        fy, icod2)
            call fointe('FM', zk8(iforc-1+3), 4, nompar, valpar,&
                        fz, icod3)
            call fointe('FM', zk8(iforc-1+4), 4, nompar, valpar,&
                        mx, icod4)
            call fointe('FM', zk8(iforc-1+5), 4, nompar, valpar,&
                        my, icod5)
            call fointe('FM', zk8(iforc-1+6), 4, nompar, valpar,&
                        mz, icod6)
        endif
!
        do 30 i = 1, nno
            i1 = 6* (i-1)
            effglb(i1+1) = effglb(i1+1) + jacp*zr(ivf+k+i-1)*fx
            effglb(i1+2) = effglb(i1+2) + jacp*zr(ivf+k+i-1)*fy
            effglb(i1+3) = effglb(i1+3) + jacp*zr(ivf+k+i-1)*fz
            effglb(i1+4) = effglb(i1+4) + jacp*zr(ivf+k+i-1)*mx
            effglb(i1+5) = effglb(i1+5) + jacp*zr(ivf+k+i-1)*my
            effglb(i1+6) = effglb(i1+6) + jacp*zr(ivf+k+i-1)*mz
30      continue
!
40  end do
!
!     -- AFFECTATION DU RESULTAT:
!
    do 50 ino = 1, nno
        zr(ivectu-1+ (ino-1)*6+1) = effglb((ino-1)*6+1)
        zr(ivectu-1+ (ino-1)*6+2) = effglb((ino-1)*6+2)
        zr(ivectu-1+ (ino-1)*6+3) = effglb((ino-1)*6+3)
        zr(ivectu-1+ (ino-1)*6+4) = effglb((ino-1)*6+4)
        zr(ivectu-1+ (ino-1)*6+5) = effglb((ino-1)*6+5)
        zr(ivectu-1+ (ino-1)*6+6) = effglb((ino-1)*6+6)
50  end do
!
end subroutine
