subroutine te0411(option, nomte)
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/dffno.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/normev.h"
#include "asterfort/pmat.h"
#include "asterfort/pmavec.h"
#include "asterfort/provec.h"
#include "asterfort/pscvec.h"
#include "asterfort/tecael.h"
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!                 CALCUL DE ROSETTE, OPTION : SIRO_ELEM
!
! IN   OPTION    : OPTION DE CALCUL
! IN   NOMTE     : NOM DU TYPE ELEMENT
!
! ----------------------------------------------------------------------
    integer :: isig, nnop, ndim, nnos, npg, ipoids, ivf, idfde, jgano, i
    integer :: igeom, j, ifonc, ino, iadzi, iazk24, isigm, iaux1, iaux2
!
    real(kind=8) :: prec, nort1, nort2, norno, l1, l2, delta
    real(kind=8) :: vtan1(3), vtan2(3), vt1(3), vt2(3), vno(3)
    real(kind=8) :: v1(3), v2(3), vtmp(3), vtmp2(3)
    real(kind=8) :: sigg(3, 3), mlg(3, 3), mtmp(3, 3), sigl(3, 3), mgl(3, 3)
    real(kind=8) :: det
    real(kind=8) :: dff(162), x1
!
    character(len=8) :: elrefe
    parameter(prec=1.0d-10)
! ----------------------------------------------------------------------
!
    call tecael(iadzi, iazk24, noms=0)
    call elref1(elrefe)
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nnop,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call jevech('PSIG3D', 'L', isig)
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PPJSIGM', 'E', isigm)
!
!     CALCUL DES DERIVEES DES FONCTIONS DE FORMES AUX NOEUDS DE L'ELREFE
    call dffno(elrefe, ndim, nnop, nnos, dff)
!
! --- ------------------------------------------------------------------
! --- CALCUL DU REPERE LOCAL : (VT1, VT2, VNO)
!        VT1, VT2 = VECTEURS TANGENTS A L'ELEMENT
!        VNO = VECTEUR NORMAL A L'ELEMENT
!
!     INITIALISATION
    do 9 i = 1, 3
        vt1(i) = 0.d0
        vt2(i) = 0.d0
        vno(i) = 0.d0
        do 8 j = i, 3
            sigg(i,j)=0.d0
 8      continue
 9  end do
!
! --- ------------------------------------------------------------------
! --- BOUCLE SUR LES NOEUDS DE L'ELEMENT
    do 10 ino = 1, nnop
!
        do 12 i = 1, 3
            vtan1(i) = 0.d0
            vtan2(i) = 0.d0
12      continue
!
        do 20 ifonc = 1, nnop
            iaux1 = igeom-1+3*(ifonc-1)
            iaux2 = (ino-1)*nnop*2 + ifonc
            vtan1(1) = vtan1(1) + zr(iaux1+1)*dff(iaux2)
            vtan1(2) = vtan1(2) + zr(iaux1+2)*dff(iaux2)
            vtan1(3) = vtan1(3) + zr(iaux1+3)*dff(iaux2)
            vtan2(1) = vtan2(1) + zr(iaux1+1)*dff(iaux2+nnop)
            vtan2(2) = vtan2(2) + zr(iaux1+2)*dff(iaux2+nnop)
            vtan2(3) = vtan2(3) + zr(iaux1+3)*dff(iaux2+nnop)
20      continue
!
        vt1(1)=vt1(1)+vtan1(1)
        vt1(2)=vt1(2)+vtan1(2)
        vt1(3)=vt1(3)+vtan1(3)
        vt2(1)=vt2(1)+vtan2(1)
        vt2(2)=vt2(2)+vtan2(2)
        vt2(3)=vt2(3)+vtan2(3)
!
        sigg(1,1)=sigg(1,1)+zr(isig+6*(ino-1))
        sigg(2,2)=sigg(2,2)+zr(isig+6*(ino-1)+1)
        sigg(3,3)=sigg(3,3)+zr(isig+6*(ino-1)+2)
        sigg(1,2)=sigg(1,2)+zr(isig+6*(ino-1)+3)
        sigg(1,3)=sigg(1,3)+zr(isig+6*(ino-1)+4)
        sigg(2,3)=sigg(2,3)+zr(isig+6*(ino-1)+5)
!
10  end do
! --- ------------------------------------------------------------------
! --- VECTEURS TANGENTS PAR MOYENNE DE CHAQUE COMPOSANTE
    vt1(1)=vt1(1)/nnop
    vt1(2)=vt1(2)/nnop
    vt1(3)=vt1(3)/nnop
    vt2(1)=vt2(1)/nnop
    vt2(2)=vt2(2)/nnop
    vt2(3)=vt2(3)/nnop
! --- ------------------------------------------------------------------
! --- TENSEUR DES CONTRAINTES PAR MOYENNE DE CHAQUE COMPOSANTE
    sigg(1,1)=sigg(1,1)/nnop
    sigg(2,2)=sigg(2,2)/nnop
    sigg(3,3)=sigg(3,3)/nnop
    sigg(1,2)=sigg(1,2)/nnop
    sigg(1,3)=sigg(1,3)/nnop
    sigg(2,3)=sigg(2,3)/nnop
    sigg(2,1)=  sigg(1,2)
    sigg(3,1)=  sigg(1,3)
    sigg(3,2)=  sigg(2,3)
!
    call normev(vt1, nort1)
    call normev(vt2, nort2)
    call provec(vt1, vt2, vno)
    call normev(vno, norno)
    call provec(vno, vt1, vt2)
! --- ------------------------------------------------------------------
! --- EXPRESSION DES VECTEURS CONTRAINTES DANS LE REPERE LOCAL :
!        (VT1,VT2,VNO)
!        SIX_L = (SIXT1,SIXT2,SIXNO) DANS (VT1,VT2,VNO)
!        SIY_L = (SIYT1,SIYT2,SIYNO) DANS (VT1,VT2,VNO)
!        SIZ_L = (SIZT1,SIZT2,SIZNO) DANS (VT1,VT2,VNO)
!
    det=vt1(1)*vt2(2)*vno(3)+vt2(1)*vno(2)*vt1(3)+vno(1)*vt1(2)*vt2(3)&
     &   -vno(1)*vt2(2)*vt1(3)-vt1(1)*vno(2)*vt2(3)-vt2(1)*vt1(2)*vno(3)
    ASSERT(abs(det).gt.prec)
!
    mgl(1,1) = vt1(1)
    mgl(2,1) = vt2(1)
    mgl(3,1) = vno(1)
    mgl(1,2) = vt1(2)
    mgl(2,2) = vt2(2)
    mgl(3,2) = vno(2)
    mgl(1,3) = vt1(3)
    mgl(2,3) = vt2(3)
    mgl(3,3) = vno(3)
!
    mlg(1,1) = vt1(1)
    mlg(2,1) = vt1(2)
    mlg(3,1) = vt1(3)
    mlg(1,2) = vt2(1)
    mlg(2,2) = vt2(2)
    mlg(3,2) = vt2(3)
    mlg(1,3) = vno(1)
    mlg(2,3) = vno(2)
    mlg(3,3) = vno(3)
!
    call pmat(3, sigg, mlg, mtmp)
    call pmat(3, mgl, mtmp, sigl)
! --- ------------------------------------------------------------------
! --- CALCUL DES OPTIONS : SIRO_ELEM_SIT1 & SIRO_ELEM_SIT2
!        DETERMINATION DES 2 MODES PROPRES TELS QUE SIXT2=SIYT1=0
    delta=(sigl(1,1)+sigl(2,2))**2 -&
     &       4.d0*(sigl(1,1)*sigl(2,2)-sigl(1,2)*sigl(2,1))
    x1= (sigl(1,1)+sigl(2,2))**2
    if (abs(delta) .le. x1*prec) delta=0.d0
    if (delta .lt. 0.d0) then
        write(6,*) 'DEBUG DELTA=',delta
        write(6,*) 'DEBUG SIGL=',sigl
    endif
    ASSERT(delta.ge.0.d0)
    l1=(sigl(1,1)+sigl(2,2)-sqrt(delta))/2
    l2=(sigl(1,1)+sigl(2,2)+sqrt(delta))/2
    if (abs(sigl(1,1)-l1) .lt. prec) then
        v1(1)=-(sigl(2,2)-l1)
        v1(2)=sigl(2,1)
        v1(3)=0.d0
    else
        v1(1)=-sigl(1,2)
        v1(2)=sigl(1,1)-l1
        v1(3)=0.d0
    endif
    if (abs(sigl(1,1)-l2) .lt. prec) then
        v2(1)=-(sigl(2,2)-l2)
        v2(2)=sigl(2,1)
        v2(3)=0.d0
    else
        v2(1)=-sigl(1,2)
        v2(2)=sigl(1,1)-l2
        v2(3)=0.d0
    endif
    call normev(v1, nort1)
    call pscvec(3, l1, v1, v1)
    call normev(v2, nort2)
    call pscvec(3, l2, v2, v2)
!
    call pmavec('ZERO', 3, mlg, v1, vtmp)
    zr(isigm-1+8) = vtmp(1)
    zr(isigm-1+9) = vtmp(2)
    zr(isigm-1+10)= vtmp(3)
    zr(isigm-1+11)= l1
!
    call pmavec('ZERO', 3, mlg, v2, vtmp)
    zr(isigm-1+12)= vtmp(1)
    zr(isigm-1+13)= vtmp(2)
    zr(isigm-1+14)= vtmp(3)
    zr(isigm-1+15)= l2
! --- ------------------------------------------------------------------
! --- CALCUL DES OPTIONS : SIRO_ELEM_SIGN & SIRO_ELEM_SIGT
    vtmp(1)=0.d0
    vtmp(2)=0.d0
    vtmp(3)=sigl(3,3)
    call pmavec('ZERO', 3, mlg, vtmp, vtmp2)
    zr(isigm-1+1)= vtmp2(1)
    zr(isigm-1+2)= vtmp2(2)
    zr(isigm-1+3)= vtmp2(3)
    zr(isigm-1+4)= sigl(3,3)
!
    vtmp(1)=sigl(3,1)
    vtmp(2)=sigl(3,2)
    vtmp(3)=0.d0
!
    call pmavec('ZERO', 3, mlg, vtmp, vtmp2)
    zr(isigm-1+5)= vtmp2(1)
    zr(isigm-1+6)= vtmp2(2)
    zr(isigm-1+7)= vtmp2(3)
!
!
end subroutine
