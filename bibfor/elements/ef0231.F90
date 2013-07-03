subroutine ef0231(nomte)
    implicit none
#include "jeveux.h"
#include "asterc/r8nnem.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm1d.h"
#include "asterfort/elref1.h"
#include "asterfort/elref5.h"
#include "asterfort/jevech.h"
#include "asterfort/ppgan2.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/u2mess.h"
    character(len=16) :: nomte
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     CALCUL DE EFGE_ELNO
!     ------------------------------------------------------------------
!
    character(len=8) :: nomres(3), elrefe
    integer :: icodre(3)
    real(kind=8) :: e, nu, tpg, tgmoy, tgsup, tginf, tref
    real(kind=8) :: x3, eps(5), c, h, epsthe, valres(3)
    real(kind=8) :: e11, e22, k11, k22, ep11, ep22
    real(kind=8) :: dfdx(3), effopg(24)
    real(kind=8) :: jac, r, cosa, sina, cour
    integer :: i, k, kp, igeom, imate, icaco, idepl
    integer :: nno, npg, idfdk, ivf, iret, iret2, iret1, iret3, iret4
    integer :: jcoopg, ip, correc, jdfd2
!
!-----------------------------------------------------------------------
    integer :: ieffor, ipoids, jgano, ndim, nnos
!-----------------------------------------------------------------------
    call elref1(elrefe)
!
!
    call elref5(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, jcoopg, ivf, idfdk,&
                jdfd2, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PCACOQU', 'L', icaco)
    call jevech('PDEPLAR', 'L', idepl)
    call jevech('PEFFORR', 'E', ieffor)
    call rcvarc(' ', 'TEMP', 'REF', 'RIGI', 1,&
                1, tref, iret)
!
    h=zr(icaco)
!JMP  CORREC = CORRECTION DE METRIQUE = 0 (NON) OU 1 (OUI)
!JMP  CORREC = ZR(ICACO+2)
    correc=nint(zr(icaco+2))
!
    do 10 i = 1, npg*6
        effopg(i)=0.d0
10  end do
!
!
    do 50 kp = 1, npg
        k=(kp-1)*nno
        call dfdm1d(nno, zr(ipoids+kp-1), zr(idfdk+k), zr(igeom), dfdx,&
                    cour, jac, cosa, sina)
!
        do 20 i = 1, 5
            eps(i)=0.d0
20      continue
        r=0.d0
        do 30 i = 1, nno
            eps(1)=eps(1)+dfdx(i)*zr(idepl+3*i-3)
            eps(2)=eps(2)+dfdx(i)*zr(idepl+3*i-2)
            eps(3)=eps(3)+dfdx(i)*zr(idepl+3*i-1)
            eps(4)=eps(4)+zr(ivf+k+i-1)*zr(idepl+3*i-3)
            eps(5)=eps(5)+zr(ivf+k+i-1)*zr(idepl+3*i-1)
            r=r+zr(ivf+k+i-1)*zr(igeom+2*i-2)
30      continue
!
        e11=eps(2)*cosa-eps(1)*sina
        k11=eps(3)
        if (nomte .eq. 'MECXSE3') then
            e22=eps(4)/r
            k22=-eps(5)*sina/r
        else
            e22=0.d0
            k22=0.d0
        endif
!
        call rcvarc(' ', 'TEMP', '+', 'RIGI', kp,&
                    1, tginf, iret1)
        call rcvarc(' ', 'TEMP', '+', 'RIGI', kp,&
                    2, tgmoy, iret2)
        call rcvarc(' ', 'TEMP', '+', 'RIGI', kp,&
                    3, tgsup, iret3)
        iret4=iret1+iret2+iret3
        call assert(iret4.eq.0 .or. iret4.eq.3)
!
!
!---- UTILISATION DE 4 POINTS DE GAUSS DANS L'EPAISSEUR
!---- COMME POUR LA LONGUEUR
!
        do 40 ip = 1, npg
            x3=zr(jcoopg-1+ip)
            if (iret4 .eq. 0) then
                tpg=tgmoy*(1.d0-(x3)**2)+tgsup*x3*(1.d0+x3)/2.d0-&
                tginf*x3*(1.d0-x3)/2.d0
            else
                tpg=r8nnem()
            endif
            x3=x3*h/2.d0
            ep11=(e11+x3*k11)/(1.d0+(correc*x3*cour))
            nomres(1)='E'
            nomres(2)='NU'
            nomres(3)='ALPHA'
            call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                        ' ', 'ELAS', 1, 'TEMP', tpg,&
                        2, nomres, valres, icodre, 1)
            call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                        ' ', 'ELAS', 1, 'TEMP', tpg,&
                        1, nomres(3), valres(3), icodre(3), 0)
            e=valres(1)
            nu=valres(2)
            if (iret4 .eq. 0) then
                if ((icodre(3).ne.0) .or. (iret.eq.1)) then
                    call u2mess('F', 'CALCULEL_15')
                else
                    epsthe=(tpg-tref)*valres(3)*e/(1.d0-nu)
                endif
            else
                epsthe=0.d0
            endif
!
            c=e/(1.d0-nu*nu)
            if (nomte(3:4) .eq. 'CX') then
                ep22=(e22+x3*k22)/(1.d0+(correc*cosa*x3/r))
                effopg(6*(kp-1)+1)=effopg(6*(kp-1)+1)+ zr(ipoids-1+ip)&
                *(h/2.d0)* (c*(ep11+nu*ep22)-epsthe)
                effopg(6*(kp-1)+2)=effopg(6*(kp-1)+2)+ zr(ipoids-1+ip)&
                *(h/2.d0)* (c*(nu*ep11+ep22)-epsthe)
                effopg(6*(kp-1)+4)=effopg(6*(kp-1)+4)+ zr(ipoids-1+ip)&
                *x3*(h/2.d0)* (c*(ep11+nu*ep22)-epsthe)
                effopg(6*(kp-1)+5)=effopg(6*(kp-1)+5)+ zr(ipoids-1+ip)&
                *x3*(h/2.d0)* (c*(nu*ep11+ep22)-epsthe)
            else if (nomte.eq.'METCSE3 ') then
                effopg(6*(kp-1)+1)=effopg(6*(kp-1)+1)+ zr(ipoids-1+ip)&
                *(h/2.d0)* (e*(ep11-epsthe))
                effopg(6*(kp-1)+4)=effopg(6*(kp-1)+4)+ zr(ipoids-1+ip)&
                *x3*(h/2.d0)* (e*(ep11-epsthe))
                effopg(6*(kp-1)+2)=0.d0
                effopg(6*(kp-1)+5)=0.d0
            else
                effopg(6*(kp-1)+1)=effopg(6*(kp-1)+1)+ zr(ipoids-1+ip)&
                *(h/2.d0)*(c*ep11-epsthe)
                effopg(6*(kp-1)+2)=effopg(6*(kp-1)+2)+ zr(ipoids-1+ip)&
                *(h/2.d0)* (c*nu*ep11-epsthe)
                effopg(6*(kp-1)+4)=effopg(6*(kp-1)+4)+ zr(ipoids-1+ip)&
                *x3*(h/2.d0)* (c*ep11-epsthe)
                effopg(6*(kp-1)+5)=effopg(6*(kp-1)+5)+ zr(ipoids-1+ip)&
                *x3*(h/2.d0)* (c*nu*ep11-epsthe)
            endif
!
40      continue
        effopg(6*(kp-1)+3)=0.d0
        effopg(6*(kp-1)+6)=0.d0
!
50  end do
!
    call ppgan2(jgano, 1, 6, effopg, zr(ieffor))
end subroutine
