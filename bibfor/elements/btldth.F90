subroutine btldth(fami, xi3, nb1, kpg, btild,&
                  wgt, indic, young, nu, alpha,&
                  temper, forthi)
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
!
    implicit none
!
#include "jeveux.h"
#include "asterc/r8nnem.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
    integer :: nb1, kpg
    real(kind=8) :: wgt, young, nu, alpha, xi3
    real(kind=8) :: btild(5, 42), forthi(1), vecthr(2)
    integer :: jcou, imoy, iadzi, iazk24
    character(len=24) :: valk(3)
    character(len=8) :: nommai
    character(len=4) :: fami
    real(kind=8) :: p1xi3, p2xi3, p3xi3
!
!
!     CALCUL DE TEMPERATURE AUX PTS D'INTEGRATION
!
!
!-----------------------------------------------------------------------
    integer :: i, indic, iret1, iret2, iret3, iret4, k
!
    real(kind=8) :: temper, tinf, tmoy, tref, tsup
!-----------------------------------------------------------------------
    p1xi3= 1-xi3*xi3
    p2xi3=-xi3*(1-xi3)/2.d0
    p3xi3= xi3*(1+xi3)/2.d0
    call jevech('PNBSP_I', 'L', jcou)
    imoy=(3*zi(jcou)+1)/2
    call rcvarc(' ', 'TEMP', 'REF', fami, 1,&
                1, tref, iret1)
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                1, tinf, iret2)
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                imoy, tmoy, iret3)
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                3*zi(jcou), tsup, iret4)
    if ((iret2+iret3+iret4) .eq. 0) then
        if ((iret1.eq.1) .or. (indic.eq.0)) then
            call tecael(iadzi, iazk24)
            nommai=zk24(iazk24-1+3)(1:8)
            valk(1)=nommai
            valk(2)='TEMP_REF'
            call utmess('F', 'CALCULEL_32', nk=2, valk=valk)
        else
            temper=(tmoy*p1xi3+tinf*p2xi3+tsup*p3xi3)-tref
        endif
    else
        temper = r8nnem()
    endif
!
!
    if (indic .eq. 1) then
!
        vecthr(1)=young*alpha*temper/(1.d0-nu)
        vecthr(2)=vecthr(1)
!
!     CONSTRUCTION DES EFFORTS DUS AUX DILATATIONS THERMIQUES
!
        do 30 i = 1, 5*nb1+2
            forthi(i)=0.d0
            do 40 k = 1, 2
                forthi(i)=forthi(i)+btild(k,i)*vecthr(k)*wgt
!        FORTHI(I)=FORTHI(I)+BTILD(K,I)*VECTHR(K)
40          end do
30      end do
!
    endif
!
!
end subroutine
