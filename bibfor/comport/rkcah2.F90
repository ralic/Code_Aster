subroutine rkcah2(rela_comp, y, pas, nvi, w,&
                  wk, h, eps, iret)
implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     INTEGRATION DE LOIS DE COMPORTEMENT PAR  RUNGE KUTTA
!     CALCUL DU NOUVEAU PAS DE TEMPS (DIMINUTION)
!      IN rela_comp    :  NOM DU MODELE DE COMPORTEMENT
!         Y       :  VARIABLES INTERNES
!     OUT H       :  PAS DE TEMPS
!
#include "asterc/r8prem.h"
#include "asterfort/utmess.h"
    integer :: ne, ny, na, nvi, iret
    character(len=16) :: rela_comp
    real(kind=8) :: y(*), h, w, dmg0, dmg1, maxout, maxdom, wk(*), eps
    real(kind=8) :: prec, coef, pas
    parameter  ( maxdom = 9.90d-01  )
!
    ne=0
    ny=nvi
    na=ny+nvi
    prec=r8prem()
!
    iret=0
!
    maxout=maxdom-eps
!
    if (rela_comp(1:9) .eq. 'VENDOCHAB') then
!        TRAITEMENT VENDOCHAB
        dmg1=y(9)
!        TEST SUR LE NIVEU DE DOMMAGE--
        if (dmg1 .ge. maxdom) then
            dmg0=(dmg1-wk(ne+9))-(wk(na+9)*h)
            h=(maxout-dmg0)/((wk(ne+9)/h)+wk(na+9))
        else
            w=w/abs(eps)
            w=min(w,1.0d08)
            coef=w**(-2.0d-01)*9.0d-01
            h=h*coef
        endif
!        FIN TEST SUR LE NIVEAU DE DOMMAGE
!
    else
!
        w=w/abs(eps)
        w=min(w,1.0d08)
!        CALCUL CLASSIQUE DU NOUVEAU PAS DE TEMPS (ISSU DE RK4)
!        POUR W=1.D8, COEF=2.26070E-02
        coef=w**(-2.0d-01)*9.0d-01
        h=h*coef
    endif
!
    if ((h/pas) .lt. prec) then
        call utmess('I', 'ALGORITH3_83')
        iret=1
    endif
!
end subroutine
