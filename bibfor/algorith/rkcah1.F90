subroutine rkcah1(comp, y, pas, nvi, w,&
                  wk, h, eps, iret)
    implicit none
!     ================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
!     INTEGRATION DE LOIS DE COMPORTEMENT PAR  RUNGE KUTTA
!     CALCUL DU NOUVEAU PAS DE TEMPS (AUGMENTATION)
!      IN COMP    :  NOM DU MODELE DE COMPORTEMENT
!         Y       :  VARIABLES INTERNES
!         PAS     :  INTERVALLE DE TEMPS TF-TD
!     OUT H       :  PAS DE TEMPS
!
    integer :: ne, ny, na, nvi, ii, iret
    character(len=16) :: loi, comp(*)
    real(kind=8) :: pas, h, w, dmg0, eps, maxout, maxdom, wk(*), y(*)
    parameter  ( maxdom = 9.90d-01  )
!
    loi=comp(1)
    ne=0
    ny=nvi
    na=ny+nvi
    iret=0
!
    maxout=maxdom-eps
!
    if (loi(1:9) .eq. 'VENDOCHAB') then
!        TRAITEMENT VENDOCHAB
!        TEST SUR LE NIVEAU DE DOMMAGE--
        if (y(9) .ge. maxdom) then
            dmg0=(y(9)-wk(9))-(wk(na+9)*h)
            if (dmg0 .ge. maxout) then
                do 99 ii = 1, nvi
                    y(ii)=(y(ii)-wk(ne+ii))-(wk(na+ii)*h)
99              continue
                iret=1
            else
                h=(maxout-dmg0)/((wk(ne+9)/h)+wk(na+9))
                if (h .gt. pas) h=pas
            endif
        else
!           FIN TEST SUR LE NIVEAU DE DOMMAGE
            w=w/abs(eps)
            w=max(w,1.0d-05)
            h=h*w**(-2.0d-01)*9.0d-01
            if (h .gt. pas) h=pas
        endif
!        FIN TRAITEMENT VENDOCHAB
!
    else
!        IP.NE.1
!        CALCUL CLASSIQUE DU NOUVEAU PAS DE TEMPS (ISSU DE RK4)
        w=w/abs(eps)
        w=max(w,1.0d-05)
!        POUR 1.0D-05, COEF=9.
        h=h*w**(-2.0d-01)*9.0d-01
        if (h .gt. pas) h=pas
!
    endif
!
end subroutine
