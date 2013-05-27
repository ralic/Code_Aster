subroutine lcumme(youn, xnu, ifou, dep)
!
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
!
! ROUTINE APPELE DANS FLU
! LCUMME    SOURCE    BENBOU   02/02/7
!
!_______________________________________________________________________
!
!    FORMATION DE LA MATRICE D ELASTICITE DE HOOKE : DEP(NSTRS,NSTRS)
!
! IN  YOUN     : MODULE D YOUNG
! IN  XNU      : COEFFICIENT DE POISSON
! IN  IFOU     : TYPE DE MODELISATION MECANIQUE (3D,CP,AXY...)
! OUT DEP      : MATRICE DE HOOKE
!_______________________________________________________________________
!
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/matini.h'
    integer :: ifou
! MODIFI DU 6 JANVIER 2002 - YLP SUPPRESSION DES DECLARATIONS
! IMPLICITES DES TABLEAUX
!      REAL*8 DEP(NSTRS,NSTRS)
    real(kind=8) :: dep(6, 6)
    real(kind=8) :: e1, xnu, youn
!
!  INITIALISATION DE LA MATRICE D ELASTICITE
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call matini(6, 6, 0.d0, dep)
!
    if (ifou .eq. -2) then
!
! CALCUL DES MATRICES D ELASTICITE SUIVANT LE TYPE DE CALCUL : DEP
!  - CONTRAINTES PLANES => EQUATION (3.8-1)
!
        e1=youn/(1.d0-xnu*xnu)
        dep(1,1)=e1
        dep(1,2)=xnu*e1
        dep(2,1)=xnu*e1
        dep(2,2)=e1
        dep(3,3)=e1*(1.d0-xnu)/2.d0
        goto 100
!
    else if ((ifou.eq.-1).or.(ifou.eq.0)) then
!
!  - DEFORMATION PLANE OU AXISYMETRIQUE => EQUATION (3.8-2)
!
        e1=youn/(1.d0+xnu)/(1.d0-2.d0*xnu)
        dep(1,1)=e1*(1.d0-xnu)
        dep(1,2)=e1*xnu
        dep(1,3)=e1*xnu
        dep(2,1)=e1*xnu
        dep(2,2)=e1*(1.d0-xnu)
        dep(2,3)=e1*xnu
        dep(3,1)=e1*xnu
        dep(3,2)=e1*xnu
        dep(3,3)=e1*(1.d0-xnu)
        dep(4,4)=e1*(1.d0-2.d0*xnu)
!         DEP(4,4)=E1*(1.D0-2.D0*XNU)/2.D0
        goto 100
    else if (ifou.eq.2) then
!
!  - CALCUL TRIDIMENSIONEL => EQUATION (3.8-3)
!
        e1=youn/(1.d0+xnu)/(1.d0-2.d0*xnu)
        dep(1,1)=e1*(1.d0-xnu)
        dep(1,2)=e1*xnu
        dep(1,3)=e1*xnu
        dep(2,1)=e1*xnu
        dep(2,2)=e1*(1.d0-xnu)
        dep(2,3)=e1*xnu
        dep(3,1)=e1*xnu
        dep(3,2)=e1*xnu
        dep(3,3)=e1*(1.d0-xnu)
        dep(4,4)=e1*(1.d0-2.d0*xnu)
        dep(5,5)=e1*(1.d0-2.d0*xnu)
        dep(6,6)=e1*(1.d0-2.d0*xnu)
!         DEP(4,4)=E1*(1.D0-2.D0*XNU)/2.D0
!         DEP(5,5)=E1*(1.D0-2.D0*XNU)/2.D0
!         DEP(6,6)=E1*(1.D0-2.D0*XNU)/2.D0
        goto 100
    endif
!
    call assert(.false.)
!
100  continue
!
end subroutine
