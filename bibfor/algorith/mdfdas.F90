subroutine mdfdas(dnorm, vnorm, vitloc, cost, sint,&
                  coefk1, coefk2, coefpy, coefc, coefad,&
                  xmax, fdispo, flocal)
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
!
!***********************************************************************
!    CALCULE LA FORCE DUE A LA PRESENCE D'UN DISPOSITIF ANTI SISMIQUE
!
!-----------------------------------------------------------------------
!   IN :  DNORM : DISTANCE NORMALE
!   OUT :  VNORM : VITESSE  NORMALE
!   IN :  VITLOC : VITESSE DANS LE REPERE LOCAL
!   IN :  ACCLOC : ACCELERATION DANS LE REPERE LOCAL
!   IN :  COST,SINT : DIRECTION DE LA NORMALE A L'OBSTACLE
!   IN :  COEFK1 : VALEUR DE RIGI_K1
!   IN :  COEFK2 : VALEUR DE RIGI_K2
!   IN :  COEFPY : VALEUR DE SEUIL_FX
!   IN :  COEFC : VALEUR DE C
!   IN :  COEFAD : VALEUR DE PUIS_ALPHA
!   IN :  XMAX : VALEUR DE DX_MAX
!   IN :  CNORM : AMORTISSEUR NORMALE DE CHOC
!   OUT :  FDISPO : FORCE NORMALE DUE AU DISPO ANTI SISMIQUE
!   OUT :  FLOCAL : FORCE NORMALE DE CHOC REP. LOCAL
!-----------------------------------------------------------------------
    implicit none
    real(kind=8) :: vitloc(6), flocal(3), fdispo
    real(kind=8) :: coefk1, coefk2, coefpy, coefc, coefad, xmax, cost, sint
!-----------------------------------------------------------------------
    real(kind=8) :: dnorm, vnorm
!-----------------------------------------------------------------------
    vnorm = vitloc(2)*cost + vitloc(3)*sint
    if (vnorm .eq. 0.d0) then
        fdispo = -coefk2*dnorm - (coefk1-coefk2)*dnorm/sqrt(1.d0+( coefk1*dnorm/coefpy)**2)
    else
        fdispo = -coefk2*dnorm - (coefk1-coefk2)*dnorm/sqrt(1.d0+( coefk1*dnorm/coefpy)**2) - coe&
                 &fc*sign(1.d0,vnorm)*abs(vnorm* dnorm/xmax)**coefad
    endif
    flocal(1)=0.d0
    flocal(2)=fdispo*cost
    flocal(3)=fdispo*sint
end subroutine
