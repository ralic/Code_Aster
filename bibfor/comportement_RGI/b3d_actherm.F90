subroutine b3d_actherm(teta1,eafluage,xmg0,xmw0,taug0,&
      etag1,etaw1)
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
! person_in_charge: etienne grimal at edf.fr
!=====================================================================
!=====================================================================
!      traitement de l effet de la temperature sur les viscosites
!      la temperature de reference est suppose egale a 20 C
!=====================================================================
        implicit none
        real(kind=8) :: teta1
        real(kind=8) :: eafluage,unsurt,easurr,cvth1,unsurtr
        real(kind=8) :: xmg0
        real(kind=8) :: xmw0
        real(kind=8) :: taug0
        real(kind=8) :: etag1
        real(kind=8) :: etaw1
!      actualisation de la part viscoplastique, calcul de la pression,
!      et des nouveaux endommagements de pression de gel 
!      temperature pour viscosite du gel debut de pas 
       unsurt=(1.D0/(teta1+273.D0))
!      la temperature de reference est 20 C
       unsurtr=0.003412969283276451D0
!      calcul du terme d activation d Arrhenius
       easurr=eafluage/8.31D0
       cvth1=exp(-easurr*(unsurt-unsurtr))
       etag1=taug0*xmg0/cvth1
       etaw1=taug0*xmw0/cvth1
end subroutine
