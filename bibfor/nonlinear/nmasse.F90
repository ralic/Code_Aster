subroutine nmasse(fami, kpg, ksp, poum, icodma,&
                  materi, inst, e, nu, deuxmu,&
                  troisk)
!
    implicit none
    include 'asterfort/rcvalb.h'
    integer :: kpg, ksp, icodma
    real(kind=8) :: inst
    real(kind=8) :: e, nu, deuxmu, troisk
    character(len=*) :: materi, fami, poum
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!      VISCO_PLASTICITE FLUAGE SOUS IRRADIATION
!      RECUPERATION DU PARAMETRE VARIABLE (TEMPS)
!      CARACTERISTIQUES ELASTIQUES: YOUNG, POISSON, ALPHA
!
! IN  ICODMA  : ADRESSE DU MATERIAU CODE
! IN  INST    : INSTANT CONSIDERE
! OUT E       : MODULE DE YOUNG
! OUT NU      : COEFFICIENT DE POISSON
! OUT ALPHA   : COEFFICIENT DE DILATATION
! OUT DEUXMU  : COEFFICIENT DE LAME 1
! OUT TROISK  : COEFFICIENT DE LAME 2
!
    character(len=8) :: nomres(2), nompar
    real(kind=8) :: valres(2), valpar
    integer :: icodre(2)
!
    nompar='INST'
    valpar=inst
! CARACTERISTIQUES: MODULE D'YOUNG, COEFFICIENT DE POISSON
    nomres(1)='E'
    nomres(2)='NU'
!
    call rcvalb(fami, kpg, ksp, poum, icodma,&
                materi, 'ELAS', 1, nompar, valpar,&
                2, nomres, valres, icodre, 2)
!
    e = valres(1)
    nu = valres(2)
!
    deuxmu = e/(1.d0+nu)
    troisk = e/(1.d0-2.d0*nu)
!
!
end subroutine
