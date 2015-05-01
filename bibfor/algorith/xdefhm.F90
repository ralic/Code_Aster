subroutine xdefhm(dimdef, dimenr, addeme, adenme, addep1,&
                  ndim, degem1, degep1, defgem, defgep, adenhy)
!
    implicit none
!
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
! person_in_charge: daniele.colombo at ifpen.fr
!
!          BUT : ASSEMBLER LES DEFORMATIONS GENERALISEES DU MODELE
!                HM EN XFEM
!
! IN   DIMDEF  : DIMENSION DU TABLEAU DES DEF GENERALISEES ASSEMBLE
! IN   DIMENR  : DIMENSION DU TABLEAU DES DEF GENERALISEES BRUT
! IN   ADDEME  : ADRESSE DES DEFORMATIONS GENERALISEES CLASSIQUE
! IN   ADENME  : ADRESSE DES DEFORMATIONS GENERALISEES HEAVISIDE
! OUT  DEFGEM  : TABLEAU ASSEMBLE A L'INSTANT -
! OUT  DEFGEP  : TABLEAU ASSEMBLE A L'INSTANT +
!     ------------------------------------------------------------------
    integer :: dimdef, dimenr, addeme, adenme, ndim, addep1, i, adenhy
    real(kind=8) :: degem1(dimenr), degep1(dimenr)
    real(kind=8) :: defgem(dimdef), defgep(dimdef)
!
    do i = 1, dimdef
        defgem(i)=0.d0
        defgep(i)=0.d0
    end do
!
! ASSEMBLAGE (DEF CLASSIQUES + DEF HEAVISIDE) A L'INSTANT -
    do i = 1, ndim
        defgem(addeme-1+i)=degem1(addeme-1+i)+degem1(adenme-1+i)
    end do
!
    do i = 1, 6
        defgem(addeme-1+ndim+i)=degem1(addeme-1+ndim+i)
    end do
!
    defgem(addep1)=degem1(addep1) + degem1(adenhy)
!
    do i = 1, ndim
        defgem(addep1+i)=degem1(addep1+i) 
    end do
!
! ASSEMBLAGE (DEF CLASSIQUES + DEF HEAVISIDE) A L'INSTANT +
    do i = 1, ndim
        defgep(addeme-1+i)=degep1(addeme-1+i)+degep1(adenme-1+i)
    end do
!
    do i = 1, 6
        defgep(addeme-1+ndim+i)=degep1(addeme-1+ndim+i)
    end do
!
    defgep(addep1)=degep1(addep1) + degep1(adenhy)
!
    do i = 1, ndim
        defgep(addep1+i)=degep1(addep1+i)
    end do
end subroutine
