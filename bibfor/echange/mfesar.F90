subroutine mfesar(ifichi, nomail, typgeo, nomatt, nbrval,&
                  tabval, codret)
!           CONFIGURATION MANAGEMENT OF EDF VERSION
! ==================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D              WWW.CODE-ASTER.ORG
!
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
! MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS
! PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE
! LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,
! BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ==================================================================
! person_in_charge: nicolas.sellenet at edf.fr
    implicit none
    include 'med/mmhraw.h'
    character(len=*) :: nomail, nomatt
    integer :: ifichi, typgeo, nbrval, codret
    real(kind=8) :: tabval(*)
    integer :: numdt, numit
    parameter    (numdt = -1)
    parameter    (numit = -1)
#ifdef _DISABLE_MED
    call u2mess('F', 'FERMETUR_2')
#else
! BEGIN MED
#ifdef _USE_MED_SHORT_INT
    integer(kind=4) :: ifich4, typge4, nbrva4, codre4, numdt4, numit4
#ifdef _DEBUG_MED
    print *,'=== MFESAR ==='
#endif
    ifich4 = ifichi
    typge4 = typgeo
    nbrva4 = nbrval
    numdt4 = numdt
    numit4 = numit
    call mmhraw(ifich4, nomail, numdt4, numit4, typge4,&
                nomatt, nbrva4, tabval, codre4)
    codret = codre4
#else
    call mmhraw(ifichi, nomail, numdt, numit, typgeo,&
                nomatt, nbrval, tabval, codret)
#endif
! END MED
#endif
end subroutine
