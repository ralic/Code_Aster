subroutine infdbg(optioz, ifm, niv)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
    implicit      none
    include 'asterfort/infniv.h'
    character(len=*) :: optioz
    integer :: ifm, niv
    character(len=16) :: czcont, czmeca, czpilo, czfact, czappa
    common /czdbg/czcont,czmeca,czpilo,czfact,czappa
! ----------------------------------------------------------------------
!
! ROUTINE POUR AFFICHAGE INFOS DEBUG
!
! POUR L'INSTANT, ON DECONNECTE (FICHE 10620 DU REX)
!
! ----------------------------------------------------------------------
!
!
! IN  OPTION : OPTION DE DEBUGGAGE
!                'CONTACT': DEBUG POUR LE CONTACT
! OUT IFM    : UNITE D'IMPRESSION
! OUT NIV    : NIVEAU D'IMPRESSION
!
! ----------------------------------------------------------------------
!
    character(len=16) :: option
!
! ----------------------------------------------------------------------
!
    option = optioz
!
    if (option(1:7) .eq. 'CONTACT') then
        call infniv(ifm, niv)
        if (czcont .ne. 'CONTACT') niv=1
    else if (option.eq.'XFEM') then
        call infniv(ifm, niv)
        elseif (option.eq.'MECA_NON_LINE' .or. option.eq.'MECANONLINE')&
    then
        call infniv(ifm, niv)
        if (czmeca .ne. 'MECA_NON_LINE') niv=1
    else if (option.eq.'PRE_CALCUL') then
        call infniv(ifm, niv)
        niv = 1
    else if (option.eq.'PILOTE') then
        call infniv(ifm, niv)
        if (czpilo .ne. 'PILOTE') niv=1
    else if (option(1:6).eq.'FACTOR') then
        call infniv(ifm, niv)
        if (czfact .ne. 'FACTORISATION') niv=1
    else if (option.eq.'APPARIEMENT') then
        call infniv(ifm, niv)
        if (czappa .ne. 'APPARIEMENT') niv=1
    else
        call infniv(ifm, niv)
    endif
!
!
!
!
end subroutine
