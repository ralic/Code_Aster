subroutine dbgcha(valinc, instap, iterat)
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
    implicit none
    include 'asterfort/codent.h'
    include 'asterfort/codree.h'
    include 'asterfort/irchmd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/nmchex.h'
    real(kind=8) :: instap
    integer :: iterat
    character(len=19) :: valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
!    IMPRESSION D'UN CHAMP POUR DEBUG
!
! ----------------------------------------------------------------------
!
!
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  INSTAP : INSTANT CORRESPONDANT AU CHAMP
! IN  ITERAT : ITERATION CORRESPONDANT AU CHAMP
!
    character(len=19) :: depplu
    character(len=8) :: instxt, itetxt
    integer :: codret
    logical :: dbg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    dbg=.false.
!
    if (dbg) then
        call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
        call codree(instap, 'G', instxt)
        call codent(iterat, 'G', itetxt)
        call irchmd(80, depplu, 'REEL', 'INST:'//instxt//'ITERAT:'// itetxt, codret)
    endif
!
    call jedema()
end subroutine
