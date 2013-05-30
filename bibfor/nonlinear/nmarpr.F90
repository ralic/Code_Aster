subroutine nmarpr(result, sddisc, lreuse, numder, insder,&
                  numarc)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/nmttch.h'
    include 'asterfort/u2mesr.h'
    real(kind=8) :: insder
    logical :: lreuse
    integer :: numder, numarc
    character(len=19) :: sddisc
    character(len=8) :: result
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (ARCHIVAGE)
!
! PREMIER NUMERO A ARCHIVER
!
! ----------------------------------------------------------------------
!
!
! IN  RESULT : NOM DE LA SD RESULTAT
! IN  SDDISC : SD DISCRETISATION
! IN  NUMDER : DERNIER NUMERO ARCHIVE DANS RESULT
!               (OU 0 SI NON REENTRANT)
! IN  INSDER : DERNIER INSTANT ARCHIVE DANS RESULT
!               (R8VIDE SI NON REENTRANT)
! IN  LREUSE : .TRUE. SI CONCEPT REENTRANT
! OUT NUMARC : NUMERO DU PREMIER PAS A ARCHIVER
!
!
!
!
    character(len=24) :: tpsdit
    integer :: jtemps
    real(kind=8) :: valr(2), inst2
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- NOM SD_DISC
!
    tpsdit = sddisc(1:19)//'.DITR'
    call jeveuo(tpsdit, 'L', jtemps)
!
    if (lreuse) then
!
! ----- FUTUR INSTANT A ARCHIVER
!
        inst2 = zr(jtemps+2-1)
!
! ----- L'INSTANT INITIAL EST-IL SUPERIEUR AU DERNIER INSTANT ?
!
        if (inst2 .le. insder) then
            valr(1) = insder
            valr(2) = inst2
            call u2mesr('I', 'ARCHIVAGE_1', 2, valr)
            call nmttch(result, inst2, numder)
            numarc = numder
        else
            numarc = numder + 1
        endif
!
    else
        call assert(numder.eq.0)
        numarc = 0
    endif
!
    call jedema()
!
end subroutine
