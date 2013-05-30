subroutine nmlere(sddisc, action, infz, iterat, valr)
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
    implicit      none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=19) :: sddisc
    character(len=1) :: action
    character(len=*) :: infz
    integer :: iterat
    real(kind=8) :: valr(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES - SD DISCRETISATION)
!
! LECTURE/ECRITURE DANS SD STOCKAGE DES RESIDUS
!
! ----------------------------------------------------------------------
!
! IN  SDDISC : SD DISCRETISATION
! IN  ACTION : 'L' OU 'E'
! IN  ITERAT : NUMERO ITERATION NEWTON
! IN  INFO   : TYPE D'INFO A STOCKER OU A LIRE
! I/O VALR   : REEL   A ECRIRE OU A LIRE
!
! ----------------------------------------------------------------------
!
    character(len=24) :: infore
    integer :: jifre
    character(len=24) :: info
    integer :: iter
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    infore = sddisc(1:19)//'.IFRE'
    call jeveuo(infore, 'E', jifre)
    info = infz
!
    call assert((action.eq.'E').or.(action.eq.'L'))
!
    if (info .eq. 'VRELA') then
        if (action .eq. 'E') then
            zr(jifre+3*iterat+1-1) = valr(1)
        else
            valr(1) = zr(jifre+3*iterat+1-1)
        endif
    else if (info.eq.'VMAXI') then
        if (action .eq. 'E') then
            zr(jifre+3*iterat+2-1) = valr(1)
        else
            valr(1) = zr(jifre+3*iterat+2-1)
        endif
    else if (info.eq.'VCHAR') then
        if (action .eq. 'E') then
            zr(jifre+3*iterat+3-1) = valr(1)
        else
            valr(1) = zr(jifre+3*iterat+3-1)
        endif
!
    else if (info.eq.'VRELA_TOUS') then
        if (action .eq. 'L') then
            do 10 iter = 0, iterat
                valr(iter+1) = zr(jifre+3*iter+1-1)
10          continue
        else
            call assert(.false.)
        endif
    else if (info.eq.'VMAXI_TOUS') then
        if (action .eq. 'L') then
            do 15 iter = 0, iterat
                valr(iter+1) = zr(jifre+3*iter+2-1)
15          continue
        else
            call assert(.false.)
        endif
    else if (info.eq.'VCHAR_TOUS') then
        if (action .eq. 'L') then
            do 20 iter = 0, iterat
                valr(iter+1) = zr(jifre+3*iter+3-1)
20          continue
        else
            call assert(.false.)
        endif
    else
        call assert(.false.)
    endif
!
    call jedema()
!
end subroutine
