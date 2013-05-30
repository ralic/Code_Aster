subroutine apzonr(sdappa, izone, questz, valr)
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
    include 'asterfort/apmmvd.h'
    include 'asterfort/assert.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=19) :: sdappa
    integer :: izone
    real(kind=8) :: valr
    character(len=*) :: questz
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (UTILITAIRE)
!
! INFO. DE TYPE REEL SUR LA ZONE COURANTE
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  IZONE  : NUMERO DE LA ZONE
! IN  QUESTI : QUESTION
! OUT VALR   : REPONSE A LA QUESTION
!
!
!
!
    integer :: ifm, niv
    character(len=24) :: apinzr
    integer :: jpinzr
    character(len=24) :: questi
    integer :: zinzr
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('APPARIEMENT', ifm, niv)
!
! --- ACCES SDAPPA
!
    apinzr = sdappa(1:19)//'.INZR'
    call jeveuo(apinzr, 'L', jpinzr)
!
! --- INITIALISATIONS
!
    questi = questz
    valr = 0.d0
    zinzr = apmmvd('ZINZR')
!
! --- REPONSE
!
    if (questi .eq. 'TOLE_APPA') then
        valr = zr(jpinzr+zinzr*(izone-1)+4 -1)
    else if (questi.eq.'TOLE_PROJ_EXT') then
        valr = zr(jpinzr+zinzr*(izone-1)+5 -1)
    else
        call assert(.false.)
    endif
!
    call jedema()
!
end subroutine
