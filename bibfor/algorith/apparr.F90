subroutine apparr(sdappa, questz, valr)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=19) :: sdappa
    real(kind=8) :: valr
    character(len=*) :: questz
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (UTILITAIRE)
!
! CONSTANTES - PARAMETRE REEL
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  QUESTI : QUESTION
!               PROJ_NEWT_RESI     TOLERANCE POUR NEWTON PROJECTION
! OUT VALR   : REPONSE A LA QUESTION
!
!
!
!
    integer :: ifm, niv
    character(len=24) :: apinfr
    integer :: jpinfr
    character(len=16) :: questi
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('APPARIEMENT', ifm, niv)
!
! --- ACCES SDAPPA
!
    apinfr = sdappa(1:19)//'.INFR'
    call jeveuo(apinfr, 'L', jpinfr)
!
! --- INITIALISATIONS
!
    valr = 0.d0
    questi = questz
!
! --- QUESTION
!
    if (questi .eq. 'PROJ_NEWT_RESI') then
        valr = zr(jpinfr+1-1)
    else
        call assert(.false.)
    endif
!
    call jedema()
!
end subroutine
