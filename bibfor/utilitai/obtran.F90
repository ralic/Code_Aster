subroutine obtran(nomst1, nompa1, nomst2, nompa2)
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
    include      'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/obgetb.h'
    include 'asterfort/obgeti.h'
    include 'asterfort/obgetk.h'
    include 'asterfort/obgeto.h'
    include 'asterfort/obgetr.h'
    include 'asterfort/obpara.h'
    include 'asterfort/obsetb.h'
    include 'asterfort/obseti.h'
    include 'asterfort/obsetk.h'
    include 'asterfort/obseto.h'
    include 'asterfort/obsetr.h'
    character(len=24) :: nomst1, nomst2
    character(len=*) :: nompa1, nompa2
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (GESTION STRUCT)
!
! TRANSFERT D'UN PARAMETRE D'UN STRUCT A L'AUTRE
!
! ----------------------------------------------------------------------
!
!
! IN  NOMST1 : NOM DU STRUCT 1
! IN  NOMPA1 : NOM DU PARAMETRE DANS LE STRUCT 1
! IN  NOMST2 : NOM DU STRUCT 2
! IN  NOMPA2 : NOM DU PARAMETRE DANS LE STRUCT 2
!
! ----------------------------------------------------------------------
!
    character(len=24) :: valk, valo
    logical :: valb
    integer :: vali
    real(kind=8) :: valr
    character(len=1) :: typpa1, typpa2
    integer :: indice
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    indice = 0
    typpa1 = ' '
    typpa2 = ' '
!
! --- REPERAGE PARAMETRE DANS LA SD IN
!
    call obpara(nomst1, nompa1, indice, typpa1)
!
! --- LECTURE VALEUR DANS LA SD IN
!
    if (typpa1 .eq. 'B') then
        call obgetb(nomst1, nompa1, valb)
    else if (typpa1.eq.'I') then
        call obgeti(nomst1, nompa1, vali)
    else if (typpa1.eq.'R') then
        call obgetr(nomst1, nompa1, valr)
    else if (typpa1.eq.'K') then
        call obgetk(nomst1, nompa1, valk)
    else if (typpa1.eq.'O') then
        call obgeto(nomst1, nompa1, valo)
    else
        call assert(.false.)
    endif
!
! --- REPERAGE PARAMETRE DANS LA SD OUT
!
    call obpara(nomst2, nompa2, indice, typpa2)
    if (typpa1 .ne. typpa2) then
        write(6,*) 'TYPE DES PARAMETRES INCOMPATIBLES: ',typpa1,&
        typpa2
        call assert(.false.)
    endif
!
! --- ECRITURE VALEUR DANS LA SD OUT
!
    if (typpa1 .eq. 'B') then
        call obsetb(nomst2, nompa2, valb)
    else if (typpa1.eq.'I') then
        call obseti(nomst2, nompa2, vali)
    else if (typpa1.eq.'R') then
        call obsetr(nomst2, nompa2, valr)
    else if (typpa1.eq.'K') then
        call obsetk(nomst2, nompa2, valk)
    else if (typpa1.eq.'O') then
        call obseto(nomst2, nompa2, valo)
    else
        call assert(.false.)
    endif
!
    call jedema()
end subroutine
