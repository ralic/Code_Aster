subroutine oblsai(sdlist, istru, lacti)
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
    include       'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/obgeti.h'
    include 'asterfort/obgeto.h'
    include 'asterfort/obseti.h'
    character(len=24) :: sdlist
    integer :: istru
    logical :: lacti
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (GESTION STRUCT - LISTE DE STRUCTS)
!
! ACTIVATION D'UN STRUCT DANS LA LISTE - ACCES PAR INDICE
!
! ----------------------------------------------------------------------
!
!
! IN  SDLIST : NOM DE LA LISTE
! IN  ISTRU  : INDICE DU STRUCT DANS LA LISTE
! IN  LACTI  : .TRUE. SI ACTIVATION
!
! ----------------------------------------------------------------------
!
    character(len=24) :: lisact
    integer :: jlisac
    integer :: nbstru, nbacti, i
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES ACTIVATION
!
    call obgeto(sdlist, 'ACT_STRUCTS', lisact)
    call jeveuo(lisact, 'E', jlisac)
!
! --- ACTIVATION
!
    if (lacti) then
        zi(jlisac-1+istru) = 1
    else
        zi(jlisac-1+istru) = 0
    endif
!
! --- RECALCUL NOMBRE DE STRUCTS ACTIFS
!
    nbacti = 0
    call obgeti(sdlist, 'NBRE_STRUCTS', nbstru)
    do 10 i = 1, nbstru
        if (zi(jlisac-1+i) .eq. 1) nbacti = nbacti +1
10  end do
    call assert(nbacti.le.nbstru)
    call obseti(sdlist, 'NBRE_ACTI', nbacti)
!
    call jedema()
end subroutine
