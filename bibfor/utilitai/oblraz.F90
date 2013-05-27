subroutine oblraz(sdlist)
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
    implicit      none
    include       'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/obgeti.h'
    include 'asterfort/obgett.h'
    include 'asterfort/oblsai.h'
    character(len=24) :: sdlist
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (GESTION STRUCT - LISTE DE STRUCTS)
!
! DESACTIVATION DE TOUS LES STRUCTS DANS LA LISTE
!
! ----------------------------------------------------------------------
!
! IN  SDLIST : NOM DE LA LISTE
!
! ----------------------------------------------------------------------
!
    integer :: istru, nbstru
    character(len=24) :: typesd
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- VERIFICATION
!
    call obgett(sdlist, typesd)
    if (typesd .ne. 'LISTE_STRUCTS') call assert(.false.)
!
! --- DESACTIVATION
!
    call obgeti(sdlist, 'NBRE_STRUCTS', nbstru)
    do 10 istru = 1, nbstru
        call oblsai(sdlist, istru, .false.)
10  end do
!
    call jedema()
end subroutine
