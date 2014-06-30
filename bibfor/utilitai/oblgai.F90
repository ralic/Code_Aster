subroutine oblgai(sdlist, istru, lacti)
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/obgeto.h"
#include "asterfort/obgett.h"
    character(len=24) :: sdlist
    integer :: istru
    logical(kind=1) :: lacti
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (GESTION STRUCT - LISTE DE STRUCTS)
!
! DIT SI LE STRUCT EST ACTIF OU PAS - ACCES PAR INDICE
!
! ----------------------------------------------------------------------
!
!
! IN  SDLIST : NOM DE LA LISTE
! IN  ISTRU  : INDICE DU STRUCT DANS LA LISTE
! OUT LACTI  : .TRUE. SI STRUCT ACTIVE
!
! ----------------------------------------------------------------------
!
    character(len=24) :: lisact
    integer :: jlisac
    integer :: la
    character(len=24) :: typesd
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- VERIFICATIONS
!
    call obgett(sdlist, typesd)
    if (typesd .ne. 'LISTE_STRUCTS') ASSERT(.false.)
!
! --- ACTIVATION
!
    call obgeto(sdlist, 'ACT_STRUCTS', lisact)
    call jeveuo(lisact, 'L', jlisac)
!
! --- ACTIVATION ?
!
    la = zi(jlisac-1+istru)
    if (la .eq. 0) then
        lacti = .false.
    else if (la.eq.1) then
        lacti = .true.
    else
        ASSERT(.false.)
    endif
!
    call jedema()
end subroutine
