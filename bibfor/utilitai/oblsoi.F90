subroutine oblsoi(sdlist, idnvaz, nomstr)
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
!
#include "asterfort/assert.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/obgeto.h"
#include "asterfort/obgett.h"
    character(len=24) :: sdlist
    character(len=*) :: idnvaz
    character(len=24) :: nomstr
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (GESTION STRUCT - LISTE DE STRUCTS)
!
! SETUP D'UN STRUCT DANS UNE LISTE DE STRUCTS - ACCES PAR INDICE
!
! ----------------------------------------------------------------------
!
!
! IN  SDLIST : NOM DE LA LISTE
! IN  IDNVAL : VALEUR DU PARAMETRE IDENTIFIANT LE STRUCT
! IN  NOMSTR : NOM DU STRUCT A METTRE DANS LA LISTE
!
! ----------------------------------------------------------------------
!
    character(len=24) :: lisnom
    integer :: jlisno
    character(len=24) :: typesd, idnval
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- VERIFICATION
!
    call obgett(sdlist, typesd)
    if (typesd .ne. 'LISTE_STRUCTS') ASSERT(.false.)
    idnval = idnvaz
!
! --- SETUP DU STRUCT
!
    call obgeto(sdlist, 'NOM_STRUCTS', lisnom)
    call jecroc(jexnom(lisnom, idnval))
    call jeveuo(jexnom(lisnom, idnval), 'E', jlisno)
    zk24(jlisno) = nomstr
!
    call jedema()
end subroutine
