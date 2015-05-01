subroutine nmdoco(modele, carele, compor)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "jeveux.h"
#include "asterfort/cesvar.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    character(len=24) :: modele, carele
    character(len=24) :: compor
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! EXTENSION DE LA CARTE COMPORTEMENT
!   TRANSFO. EN CHAM_ELEM_S
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! I/O COMPOR : CARTE COMPORTEMENT ETENDUE EN CHAM_ELEM_S
!
!
!
!
    integer :: iret
    character(len=24) :: ligrmo
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- INITIALISATIONS
!
    call dismoi('NOM_LIGREL', modele, 'MODELE', repk=ligrmo)
!
! --- EXTENSION DU COMPORTEMENT : NOMBRE DE VARIABLES INTERNES
!
    call exisd('CHAM_ELEM_S', compor, iret)
    if (iret .eq. 0) then
        call cesvar(carele, compor, ligrmo, compor)
    endif
!
    call jedema()
end subroutine
