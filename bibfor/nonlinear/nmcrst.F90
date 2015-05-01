subroutine nmcrst(sdstat)
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
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    character(len=24) :: sdstat
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - INITIALISATIONS)
!
! CREATION DE LA SD STATISTIQUES
!
! ----------------------------------------------------------------------
!
!
! IN  SDSTAT : SD STATISTIQUES
!
!
!
!
    integer :: nbstat
    parameter    (nbstat=20)
!
    character(len=24) :: stvip, stvit, stvin
    integer :: jstvip, jstvit, jstvin
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CREATION SD STATISTIQUES'
    endif
!
! --- NOM DES SDS
!
    stvit = sdstat(1:19)//'.VLIT'
    stvip = sdstat(1:19)//'.VLIP'
    stvin = sdstat(1:19)//'.VLIN'
    call wkvect(stvit, 'V V I', nbstat, jstvit)
    call wkvect(stvip, 'V V I', nbstat, jstvip)
    call wkvect(stvin, 'V V I', nbstat, jstvin)
!
    call jedema()
end subroutine
