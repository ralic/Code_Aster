subroutine medomm(model, mate, cara_elem)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/rcmfmc.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=*), intent(out) :: model
    character(len=*), intent(out) :: mate
    character(len=*), intent(out) :: cara_elem
!
! --------------------------------------------------------------------------------------------------
!
! Mechanics - Initializations
!
! Get parameters from command file
!
! --------------------------------------------------------------------------------------------------
!
! Out model            : name of model
! Out mate             : name of material characteristics (field)
! Out cara_elem        : name of elementary characteristics (field)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nocc
    character(len=8) :: concept, answer
!
! --------------------------------------------------------------------------------------------------
!
    model     = ' '
    mate      = ' '
    cara_elem = ' '
!
! - Get model
!
    concept = ' '
    call getvid(' ', 'MODELE', scal=concept, nbret=nocc)
    ASSERT(nocc .ne. 0)
    model = concept
!
! - Get material characteristics field
!
    concept = ' '
    call getvid(' ', 'CHAM_MATER', scal=concept, nbret=nocc)
    call dismoi('BESOIN_MATER', model, 'MODELE', repk=answer)
    if ((nocc.eq.0) .and. (answer .eq. 'OUI')) then
        call utmess('A', 'MECHANICS1_40')
    endif
    if (nocc .ne. 0) then
        call rcmfmc(concept, mate)
    else
        mate = ' '
    endif
!
! - Get elementary characteristics
!
    concept = ' '
    call getvid(' ', 'CARA_ELEM', scal=concept, nbret=nocc)
    call dismoi('EXI_RDM', model, 'MODELE', repk=answer)
    if ((nocc.eq.0) .and. (answer .eq. 'OUI')) then
        call utmess('A', 'MECHANICS1_39')
    endif
    cara_elem = concept
!
end subroutine
