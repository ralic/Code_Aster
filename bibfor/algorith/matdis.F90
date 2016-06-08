subroutine matdis(matd)
    implicit none
#include "asterc/getexm.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/utmess.h"
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
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
!
! out K3 matd  : 'OUI' / 'NON'  (MATR_DISTRIBUEE)
!
! ----------------------------------------------------------------------
    character(len=3) :: matd
! ----------------------------------------------------------------------
    integer :: ibid, eximc, eximo
    character(len=8) :: modele, partit
    character(len=19) :: ligrmo

!   -- MATR_DISTRIBUEE ?
    matd = 'NON'
    eximc = getexm('SOLVEUR', 'MATR_DISTRIBUEE')
    eximo = getexm(' ', 'MODELE')
    if (eximc .eq. 1 .and. eximo .eq. 1) then
        call getvtx('SOLVEUR', 'MATR_DISTRIBUEE', iocc=1, scal=matd, nbret=ibid)
        if (ibid .eq. 0) then
            matd = 'NON'
        endif
        call getvid(' ', 'MODELE', scal=modele, nbret=ibid)
        ASSERT(ibid .eq. 1)
        ligrmo = modele//'.MODELE'
        call dismoi('PARTITION', ligrmo, 'LIGREL', repk=partit)
        if (partit.eq.' ' .and. matd.eq.'OUI') then
            matd = 'NON'
            call utmess('I', 'ASSEMBLA_3')
        endif
    endif
end subroutine
