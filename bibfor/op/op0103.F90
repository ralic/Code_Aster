subroutine op0103()
    implicit none
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!------------------------------------------------------------------
!                   MODI_MODELE
!------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/ajlipa.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/getvis.h"
#include "asterfort/gcncon.h"
#include "asterfort/fetskp.h"
#include "asterfort/fetcrf.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/assert.h"
#include "asterfort/asmpi_info.h"
    character(len=8) :: model, methode, sd_partit1
    character(len=24) :: kdis
    integer :: ib,nbproc,n1,nbpart
    mpi_int :: mrank, msize
!------------------------------------------------------------------
!
    call jemarq()
!
!   -- modification de la partition  :
!   ---------------------------------------------------
    call getvid(' ', 'MODELE', scal=model, nbret=ib)
    ASSERT(ib.eq.1)
    call asmpi_info(rank=mrank, size=msize)
    nbproc = to_aster_int(msize)
    call getvtx('PARTITION', 'PARALLELISME', iocc=1, scal=kdis, nbret=n1)
    ASSERT(n1.eq.1)
    if (nbproc.eq.1) kdis='CENTRALISE'

!   -- pour MODI_MODELE : 'GROUP_ELEM+' == 'GROUP_ELEM'
    if (kdis.eq.'GROUP_ELEM+') kdis='GROUP_ELEM'

    sd_partit1=' '
    if (kdis.eq.'SOUS_DOMAINE') then
        call gcncon('_', sd_partit1)
        call getvtx('PARTITION', 'METHODE', iocc=1, scal=methode, nbret=n1)
        ASSERT(n1.eq.1)
        call getvis('PARTITION', 'NB_PART', iocc=1, scal=nbpart, nbret=n1)
        if (n1.eq.0) nbpart=nbproc
        call fetskp(model,methode,nbpart)
        call fetcrf(sd_partit1,model,nbpart)
    endif

    call ajlipa(model, 'G', kdis, sd_partit1)

!
    call jedema()
end subroutine
