subroutine op0021()
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
!     COMMANDE:  DEFI_PARTITION
!----------------------------------------------------------------------
    implicit none

#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/fetcrf.h"
#include "asterfort/fetskp.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/getvid.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/asmpi_info.h"
    integer :: ifm, niv, ibid, nbpart, nbproc, n1
    character(len=8) :: sd_partit1, model
    character(len=16) :: k16bid, nomcmd, methode
    mpi_int :: mrank, msize
!-----------------------------------------------------------------------
    call jemarq()
    call asmpi_info(rank=mrank, size=msize)
    nbproc = to_aster_int(msize)

    call getvis(' ', 'INFO', scal=niv, nbret=ibid)
    call infmaj()
    call infniv(ifm, niv)

    call getres(sd_partit1, k16bid, nomcmd)
    call getvid(' ', 'MODELE', scal=model)
    call getvtx(' ', 'METHODE', scal=methode)
    call getvis(' ', 'NB_PART', scal=nbpart, nbret=n1)
    if (n1.eq.0) nbpart=nbproc


    call fetskp(model,methode,nbpart)
    call fetcrf(sd_partit1,model,nbpart)

    call jedema()
end subroutine
