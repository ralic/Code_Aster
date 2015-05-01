subroutine op0021()
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     COMMANDE:  DEFI_PARTITION
!----------------------------------------------------------------------
    implicit none

#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/fetcrf.h"
#include "asterfort/fetskp.h"
#include "asterfort/getvis.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: ifm, niv, ibid
    character(len=8) :: result
    character(len=16) :: k16bid, nomcmd
!-----------------------------------------------------------------------
    call jemarq()

    call getvis(' ', 'INFO', scal=niv, nbret=ibid)
    call infmaj()
    call infniv(ifm, niv)

!   -- creation des group_ma dans le maillage :
    call fetskp()

!   -- creation de la sd_partit :
    call getres(result, k16bid, nomcmd)
    call fetcrf(result)

    call jedema()
end subroutine
