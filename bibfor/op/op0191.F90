subroutine op0191()
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! --------------------------------------------------------------------------------------------------
!
!     COMMANDE : MODI_REPERE
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
!
#include "asterf_types.h"
#include "asterc/getres.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/modirepcham.h"
#include "asterfort/modirepresu.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: n0
    character(len=16) :: concep, nomcmd
    character(len=19) :: resuou, resuin
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
!   Récupération du nom de la commande
!       le nom utilisateur du résultat : resuou
!       le nom du concept résultat     : concep (en cas de reuse)
!       le nom de la commande          : nomcmd
    call getres(resuou, concep, nomcmd)
!
    call getvid(' ', 'CHAM_GD',  scal=resuin, nbret=n0)
    if ( n0.ne.0 ) then
        call modirepcham(resuou, resuin )
        goto 999
    endif
    call getvid(' ', 'RESULTAT', scal=resuin, nbret=n0)
    if ( n0.ne.0 ) then
        call modirepresu(resuou, resuin )
        goto 999
    endif
!
999 continue
    call jedema()
end subroutine
