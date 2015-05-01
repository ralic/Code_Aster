subroutine op0052()
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
! person_in_charge: nicolas.sellenet at edf.fr
! ----------------------------------------------------------------------
!  COMMANDE CALC_CHAMP
! ----------------------------------------------------------------------
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/ccbcop.h"
#include "asterfort/ccchut.h"
#include "asterfort/cclopu.h"
#include "asterfort/ccvrpu.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/onerrf.h"
#include "asterfort/refdcp.h"
#include "asterfort/rsutnu.h"
#include "asterfort/utmess.h"
    character(len=6) :: nompro
    parameter  (nompro='OP0052')
!
    integer :: ifm, niv, ibid, n0, iret, np, nc
    integer :: nbordr, nbropt
!
    real(kind=8) :: prec
!
    character(len=8) :: resuc1, resuco, crit
    character(len=16) :: compex, k16bid, concep, nomcmd
    character(len=19) :: lisord, lisopt
!
    call jemarq()
!
    lisopt = '&&'//nompro//'.LIS_OPTION'
    lisord = '&&'//nompro//'.NUME_ORDRE'
!
    call infmaj()
    call infniv(ifm, niv)
!
!     ON STOCKE LE COMPORTEMENT EN CAS D'ERREUR AVANT MNL : COMPEX
!     PUIS ON PASSE DANS LE MODE "VALIDATION DU CONCEPT EN CAS D'ERREUR"
    call onerrf(' ', compex, ibid)
    call onerrf('EXCEPTION+VALID', k16bid, ibid)
!
!     RECUPERATION DES NOMS DES SD RESULTAT
    call getres(resuc1, concep, nomcmd)
    call getvid(' ', 'RESULTAT', scal=resuco, nbret=n0)
!
    call getvr8(' ', 'PRECISION', scal=prec, nbret=np)
    call getvtx(' ', 'CRITERE', scal=crit, nbret=nc)
    call rsutnu(resuco, ' ', 0, lisord, nbordr,&
                prec, crit, iret)
    if (iret .eq. 10) then
        call utmess('A', 'CALCULEL4_8', sk=resuco)
        goto 9999
    endif
    if (iret .ne. 0) then
        call utmess('A', 'ALGORITH3_41')
        goto 9999
    endif
!
!     ON VEUT INTERDIRE LA REENTRANCE DE LA COMMANDE SI
!     ON UTILISE L'UN DES MOTS CLES : MODELE, CARAEL_ELEM,
!     CHAM_CHMATER OU EXCIT
    if (resuco .eq. resuc1) then
        call ccvrpu(resuco, lisord, nbordr)
    endif
!
!     FABRICATION DE LA LISTE DES OPTIONS
    call cclopu(resuco, resuc1, lisord, nbordr, lisopt,&
                nbropt)
!
!     APPEL A LA ROUTINE PREPARANT L'APPEL A CALCOP
    call ccbcop(resuco, resuc1, lisord, nbordr, lisopt,&
                nbropt)
!
    call jedetr(lisopt)
!
    call ccchut(resuco, resuc1, lisord, nbordr)
!
9999  continue
!     ON REMET LE MECANISME D'EXCEPTION A SA VALEUR INITIALE
    call onerrf(compex, k16bid, ibid)
!
    call refdcp(resuco, resuc1)
!
    call jedema()
!
end subroutine
