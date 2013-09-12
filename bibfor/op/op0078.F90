subroutine op0078()
    implicit none
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
!
!     OPERATEUR REST_COND_TRAN
!
! ----------------------------------------------------------------------
!
!
!
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/tran78.h"
#include "asterfort/u2mess.h"
    character(len=8) :: nomres, resin
    character(len=16) :: nomcmd, typres, champ(4)
    integer :: i, j
    integer :: ir, nbcham
    integer :: iarg
!
!     -----------------------------------------------------------------
!
!
    call jemarq()
    call infmaj()
!
!     -----------------------------------------------------------------
!
!
    call getres(nomres, typres, nomcmd)
!
! --- PHASE DE TEST SUR LES CHAMPS A RESTITUER
!
    call getvtx(' ', 'NOM_CHAM', nbval=4, vect=champ, nbret=nbcham)
    if (nbcham .lt. 0) then
        call u2mess('E', 'ALGORITH9_44')
    else
        do 20 i = 1, nbcham
            do 10 j = i+1, nbcham
                if (champ(i) .eq. champ(j)) then
                    call u2mess('E', 'ALGORITH9_30')
                endif
10          continue
20      continue
    endif
!
    call getvid(' ', 'RESULTAT', scal=resin, nbret=ir)
    call tran78(nomres, typres, resin)
!
!
    call jedema()
end subroutine
