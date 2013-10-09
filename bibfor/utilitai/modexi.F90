subroutine modexi(modelz, nomodz, iexi)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!.======================================================================
    implicit none
!
!     MODEXI  -- SI LA MODELISATION NOMODZ EXISTE DANS LE MODELE
!                MODELZ ALORS IEXI = 1
!                       SINON IEXI = 0
!
!   ARGUMENT        E/S  TYPE         ROLE
!    MODELZ          IN    K*     NOM DU MODELE
!    NOMODZ          IN    K*     NOM DE LA MODELISATION
!    IEXI            OUT   R      = 1 SI LA MODELISATION EXISTE
!                                     DANS LE MODELE
!                                 = 0 SINON
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    character(len=*) :: modelz, nomodz
! -----  VARIABLES LOCALES
    integer :: repi
    character(len=8) :: modele
    character(len=16) :: nomte, nomodl
    character(len=19) :: nolig
    character(len=24) :: repk
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
!-----------------------------------------------------------------------
    integer :: ialiel, iexi, igrel, iret, itypel, l
    integer :: nbgrel, nel
!-----------------------------------------------------------------------
    call jemarq()
!
! --- INITIALISATIONS :
!     ---------------
    modele = modelz
    iexi = 0
    repi = 0
    l = len(nomodz)
!
    nolig = modele//'.MODELE'
    call jeexin(nolig//'.LIEL', iret)
    if (iret .ne. 0) then
        call jelira(nolig//'.LIEL', 'NUTIOC', nbgrel)
        if (nbgrel .le. 0) then
            call utmess('F', 'UTILITAI_1')
        endif
        nomodl=' '
        do 10 igrel = 1, nbgrel
            call jeveuo(jexnum(nolig//'.LIEL', igrel), 'L', ialiel)
            call jelira(jexnum(nolig//'.LIEL', igrel), 'LONMAX', nel)
            itypel= zi(ialiel -1 +nel)
            call jenuno(jexnum('&CATA.TE.NOMTE', itypel), nomte)
            call dismoi('MODELISATION', nomte, 'TYPE_ELEM', repk=repk)
            nomodl=repk(1:16)
            if (nomodl(1:l) .eq. nomodz(1:l)) then
                iexi = 1
                goto 20
            endif
 10     continue
 20     continue
    endif
    call jedema()
end subroutine
