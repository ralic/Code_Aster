subroutine acevtr(noma, nomo, ityp, noms, itab,&
                  nn, idim)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!.======================================================================
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/testli.h"
#include "asterfort/utmess.h"
!
    character(len=24) :: noms(*)
    character(len=8) :: nomo, noma
    integer :: ityp, nn, idim, itab(*)
!
!   ARGUMENT        E/S  TYPE         ROLE
!.========================= DEBUT DES DECLARATIONS ====================
! -----  VARIABLES LOCALES
    integer :: repi
    character(len=16) :: nomte, nomodl, chaine
    character(len=19) :: nolig
    character(len=24) :: repk
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
!-----------------------------------------------------------------------
    integer :: ialiel, ierr, igrel, ima, iret, itypel
    integer :: kma, kmai, nbgrel, nel
!-----------------------------------------------------------------------
    call jemarq()
!
! --- INITIALISATIONS :
!     ---------------
    repi = 0
    if (idim .eq. 2) then
        chaine='2D_DIS_TR'
    else
        chaine='DIS_TR'
    endif
!
    nolig = nomo//'.MODELE'
    call jeexin(nolig//'.LIEL', iret)
    if (iret .ne. 0) then
        call jelira(nolig//'.LIEL', 'NUTIOC', nbgrel)
! le nombre de grels du LIGREL du modele est nul.
        ASSERT(nbgrel.gt.0)
        nomodl=' '
        ierr=0
        do igrel = 1, nbgrel
            call jeveuo(jexnum(nolig//'.LIEL', igrel), 'L', ialiel)
            call jelira(jexnum(nolig//'.LIEL', igrel), 'LONMAX', nel)
            itypel= zi(ialiel -1 +nel)
            call jenuno(jexnum('&CATA.TE.NOMTE', itypel), nomte)
            call dismoi('MODELISATION', nomte, 'TYPE_ELEM', repk=repk)
            nomodl=repk(1:16)
            if (nomodl .ne. chaine) then
                if (ityp .eq. 0) then
                    ierr=1
                    kmai=zi(ialiel)
                    goto 20
                else
                    do kma = 1, nn
                        if (ityp .eq. 1) then
                            call jenonu(jexnom(noma//'.NOMMAI', noms( kma)), ima)
                        else
                            ima=itab(kma)
                        endif
                        call testli(ima, zi(ialiel), nel-1, kmai, ierr)
                        if (ierr .eq. 1) goto 20
                    end do
                endif
            endif
        end do
    endif
 20 continue
!     IF (IERR.EQ.1)  WRITE(*,*) 'KMAI',KMAI,'IGREL',IGREL,
!    .       'NOMODL',NOMODL,'CHAINE',CHAINE
    if (ierr .eq. 1) then
        call utmess('F', 'DISCRETS_9')
    endif
    call jedema()
end subroutine
