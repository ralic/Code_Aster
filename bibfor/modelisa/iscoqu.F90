subroutine iscoqu(nomo, numail, lcoque)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    character(len=8) :: nomo
    integer :: numail
    logical :: lcoque
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - LECTURE DONNEES)
!
! DETECTE SI UN ELEMENT EST DE TYPE COQUE_3D
!
! ----------------------------------------------------------------------
!
!
! IN  NOMO   : NOM DU MODELE
! IN  NUMAIL : NUMERO ABSOLU DE LA MAILLE
! OUT LCOQUE : .TRUE. SI COQUE_3D
!
!
!
!
    integer :: iret, igrel, iel
    integer :: ialiel, itypel
    integer :: nbgrel, nel, numai2
    character(len=8) :: nomte, k8bid
    character(len=19) :: ligrmo
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    lcoque = .false.
!
! --- LIGREL DU MODELE
!
    ligrmo = nomo(1:8)//'.MODELE'
    call jeexin(ligrmo//'.LIEL', iret)
    if (iret .eq. 0) then
        ASSERT(.false.)
    endif
!
! --- NOMBRE DE GREL
!
    call jelira(ligrmo(1:19)//'.LIEL', 'NUTIOC', nbgrel, k8bid)
!
! --- BOUCLE SUR LES GREL
!
    do 40 igrel = 1, nbgrel
!
! --- TYPE DU GREL COURANT
!
        call jeveuo(jexnum(ligrmo(1:19)//'.LIEL', igrel), 'L', ialiel)
        call jelira(jexnum(ligrmo(1:19)//'.LIEL', igrel), 'LONMAX', nel, k8bid)
        itypel = zi(ialiel-1+nel)
        call jenuno(jexnum('&CATA.TE.NOMTE', itypel), nomte)
!
! --- CAS DES COQUES_3D
!
        if ((nomte.eq.'MEC3QU9H') .or. (nomte.eq.'MEC3TR7H')) then
!
! --- BOUCLE DANS LE GREL
!
            do 30 iel = 1, nel - 1
                numai2 = zi(ialiel-1+iel)
                if (numai2 .eq. numail) then
                    lcoque = .true.
                    goto 40
                endif
30          continue
        endif
40  end do
!
    call jedema()
end subroutine
