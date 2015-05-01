subroutine nmvcaf(index, chainz, exicha, comz)
!
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
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    aster_logical :: exicha
    character(len=4) :: index
    character(len=*) :: comz, chainz
    character(len=19) :: chain
    character(len=14) :: com
!
!
! ----------------------------------------------------------------------
!  AFFECTATION D'UNE VARIABLE DE COMMANDE (UNIQUEMENT POUR PORTAGE)
! ----------------------------------------------------------------------
! IN         INDEX   K4  INDEX DE LA VARIABLE DE COMMANDE
! IN         CHAIN   K19 SD CHAMP_GD  DE LA VARI DE COMMANDE AFFECTEE
! IN         EXICHA   L  TRUE SI CHAMP EXISTE VRAIMENT (PAS UN DEFAUT)
! IN/JXVAR   COM     K14 SD VARI_COM
! ----------------------------------------------------------------------
!
!
!
!
    integer :: iret, iex
    character(len=19) :: chaout
!
    call jemarq()
    com = comz
    chain = chainz
!
!    AFFECTATION DU CHAMP
    call exisd('CHAMP_GD', chain, iret)
    ASSERT(iret.ne.0)
    chaout = com // '.' // index
    call copisd('CHAMP_GD', 'V', chain, chaout)
!
!    CHAMPS REELS (TRUE) OU PAR DEFAUT (FALSE)
    call jeexin(com//'.EXISTENCE', iret)
    if (iret .eq. 0) then
        call wkvect(com//'.EXISTENCE', 'V V L ', 4, iex)
    else
        call jeveuo(com//'.EXISTENCE', 'E', iex)
    endif
!
    if (index .eq. 'TEMP') then
        zl(iex+0) = exicha
!
    else if (index.eq.'HYDR') then
        zl(iex+1) = exicha
!
    else if (index.eq.'SECH') then
        zl(iex+2) = exicha
!
    endif
!
    call jedema()
!
!
!
!
end subroutine
