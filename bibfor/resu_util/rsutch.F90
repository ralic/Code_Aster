subroutine rsutch(nomsd, nomsy, iordr, nomcha, lverif)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsutrg.h"
    integer :: iordr
    character(len=*) :: nomsd, nomsy, nomcha
    aster_logical :: lverif
! ----------------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
!
! DETERMINATION DU NOM DU CHAMP19 CORRESPONDANT A NOMSD(IORDR,NOMSY)
! ----------------------------------------------------------------------
! IN  : NOMSD  : NOM DE LA STRUCTURE "RESULTAT"
! IN  : NOMSY  : NOM SYMBOLIQUE DU CHAMP
! IN  : IORDR  : NUMERO D'ORDRE DU CHAMP
! OUT : NOMCHA : NOM DU CHAMP
!      LE NOM EST DE LA FORME : 'NOMSD(1:8).III.JJJJJJ'
!      OU : III    : NUMERO ASSOCIE AU NOM SYMBOLIQUE
!           JJJJJJ : NUMERO DE RANGEMENT - 1
! IN  : LVERIF : .TRUE. : SI .TACH EST REMPLI, ON VERIFIE QUE LA
!                REGLE DE NOMMAGE EST VERIFIEE.
! ----------------------------------------------------------------------
!
!
    character(len=3) :: nuch
    character(len=6) :: chford
    character(len=19) :: resu19, nomch3, nomch2
    integer :: isymb, irang, jtach, nbordr
! ----------------------------------------------------------------------
!
    resu19 = nomsd
!
    call jenonu(jexnom(resu19//'.DESC', nomsy), isymb)
    ASSERT(isymb.gt.0)
    call codent(isymb, 'D0', nuch)
!
    call rsutrg(nomsd, iordr, irang, nbordr)
    ASSERT(irang.ge.0)
    ASSERT(irang.le.nbordr)
!
!
!     -- NOMCH2 : NOM QUE LE CHAMP DOIT AVOIR :
    if (irang .eq. 0) then
        call codent(nbordr, 'D0', chford)
    else
        call codent(irang-1, 'D0', chford)
    endif
    nomch2 = resu19(1:8)//'.'//nuch//'.'//chford
!
!
!     -- ON VERIFIE LA COHERENCE DE NOMCH2 AVEC L'OBJET .TACH :
    if (irang .gt. 0 .and. lverif) then
        call jeveuo(jexnum(resu19//'.TACH', isymb), 'L', jtach)
        nomch3 = zk24(jtach-1+irang)(1:19)
        if (nomch3 .ne. ' ') then
            ASSERT(nomch3.eq.nomch2)
        endif
    endif
!
    nomcha = nomch2
!
end subroutine
