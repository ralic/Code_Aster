subroutine rsnoch(nomsd, nomsy, iordr)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsutrg.h"
#include "asterfort/sdmpic.h"
#include "asterfort/utmess.h"
!
    integer :: iordr
    character(len=*) :: nomsd, nomsy
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
!  BUT : "NOTER" UN CHAMP DANS UNE SD_RESULTAT
!        ON VERIFIE QUE :
!           - LA PLACE EST LICITE (NOMSY OK ET IORDR<=NBORDR_MAX)
!           - LE CHAMP QUI VA ETRE "NOTE" DANS NOMSD
!             (REGLE DE NOMMAGE DE RSUTCH.F)
!             EXISTE REELLEMENT (EXISD.F)
! ----------------------------------------------------------------------
! IN  : NOMSD  : NOM DE LA STRUCTURE "RESULTAT"
! IN  : NOMSY  : NOM SYMBOLIQUE DU CHAMP A NOTER.
! IN  : IORDR  : NUMERO D'ORDRE DU CHAMP A NOTER.
! ----------------------------------------------------------------------
!
    character(len=16) :: noms2
    character(len=19) :: nomd2, chnote
    character(len=24) :: valk(2)
    character(len=8) :: repk
    integer :: normax, iretou, nordr, irang, jordr, iret, ibid, jtach
! ----------------------------------------------------------------------
!
    call jemarq()
!
    noms2 = nomsy
    nomd2 = nomsd
!
!
!     -- CALCUL ET VALIDATION DU NUMERO DE RANGEMENT :IRANG
!     -----------------------------------------------------
    call jelira(nomd2//'.ORDR', 'LONMAX', normax)
    call rsutrg(nomd2, iordr, iretou, nordr)
    if (iretou .eq. 0) then
        irang = nordr + 1
        if (irang .gt. normax) then
            call utmess('F', 'UTILITAI4_42')
        endif
        call jeecra(nomd2//'.ORDR', 'LONUTI', irang)
        call jeveuo(nomd2//'.ORDR', 'E', jordr)
!       -- ON VERIFIE QUE LE NOUVEAU IORDR EST SUPERIEUR
!          AU DERNIER IORDR DEJA STOCKE (IORDR CROISSANTS) :
        if (irang .gt. 1) then
            ASSERT(zi(jordr+irang-2).lt.iordr)
        endif
        zi(jordr-1+irang) = iordr
    else
        irang = iretou
    endif
!
!
!     -- ON VERIFIE LE NOM SYMBOLIQUE :
!     -------------------------------------------
    call jenonu(jexnom(nomd2//'.DESC', noms2), iret)
    if (iret .eq. 0) then
        valk(1) = noms2
        valk(2) = nomd2
        call utmess('F', 'UTILITAI4_43', nk=2, valk=valk)
    endif
!
!
!     -- CHNOTE : NOM QUE DOIT AVOIR LE CHAMP A NOTER :
!        (REGLE DE NOMMAGE DE RSUTCH.F)
!     -------------------------------------------------
    call rsexch(' ', nomd2, noms2, iordr, chnote,&
                iret)
!
!     -- ON VERIFIE L'EXISTENCE DE CHNOTE :
!     -------------------------------------------
    if (iret .eq. 100) then
        call utmess('F', 'UTILITAI_55', sk=chnote)
    endif
    ASSERT(iret.eq.0)
!
!
!     --- ON STOCKE LE NOM DU CHAMP :
!     ------------------------------
    call jenonu(jexnom(nomd2//'.DESC', noms2), ibid)
    call jeveuo(jexnum(nomd2//'.TACH', ibid), 'E', jtach)
!
    zk24(jtach+irang-1)(1:19) = chnote
!
!
!     -- SI LE CHAMP EST UN CHAM_ELEM MPI_INCOMPLET, ON LE COMPLETE:
!     --------------------------------------------------------------
    call dismoi('TYPE_CHAMP', chnote, 'CHAMP', repk=repk)
    if (repk(1:2) .eq. 'EL') call sdmpic('CHAM_ELEM', chnote)
!
    call jedema()
end subroutine
