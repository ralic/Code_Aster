subroutine rsexch(kstop, nomsd, nomsy, iordr, chextr,&
                  icode)
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsutch.h"
#include "asterfort/rsutrg.h"
#include "asterfort/u2mesg.h"
    integer :: iordr, icode
    character(len=*) :: kstop, nomsd, nomsy, chextr
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
!      RECUPERATION DU NOM DU CHAMP-GD  CORRESPONDANT A:
!          NOMSD(IORDR,NOMSY).
! ----------------------------------------------------------------------
! IN  : KSTOP  : ' ' / 'F' : VOIR ICODE  (CI-DESSOUS)
! IN  : NOMSD  : NOM DE LA STRUCTURE "RESULTAT"
! IN  : NOMSY  : NOM SYMBOLIQUE DU CHAMP A CHERCHER.
! IN  : IORDR  : NUMERO D'ORDRE DU CHAMP A CHERCHER.
! OUT : CHEXTR : NOM DU CHAMP EXTRAIT (SI POSSIBLE)
!                SINON : '???' (CODE=110,101,102)
! OUT : ICODE  : CODE RETOUR  (SI KSTOP=' ') :
!    0 : LE CHAMP CHEXTR EST "POSSIBLE" ET IL EXISTE (EXISD)
!  100 : LE CHAMP CHEXTR EST "POSSIBLE" MAIS IL N'EXISTE PAS
!  110 : IL FAUT AGRANDIR NOMSD AVANT DE STOCKER IORDR
!  101 : LE NOM SYMBOLIQUE NOMSY EST INTERDIT DANS LA SD
!  102 : LE NUMERO D'ORDRE N'EXISTE PAS DANS LS SD ET IL
!        NE PEUT PAS ETRE AJOUTE CAR IL N'EST PAS SUPERIEUR
!        AU PLUS GRAND.
!  SI KSTOP='F' :
!    SI ICODE=100 => ERREUR <F> DESTINEE A L'UTILISATEUR
!    SI ICODE>100 => ERREUR <F> DESTINEE AU PROGRAMMEUR
! ----------------------------------------------------------------------
!
    character(len=16) :: noms2
    character(len=19) :: nomd2, chext2, chext3
    character(len=24) :: valk(3)
    integer :: iexi, irang, isymb, jtach, nbordr, nbormx, jordr
    real(kind=8) :: rbid
! ----------------------------------------------------------------------
    call jemarq()
    icode = -99
    noms2 = nomsy
    nomd2 = nomsd
    chextr = '???'
    ASSERT(kstop.eq.' '.or.kstop.eq.'F')
!
!
!     --- NOM SYMBOLIQUE PERMIS ?
    call jenonu(jexnom(nomd2//'.DESC', noms2), isymb)
    if (isymb .eq. 0) then
        icode=101
        goto 10
    endif
!
!
!     --- RECUPERATION DU NUMERO DE RANGEMENT ---
    call rsutrg(nomd2, iordr, irang, nbordr)
!
!
!     -- LE NUMERO DE RANGEMENT EXISTE :
!     -----------------------------------------
    if (irang .gt. 0) then
        call jeveuo(jexnum(nomd2//'.TACH', isymb), 'L', jtach)
        chext2 = zk24(jtach+irang-1)(1:19)
        if (chext2 .eq. ' ') then
            call rsutch(nomsd, noms2, iordr, chext2, .true.)
        else
            call rsutch(nomsd, noms2, iordr, chext3, .true.)
            ASSERT(chext2.eq.chext3)
        endif
!
!
!     --- LE NUMERO DE RANGEMENT N'EXISTE PAS :
!     -----------------------------------------
    else
        call jelira(nomd2//'.ORDR', 'LONMAX', nbormx)
        if (nbordr .ge. nbormx) then
            icode = 110
            goto 10
        endif
!
!       -- ON VERIFIE QUE LE NOUVEAU IORDR EST PLUS GRAND
!          QUE L'ANCIEN PLUS GRAND :
        if (nbordr .ge. 1) then
            call jeveuo(nomd2//'.ORDR', 'L', jordr)
            if (iordr .le. zi(jordr-1+nbordr)) then
                icode=102
                goto 10
            endif
        endif
!
        call rsutch(nomsd, noms2, iordr, chext2, .true.)
    endif
!
!
!     --- LA SD CHEXTR EXISTE-T-ELLE ? :
!     -----------------------------------------
    chextr = chext2
    ASSERT(chextr.ne.' ')
    call exisd('CHAMP_GD', chextr, iexi)
    if (iexi .gt. 0) then
        icode = 0
    else
        icode = 100
    endif
!
10  continue
    if (kstop .eq. 'F' .and. icode .ne. 0) then
        valk(1)=nomsd
        valk(2)=nomsy
        if (icode .eq. 100) then
            call u2mesg('F', 'CALCULEL_29', 2, valk, 1,&
                        iordr, 0, rbid)
        else
            call u2mesg('F', 'CALCULEL_29', 2, valk, 1,&
                        iordr, 0, rbid)
        endif
    endif
!
!
    call jedema()
end subroutine
