subroutine op0176()
    implicit   none
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
!
!     BUT:
!       OPERATEUR D'EXTRACTION
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
!
!      SORTIE :
!-------------
!
! ......................................................................
!
!
!
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterfort/ajrefd.h"
#include "asterfort/dyarc0.h"
#include "asterfort/extrs1.h"
#include "asterfort/extrs2.h"
#include "asterfort/infmaj.h"
#include "asterfort/irecri.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsinfo.h"
#include "asterfort/rsnopa.h"
#include "asterfort/rsorac.h"
#include "asterfort/titre.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=6) :: nompro
    parameter ( nompro = 'OP0176' )
!
    integer :: ibid, nbordr, jordr, nbexcl, jexcl, nbarch, jarch
    integer :: nbac, nbpa, jpa, iret, nbnosy, nbpara, nbrest
    integer :: izero, nive, versio, iul
!
    real(kind=8) :: r8b
!
    character(len=1) :: cecr
    character(len=8) :: k8b, form, formar, tabid(1)
    character(len=8) :: noma, nomo
    character(len=16) :: typcon, nomcmd
    character(len=19) :: resuou, resuin
    character(len=24) :: lisarc, lichex, nompar
!
    logical :: fals, true, lbid, lrest
!
    complex(kind=8) :: c16b
    integer :: iarg, nmail, nmode
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    true = .true.
    fals = .false.
!
    lisarc = '&&'//nompro//'.LISTE.ARCH'
    lichex = '&&'//nompro//'.LISTE.CHAM'
    nompar = '&&'//nompro//'.NOMS_PARA '
    formar = '1PE12.5'
    nive = 3
    versio = 0
!
    call getres(resuou, typcon, nomcmd)
!
    call getvid(' ', 'RESULTAT', 1, iarg, 1,&
                resuin, ibid)
!
!     --- CHAMPS ---
!
    call jelira(resuin//'.DESC', 'NOMMAX', nbnosy, k8b)
    if (nbnosy .eq. 0) goto 9997
!
!     --- NOMBRE DE NUMERO D'ORDRE ---
!
    call rsorac(resuin, 'LONUTI', ibid, r8b, k8b,&
                c16b, r8b, k8b, nbordr, 1,&
                ibid)
    call wkvect('&&'//nompro//'.NUME_ORDRE', 'V V I', nbordr, jordr)
    call rsorac(resuin, 'TOUT_ORDRE', ibid, r8b, k8b,&
                c16b, r8b, k8b, zi(jordr), nbordr,&
                ibid)
!
!     --- ACCES ET PARAMETRES ---
!
    call rsnopa(resuin, 2, nompar, nbac, nbpa)
    nbpara = nbac + nbpa
    call jeveuo(nompar, 'L', jpa)
!
!     --- CHAMPS EXCLUS ET PAS D'ARCHIVAGE ---
!
    call dyarc0(resuin, nbnosy, nbarch, lisarc, nbexcl,&
                lichex)
!
!     --- MOT-CLE RESTREINT
    call getfac('RESTREINT', nbrest)
    lrest = .false.
    if (nbrest .gt. 0) then
        lrest = .true.
        call getvid('RESTREINT', 'MAILLAGE', 1, iarg, 1,&
                    noma, nmail)
        call getvid('RESTREINT', 'MODELE', 1, iarg, 1,&
                    nomo, nmode)
    endif
    if ((nbarch.eq.0) .and. (nbrest.eq.0)) then
        goto 9997
    else if ((nbarch.eq.0)) then
        jarch = jordr
        nbarch = nbordr
    else
        call jeveuo(lisarc, 'L', jarch)
    endif
    call jeveuo(lichex, 'L', jexcl)
!
!     --- ALLOCATION DE LA STRUCTURE SORTIE SI ELLE N'EXISTE PAS ---
!
    call jeexin(resuou//'.DESC', iret)
    if (iret .eq. 0) then
        call rscrsd('G', resuou, typcon, nbarch)
    endif
!
!
    if (resuin .eq. resuou) then
        if (lrest) call u2mess('F', 'PREPOST2_5')
        call extrs1(resuin, nbordr, zi(jordr), nbpara, zk16(jpa),&
                    nbarch, zi(jarch), nbexcl, zk16(jexcl), nbnosy)
    else
        call extrs2(resuin, resuou, typcon, lrest, noma,&
                    nomo, nbordr, zi(jordr), nbpara, zk16(jpa),&
                    nbarch, zi(jarch), nbexcl, zk16(jexcl), nbnosy)
    endif
!
    call titre()
!
!     --- IMPRESSION ---
!
    form = 'RESULTAT'
    iul = iunifi( 'MESSAGE' )
    call rsinfo(resuou, iul)
!
    call rsorac(resuou, 'LONUTI', ibid, r8b, k8b,&
                c16b, r8b, k8b, nbordr, 1,&
                ibid)
    call rsorac(resuou, 'TOUT_ORDRE', ibid, r8b, k8b,&
                c16b, r8b, k8b, zi(jordr), nbordr,&
                ibid)
    k8b = '        '
    cecr = 'T'
    izero = 0
    tabid(1) = ' '
    call irecri(resuou, form, iul, k8b, lbid,&
                izero, tabid, ' ', nbpara, zk16(jpa),&
                nbordr, zi(jordr), true, 'RESU', 1,&
                cecr, k8b, fals, izero, ibid,&
                izero, ibid, izero, k8b, fals,&
                r8b, fals, r8b, fals, fals,&
                formar, nive, versio)
!
9997  continue
!
!
!
!     -- CREATION DE L'OBJET .REFD SI NECESSAIRE:
!     -------------------------------------------
    call ajrefd(resuin, resuou, 'COPIE')
!
!
    call jedema()
!
end subroutine
