subroutine sschge(nomacr)
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
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/assvec.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/me2mme.h"
#include "asterfort/rcmfmc.h"
#include "asterfort/ss2mm2.h"
#include "asterfort/ssvau1.h"
#include "asterfort/utmess.h"
!
    character(len=8) :: nomacr
! ----------------------------------------------------------------------
!     BUT: TRAITER LE MOT CLEF "CAS_CHARGE"
!             DE L'OPERATEUR MACR_ELEM_STAT
!     LES CHARGEMENTS SERONT CONDENSES LORS DE L'ASSEMBLAGE.
!
!     IN: NOMACR : NOM DU MACR_ELEM_STAT
!
!     OUT: LES OBJETS SUIVANTS DU MACR_ELEM_STAT SONT CALCULES:
!             NOMACR.LICA(NOMCAS)
!             NOMACR.LICH(NOMCAS)
!
! ----------------------------------------------------------------------
!
!
    character(len=1) :: base
    character(len=8) :: kbid
    character(len=14) :: nu
    character(len=8) :: nomo, materi, cara
    character(len=24) :: mate
    character(len=8) :: vprof, nomcas
    character(len=19) :: vecas, vecel
!
!-----------------------------------------------------------------------
    integer :: iadesm, ialica, ialich, iarefm, iavale, icas, iocc
    integer :: kk, n1, n2, nch, nddle, nddli, nddlt
    integer :: nocc
    real(kind=8) :: time
!-----------------------------------------------------------------------
    call jemarq()
!
    base = 'V'
    call jeveuo(nomacr//'.REFM', 'L', iarefm)
    nomo= zk8(iarefm-1+1)
    materi = zk8(iarefm-1+3)
    if (materi .ne. '        ') then
        call rcmfmc(materi, mate)
    else
        mate = ' '
    endif
    cara = zk8(iarefm-1+4)
    nu= zk8(iarefm-1+5)
    if (nu(1:8) .ne. nomacr) ASSERT(.false.)
!
    vecel = '&&VECEL            '
    vecas = nomacr//'.CHARMECA'
!
    call jeveuo(nomacr//'.DESM', 'E', iadesm)
    call jelira(nomacr//'.LICH', 'LONMAX', nch)
    nch= nch-1
    vprof= ' '
    nddle= zi(iadesm-1+4)
    nddli= zi(iadesm-1+5)
    nddlt= nddle+nddli
!
!     -- ON VERIFIE LA PRESENCE PARFOIS NECESSAIRE DE CARA_ELEM
!        ET CHAM_MATER :
    call getfac('CAS_CHARGE', nocc)
!
    do iocc = 1, nocc
!
        call getvtx('CAS_CHARGE', 'NOM_CAS', iocc=iocc, scal=nomcas, nbret=n1)
        call jecroc(jexnom(nomacr//'.LICA', nomcas))
        call jecroc(jexnom(nomacr//'.LICH', nomcas))
        call jenonu(jexnom(nomacr//'.LICA', nomcas), icas)
        call jeveuo(jexnum(nomacr//'.LICA', icas), 'E', ialica)
        call jeveuo(jexnum(nomacr//'.LICH', icas), 'E', ialich)
!
!       -- MISE A JOUR DE .LICH:
!       ------------------------
        call getvtx('CAS_CHARGE', 'SUIV', iocc=iocc, scal=kbid, nbret=n1)
        if (kbid(1:3) .eq. 'OUI') then
            zk8(ialich-1+1)= 'OUI_SUIV'
        else
            zk8(ialich-1+1)= 'NON_SUIV'
        endif
        call getvid('CAS_CHARGE', 'CHARGE', iocc=iocc, nbval=0, nbret=n1)
        if (-n1 .gt. nch) then
            call utmess('F', 'SOUSTRUC_40')
        endif
        call getvid('CAS_CHARGE', 'CHARGE', iocc=iocc, nbval=-n1, vect=zk8(ialich+1),&
                    nbret=n2)
!
!       -- INSTANT:
!       -----------
        call getvr8('CAS_CHARGE', 'INST', iocc=iocc, scal=time, nbret=n2)
!
!       -- CALCULS VECTEURS ELEMENTAIRES DU CHARGEMENT :
!       ------------------------------------------------
        call me2mme(nomo, -n1, zk8(ialich+1), mate, cara,&
                    time, vecel, 0, base)
        call ss2mm2(nomo, vecel, nomcas)
!
!        -- ASSEMBLAGE:
        call assvec('V', vecas, 1, vecel, [1.d0],&
                    nu, vprof, 'ZERO', 1)
!
!       -- RECOPIE DE VECAS.VALE DANS .LICA(1:NDDLT) :
        call jeveuo(vecas//'.VALE', 'L', iavale)
        do kk = 1, nddlt
            zr(ialica-1+kk)=zr(iavale-1+kk)
        end do
!
!       -- CONDENSATION DE .LICA(1:NDDLT) DANS .LICA(NDDLT+1,2*NDDLT) :
        call ssvau1(nomacr, ialica, ialica+nddlt)
!
!       -- ON COMPTE LES CAS DE CHARGE EFFECTIVEMENT CALCULES:
        zi(iadesm-1+7) = icas
!
        call detrsd('VECT_ELEM', vecel)
        call detrsd('CHAMP_GD', vecas)
    end do
!
!
    call jedema()
end subroutine
