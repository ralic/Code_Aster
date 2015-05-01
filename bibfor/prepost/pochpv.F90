subroutine pochpv(trange, nbbloc, tdebut, tfin, offset,&
                  trepos, nbclas, nomres, loptio)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/statim.h"
#include "asterfort/stchpv.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: trange, nomres
! ----------------------------------------------------------------------
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
!     CALCUL ET IMPRESSION DES STATISTIQUES DE CHOC VIBRATIONS USURE
!     (ALGORITHME CALCUL DYNAMIQUE TEMPOREL A PAS VARIABLE)
!
! ----------------------------------------------------------------------
    character(len=19) :: nomk19
    aster_logical :: loptio
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!
!     --- RECUPERATION DES VECTEURS CONTENANT LES RESULTATS ---
!-----------------------------------------------------------------------
    integer :: idvint, idwk1, idwk2, idwk3, idwk4, nbbloc
    integer :: nbchoc, nbclas, nbpt
    real(kind=8) :: offset, tdebut, tfin, tmax, tmin, trepos
    real(kind=8), pointer :: vcho(:) => null()
    real(kind=8), pointer :: fcho(:) => null()
    integer, pointer :: desc(:) => null()
    real(kind=8), pointer :: dloc(:) => null()
    character(len=8), pointer :: inti(:) => null()
    integer, pointer :: icho(:) => null()
    real(kind=8), pointer :: disc(:) => null()
    character(len=8), pointer :: ncho(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    nomk19='                   '
    nomk19(1:8)=trange
!
    call jeveuo(nomk19//'.DESC', 'L', vi=desc)
    nbchoc = desc(3)
!
    call jeveuo(nomk19//'.DISC', 'L', vr=disc)
    call jelira(nomk19//'.DISC', 'LONMAX', nbpt)
    tmax = disc(nbpt)
    tmin = disc(1)
    if (tfin .gt. tmax) tfin = tmax
    if (tdebut .lt. tmin) tdebut = tmin
    if (tdebut .ge. tfin) then
        call utmess('F', 'PREPOST4_47')
    endif
!
    call jeveuo(nomk19//'.FCHO', 'L', vr=fcho)
    call jeveuo(nomk19//'.DLOC', 'L', vr=dloc)
    call jeveuo(nomk19//'.VCHO', 'L', vr=vcho)
    call jeveuo(nomk19//'.ICHO', 'L', vi=icho)
    call jeveuo(nomk19//'.NCHO', 'L', vk8=ncho)
    call jeveuo(nomk19//'.INTI', 'L', vk8=inti)
    call jeveuo(nomk19//'.VINT', 'L', idvint)
!
    call wkvect('&&OP0130.WK1', 'V V R', nbpt, idwk1)
    call wkvect('&&OP0130.WK2', 'V V R', nbpt, idwk2)
    call wkvect('&&OP0130.WK3', 'V V R', nbpt, idwk3)
    call wkvect('&&OP0130.IWK4', 'V V I', nbpt, idwk4)
!
    if (loptio) then
        call stchpv(nbchoc, nbpt, disc, dloc, fcho,&
                    vcho, icho, zr(idwk1), zr(idwk2), zr(idwk3),&
                    zi(idwk4), tdebut, tfin, nbbloc, offset,&
                    ncho, inti, nomres)
    else
        call statim(nbchoc, nbpt, disc, fcho, vcho,&
                    zr( idvint), zr(idwk1), zr(idwk2), zr(idwk3), tdebut,&
                    tfin, nbbloc, offset, trepos, nbclas,&
                    ncho, inti, nomres)
    endif
!
    call jedetr('&&OP0130.WK1')
    call jedetr('&&OP0130.WK2')
    call jedetr('&&OP0130.WK3')
    call jedetr('&&OP0130.IWK4')
!
    call jedema()
end subroutine
