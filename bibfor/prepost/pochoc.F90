subroutine pochoc(trange, nbbloc, tdebut, tfin, offset,&
                  trepos, nbclas, nomres, loptio)
    implicit none
#include "jeveux.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/statch.h"
#include "asterfort/statim.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: trange, nomres
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
!     CALCUL ET IMPRESSION DES STATISTIQUES DE CHOC
!     DEUX OPTIONS PREVUES STATISTIQUES POUR VIBRATIONS USURE
!     ET STATISTIQUES POUR LES IMPACTS SOUS SEISME
!
! ----------------------------------------------------------------------
    character(len=19) :: nomk19
    logical :: loptio
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!
!     --- RECUPERATION DES VECTEURS CONTENANT LES RESULTATS ---
!-----------------------------------------------------------------------
    integer :: iddesc, iddloc, idfcho, idiadh, idinst, idncho, idnint
    integer :: idvgli, idvint, idwk1, idwk2, idwk3, idwk4, nbbloc
    integer :: nbchoc, nbclas, nbpt, ifm, info
    real(kind=8) :: offset, tdebut, tfin, tmax, tmin, trepos
!-----------------------------------------------------------------------
    call jemarq()
    call infniv(ifm, info)
!
    nomk19='                   '
    nomk19(1:8)=trange
!
    call jeveuo(nomk19//'.DESC', 'L', iddesc)
    nbchoc = zi(iddesc+2)
!
    call jeveuo(nomk19//'.DISC', 'L', idinst)
    call jelira(nomk19//'.DISC', 'LONMAX', nbpt)
    write(ifm,*) ' NB DE PAS DE TEMPS :',nbpt
    tmax = zr(idinst+nbpt-1)
    tmin = zr(idinst)
    if (tfin .gt. tmax) tfin = tmax
    if (tdebut .lt. tmin) tdebut = tmin
    if (tdebut .ge. tfin) then
        call utmess('F', 'PREPOST4_47')
    endif
!
    call jeveuo(nomk19//'.FCHO', 'L', idfcho)
    call jeveuo(nomk19//'.DLOC', 'L', iddloc)
    call jeveuo(nomk19//'.VCHO', 'L', idvgli)
    call jeveuo(nomk19//'.ICHO', 'L', idiadh)
    call jeveuo(nomk19//'.NCHO', 'L', idncho)
    call jeveuo(nomk19//'.INTI', 'L', idnint)
    call jeveuo(nomk19//'.VINT', 'L', idvint)
!
    call wkvect('&&OP0130.WK1', 'V V R', nbpt, idwk1)
    call wkvect('&&OP0130.WK2', 'V V R', nbpt, idwk2)
    call wkvect('&&OP0130.WK3', 'V V R', nbpt, idwk3)
    call wkvect('&&OP0130.IWK4', 'V V I', nbpt, idwk4)
!
    if (loptio) then
        call statch(nbchoc, nbpt, zr(idinst), zr(iddloc), zr(idfcho),&
                    zr( idvgli), zi(idiadh), zr(idwk1), zr(idwk2), zr(idwk3),&
                    zi(idwk4), tdebut, tfin, nbbloc, offset,&
                    trepos, zk8(idncho), zk8(idnint), nomres)
    else
        call statim(nbchoc, nbpt, zr(idinst), zr(idfcho), zr(idvgli),&
                    zr( idvint), zr(idwk1), zr(idwk2), zr(idwk3), tdebut,&
                    tfin, nbbloc, offset, trepos, nbclas,&
                    zk8(idncho), zk8(idnint), nomres)
    endif
    call jedetr('&&OP0130.WK1')
    call jedetr('&&OP0130.WK2')
    call jedetr('&&OP0130.WK3')
    call jedetr('&&OP0130.IWK4')
!
    call jedema()
end subroutine
