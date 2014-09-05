subroutine vpini2(eigsol, lcomod, nbvecg, nfreqg, nbpark,&
                  nbpari, nbparr, vecrer, vecrei, vecrek,&
                  vecvp, mxresf)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! -------------------------------------------------------------------------------------------------
! CREATION ET INITIALISATION DES SDS RESULTATS DE OP0045.
! RQ. ILS SONT DETRUITS DANS VPPOST.
! -------------------------------------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/isnnem.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/vecini.h"
#include "asterfort/vecink.h"
#include "asterfort/vecint.h"
#include "asterfort/vplecs.h"
#include "asterfort/wkvect.h"
!
!
! --- INPUT
!
    integer, intent(in) :: nbvecg, nfreqg, nbpark, nbpari, nbparr
    aster_logical , intent(in) :: lcomod
    character(len=19), intent(in) :: eigsol
    character(len=24), intent(in) :: vecrer, vecrei, vecrek, vecvp
!
! --- OUTPUT
!
    integer, intent(out) :: mxresf
!
! --- INPUT/OUTPUT
! None
!
! --- VARIABLES LOCALES
!
    integer :: ibid, nbvect, nfreq, iauxr, iauxi, iauxk, lresui, lresur, lresuk
    integer :: lraide, neq, indf, lvec
    real(kind=8) :: rbid, undf
    character(len=1) :: k1bid
    character(len=8) :: k8bid
    character(len=9) :: k9bid
    character(len=14) :: k14bid
    character(len=16) :: k16bid
    character(len=19) :: k19bid, raide
    character(len=24) :: kzero
    aster_logical :: lbid, lc, lkr, lns
!
! -----------------------
! --- CORPS DE LA ROUTINE
! -----------------------
!
!
! --  INITS.
    call jemarq()
    undf=r8vide()
    indf=isnnem()
    kzero=' '
!
! --  LECTURE DES PARAMETRES MODAUX
    call vplecs(eigsol, ibid, ibid, ibid, ibid,&
                ibid, ibid, nbvect, ibid, nfreq,&
                ibid, rbid, rbid, rbid, rbid,&
                rbid, rbid, rbid, rbid, rbid,&
                rbid, rbid, rbid, k1bid, k8bid,&
                k8bid, k9bid, k14bid, k14bid, k14bid,&
                k16bid, k16bid, k16bid, k16bid, k16bid,&
                k16bid, k19bid, k19bid, raide, k19bid,&
                lc, lkr, lns, lbid, lbid)
    call jeveuo(raide//'.&INT', 'E', lraide)
    neq = zi(lraide+2)
!
! --  CREATION DES SDS 
    if (lcomod) then
        if (lc .or. lns .or. (.not.lkr)) then
            ASSERT(.false.)
        endif
        mxresf = nfreqg
        iauxr=nbparr*nbvecg
        iauxi=nbpari*nbvecg
        iauxk=nbpark*nbvecg
    else
        mxresf = nfreq
        iauxr=nbparr*nbvect
        iauxi=nbpari*nbvect
        iauxk=nbpark*nbvect
    endif
!
    call wkvect(vecrei, 'V V I', iauxi, lresui)
    call vecint(iauxi, indf, zi(lresui))
    call wkvect(vecrer, 'V V R', iauxr, lresur)
    call vecini(iauxr, undf, zr(lresur))
    call wkvect(vecrek, 'V V K24', iauxk, lresuk)
    call vecink(iauxk, kzero, zk24(lresuk))
!
!
    if (lkr .and. (.not.lc) .and. (.not.lns)) then
        if (lcomod) then
            call wkvect(vecvp, 'V V R', neq*nbvecg, lvec)
        else
            call wkvect(vecvp, 'V V R', neq*nbvect, lvec)
        endif
    else
! --  CAS GENERALISE COMPLEXE OU QUADRATIQUE REEL ET COMPLEXE ---
        if (lcomod) then
            ASSERT(.false.)
        endif
        call wkvect(vecvp, 'V V C', neq*nbvect, lvec)
    endif
!
    call jedema()
!
!     FIN DE VPINI2
!
end subroutine
