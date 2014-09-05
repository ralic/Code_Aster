subroutine vpcals(eigsol, vecrer, vecrei, vecrek, vecvp,&
                  matopa, mxresf, neqact, nblagr, omemax,&
                  omemin, omeshi, solveu, vecblo, veclag,&
                  sigma, npivot, flage, nconv, vpinf,&
                  vpmax)
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
! ROUTINE EFFECTUANT LE CALCUL MODAL PARAMETRE DANS EIGSOL PAR LA METHODE DE SORENSEN
! -------------------------------------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8depi.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/freqom.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rectfc.h"
#include "asterfort/rectfr.h"
#include "asterfort/vecint.h"
#include "asterfort/vpbosc.h"
#include "asterfort/vpbost.h"
#include "asterfort/vpecri.h"
#include "asterfort/vplecs.h"
#include "asterfort/vpordi.h"
#include "asterfort/vpordo.h"
#include "asterfort/vpsorc.h"
#include "asterfort/vpsorn.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/wpsorc.h"
#include "asterfort/wpsorn.h"
#include "asterfort/wp3vec.h"
#include "asterfort/wp4vec.h"
#include "asterfort/wp5vec.h"
!
! --- INPUT
!
    integer, intent(in) :: mxresf, neqact, nblagr
    real(kind=8), intent(in) :: omemax, omemin, omeshi
    complex(kind=8), intent(in) :: sigma
    character(len=19), intent(in) :: eigsol, matopa, solveu
    character(len=24), intent(in) :: vecrer, vecrei, vecrek, vecvp, vecblo, veclag
!
! --- OUTPUT
!
    integer, intent(out) :: nconv
    real(kind=8), intent(out) :: vpinf, vpmax
    aster_logical , intent(out) :: flage
!
!
! --- INPUT/OUTPUT
!
    integer, intent(inout) :: npivot
!
!
! --- VARIABLES LOCALES
!
    integer :: ibid, imet, lamor, lmasse, lmatra, lraide, maxitr, nbvect, neq, nfreq
    integer :: lonwl, lselec, lresid, lworkd, lworkl, lworkv, ldsor, laux, lworkr
    integer :: lauc, laur, laul, ldiagr, lsurdr, lprod, lddl
    integer :: nfreq1, izero, mfreq, ifreq, ifm, niv, priram(8)
    integer :: lresui, lresur, lresuk, lvec
    real(kind=8) :: alpha, quapi2, omecor, precdc, precsh, rbid, rzero, tolsor
    character(len=1) :: appr
    character(len=8) :: method, k8bid
    character(len=9) :: k9bid
    character(len=14) :: k14bid
    character(len=16) :: k16bid, optiof, stoper, typres
    character(len=19) :: amor, k19bid, masse, raide
    character(len=24) :: kmetho, k24bid
    aster_logical :: lbid, lc, lkr, lns, lpg
!
! -----------------------
! --- CORPS DE LA ROUTINE
! -----------------------
!
!
! ---  INITS.
!
    call jemarq()
    call infniv(ifm, niv)
    if (niv .eq. 2) then
        priram(1) = 2
        priram(2) = 2
        priram(3) = 2
        priram(4) = 2
        priram(5) = 0
        priram(6) = 0
        priram(7) = 0
        priram(8) = 2
    else
        call vecint(8, 0, priram)
    endif
    quapi2=r8depi()*r8depi()
    izero=0
    rzero=0.d0
    kmetho='SORENSEN'
    nconv=0
    flage=.false.
    call jeveuo(vecrer, 'E', lresur)
    call jeveuo(vecrei, 'E', lresui)
    call jeveuo(vecrek, 'E', lresuk)
    call jeveuo(vecvp, 'E', lvec)
    call jeveuo(vecblo, 'L', lprod)
    call jeveuo(veclag, 'L', lddl)
!
! --- LECTURE DES DONNEES DE EIGSOL
!
    call vplecs(eigsol, ibid, maxitr, ibid, ibid,&
                ibid, ibid, nbvect, ibid, nfreq,&
                ibid, alpha, omecor, rbid, rbid,&
                precdc, precsh, rbid, rbid, rbid,&
                rbid, rbid, tolsor, appr, k8bid,&
                method, k9bid, k14bid, k14bid, k14bid,&
                k16bid, optiof, stoper, k16bid, k16bid,&
                typres, amor, masse, raide, k19bid,&
                lc, lkr, lns, lpg, lbid)
    ASSERT(method(1:8).eq.'SORENSEN')
!
! ---  DESCRIPTEURS MATRICES
    call jeveuo(raide//'.&INT', 'E', lraide)
    neq = zi(lraide+2)
    call jeveuo(masse//'.&INT', 'E', lmasse)
    if (lc) then
        call jeveuo(amor//'.&INT', 'E', lamor)
    else
        lamor=0
    endif
    call jeveuo(matopa//'.&INT', 'E', lmatra)
!
! --- PRE-ALLOCATIONS MEMOIRE
!
    lonwl = 3*nbvect**2+6*nbvect
    call wkvect('&&VPCALS.SELECT', 'V V L', nbvect, lselec)
!     ------------------------------------------------------------------
!     -------  SORENSEN PB GENERALISE REEL SYMETRIQUE  --------
!     ------------------------------------------------------------------
    if ((.not.lc) .and. lkr .and. (.not.lns)) then
        call wkvect('&&VPCALS.RESID', 'V V R', neq, lresid)
        call wkvect('&&VPCALS.VECT.WORKD', 'V V R', 3*neq, lworkd)
        call wkvect('&&VPCALS.VECT.WORKL', 'V V R', lonwl, lworkl)
        call wkvect('&&VPCALS.VECT.WORKV', 'V V R', 3*nbvect, lworkv)
        call wkvect('&&VPCALS.VAL.PRO', 'V V R', 2*(nfreq+1), ldsor)
        call wkvect('&&VPCALS.VECT.AUX', 'V V R', neq, laux)
!     --------------------------------- ---------------------------------
!     -------  SORENSEN PB GENERALISE COMPLEXE OU REEL NON SYM  --------
!     ------------------------------------------------------------------
    else if ((.not.lc) .and. (lns .or. (.not.lkr))) then
        call wkvect('&&VPCALS.RESID', 'V V C', neq, lresid)
        call wkvect('&&VPCALS.VECT.WORKD', 'V V C', 3*neq, lworkd)
        call wkvect('&&VPCALS.VECT.WORKL', 'V V C', lonwl, lworkl)
        call wkvect('&&VPCALS.VECT.WORKV', 'V V C', 3*nbvect, lworkv)
        call wkvect('&&VPCALS.VAL.PRO', 'V V C', (nfreq+1), ldsor)
        call wkvect('&&VPCALS.VECT.AUX', 'V V C', neq, laux)
        call wkvect('&&VPCALS.VECT.AUR', 'V V R', nbvect, lworkr)
!     ------------------------------------------------------------------
!     -------  SORENSEN PB QUADRATIQUE REEL  SYM  ----------------------
!     -------  APPROCHE REELLE OU IMAGINAIRE      ----------------------
!     ------------------------------------------------------------------
    else if ((lkr .and. lc) .and. (appr .ne. 'C')) then
        call wkvect('&&VPCALS.RESID', 'V V R', 2*neq, lresid)
        call wkvect('&&VPCALS.VECT.WORKD', 'V V R', 6*neq, lworkd)
        call wkvect('&&VPCALS.VECT.AUX', 'V V R', 2*neq, laux)
        call wkvect('&&VPCALS.VECT.AUC', 'V V C', 2*neq*(nbvect+1), lauc)
        call wkvect('&&VPCALS.VECT.AUR', 'V V R', 2*neq*(nbvect+1), laur)
        call wkvect('&&VPCALS.VECT.AUL', 'V V C', neq*(nbvect+1), laul)
        call wkvect('&&VPCALS.VAL.PR', 'V V R', nbvect+1, ldiagr)
        call wkvect('&&VPCALS.VAL.PI', 'V V R', nbvect+1, lsurdr)
        call wkvect('&&VPCALS.VAL.PRO', 'V V R', 2*(nfreq+1), ldsor)
        call wkvect('&&VPCALS.VECT.WORKL', 'V V R', lonwl, lworkl)
        call wkvect('&&VPCALS.VECT.WORKV', 'V V R', 3*nbvect, lworkv)
!     ------------------------------------------------------------------
!     -------  SORENSEN PB QUADRATIQUE REEL, SYM OU NON  ---------------
!     -------  APPROCHE COMPLEXE                         ---------------
!     ------------------------------------------------------------------
    else if ((lkr .and. lc) .and. (appr .eq. 'C')) then
        call wkvect('&&VPCALS.RESID', 'V V C', 2*neq, lresid)
        call wkvect('&&VPCALS.VECT.WORKD', 'V V C', 6*neq, lworkd)
        call wkvect('&&VPCALS.VECT.AUX', 'V V C', 2*neq, laux)
        call wkvect('&&VPCALS.VECT.AUC', 'V V C', 2*neq*(nbvect+1), lauc)
        call wkvect('&&VPCALS.VECT.AUR', 'V V R', 2*neq*(nbvect+1), laur)
        call wkvect('&&VPCALS.VECT.WORKL', 'V V C', lonwl, lworkl)
        call wkvect('&&VPCALS.VECT.WORKV', 'V V C', 3*nbvect, lworkv)
        call wkvect('&&VPCALS.VAL.PRO', 'V V C', 2*(nfreq+1), ldsor)
!     ------------------------------------------------------------------
!     -------  SORENSEN PB QUADRATIQUE COMPLEXE SYM  -------------------
!     -------  APPROCHE COMPLEXE                     -------------------
!     ------------------------------------------------------------------
    else if ((.not.lkr) .and. lc) then
        call wkvect('&&VPCALS.RESID', 'V V C', 2*neq, lresid)
        call wkvect('&&VPCALS.VECT.WORKD', 'V V C', 6*neq, lworkd)
        call wkvect('&&VPCALS.VECT.WORKL', 'V V C', lonwl, lworkl)
        call wkvect('&&VPCALS.VECT.WORKV', 'V V C', 3*nbvect, lworkv)
        call wkvect('&&VPCALS.VAL.PRO', 'V V C', 2*(nfreq+1), ldsor)
        call wkvect('&&VPCALS.VECT.AUX', 'V V C', 2*neq, laux)
        call wkvect('&&VPCALS.VECT.AUC', 'V V C', 2*neq*(nbvect+1), lauc)
        call wkvect('&&VPCALS.VECT.AUR', 'V V R', 2*neq*(nbvect+1), laur)
    else
        ASSERT(.false.)
    endif
!
! --- CALCUL MODAL PROPREMENT DIT
!
    if (.not.lc) then
!
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!     ---------------------  PROBLEME GENERALISE   ---------------------
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!
        if (lkr .and. (.not.lns)) then
!     ------------------------------------------------------------------
!     -------  SORENSEN PB GENERALISE REEL SYMETRIQUE  --------
!     ------------------------------------------------------------------
            call vpsorn(lmasse, lmatra, neq, nbvect, nfreq,&
                        tolsor, zr(lvec), zr(lresid), zr(lworkd), zr(lworkl),&
                        lonwl, zl(lselec), zr(ldsor), omeshi, zr(laux),&
                        zr(lworkv), zi(lprod), zi(lddl), neqact, maxitr,&
                        ifm, niv, priram, alpha, omecor,&
                        nconv, flage, solveu)
            call rectfr(nconv, nconv, omeshi, npivot, nblagr,&
                        zr(ldsor), nfreq+1, zi(lresui), zr(lresur), mxresf)
            nfreq1=nfreq+1
            call vpbost(typres, nconv, nconv, omeshi, zr(ldsor),&
                        nfreq1, vpinf, vpmax, precdc, method,&
                        omecor)
            if (typres(1:9) .eq. 'DYNAMIQUE') call vpordi(1, 0, nconv, zr(lresur+mxresf),&
                                                          zr(lvec), neq, zi(lresui))
!
            do imet = 1, nconv
                zi(lresui-1+mxresf+imet) = izero
                zr(lresur-1+imet) = freqom(zr(lresur-1+mxresf+imet))
!           SI OPTION 'PLUS_GRANDE' : CONVERSION EN VALEUR PHYSIQUE
                if (lpg) zr(lresur-1+imet) = +1.d0 / (quapi2 * zr( lresur-1+imet))
                zr(lresur-1+2*mxresf+imet) = rzero
                zk24(lresuk-1+ mxresf+imet)= kmetho
            enddo
            if (typres(1:9) .ne. 'DYNAMIQUE') then
                call vpordo(0, 0, nconv, zr(lresur+mxresf), zr(lvec),&
                            neq)
                do imet = 1, nconv
                    zr(lresur-1+imet) = freqom(zr(lresur-1+mxresf+ imet))
                    zi(lresui-1+imet) = imet
                enddo
            endif
!
        else if (lns .or. (.not.lkr)) then
!     ------------------------------------------------------------------
!     -------  SORENSEN PB GENERALISE COMPLEXE OU REEL NON SYM  --------
!     ------------------------------------------------------------------
            call vpsorc(lmasse, lmatra, neq, nbvect, nfreq,&
                        tolsor, zc(lvec), zc(lresid), zc(lworkd), zc(lworkl),&
                        lonwl, zl(lselec), zc(ldsor), sigma, zc(laux),&
                        zc(lworkv), zr( lworkr), zi(lprod), zi(lddl), neqact,&
                        maxitr, ifm, niv, priram, abs(alpha),&
                        nconv, flage, solveu)
            npivot = nblagr
            nfreq1=nfreq+1
            call rectfc(nconv, nconv, sigma, npivot, nblagr,&
                        zc(ldsor), nfreq1, zi(lresui), zr(lresur), nfreq)
            call vpbosc(typres, nconv, nconv, sigma, zc(ldsor),&
                        nfreq1, vpinf, vpmax, precdc, method,&
                        omecor)
            do imet = 1, nconv
                zi(lresui-1+ mxresf+imet) = izero
                zr(lresur-1+imet) = freqom(zr(lresur-1+mxresf+imet))
                zk24(lresuk-1+ mxresf+imet)= kmetho
            enddo
!
        else
            ASSERT(.false.)
        endif
!
    else
!
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!     ---------------------  PROBLEME QUADRATIQUE   --------------------
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!
        if (lkr) then
            select case (appr)
                case('R','I')
!     ------------------------------------------------------------------
!     -------  SORENSEN PB QUADRATIQUE REEL  SYM  ----------------------
!     -------  APPROCHE REELLE OU IMAGINAIRE      ----------------------
!     ------------------------------------------------------------------
                call wpsorn(appr, lmasse, lamor, lmatra, neq,&
                            nbvect, nfreq, tolsor, zc(lvec), zr(lresid),&
                            zr(lworkd), zr(lworkl), lonwl, zl(lselec), zr(ldsor),&
                            zr(lsurdr), zr(ldiagr), sigma, zr(laux), zr(lworkv),&
                            zi(lprod), zi(lddl), neqact, maxitr, ifm,&
                            niv, priram, abs(alpha), nconv, flage,&
                            zr(laur), zc(lauc), zc(laul), solveu)
                nfreq = nconv / 2
                call wp3vec(appr, optiof, nfreq, nconv, neq,&
                            sigma, zr(lsurdr), zr(ldiagr), zc(lvec), mxresf,&
                            zi(lresui), zr(lresur), zi(lprod), zc(lauc), omecor)
                case('C')
!     ------------------------------------------------------------------
!     -------  SORENSEN PB QUADRATIQUE REEL, SYM OU NON  ---------------
!     -------  APPROCHE COMPLEXE                         ---------------
!     ------------------------------------------------------------------
                call wpsorc(lmasse, lamor, lmatra, neq, nbvect,&
                            nfreq, tolsor, zc(lvec), zc(lresid), zc(lworkd),&
                            zc(lworkl), lonwl, zl(lselec), zc(ldsor), sigma,&
                            zc(laux), zc(lworkv), zi(lprod), zi(lddl), neqact,&
                            maxitr, ifm, niv, priram, abs(alpha),&
                            nconv, flage, zc(lauc), zr(laur), solveu)
                nfreq = nconv / 2
                call wp4vec(nfreq, nconv, neq, sigma, zc(ldsor),&
                            zc(lvec), mxresf, zi(lresui), zr(lresur), zi(lprod),&
                            zc(lauc), omecor)
            case default
                ASSERT(.false.)
            end select
            do imet = 1, nfreq
                zi(lresui-1+mxresf+imet) = izero
                zr(lresur-1+imet) = freqom(zr(lresur-1+mxresf+ imet))
                zk24(lresuk-1+mxresf+imet) = kmetho
            enddo
!
        else
!     ------------------------------------------------------------------
!     -------  SORENSEN PB QUADRATIQUE COMPLEXE SYM  -------------------
!     -------  APPROCHE COMPLEXE                     -------------------
!     ------------------------------------------------------------------
            if (lns) then
                ASSERT(.false.)
            endif
            call wpsorc(lmasse, lamor, lmatra, neq, nbvect,&
                        nfreq, tolsor, zc(lvec), zc(lresid), zc(lworkd),&
                        zc(lworkl), lonwl, zl(lselec), zc(ldsor), sigma,&
                        zc(laux), zc(lworkv), zi(lprod), zi(lddl), neqact,&
                        maxitr, ifm, niv, priram, abs(alpha),&
                        nconv, flage, zc(lauc), zr(laur), solveu)
            nfreq = nconv / 2
            call wp5vec(nfreq, nconv, neq, zc(ldsor), zc(lvec),&
                        mxresf, zi(lresui), zr(lresur), zc(lauc))
            do imet = 1, nfreq
                zi(lresui-1+mxresf+imet) = izero
                zr(lresur-1+imet) = freqom(zr(lresur-1+mxresf+ imet))
                zk24(lresuk-1+mxresf+imet) = kmetho
            end do
        endif
    endif
!
! ---- NOMBRE DE MODES CONVERGES
! ---- SI LE SOLVEUR MODAL A BIEN ACHEVE SON TRAVAIL ON FAIT CETTE AFFEC
! ---- TATION SINON ON NE TIENT COMPTE QUE DES NCONV MODES REELLEMENT CV
    if (.not.flage) nconv = nfreq
!
!     ------------------------------------------------------------------
!     -------------------- CORRECTION : OPTION BANDE -------------------
!     ------------------------------------------------------------------
!
! --- SI OPTION BANDE ON NE GARDE QUE LES FREQUENCES DANS LA BANDE
    mfreq = nconv
    if (optiof(1:5) .eq. 'BANDE') then
        if (lc .or. lns .or. .not.lkr) then
            ASSERT(.false.)
        endif
        do ifreq = mfreq - 1, 0
            if ((zr(lresur+mxresf+ifreq).gt.omemax) .or. (zr(lresur+ mxresf+ifreq).lt.omemin)) &
            nconv = nconv - 1
        enddo
        if (mfreq .ne. nconv) call utmess('I', 'ALGELINE2_17')
    endif
!
! ---  ON MODIFIE LES VALEURS NFREQ DE LA SD EIGENSOLVER
    call vpecri(eigsol, 'I', 1, k24bid, rbid,&
                nfreq)
!
!
! --- NETTOYAGE OBJETS TEMPORAIRES
!
    call jedetr('&&VPCALS.SELECT')
    if ((.not.lc) .and. lkr .and. (.not.lns)) then
        call jedetr('&&VPCALS.RESID')
        call jedetr('&&VPCALS.VECT.WORKD')
        call jedetr('&&VPCALS.VECT.WORKL')
        call jedetr('&&VPCALS.VECT.WORKV')
        call jedetr('&&VPCALS.VAL.PRO')
        call jedetr('&&VPCALS.VECT.AUX')
    else if ((.not.lc) .and. (lns .or. (.not.lkr))) then
        call jedetr('&&VPCALS.RESID')
        call jedetr('&&VPCALS.VECT.WORKD')
        call jedetr('&&VPCALS.VECT.WORKL')
        call jedetr('&&VPCALS.VECT.WORKV')
        call jedetr('&&VPCALS.VAL.PRO')
        call jedetr('&&VPCALS.VECT.AUX')
        call jedetr('&&VPCALS.VECT.AUR')
    else if ((lkr .and. lc) .and. (appr .ne. 'C')) then
        call jedetr('&&VPCALS.RESID')
        call jedetr('&&VPCALS.VECT.WORKD')
        call jedetr('&&VPCALS.VECT.AUX')
        call jedetr('&&VPCALS.VECT.AUC')
        call jedetr('&&VPCALS.VECT.AUR')
        call jedetr('&&VPCALS.VECT.AUL')
        call jedetr('&&VPCALS.VAL.PR')
        call jedetr('&&VPCALS.VAL.PI')
        call jedetr('&&VPCALS.VAL.PRO')
        call jedetr('&&VPCALS.VECT.WORKL')
        call jedetr('&&VPCALS.VECT.WORKV')
    else if ((lkr .and. lc) .and. (appr .eq. 'C')) then
        call jedetr('&&VPCALS.RESID')
        call jedetr('&&VPCALS.VECT.WORKD')
        call jedetr('&&VPCALS.VECT.AUX')
        call jedetr('&&VPCALS.VECT.AUC')
        call jedetr('&&VPCALS.VECT.AUR')
        call jedetr('&&VPCALS.VECT.WORKL')
        call jedetr('&&VPCALS.VECT.WORKV')
        call jedetr('&&VPCALS.VAL.PRO')
    else if ((.not.lkr) .and. lc) then
        call jedetr('&&VPCALS.RESID')
        call jedetr('&&VPCALS.VECT.WORKD')
        call jedetr('&&VPCALS.VECT.WORKL')
        call jedetr('&&VPCALS.VECT.WORKV')
        call jedetr('&&VPCALS.VAL.PRO')
        call jedetr('&&VPCALS.VECT.AUX')
        call jedetr('&&VPCALS.VECT.AUC')
        call jedetr('&&VPCALS.VECT.AUR')
    endif
    call jedema()
!
!     FIN DE VPCALS
!
end subroutine
