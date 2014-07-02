subroutine vpcalj(eigsol, vecrer, vecrei, vecrek, vecvp,&
                  matopa, matpsc, mxresf, nblagr, omemax,&
                  omemin, omeshi, solveu, vecblo, npivot,&
                  flage, nconv, vpinf, vpmax)
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
! ROUTINE EFFECTUANT LE CALCUL MODAL PARAMETRE DANS EIGSOL PAR LA METHODE DE JACOBI
! -------------------------------------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8depi.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/freqom.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rectfr.h"
#include "asterfort/sspace.h"
#include "asterfort/utmess.h"
#include "asterfort/vpbost.h"
#include "asterfort/vplecs.h"
#include "asterfort/vpordi.h"
#include "asterfort/vpordo.h"
#include "asterfort/wkvect.h"
!
! --- INPUT
!
    integer, intent(in) :: mxresf, npivot, nblagr
    real(kind=8), intent(in) :: omemax, omemin, omeshi
    character(len=19), intent(in) :: eigsol, matopa, matpsc, solveu
    character(len=24), intent(in) :: vecrer, vecrei, vecrek, vecvp, vecblo
!
! --- OUTPUT
!
    integer, intent(out) :: nconv
    real(kind=8), intent(out) :: vpinf, vpmax
    aster_logical , intent(out) :: flage
!
! --- INPUT/OUTPUT
! None
!
! --- VARIABLES LOCALES
!
    integer :: ibid, iret, imet, lmasse, lmatra, lraide, nbvect, neq, nfreq
    integer :: lprod, lmtpsc, lvalpr, itemax, nperm, nitbat, nitjac
    integer :: mfreq, ifreq
    integer :: lresui, lresur, lresuk, lvec
    real(kind=8) :: quapi2, omecor, precdc, rbid, rzero, tol, toldyn
    character(len=1) :: k1bid
    character(len=8) :: method, k8bid
    character(len=9) :: k9bid
    character(len=14) :: k14bid
    character(len=16) :: k16bid, optiof, typres
    character(len=19) :: k19bid, masse, raide
    character(len=24) :: kmetho
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
    quapi2=r8depi()*r8depi()
    rzero=0.d0
    kmetho='LANCZOS'
!
    nconv=0
    flage=.false.
    call jeveuo(vecrer, 'E', lresur)
    call jeveuo(vecrei, 'E', lresui)
    call jeveuo(vecrek, 'E', lresuk)
    call jeveuo(vecvp, 'E', lvec)
    call jeveuo(vecblo, 'L', lprod)
!
! --- LECTURE DES DONNEES DE EIGSOL
!
    call vplecs(eigsol, itemax, ibid, ibid, ibid,&
                ibid, ibid, nbvect, ibid, nfreq,&
                nperm, rbid, omecor, rbid, rbid,&
                precdc, rbid, rbid, rbid, rbid,&
                tol, toldyn, rbid, k1bid, k8bid,&
                method, k9bid, k14bid, k14bid, k14bid,&
                k16bid, optiof, k16bid, k16bid, k16bid,&
                typres, k19bid, masse, raide, k19bid,&
                lc, lkr, lns, lpg, lbid)
    ASSERT(method(1:6).eq.'JACOBI')
!
! ---  DESCRIPTEURS MATRICES
    call jeveuo(raide//'.&INT', 'E', lraide)
    neq = zi(lraide+2)
    call jeveuo(masse//'.&INT', 'E', lmasse)
    call jeveuo(matopa//'.&INT', 'E', lmatra)
    call jeexin(matpsc//'.&INT', iret)
    if (iret .eq. 0) then
        lmtpsc=0
    else
        call jeveuo(matpsc//'.&INT', 'E', lmtpsc)
    endif
!
! --- PRE-ALLOCATIONS MEMOIRE
!
    call wkvect('&&VPCALJ.VALPRO', 'V V R', nbvect, lvalpr)
!
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
!     ------------------------------------------------------------------
!     -------  JACOBI PB GENERALISE REEL SYMETRIQUE --------------------
!     ------------------------------------------------------------------
        if ((.not.lkr) .or. lns) ASSERT(.false.)
        call sspace(lmtpsc, lmatra, lmasse, neq, nbvect,&
                    nfreq, zi(lprod), itemax, nperm, tol,&
                    toldyn, zr(lvec), zr(lvalpr), nitjac, nitbat,&
                    solveu)
        call rectfr(nfreq, nbvect, omeshi, npivot, nblagr,&
                    zr(lvalpr), nbvect, zi(lresui), zr(lresur), mxresf)
        call vpbost(typres, nfreq, nbvect, omeshi, zr(lvalpr),&
                    nbvect, vpinf, vpmax, precdc, method,&
                    omecor)
        if (typres(1:9) .eq. 'DYNAMIQUE') call vpordi(1, 0, nfreq, zr(lresur+mxresf), zr(lvec),&
                                                      neq, zi(lresui))
!
        do imet = 1, nfreq
            zi(lresui-1+2*mxresf+imet) = nitbat
            zi(lresui-1+4*mxresf+imet) = nitjac
            zr(lresur-1+imet) = freqom(zr(lresur-1+mxresf+imet))
!           SI OPTION 'PLUS_GRANDE' : CONVERSION EN VALEUR PHYSIQUE
            if (lpg) zr(lresur-1+imet) = +1.d0 / (quapi2 * zr( lresur-1+imet))
            zr(lresur-1+2*mxresf+imet) = rzero
            zk24(lresuk-1+ mxresf+imet)= 'BATHE_WILSON'
        end do
        if (typres(1:9) .ne. 'DYNAMIQUE') then
            call vpordo(0, 0, nfreq, zr(lresur+mxresf), zr(lvec),&
                        neq)
            do imet = 1, nfreq
                zr(lresur-1+imet) = freqom(zr(lresur-1+mxresf+ imet))
                zi(lresui-1+imet) = imet
            end do
        endif
    else
!
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!     ---------------------  PROBLEME QUADRATIQUE   --------------------
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
        ASSERT(.false.)
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
        if (lc .or. lns .or. .not.lkr) ASSERT(.false.)
        do ifreq = mfreq - 1, 0
            if ((zr(lresur+mxresf+ifreq).gt.omemax) .or. (zr(lresur+ mxresf+ifreq).lt.omemin)) &
            nconv = nconv - 1
        enddo
        if (mfreq .ne. nconv) call utmess('I', 'ALGELINE2_17')
    endif
!
! --- NETTOYAGE OBJETS TEMPORAIRES
    call jedetr('&&VPCALJ.VALPRO')
    call jedema()
!
!     FIN DE VPCALJ
!
end subroutine
