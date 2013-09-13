subroutine rcevol(typtab, nommat, symax, nbopt, option)
    implicit none
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rcev22.h"
#include "asterfort/rcevfa.h"
#include "asterfort/rcevo0.h"
#include "asterfort/rcevo1.h"
#include "asterfort/rcevo2.h"
#include "asterfort/rcevoa.h"
#include "asterfort/rcevod.h"
#include "asterfort/rcevom.h"
#include "asterfort/rcevse.h"
#include "asterfort/rcevsn.h"
#include "asterfort/rcevsp.h"
#include "asterfort/utmess.h"
    integer :: nbopt
    real(kind=8) :: symax
    character(len=8) :: nommat
    character(len=16) :: typtab, option(*)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!
!     OPERATEUR POST_RCCM:    TYPE_ANALYSE = 'COMPOSANT'
!                           TYPE_RESU_MECA = 'EVOLUTION'
!
!     ------------------------------------------------------------------
!
    integer :: i, j, n1, nbinti, jinti, nbtran
    real(kind=8) :: para(3), sm
    logical :: lpmpb, lsn, lfatig, flexio, lrocht, lamorc, kemixt
    character(len=8) ::  typeke
    character(len=16) :: kinti
    character(len=24) :: cinst, csili, csiex, csno, csne, csneo, csnee, cspo
    character(len=24) :: cspe, cfao, cfae, cnoc, cresu, cresp, intitu, cspto
    character(len=24) :: cspte, cspmo, cspme, cstex, csmex
! DEB ------------------------------------------------------------------
!
! --- VECTEUR DES INSTANTS DEMANDES
!
    cinst = '&&RCEVOL.INSTANTS'
!
! --- VECTEUR DES TRANSITOIRES
!
    cresu = '&&RCEVOL.RESU_MECA'
    cresp = '&&RCEVOL.RESU_PRES'
!
! --- VECTEUR DES CONTRAINTES LINEARISEES AUX EXTREMITES (PMPB, SN)
!
    csili = '&&RCEVOL.SIGM_LINE'
!
! --- VECTEUR DES CONTRAINTES TOTALES AUX EXTREMITES (SP)
!
    csiex = '&&RCEVOL.SIGM_EXTR'
!
! --- VECTEUR DES CONTRAINTES THERMIQUES AUX EXTREMITES (SPTH)
!
    cstex = '&&RCEVOL.STHE_EXTR'
!
! --- VECTEUR DES CONTRAINTES MECANIQUES AUX EXTREMITES (SPMECA)
!
    csmex = '&&RCEVOL.SMEC_EXTR'
!
! --- VECTEUR DES NB_OCCUR
!
    cnoc = '&&RCEVOL.NB_OCCUR'
!
! --- CALCUL DE GRANDEURS A L'ORIGINE ET A L'EXTREMITE
!
    csno = '&&RCEVOL.CALCUL_SN .ORIG'
    csne = '&&RCEVOL.CALCUL_SN .EXTR'
    csneo = '&&RCEVOL.CALCUL_SNE.ORIG'
    csnee = '&&RCEVOL.CALCUL_SNE.EXTR'
    cspo = '&&RCEVOL.CALCUL_SP .ORIG'
    cspe = '&&RCEVOL.CALCUL_SP .EXTR'
    cspto = '&&RCEVOL.CALCUL_SPT.ORIG'
    cspme = '&&RCEVOL.CALCUL_SPT.EXTR'
    cspmo = '&&RCEVOL.CALCUL_SPM.ORIG'
    cspte = '&&RCEVOL.CALCUL_SPM.EXTR'
    cfao = '&&RCEVOL.FATIGUE   .ORIG'
    cfae = '&&RCEVOL.FATIGUE   .EXTR'
!
!     ------------------------------------------------------------------
!                             LES OPTIONS
!     ------------------------------------------------------------------
    lfatig = .false.
    lsn = .false.
    lpmpb = .false.
    flexio = .false.
    lrocht = .false.
    lamorc = .false.
!
    do 10 i = 1, nbopt
        if (option(i) .eq. 'PM_PB') then
            lpmpb = .true.
        else if (option(i) .eq. 'SN') then
            lsn = .true.
        else if (option(i) .eq. 'FATIGUE_ZH210') then
            lfatig = .true.
            lsn = .true.
        else if (option(i) .eq. 'AMORCAGE') then
            lamorc = .true.
        endif
10  end do
!
    if (lamorc .and. (lpmpb .or. lsn .or. lfatig)) then
        call utmess('F', 'POSTRCCM_3')
    endif
!
    kemixt = .false.
    call getvtx(' ', 'TYPE_KE', scal=typeke, nbret=n1)
    if (typeke .eq. 'KE_MIXTE') kemixt = .true.
!
!     ------------------------------------------------------------------
!                      TRAITEMENT DE L'AMORCAGE
!     ------------------------------------------------------------------
!
    if (lamorc) then
        call rcevoa(typtab, nommat)
        goto 9999
    endif
!
!     ------------------------------------------------------------------
!                            LE MATERIAU
!     ------------------------------------------------------------------
!
    call rcevo1(nommat, lfatig, sm, para, symax)
!
!     ------------------------------------------------------------------
!                     NOMBRE DE LIGNE A "POST_RCCM"
!     ------------------------------------------------------------------
!
    intitu = '&&RCEVOL.INTITULE'
    call rcevo0(intitu, nbinti, lsn, lfatig, nbtran)
    call jeveuo(intitu, 'L', jinti)
!
!     ------------------------------------------------------------------
!
    do 100 i = 1, nbinti
!
        do 110 j = 1, nbtran
!
            kinti = zk16(jinti-1+nbtran*(i-1)+j)
!
!         --------------------------------------------------------------
!                    TRAITEMENT DU MOT CLE FACTEUR TRANSITOIRE
!         --------------------------------------------------------------
!
            if (lsn .and. .not.lfatig .and. nbtran .gt. 1) then
                call rcev22(nbinti, kinti, j, csili, cinst,&
                            csiex, lfatig, flexio, lrocht, cnoc,&
                            cresu, cresp)
            else
                call rcevo2(nbinti, kinti, csili, cinst, csiex,&
                            kemixt, cstex, csmex, lfatig, flexio,&
                            lrocht, cnoc, cresu, cresp)
            endif
!
            if (lrocht .and. symax .eq. r8vide()) then
                call utmess('A', 'POSTRCCM_4')
                lrocht = .false.
            endif
!
!         --------------------------------------------------------------
!                          TRAITEMENT DES OPTIONS
!         --------------------------------------------------------------
!
            if (lsn) call rcevsn(csili, cinst, csno, csne)
!
            if (flexio) call rcevse(csili, cinst, csneo, csnee)
!
            if (lfatig) then
                call rcevsp(csiex, kemixt, cstex, csmex, cinst,&
                            cspo, cspe, cspto, cspte, cspmo,&
                            cspme)
                call rcevfa(nommat, para, sm, cnoc, csno,&
                            csne, cspo, cspe, kemixt, cspto,&
                            cspte, cspmo, cspme, cfao, cfae)
            endif
!
!         --------------------------------------------------------------
!                                 ARCHIVAGE
!         --------------------------------------------------------------
!
            if (typtab .eq. 'VALE_MAX') then
!
                call rcevom(csili, cinst, cnoc, sm, lfatig,&
                            lpmpb, lsn, csno, csne, flexio,&
                            csneo, csnee, cfao, cfae, cspo,&
                            cspe, cresu, kinti, i, j,&
                            lrocht, symax, cresp, kemixt, cspto,&
                            cspte, cspmo, cspme)
!
            else
!
                call rcevod(csili, cinst, cnoc, sm, lfatig,&
                            lpmpb, lsn, csno, csne, flexio,&
                            csneo, csnee, cfao, cfae, cspo,&
                            cspe, cresu, kinti, i, j,&
                            lrocht, symax, cresp, kemixt, cspto,&
                            cspte, cspmo, cspme)
!
            endif
!
            call jedetr(cinst)
            call jedetr(cresu)
            call jedetr(cresp)
            call jedetr(csili)
            call jedetr(csiex)
            call jedetr(cnoc)
            call jedetr(csno)
            call jedetr(csne)
            call jedetr(csneo)
            call jedetr(csnee)
            call jedetr(cspo)
            call jedetr(cspe)
            call jedetr(cfao)
            call jedetr(cfae)
            if (kemixt) then
                call jedetr(cstex)
                call jedetr(csmex)
                call jedetr(cspmo)
                call jedetr(cspme)
                call jedetr(cspto)
                call jedetr(cspte)
            endif
!
110      continue
!
100  end do
!
    call jedetr(intitu)
!
9999  continue
!
end subroutine
