subroutine vafcar(tpgz, mclfz, nmobjz, npo, ndi,&
                  nco, nca, nba, nma, ngb,&
                  nmb, nutyel, ntyele, car, ncar,&
                  ivr, kioc, ier)
    implicit none
#include "asterc/getres.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
    integer :: ntyele(*), ivr(*)
    character(len=6) :: kioc
    character(len=8) :: tpg, nomobj, carz
    character(len=*) :: tpgz, nmobjz, car(*), mclfz
    character(len=16) :: mclf
    character(len=8) :: nomu
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
! ----------------------------------------------------------------------
!       VERIFICATION DE LA BONNE AFFECTATION DES DONNEES :
!         CARAC POUTRE      >  ELEMENT POUTRE
!         CARAC DISCRET     >  ELEMENT DISCRET DE TYPE L OU N
!         CARAC COQUE       >  ELEMENT COQUE
!         CARAC ORIENTATION >  ELEMENT DISCRET OU POUTRE
!         CARAC DEFI_ARC    >  ELEMENT POUTRE COURBE
!         CARAC CABLE       >  ELEMENT CABLE
!         CARAC BARRE       >  ELEMENT BARRE
!         CARAC MASSIF      >  ELEMENT THERMIQUE
!         CARAC GRILLE      >  ELEMENT GRILLE
!         CARAC MEMBRANE    >  ELEMENT MEMBRANE
! ----------------------------------------------------------------------
    character(len=16) :: concep, cmd
    character(len=17) :: tpge
    character(len=24) :: valk(4)
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ier, l, l1, nba, nca, ncar
    integer :: nco, ndi, ngb, nmb, nma, npd, npf, npo
    integer :: nutyel
!-----------------------------------------------------------------------
    tpg = tpgz
    nomobj = nmobjz
    mclf = mclfz
!
    call getres(nomu, concep, cmd)
!
! --- VERIFICATION DE L AFFECTATION DE LA MAILLE PAR UN ELEMENT
    tpge = tpg//' '//nomobj
    if (nutyel .eq. 0) then
        if (ivr(1) .eq. 1 .or. ivr(2) .eq. 1) then
            valk(1) = kioc
            valk(2) = mclf
            valk(3) = tpge
            call u2mesk('A', 'MODELISA7_63', 3, valk)
            ier = ier + 1
        endif
        goto 9999
    endif
!
! --- VERIFICATION DU BON TYPE DE L ELEMENT
    if (mclf(1:6) .eq. 'POUTRE') then
        npd = 1
        npf = npo
        elseif ( (mclf(1: 7).eq.'DISCRET') .or. (mclf(1:12)&
    .eq.'RIGI_PARASOL') .or. mclf(1:9).eq.'MASS_AJOU') then
        npd = npo + 1
        npf = npo + ndi
    else if (mclf(1:11).eq.'ORIENTATION') then
        npd = 1
        npf = npo + ndi
    else if (mclf(1:5).eq.'COQUE') then
        npd = npo + ndi + 1
        npf = npo + ndi + nco
    else if (mclf(1:5) .eq.'CABLE') then
        npd = npo + ndi + nco + 1
        npf = npo + ndi + nco + nca
    else if (mclf(1:5) .eq.'BARRE') then
        npd = npo + ndi + nco + nca + 1
        npf = npo + ndi + nco + nca + nba
    else if (mclf(1:8).eq.'DEFI_ARC') then
        npd = 4
        npf = 4
    else if (mclf(1:6).eq.'MASSIF') then
        npd = npo + ndi + nco + nca + nba + 1
        npf = npo + ndi + nco + nca + nba + nma
    else if (mclf(1:6).eq.'GRILLE') then
        npd = npo + ndi + nco + nca + nba + nma + 1
        npf = npo + ndi + nco + nca + nba + nma + ngb
    else if (mclf(1:8).eq.'MEMBRANE') then
        npd = npo + ndi + nco + nca + nba + nma + ngb + 1
        npf = npo + ndi + nco + nca + nba + nma + ngb + nmb
    else
        valk(1) = mclf
        call u2mesg('A', 'MODELISA9_11', 1, valk, 0,&
                    0, 0, 0.d0)
    endif
!
    do 10 i = npd, npf
        if (nutyel .eq. ntyele(i)) goto 20
10  end do
    if (ivr(1) .eq. 1 .or. ivr(2) .eq. 1) then
        valk(1) = kioc
        valk(2) = mclf
        valk(3) = tpge
        call u2mesk('A', 'MODELISA7_64', 3, valk)
        ier = ier + 1
    endif
    goto 9999
20  continue
!
! --- CAS PARTICULIER DES ELEMENTS DISCRETS
    if (mclf(1:7) .eq. 'DISCRET') then
        do 100 i = 1, ncar
            carz = car(i)
            if (carz(3:4) .eq. 'T_') then
                l = 1
                l1 = 5
            else if (carz(3:4).eq.'TR') then
                l = 3
                l1 = 7
            endif
            if ((&
                (&
                carz(5:5) .eq. 'N' .or. carz(6:6) .eq. 'N' .or. carz(7: 7) .eq. 'N' .or.&
                carz(8:8) .eq. 'N'&
                )&
                .and. (nutyel.ne.ntyele( npo+l) .and. nutyel.ne.ntyele(npo+l1))&
                )&
                .or.&
                (&
                (&
                carz(5:5) .eq. 'L' .or. carz(6:6) .eq. 'L' .or. carz(7:7) .eq. 'L' .or.&
                carz(8:8) .eq. 'L'&
                )&
                .and. (nutyel .ne. ntyele(npo+1+l) .and. nutyel .ne. ntyele(npo+1+l1))&
                )&
                .or.&
                (&
                (carz(5:5) .eq.'D' .or. carz(6:6).eq.'D') .and.&
                (&
                nutyel .ne. ntyele( npo+l) .and. nutyel .ne. ntyele(npo+l1) .and. nutyel&
                .ne. ntyele(npo+1+l) .and. nutyel .ne. ntyele(npo+1+l1)&
                )&
                )&
                .or.&
                (&
                (carz(1:5).eq.'M_T_D' .or. carz(1:6).eq.'M_TR_D') .and.&
                ( nutyel .eq. ntyele(npo+1+l1) .and. nutyel .ne. ntyele( npo+l1) )&
                )) then
                if (ivr(1) .eq. 1 .or. ivr(2) .eq. 1) then
                    valk(1) = kioc
                    valk(2) = mclf
                    valk(3) = tpge
                    valk(4) = carz
                    call u2mesk('A', 'MODELISA7_65', 4, valk)
                    ier = ier + 1
                endif
            endif
100      continue
    endif
!
9999  continue
end subroutine
