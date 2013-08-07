subroutine mousto(guidag, dimtub, voltub, tubuse, dimobs,&
                  volobs, obsuse, rcray, rcarte, sect,&
                  arete, arete2, ns, obcont, epais,&
                  ecray, resu, denc, perce)
    implicit   none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lunule.h"
#include "asterfort/mouveo.h"
#include "asterfort/tbliva.h"
#include "asterfort/u2mess.h"
#include "asterfort/usobce.h"
#include "asterfort/usoben.h"
#include "asterfort/ustuen.h"
#include "asterfort/veobst.h"
#include "asterfort/vetube.h"
    integer :: dimtub, dimobs, ns
    real(kind=8) :: volobs(*), obsuse(*), rcray, rcarte, sect(*), arete, arete2
    real(kind=8) :: epais, ecray, denc, perce, voltub(*), tubuse(*)
    character(len=8) :: obcont, guidag
    character(len=19) :: resu
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     CALCULE LES VOLUMES USES DE L'OBSTACLE
!        GUIDAGE : ENCO_1 , ENCO_2 , CERCLE
! ----------------------------------------------------------------------
    integer :: typutu(20), typuob(20), i, nbval
    integer :: nco, irayo, ithet, ibid, irett, iret2
    real(kind=8) :: parutu(20, 4), adebtu, amaxtu, afintu, proftu
    real(kind=8) :: paruob(20, 4), adebob, amaxob, afinob, profob
    real(kind=8) :: profon, ansini, ansfin, angare, angva
    real(kind=8) :: volume, rapt, rapo, r8b
    complex(kind=8) :: c16b
    character(len=8) :: k8b, k8typ, refo
    character(len=16) :: concep, nomcmd
    character(len=24) :: nomf
!-----------------------------------------------------------------------
!
!     CALCUL DES PARAMETRES DES USURES :
!     --------------------------------
!
    do 130 i = 1, ns
!
        ansini = sect(i)
        ansfin = sect(i+1)
!
        typutu(i) = 0
        adebtu = 0.d0
        amaxtu = 0.d0
        afintu = 0.d0
        proftu = 0.d0
!
        typuob(i) = 0
        adebob = 0.d0
        amaxob = 0.d0
        afinob = 0.d0
        profob = 0.d0
!
        if (guidag .eq. 'CERCLE') then
!             --------------------
! --------- USURE EN LUNULE
            volume = voltub(i) + volobs(i)
            if (volume .gt. r8prem()) then
                call lunule(rcray, rcarte, adebtu, afintu, amaxtu,&
                            ansini, ansfin, profon, volume, epais)
                rapt = voltub(i) / volume
                rapo = volobs(i) / volume
                typutu(i) = 1
                proftu = profon * rapt
                typuob(i) = 1
                adebob = adebtu
                amaxob = amaxtu
                afinob = afintu
                profob = profon * rapo
            endif
!
        else if (guidag .eq. 'ENCO_1') then
!                 --------------------
            if (( ns.eq.12 .and. (i.eq.1 .or. i.eq.12) ) .or.&
                ( ns.eq.10 .and. (i.eq.1 .or. i.eq.10) )) then
! ------------ USURE EN VE
                if (i .eq. 1) then
                    angare = arete
                else
                    angare = 360.d0 - arete
                endif
                angva = 28.d0
                volume = voltub(i)
                if (volume .gt. r8prem()) then
                    call vetube(rcray, rcarte, adebtu, afintu, angare,&
                                amaxtu, angva, proftu, volume, epais)
                    typutu(i) = 3
                endif
                volume = volobs(i)
                if (volume .gt. r8prem()) then
                    call mouveo(arete, rcarte, adebob, afinob, angare,&
                                amaxob, profob, volume, epais)
                    typuob(i) = 3
                endif
                elseif ( ( ns.eq.12 .and. (i.eq.2 .or. i.eq.11) ) .or.&
            ( ns.eq.10 .and. (i.eq.2 .or. i.eq.9 ) ) ) then
! ------------ USURE EN VE+LUNULE
                if (i .eq. 2) then
                    angare = arete
                else
                    angare = 360.d0 - arete
                endif
                angva = 11.d0
                volume = voltub(i)
                if (volume .gt. r8prem()) then
                    call vetube(rcray, rcarte, adebtu, afintu, angare,&
                                amaxtu, angva, proftu, volume, epais)
                    typutu(i) = 2
                endif
                volume = volobs(i)
                if (volume .gt. r8prem()) then
                    call veobst(arete, rcarte, adebob, afinob, angva,&
                                angare, amaxob, profob, volume, epais)
                    typuob(i) = 2
                endif
            else
! ------------ USURE EN LUNULE
                volume = voltub(i) + volobs(i)
                if (volume .gt. r8prem()) then
                    call lunule(rcray, rcarte, adebtu, afintu, amaxtu,&
                                ansini, ansfin, profon, volume, epais)
                    rapt = voltub(i) / volume
                    rapo = volobs(i) / volume
                    typutu(i) = 1
                    proftu = profon * rapt
                    typuob(i) = 1
                    adebob = adebtu
                    amaxob = amaxtu
                    afinob = afintu
                    profob = profon * rapo
                endif
                if (voltub(i) .gt. r8prem()) then
                    call lunule(rcray, rcarte, adebtu, afintu, amaxtu,&
                                ansini, ansfin, profon, voltub(i), epais)
                    proftu = profon
                endif
                if (volobs(i) .gt. r8prem()) then
                    call lunule(rcray, rcarte, adebtu, afintu, amaxtu,&
                                ansini, ansfin, profon, volobs(i), epais)
                    profob = profon
                endif
            endif
!
        else if (guidag .eq. 'ENCO_2') then
!                 --------------------
            if (( ns.eq.12 .and. (i.eq.1 .or. i.eq.12) ) .or.&
                ( ns.eq.10 .and. (i.eq.1 .or. i.eq.10) )) then
! ------------ USURE EN VE
                if (i .eq. 1) then
                    angare = arete
                else
                    angare = 360.d0 - arete
                endif
                angva = 28.d0
                volume = voltub(i)
                if (volume .gt. r8prem()) then
                    call vetube(rcray, rcarte, adebtu, afintu, angare,&
                                amaxtu, angva, proftu, volume, epais)
                    typutu(i) = 3
                endif
                volume = volobs(i)
                if (volume .gt. r8prem()) then
                    call mouveo(arete, rcarte, adebob, afinob, angare,&
                                amaxob, profob, volume, epais)
                    typuob(i) = 3
                endif
                elseif ( ( ns.eq.12 .and. (i.eq.6 .or. i.eq.7) ) .or.&
            ( ns.eq.10 .and. (i.eq.5 .or. i.eq.6) ) ) then
! ------------ USURE EN VE 2E ENCOCHE
                if (ns .eq. 12 .and. i .eq. 6) then
                    angare = arete2
                else if (ns.eq.10 .and. i.eq.5) then
                    angare = arete2
                else
                    angare = 360.d0 - arete2
                endif
                angva = 28.d0
                volume = voltub(i)
                if (volume .gt. r8prem()) then
                    call vetube(rcray, rcarte, adebtu, afintu, angare,&
                                amaxtu, angva, proftu, volume, epais)
                    typutu(i) = 5
                endif
                volume = volobs(i)
                if (volume .gt. r8prem()) then
                    call mouveo(arete, rcarte, adebob, afinob, angare,&
                                amaxob, profob, volume, epais)
                    typuob(i) = 5
                endif
                elseif ( ( ns.eq.12 .and. (i.eq.2 .or. i.eq.11) ) .or.&
            ( ns.eq.10 .and. (i.eq.2 .or. i.eq.9 ) ) ) then
! ------------ USURE EN VE+LUNULE
                if (i .eq. 2) then
                    angare = arete
                else
                    angare = 360.d0 - arete
                endif
                angva = 11.d0
                volume = voltub(i)
                if (volume .gt. r8prem()) then
                    call vetube(rcray, rcarte, adebtu, afintu, angare,&
                                amaxtu, angva, proftu, volume, epais)
                    typutu(i) = 2
                endif
                volume = volobs(i)
                if (volume .gt. r8prem()) then
                    call veobst(arete, rcarte, adebob, afinob, angva,&
                                angare, amaxob, profob, volume, epais)
                    typuob(i) = 2
                endif
                elseif ( ( ns.eq.12 .and. (i.eq.5 .or. i.eq.8) ) .or.&
            ( ns.eq.10 .and. (i.eq.4 .or. i.eq.7) ) ) then
! ------------ USURE EN VE+LUNULE 2E ENCOCHE
                if (i .eq. 4 .or. i .eq. 5) then
                    angare = arete2
                else
                    angare = 360.d0 - arete2
                endif
                angva = 11.d0
                volume = voltub(i)
                if (volume .gt. r8prem()) then
                    call vetube(rcray, rcarte, adebtu, afintu, angare,&
                                amaxtu, angva, proftu, volume, epais)
                    typutu(i) = 4
                endif
                volume = volobs(i)
                if (volume .gt. r8prem()) then
                    call veobst(arete, rcarte, adebob, afinob, angva,&
                                angare, amaxob, profob, volume, epais)
                    typuob(i) = 4
                endif
            else
! ------------ USURE EN LUNULE
                volume = voltub(i) + volobs(i)
                if (volume .gt. r8prem()) then
                    call lunule(rcray, rcarte, adebtu, afintu, amaxtu,&
                                ansini, ansfin, profon, volume, epais)
                    rapt = voltub(i) / volume
                    rapo = volobs(i) / volume
                    typutu(i) = 1
                    proftu = profon * rapt
                    typuob(i) = 1
                    adebob = adebtu
                    amaxob = amaxtu
                    afinob = afintu
                    profob = profon * rapo
                endif
            endif
        endif
!
        parutu(i,1) = adebtu
        parutu(i,2) = amaxtu
        parutu(i,3) = afintu
        parutu(i,4) = proftu
!
        paruob(i,1) = adebob
        paruob(i,2) = amaxob
        paruob(i,3) = afinob
        paruob(i,4) = profob
!
130  end do
!
!
!     CALCUL DE LA PROFONDEUR EN CUMULANT LES SECTEURS :
!     --------------------------------------------------
!
    if (guidag .eq. 'CERCLE') then
        call usobce(dimobs, obsuse, rcarte, resu, ns,&
                    paruob, typuob)
    else
        call tbliva(obcont, 1, 'LIEU', ibid, r8b,&
                    c16b, 'DEFIOBST', k8b, r8b, 'TYPE',&
                    k8typ, ibid, r8b, c16b, refo,&
                    irett)
        call tbliva(obcont, 1, 'LIEU', ibid, r8b,&
                    c16b, 'DEFIOBST', k8b, r8b, 'FONCTION',&
                    k8typ, ibid, r8b, c16b, nomf,&
                    iret2)
        ASSERT(irett.eq.0.and.iret2.eq.0)
        call jeveuo(nomf(1:19)//'.VALE', 'L', ithet)
        call jelira(nomf(1:19)//'.VALE', 'LONMAX', nbval)
        nco = nbval/2
        irayo = ithet + nco
        call usoben(guidag, dimobs, obsuse, nco, zr(irayo),&
                    zr( ithet), ns, paruob, typuob, resu,&
                    arete, arete2, rcarte, denc)
    endif
!
    call ustuen(dimtub, tubuse, rcray, resu, ns,&
                parutu, typutu)
!
!
!     VERIFICATION DU NON PERCEMENT :
!     -----------------------------
!
    do 200 i = 1, dimtub
        proftu = abs( tubuse(2*i) - rcray )
        if (proftu .gt. ecray*perce) then
            call getres(k8b, concep, nomcmd)
            call u2mess('A', 'PREPOST3_64')
            goto 202
        endif
!
200  end do
202  continue
!
end subroutine
