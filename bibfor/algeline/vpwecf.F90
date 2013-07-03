subroutine vpwecf(option, typres, nfreq, mxfreq, resufi,&
                  resufr, resufk, lamor, ktyp, lns)
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
!     ECRITURE DES FREQUENCES RELATIVEMENT A LA METHODE UTILISEE
!     IMPRESSION D'OFFICE SUR "MESSAGE"
!-----------------------------------------------------------------------
    implicit   none
!
! PARAMETRES D'APPEL
#include "asterc/isnnem.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mesr.h"
#include "asterfort/u2mess.h"
    integer :: nfreq, mxfreq, resufi(mxfreq, *), lamor
    real(kind=8) :: resufr(mxfreq, *)
    character(len=*) :: option, resufk(mxfreq, *), typres
    character(len=1) :: ktyp
    logical :: lns
!
! VARIABLES LOCALES
    integer :: ifm, ifreq, indf, niv, vali(4)
    real(kind=8) :: am, undf, erc, errmoy
    real(kind=8) :: valr(5)
    character(len=27) :: valk(4)
!     ------------------------------------------------------------------
    call infniv(ifm, niv)
    undf = r8vide()
    indf = isnnem()
    errmoy = 0.d0
    if (nfreq .eq. 0) call assert(.false.)
    if (resufk(nfreq,2) .eq. 'BATHE_WILSON') then
        if (typres .eq. 'DYNAMIQUE') then
            call u2mess('I', 'ALGELINE6_59')
        else
            call u2mess('I', 'ALGELINE6_60')
        endif
        do 10 ifreq = 1, nfreq
            am = resufr(ifreq,4)
            errmoy = errmoy + abs(am)
            valr(1)= am
            vali(1)= resufi(ifreq,1)
            vali(2)= resufi(ifreq,3)
            vali(3)= resufi(ifreq,5)
            if (typres .eq. 'DYNAMIQUE') then
                valr(2)=resufr(ifreq,1)
            else
                valr(2)=resufr(ifreq,2)
            endif
            call u2mesg('I', 'ALGELINE6_61', 0, ' ', 3,&
                        vali, 2, valr)
10      continue
        valr(1)= errmoy/nfreq
        call u2mesr('I', 'ALGELINE6_58', 1, valr)
!
    else if (resufk(nfreq,2) .eq. 'LANCZOS') then
        if (lamor .eq. 0) then
            if (typres .eq. 'DYNAMIQUE') then
                call u2mess('I', 'ALGELINE6_62')
            else
                call u2mess('I', 'ALGELINE6_63')
            endif
        else
            if (typres .eq. 'DYNAMIQUE') then
                call u2mess('I', 'ALGELINE6_64')
            else
                call u2mess('I', 'ALGELINE6_65')
            endif
        endif
        do 20 ifreq = 1, nfreq
            vali(1)= resufi(ifreq,1)
            vali(2)= resufi(ifreq,2)
            if (lamor .eq. 0) then
                am = resufr(ifreq,4)
            else
                am = resufr(ifreq,3)
            endif
            errmoy = errmoy + abs(am)
            valr(1)= am
            if (typres .eq. 'DYNAMIQUE') then
                valr(2)=resufr(ifreq,1)
            else
                valr(2)=resufr(ifreq,2)
            endif
            call u2mesg('I', 'ALGELINE6_66', 0, ' ', 2,&
                        vali, 2, valr)
20      continue
        if (lamor .eq. 0) then
            valr(1)= errmoy/nfreq
            call u2mesr('I', 'ALGELINE6_58', 1, valr)
        endif
!
    else if (resufk(nfreq,2) .eq. 'SORENSEN') then
        if ((lamor.eq.0) .and. (ktyp.eq.'R') .and. (.not.lns)) then
            if (typres .eq. 'DYNAMIQUE') then
                call u2mess('I', 'ALGELINE6_67')
            else
                call u2mess('I', 'ALGELINE6_68')
            endif
        else
            if (typres .eq. 'DYNAMIQUE') then
                call u2mess('I', 'ALGELINE6_70')
            else
                call u2mess('I', 'ALGELINE6_71')
            endif
        endif
        do 35 ifreq = 1, nfreq
            vali(1) = resufi(ifreq,1)
            if ((lamor.eq.0) .and. (ktyp.eq.'R') .and. (.not.lns)) then
                am = resufr(ifreq,4)
                errmoy = errmoy + abs(am)
            else
                am = resufr(ifreq,3)
                erc = resufr(ifreq,4)
                errmoy = errmoy + abs(erc)
            endif
            valr(1)= am
            if ((lamor.eq.0) .and. (ktyp.eq.'R') .and. (.not.lns)) then
                if (typres .eq. 'DYNAMIQUE') then
                    valr(2)=resufr(ifreq,1)
                else
                    valr(2)=resufr(ifreq,2)
                endif
                call u2mesg('I', 'ALGELINE6_69', 0, ' ', 1,&
                            vali, 2, valr)
            else
                if (typres .eq. 'DYNAMIQUE') then
                    valr(2)=resufr(ifreq,1)
                else
                    valr(2)=resufr(ifreq,2)
                endif
                valr(3)=erc
                call u2mesg('I', 'ALGELINE6_72', 0, ' ', 1,&
                            vali, 3, valr)
            endif
35      continue
        valr(1)= errmoy/nfreq
        call u2mesr('I', 'ALGELINE6_58', 1, valr)
!
    else if (resufk(nfreq,2)(1:2) .eq. 'QZ') then
        valk(1)=resufk(nfreq,2)(1:16)
        if ((lamor.eq.0) .and. (ktyp.eq.'R') .and. (.not.lns)) then
            if (typres .eq. 'DYNAMIQUE') then
                call u2mesk('I', 'ALGELINE6_73', 1, valk)
            else
                call u2mesk('I', 'ALGELINE6_74', 1, valk)
            endif
        else
            if (typres .eq. 'DYNAMIQUE') then
                call u2mesk('I', 'ALGELINE6_75', 1, valk)
            else
                call u2mesk('I', 'ALGELINE6_76', 1, valk)
            endif
        endif
        do 36 ifreq = 1, nfreq
            vali(1) = resufi(ifreq,1)
            if ((lamor.eq.0) .and. (ktyp.eq.'R') .and. (.not.lns)) then
                am = resufr(ifreq,4)
                errmoy = errmoy + abs(am)
            else
                am = resufr(ifreq,3)
                erc = resufr(ifreq,4)
                errmoy = errmoy + abs(erc)
            endif
            valr(1)= am
            if ((lamor.eq.0) .and. (ktyp.eq.'R') .and. (.not.lns)) then
                if (typres .eq. 'DYNAMIQUE') then
                    valr(2)=resufr(ifreq,1)
                else
                    valr(2)=resufr(ifreq,2)
                endif
                call u2mesg('I', 'ALGELINE6_69', 0, ' ', 1,&
                            vali, 2, valr)
            else
                if (typres .eq. 'DYNAMIQUE') then
                    valr(2)=resufr(ifreq,1)
                else
                    valr(2)=resufr(ifreq,2)
                endif
                valr(3)=erc
                call u2mesg('I', 'ALGELINE6_72', 0, ' ', 1,&
                            vali, 3, valr)
            endif
36      continue
        valr(1)= errmoy/nfreq
        call u2mesr('I', 'ALGELINE6_58', 1, valr)
!
        elseif ((resufk(nfreq,2) .eq. 'INVERSE_R' .or. resufk(nfreq,2)&
    .eq. 'INVERSE_C') .and. ( option(1:6) .eq. 'PROCHE') ) then
        if (typres .eq. 'DYNAMIQUE') then
            call u2mess('I', 'ALGELINE6_77')
        else
            call u2mess('I', 'ALGELINE6_78')
        endif
        do 40 ifreq = 1, nfreq
            if (typres .eq. 'DYNAMIQUE') then
                valr(1)=resufr(ifreq,1)
            else
                valr(1)=resufr(ifreq,2)
            endif
            vali(1) = resufi(ifreq,1)
            valr(2) = resufr(ifreq,3)
            vali(2) = resufi(ifreq,4)
            valr(3) = resufr(ifreq,15)
            valr(4) = resufr(ifreq,4)
!
            call u2mesg('I', 'ALGELINE6_79', 0, ' ', 2,&
                        vali, 4, valr)
            resufr(ifreq,14) = undf
            resufr(ifreq,15) = undf
            resufi(ifreq,2) = indf
            resufi(ifreq,3) = indf
            resufi(ifreq,4) = indf
            resufi(ifreq,8) = resufi(ifreq,4)
40      continue
        write(ifm,7777)
!
        elseif ( resufk(nfreq,2) .eq. 'INVERSE_R' .and. option(1:6) .eq.&
    'AJUSTE' ) then
        if (typres .eq. 'DYNAMIQUE') then
            call u2mess('I', 'ALGELINE6_80')
        else
            call u2mess('I', 'ALGELINE6_81')
        endif
        do 50 ifreq = 1, nfreq
            if (typres .eq. 'DYNAMIQUE') then
                valr(1)=resufr(ifreq,1)
            else
                valr(1)=resufr(ifreq,2)
            endif
            vali(1) = resufi(ifreq,1)
            valr(2) = resufr(ifreq,3)
            vali(2) = resufi(ifreq,2)
            vali(3) = resufi(ifreq,3)
            valr(3) = resufr(ifreq,14)
            vali(4) = resufi(ifreq,4)
            valr(4) = resufr(ifreq,15)
            valr(5) = resufr(ifreq,4)
            call u2mesg('I', 'ALGELINE6_82', 0, ' ', 4,&
                        vali, 5, valr)
!
            resufr(ifreq,14) = undf
            resufr(ifreq,15) = undf
            resufi(ifreq,2) = indf
            resufi(ifreq,3) = indf
            resufi(ifreq,4) = indf
            resufi(ifreq,7) = resufi(ifreq,4)
50      continue
        write(ifm,7777)
!
        elseif ( resufk(nfreq,2) .eq. 'INVERSE_R' .and. option(1:6) .eq.&
    'SEPARE' ) then
        if (typres .eq. 'DYNAMIQUE') then
            call u2mess('I', 'ALGELINE6_83')
        else
            call u2mess('I', 'ALGELINE6_84')
        endif
        do 60 ifreq = 1, nfreq
            if (typres .eq. 'DYNAMIQUE') then
                valr(1)=resufr(ifreq,1)
            else
                valr(1)=resufr(ifreq,2)
            endif
            vali(1) = resufi(ifreq,1)
            valr(2) = resufr(ifreq,3)
            vali(2) = resufi(ifreq,2)
            vali(3) = resufi(ifreq,4)
            valr(3) = resufr(ifreq,15)
            valr(4) = resufr(ifreq,4)
            call u2mesg('I', 'ALGELINE6_85', 0, ' ', 3,&
                        vali, 4, valr)
!
            resufr(ifreq,14) = undf
            resufr(ifreq,15) = undf
            resufi(ifreq,2) = indf
            resufi(ifreq,3) = indf
            resufi(ifreq,4) = indf
            resufi(ifreq,6) = resufi(ifreq,4)
60      continue
        write(ifm,7777)
!
        elseif ( resufk(nfreq,2) .eq. 'INVERSE_C' .and. ( option(1:6)&
    .eq. 'AJUSTE' .or. option(1:6) .eq. 'SEPARE' ) ) then
        if (typres .eq. 'DYNAMIQUE') then
            call u2mess('I', 'ALGELINE6_86')
        else
            call u2mess('I', 'ALGELINE6_87')
        endif
        do 70 ifreq = 1, nfreq
            if (typres .eq. 'DYNAMIQUE') then
                valr(1)=resufr(ifreq,1)
            else
                valr(1)=resufr(ifreq,2)
            endif
            vali(1) = resufi(ifreq,1)
            valr(2) = resufr(ifreq,3)
            vali(2) = resufi(ifreq,2)
            valr(3) = resufr(ifreq,14)
            vali(3) = resufi(ifreq,4)
            valr(4) = resufr(ifreq,15)
            valr(5) = resufr(ifreq,4)
            call u2mesg('I', 'ALGELINE6_88', 0, ' ', 3,&
                        vali, 5, valr)
!
            resufr(ifreq,14) = undf
            resufr(ifreq,15) = undf
            resufi(ifreq,2) = indf
            resufi(ifreq,3) = indf
            resufi(ifreq,4) = indf
            resufi(ifreq,8) = resufi(ifreq,4)
70      continue
        write(ifm,7777)
!
    endif
!
    7777 format ( / )
!
!     ------------------------------------------------------------------
end subroutine
