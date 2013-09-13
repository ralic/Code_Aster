subroutine usupru(vusurt, vusuro, nbinst, prust)
    implicit none
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
!     CALCULE LA PROFONDEUR D'USURE
!
! IN  : VUSURT : VOLUME USE TUBE A CHAQUE INSTANT
! IN  : VUSURO : VOLUME USE OBSTACLE A CHAQUE INSTANT
! IN  : NBINST : NOMBRE D'INSTANTS
! OUT : PRUST  : PROFONDEUR D'USURE DU TUBE POUR CHAQUE INSTANT
!-----------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/r8depi.h"
#include "asterc/r8dgrd.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/u2mess.h"
#include "asterfort/usubis.h"
#include "asterfort/usufon.h"
#include "asterfort/usunew.h"
    real(kind=8) :: lsup, vusurt(*), vusuro(*), prust(*), para(7)
    character(len=4) :: crit
    character(len=24) :: type, typ1, typ2
!
!-----------------------------------------------------------------------
    integer :: i, ire1, ire2, iret, n1, n2, n3
    integer :: n4, n5, nbinst
    real(kind=8) :: aimp, angl, cst1, cst2, de, depi, des3
    real(kind=8) :: des5, df, epsi, rapp, rayo
    real(kind=8) :: rayt, resu, un, uns3, uns5, v1, v2
    real(kind=8) :: vulim, x1, x11, x2, xla, zero
!-----------------------------------------------------------------------
    zero = 0.d0
    un = 1.d0
    de = 2.d0
    uns3 = un / 3.d0
    des3 = de / 3.d0
    uns5 = un / 5.d0
    des5 = de / 5.d0
    depi = r8depi()
    crit = 'RELA'
    epsi = 1.d-06
    para(1) = zero
    para(2) = zero
    para(3) = zero
    para(4) = zero
    para(5) = zero
    para(6) = zero
    para(7) = zero
!
    call getvtx(' ', 'CONTACT', scal=type, nbret=n1)
!
!     --- TUBE - BARRE ANTI VIBRATOIRE ---
    if (type(1:8) .eq. 'TUBE_BAV') then
        call getvr8(' ', 'RAYON_MOBILE', scal=rayt, nbret=n1)
        call getvr8(' ', 'LARGEUR_OBST', scal=lsup, nbret=n2)
        call getvr8(' ', 'ANGL_INCLI', scal=angl, nbret=n3)
        call getvr8(' ', 'ANGL_IMPACT', scal=aimp, nbret=n4)
        if (n4 .ne. 0) then
            rapp = cos ( aimp * r8dgrd() )
        else
            rapp = un
        endif
        if (n3 .eq. 0) then
            cst1 = ( un / ( de * rayt ) ) ** uns3
            cst2 = 3.d0 / ( 4.d0 * lsup )
            do 10 i = 1, nbinst
                v1 = vusurt(i)*rapp + vusuro(i)*rapp
                v2 = vusurt(i)*rapp / v1
                prust(i) = v2 * cst1 * ( ( cst2 * v1 ) ** des3 )
10          continue
        else
            angl = angl * r8dgrd()
            xla = lsup * angl
            cst1 = ( un / ( de * rayt ) ) ** uns5
            cst2 = 15.d0 * angl / 8.d0
            para(1) = rayt
            para(2) = lsup
            para(3) = angl
            x1 = xla
            x2 = rayt
            do 12 i = 1, nbinst
                v1 = vusurt(i)*rapp + vusuro(i)*rapp
                v2 = vusurt(i)*rapp / v1
                prust(i) = v2 * cst1 * ( ( cst2 * v1 ) ** des5 )
                if (prust(i) .gt. xla) then
                    para(4) = vusurt(i)*rapp
                    para(5) = vusuro(i)*rapp
                    if (prust(i) .ge. x2) then
                        call u2mess('A', 'PREPOST4_83')
                        prust(i) = 9999.d0
                        goto 12
                    endif
                    call usunew(type, para, crit, epsi, x1,&
                                x2, resu, iret)
                    if (iret .eq. 0) then
                        prust(i) = resu
                        x1 = resu
                    else
                        prust(i) = 9999.d0
                    endif
                endif
12          continue
        endif
!
!     --- TUBE - TROU CIRCULAIRE ---
    else if (type(1:12) .eq. 'TUBE_ALESAGE') then
        call getvr8(' ', 'RAYON_MOBILE', scal=rayt, nbret=n1)
        call getvr8(' ', 'RAYON_OBST', scal=rayo, nbret=n2)
        call getvr8(' ', 'LARGEUR_OBST', scal=lsup, nbret=n3)
        call getvr8(' ', 'ANGL_INCLI', scal=angl, nbret=n4)
        if (n4 .ne. 0) angl = angl * r8dgrd()
        para(1) = rayt
        para(2) = rayo
        para(3) = lsup
        para(4) = angl
        if (n2 .eq. 0) then
            do 20 i = 1, nbinst
                prust(i) = vusurt(i) / ( depi * lsup * rayt )
20          continue
        else
            x1 = zero
            x2 = rayt
            if (n4 .eq. 0) then
                do 22 i = 1, nbinst
                    para(5) = vusurt(i)
                    para(6) = vusuro(i)
                    call usubis(type, para, crit, epsi, x1,&
                                x2, resu, iret)
                    if (iret .eq. 0) then
                        if (resu .ge. x2) then
                            call u2mess('A', 'PREPOST4_83')
                            prust(i) = 9999.d0
                            goto 22
                        endif
                        prust(i) = resu
                        x1 = resu
                    else
                        prust(i) = 9999.d0
                    endif
22              continue
            else
!            --- CAS 3 OU D < L * THETA ---
                typ1 = 'TUBE_ALESAG_3A'
!            --- CAS 3 OU D > L * THETA ---
                typ2 = 'TUBE_ALESAG_3B'
!
                xla = lsup * angl
                x1 = zero
                x2 = de * rayo
                do 24 i = 1, nbinst
                    para(5) = vusurt(i)
                    para(6) = vusuro(i)
                    call usubis(typ1, para, crit, epsi, x1,&
                                x2, resu, ire1)
                    if (ire1 .eq. 0) then
                        if (resu .gt. xla) then
                            call usubis(typ2, para, crit, epsi, x1,&
                                        x2, resu, ire2)
                            if (ire2 .eq. 0) then
                                prust(i) = resu
                                x1 = resu
                            else
                                prust(i) = 9999.d0
                            endif
                        else
                            prust(i) = resu
                            x1 = resu
                        endif
                    else
                        prust(i) = 9999.d0
                    endif
24              continue
            endif
        endif
!
!     --- TUBE - TROU QUADRIFOLIE OU TRIFOLIE ---
        elseif ( type(1:11) .eq. 'TUBE_4_ENCO' .or. type(1:11) .eq.&
    'TUBE_3_ENCO' ) then
        call getvr8(' ', 'RAYON_MOBILE', scal=para(1), nbret=n1)
        call getvr8(' ', 'RAYON_OBST', scal=para(2), nbret=n2)
        call getvr8(' ', 'LARGEUR_OBST', scal=para(3), nbret=n3)
        call getvr8(' ', 'ANGL_INCLI', scal=para(4), nbret=n4)
        call getvr8(' ', 'ANGL_ISTHME', scal=para(7), nbret=n5)
        if (n4 .ne. 0) para(4) = para(4) * r8dgrd()
        para(7) = para(7) * r8dgrd()
        x1 = zero
        x2 = para(1)
        if (n4 .eq. 0) then
            do 30 i = 1, nbinst
                if (type(1:11) .eq. 'TUBE_4_ENCO') then
                    para(5) = vusurt(i) / de
                    para(6) = vusuro(i) / de
                else
                    para(5) = vusurt(i)
                    para(6) = vusuro(i)
                endif
                call usubis(type, para, crit, epsi, x1,&
                            x2, resu, iret)
                if (iret .eq. 0) then
                    if (resu .ge. x2) then
                        call u2mess('A', 'PREPOST4_83')
                        prust(i) = 9999.d0
                        goto 30
                    endif
                    prust(i) = resu
                    x1 = resu
                else
                    prust(i) = 9999.d0
                endif
30          continue
        else
!           --- CAS 2 OU D < L * THETA ---
            typ1 = 'TUBE_ENCO_2A'
!           --- CAS 2 OU D > L * THETA ---
            typ2 = 'TUBE_ENCO_2B'
!
            xla = para(3) * para(4)
            x1 = zero
            x2 = de * para(2)
            do 32 i = 1, nbinst
                if (type(1:11) .eq. 'TUBE_4_ENCO') then
                    para(5) = vusurt(i) / de
                    para(6) = vusuro(i) / de
                else
                    para(5) = vusurt(i)
                    para(6) = vusuro(i)
                endif
                call usubis(typ1, para, crit, epsi, x1,&
                            x2, resu, ire1)
                if (ire1 .eq. 0) then
                    if (resu .gt. xla) then
                        call usubis(typ2, para, crit, epsi, x1,&
                                    x2, resu, ire2)
                        if (ire2 .eq. 0) then
                            prust(i) = resu
                            x1 = resu
                        else
                            prust(i) = 9999.d0
                        endif
                    else
                        prust(i) = resu
                        x1 = resu
                    endif
                else
                    prust(i) = 9999.d0
                endif
32          continue
        endif
!
!     --- TUBE - TUBE ---
    else if (type(1:9) .eq. 'TUBE_TUBE') then
        call getvr8(' ', 'RAYON_MOBILE', scal=rayt, nbret=n1)
        call getvr8(' ', 'ANGL_INCLI', scal=angl, nbret=n2)
        cst1 = ( un / ( de * rayt ) ) ** uns5
        cst2 = 15.d0 * angl * r8dgrd() / 8.d0
        do 40 i = 1, nbinst
            prust(i) = cst1 * ( ( cst2 * vusurt(i) ) ** des5 )
40      continue
!
!     --- GRAPPE - ALESAGE ---
    else if (type(1:14) .eq. 'GRAPPE_ALESAGE') then
        call getvr8(' ', 'RAYON_MOBILE', scal=para(1), nbret=n1)
        call getvr8(' ', 'RAYON_OBST', scal=para(2), nbret=n2)
        x11 = zero
        x2 = para(2)
        do 50 i = 1, nbinst
            prust(i) = 9999.d0
            para(5) = vusurt(i)
            call usubis(type, para, crit, epsi, x11,&
                        x2, resu, iret)
            if (iret .eq. 0) then
                if (resu .ge. x2) then
                    call u2mess('A', 'PREPOST4_83')
                    goto 50
                endif
                prust(i) = resu
                x11 = resu
            endif
50      continue
!
!     --- GRAPPE - 1 ENCOCHE ---
!     --- GRAPPE - 2 ENCOCHE ---
        elseif ( type(1:13) .eq. 'GRAPPE_1_ENCO' .or. type(1:13) .eq.&
    'GRAPPE_2_ENCO' ) then
        if (type(1:13) .eq. 'GRAPPE_2_ENCO') then
            para(1) = -48.89d+03 / 11.d0
            para(2) = 106.03d0 / 11.d0
            para(3) = -0.88d-03 / 11.d0
        else
            para(1) = -0.5d0 * 48.89d+03 / 11.d0
            para(2) = 0.5d0 * 106.03d0 / 11.d0
            para(3) = -0.5d0 * 0.88d-03 / 11.d0
        endif
        x11 = zero
        x2 = 0.00144d0
        para(5) = zero
        call usufon(type, para, x2, vulim, df)
        do 60 i = 1, nbinst
            prust(i) = 9999.d0
            para(5) = vusurt(i)
            if (vusurt(i) .gt. vulim) goto 62
            call usunew(type, para, crit, epsi, x11,&
                        x2, resu, iret)
            if (iret .eq. 0) then
                if (resu .ge. x2) then
                    call u2mess('A', 'PREPOST4_83')
                    goto 62
                endif
                prust(i) = resu
                x11 = resu
            endif
62          continue
60      continue
!
    endif
!
end subroutine
