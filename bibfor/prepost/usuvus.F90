subroutine usuvus(puusur, vusur, nbinst, temps, isupp,&
                  nbpt, fn, vg, iret)
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
!     CALCULE LE VOLUME USE
!
! IN  : PUUSUR : PUISSANCE USURE
! OUT : VUSUR  : VOLUME USE
! IN  : NBINST : NOMBRE D'INSTANTS
! IN  : TEMPS  : LES INSTANTS
! VAR : ISUPP  : = 1, CALCULE LE VOLUME USE MOBILE
!                = 2, CALCULE LE VOLUME USE OBSTACLE
!                NE CALCULE PAS LE VOLUME USE OBSTACLE, ISUPP = 0
!-----------------------------------------------------------------------
    include 'asterc/getfac.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/usuban.h'
    include 'asterfort/usukwu.h'
    real(kind=8) :: vusur(*), temps(*), para(7), fn(*), vg(*)
    character(len=8) :: k8b
    character(len=24) :: loi, mate
    integer :: iarg
!
!-----------------------------------------------------------------------
    integer :: i, ifires, iret, isupp, n1, n2
    integer :: n3, n4, n5, n6, nbinst, nbpt, nn
!
    real(kind=8) :: puusur, t, v0, w, x1, xa, xb
    real(kind=8) :: xd, xk, xn, xs
!-----------------------------------------------------------------------
    ifires = iunifi('RESULTAT')
!
    call getvtx(' ', 'LOI_USURE', 0, iarg, 1,&
                loi, n1)
    iret = 0
    k8b=' '
!
! **********************************************************************
!                 M O D E L E     A R C H A R D
! **********************************************************************
!
    if (loi(1:7) .eq. 'ARCHARD') then
        if (isupp .eq. 1) then
            write(ifires,1000)
            call getvr8('MOBILE', 'COEF_USURE', 1, iarg, 1,&
                        xk, n1)
            if (n1 .eq. 0) then
                call getvtx(' ', 'MATER_USURE', 0, iarg, 1,&
                            mate, n2)
                call usuban(mate, isupp, para, iret)
                xk = para(1)
            endif
            write(ifires,2100)
        else if (isupp .eq. 2) then
            call getvr8('OBSTACLE', 'COEF_USURE', 1, iarg, 1,&
                        xk, n1)
            if (n1 .eq. 0) then
                call getvtx(' ', 'USURE_OBST', 0, iarg, 1,&
                            k8b, n2)
                if (k8b(1:3) .eq. 'OUI') then
                    call getvtx(' ', 'MATER_USURE', 0, iarg, 1,&
                                mate, n3)
                    call usuban(mate, isupp, para, iret)
                    xk = para(1)
                else
                    isupp = 0
                    goto 9999
                endif
            endif
            write(ifires,2200)
        endif
        write(ifires,2010) xk
        do 10 i = 1, nbinst
            vusur(i) = xk * puusur * temps(i)
10      continue
!
! **********************************************************************
!                 M O D E L E     K W U _ E P R I
! **********************************************************************
!
    else if (loi(1:8) .eq. 'KWU_EPRI') then
        if (isupp .eq. 1) then
            write(ifires,1010)
            call getvr8('MOBILE', 'COEF_USURE', 1, iarg, 1,&
                        para(1), n1)
            call getvr8('MOBILE', 'COEF_FNOR', 1, iarg, 1,&
                        para(2), n2)
            call getvr8('MOBILE', 'COEF_VTAN', 1, iarg, 1,&
                        para(3), n3)
            call getvr8('MOBILE', 'COEF_K', 1, iarg, 1,&
                        para(4), n4)
            call getvr8('MOBILE', 'COEF_C', 1, iarg, 1,&
                        para(5), n5)
            if (n4 .eq. 0) para(4) = 5.d0
            if (n5 .eq. 0) para(5) = 10.d0
            call getvtx(' ', 'MATER_USURE', 0, iarg, 1,&
                        mate, n6)
            if (n6 .ne. 0) then
                call usuban(mate, isupp, para, iret)
            endif
            write(ifires,2100)
        else if (isupp .eq. 2) then
            call getvr8('OBSTACLE', 'COEF_USURE', 1, iarg, 1,&
                        para(1), n1)
            call getvr8('OBSTACLE', 'COEF_FNOR', 1, iarg, 1,&
                        para(2), n2)
            call getvr8('OBSTACLE', 'COEF_VTAN', 1, iarg, 1,&
                        para(3), n3)
            call getvr8('OBSTACLE', 'COEF_K', 1, iarg, 1,&
                        para(4), n4)
            call getvr8('OBSTACLE', 'COEF_C', 1, iarg, 1,&
                        para(5), n5)
            if (n4 .eq. 0) para(4) = 5.d0
            if (n5 .eq. 0) para(5) = 10.d0
            call getvtx(' ', 'MATER_USURE', 0, iarg, 1,&
                        mate, n6)
            if (n6 .ne. 0) then
                call getvtx(' ', 'USURE_OBST', 0, iarg, 1,&
                            k8b, n2)
                if (k8b(1:3) .eq. 'OUI') then
                    call usuban(mate, isupp, para, iret)
                else
                    isupp = 0
                    goto 9999
                endif
            endif
            nn = n1 + n2 + n3 + n4 + n5
            if (nn .eq. 0) then
                isupp = 0
                goto 9999
            endif
            write(ifires,2200)
        endif
        write(ifires,2010) para(1)
        write(ifires,2050) para(3)
        write(ifires,2060) para(2)
        write(ifires,2070) para(4)
        write(ifires,2080) para(5)
        call usukwu(nbpt, fn, vg, para, w,&
                    iret)
        if (iret .eq. 10) then
            call u2mess('F', 'PREPOST4_85')
        endif
        do 20 i = 1, nbinst
            vusur(i) = para(1) * w * puusur * temps(i)
20      continue
!
! **********************************************************************
!                 M O D E L E     E D F _ M Z
! **********************************************************************
!
    else if (loi(1:6) .eq. 'EDF_MZ') then
        if (isupp .eq. 1) then
            write(ifires,1020)
            call getvr8('MOBILE', 'COEF_S', 1, iarg, 1,&
                        xs, n1)
            call getvr8('MOBILE', 'COEF_B', 1, iarg, 1,&
                        xb, n2)
            call getvr8('MOBILE', 'COEF_N', 1, iarg, 1,&
                        xn, n3)
            call getvr8('MOBILE', 'COEF_USURE', 1, iarg, 1,&
                        xa, n4)
            if (n1 .eq. 0) xs = 1.14d-16
            if (n2 .eq. 0) xb = 1.2d0
            if (n3 .eq. 0) xn = 2.44d-08
            if (n4 .eq. 0) xa = 1.d-13
            call getvtx(' ', 'MATER_USURE', 0, iarg, 1,&
                        mate, n5)
            if (n5 .ne. 0) then
                call usuban(mate, isupp, para, iret)
                xs = para(1)
                xb = para(2)
                xn = para(3)
                xa = para(4)
            endif
            write(ifires,2100)
        else if (isupp .eq. 2) then
            call getvr8('OBSTACLE', 'COEF_S', 1, iarg, 1,&
                        xs, n1)
            call getvr8('OBSTACLE', 'COEF_B', 1, iarg, 1,&
                        xb, n2)
            call getvr8('OBSTACLE', 'COEF_N', 1, iarg, 1,&
                        xn, n3)
            call getvr8('OBSTACLE', 'COEF_USURE', 1, iarg, 1,&
                        xa, n4)
            if (n1 .eq. 0) xs = 1.14d-16
            if (n2 .eq. 0) xb = 1.2d0
            if (n3 .eq. 0) xn = 2.44d-08
            if (n4 .eq. 0) xa = 1.d-13
            call getvtx(' ', 'MATER_USURE', 0, iarg, 1,&
                        mate, n5)
            if (n5 .ne. 0) then
                call getvtx(' ', 'USURE_OBST', 0, iarg, 1,&
                            k8b, n6)
                if (k8b(1:3) .eq. 'OUI') then
                    call usuban(mate, isupp, para, iret)
                    xs = para(2)
                    xb = para(3)
                    xn = para(4)
                    xa = para(1)
                else
                    isupp = 0
                    goto 9999
                endif
            endif
            call getfac('OBSTACLE', n6)
            nn = n1 + n2 + n3 + n4 + n5 + n6
            if (nn .eq. 0) then
                isupp = 0
                goto 9999
            endif
            write(ifires,2200)
        endif
        write(ifires,2010) xa
        write(ifires,2020) xs
        write(ifires,2030) xb
        write(ifires,2040) xn
        v0 = xa * ( puusur ** xb )
        xd = xs / v0
        if (xd .gt. 1.d0) then
            iret = 10
            call u2mess('I', 'PREPOST4_86')
            call u2mess('I', 'PREPOST4_87')
            goto 9999
        endif
        x1 = ( 1.d0 - xd ) / xn
        do 30 i = 1, nbinst
            t = temps(i)
            vusur(i) = v0 * ( xd*t + x1*( 1.d0 - exp(-xn*t) ) )
30      continue
!
    endif
!
    1000 format (/,'******* MODELE ARCHARD *******')
    1010 format (/,'******* MODELE KWU_EPRI *******')
    1020 format (/,'******* MODELE EDF_MZ *******')
    2100 format (/,'===> COEFFICIENT(S) UTILISE(S) POUR LE MOBILE :')
    2200 format (/,'===> COEFFICIENT(S) UTILISE(S) POUR L''OBSTACLE :')
    2010 format (1p,4x,'       COEFFICIENT D''USURE : ',e12.5)
    2020 format (1p,4x,'                     SEUIL : ',e12.5)
    2030 format (1p,4x,'                  EXPOSANT : ',e12.5)
    2040 format (1p,4x,'    TAUX DE RALENTISSEMENT : ',e12.5)
    2050 format (1p,4x,' COEFFICIENT DE GLISSEMENT : ',e12.5)
    2060 format (1p,4x,'      COEFFICIENT D''IMPACT : ',e12.5)
    2070 format (1p,4x,'               CONSTANTE K : ',e12.5)
    2080 format (1p,4x,'               CONSTANTE C : ',e12.5)
!
9999  continue
end subroutine
