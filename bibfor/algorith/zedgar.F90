subroutine zedgar(matos, tm, tp, instp, dinst,&
                  vim, vip)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterc/r8prem.h"
#include "asterc/r8t0.h"
#include "asterfort/assert.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
#include "asterfort/zevolu.h"
    integer :: matos
    real(kind=8) :: tm, tp, instp, dinst, vim(4), vip(4)
!
!............................................
! CALCUL PHASE METALLURGIQUE POUR EDGAR
!............................................
! IN   MATOS  :
! IN   TM     : TEMPERATURE A L INSTANT MOINS
! IN   TP     : TEMPERATURE A L INSTANT PLUS
! IN   INSTP  : INSTANT PLUS
! IN   DINST  : INCREMENT DE L INSTANT = INSTP-INSTM
! IN   VIM    : VARIABLES INTERNES A L INSTANT MOINS
! OUT  VIP    : VARIABLES INTERNES A L INSTANT PLUS
! SIGNIFICATION DES VARIABLES INTERNES
! VI(1) = PROPORTION DE PHASE FROIDE ALPHA PUR
! VI(2) = PROPORTION DE PHASE ALPHABETA
! VI(3) = TEMPERATURE
! VI(4) = TEMPS CORRESPONDANT A LA TEMPERATURE D EQUILIBRE
!         SOIT DE DEBUT DE TRANSFORMATION TDEQ
!         SOIT DE FIN DE TRANSFORMATION TFEQ
!         CETTE VARIABLE SERT POUR CALCULER LA VITESSE EN TEMPERATURE
!         (METHODE SECANTE GLISSANTE - CF DOC R40404)
!
    integer :: test, iter, kpg, spt
    real(kind=8) :: valres(12), tdeq, tfeq, k, n, t1c, t2c, ac, m, qsr, coeffc
    real(kind=8) :: t1r, t2r, ar, br, tabs, tk, tc, tr
    real(kind=8) :: vitesc, vitesr, dtemp
    real(kind=8) :: zp, zm, zeq, zinf, zsup, g, dg, zalphm, zalphp
    real(kind=8) :: zero
    character(len=24) :: nomres(12)
    integer :: icodre(12)
    integer :: cine, chau, refr
    parameter     (chau=1,refr=0)
    character(len=8) :: fami, poum
!
! 1 - CARACTERISTIQUE MATERIAU
! 1.1 - MODELE A L EQUILIBRE
!
    nomres(1)='TDEQ'
    nomres(2)='K'
    nomres(3)='N'
!
! 1.2 - MODELE AU CHAUFFAGE
!
    nomres(4)='T1C'
    nomres(5)='T2C'
    nomres(6)='AC'
    nomres(7)='M'
    nomres(8)='QSR_K'
!
! 1.3 - MODELE AU REFROIDISSEMENT
!
    nomres(9)='T1R'
    nomres(10)='T2R'
    nomres(11)='AR'
    nomres(12)='BR'
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    call rcvalb(fami, kpg, spt, poum, matos,&
                ' ', 'META_ZIRC', 1, 'TEMP', [tp],&
                12, nomres, valres, icodre, 1)
!
    tdeq = valres(1)
    k = valres(2)
    n = valres(3)
    t1c = valres(4)
    t2c = valres(5)
    ac = valres(6)
    m = valres(7)
    qsr = valres(8)
    t1r = valres(9)
    t2r = valres(10)
    ar = valres(11)
    br = valres(12)
!
    tabs=r8t0()
    tk=tp+tabs
    coeffc=ac*exp(-qsr/tk)
!
! 2 - PREPARATION - Z = FRACTION DE PHASE BETA
! 2.1 - CALCUL DE LA FRACTION DE Z A L INSTANT MOINS
!
    zalphm=vim(1)+vim(2)
    zm = 1.d0-zalphm
    zero=r8prem()
!
    if (abs(zm) .le. zero) zm=0.d0
    if (abs(zalphm) .le. zero) zm=1.d0
    if ((zm.le.1.d-05) .and. (tp.le.tdeq)) zm=0.d0
!
! 2.2 - CALCUL DE LA TEMPERATURE A L EQUILIBRE DE FIN DE TRANSFOR
!       CORRESPONDANT A 0.99 DE Z
!
    tfeq = tdeq+log(1.d0/0.01d0)**(1.d0/n)/k
!
! 2.3 - CALCUL DE LA FRACTION DE Z A L EQUILIBRE
!
    if (tp .le. tdeq) then
        zeq=0.d0
    else if (tp .gt. tdeq .and. tp .le. tfeq) then
        zeq=1.d0-exp(-((k*(tp-tdeq))**n))
    else
        zeq=1.d0
    endif
!
! 2.4 - SENS DE L'EVOLUTION METALLURGIQUE
!
    if (zm .le. 0.d0) then
        cine = chau
    else if ((zm .gt. 0.d0).and.(zm .lt. 1.d0)) then
        if (zm .lt. zeq) then
            cine = chau
        else
            cine = refr
        endif
    else if (zm .ge. 1.d0) then
        cine = refr
    endif
!
! 3 - SI ZM=0 OU ZM=1
! 3.1 - RECHERCHE DE L INSTANT CORRESPONDANT A TDEQ OU TFEQ
!       STOCKE DANS VIP(4)
! 3.2 - PUIS CALCUL DE TC OU TR POUR SAVOIR SI LA TRANSFORMATON DEBUTE
!       CHAUFFAGE ZM=0 => SI TP > TDEQ : TC=AC(VITESC)**M
!       AVEC VITESC LA VITESSE AU CHAUFFAGE
!       ET SI TP <= TC PAS DE TRANSFORMATION => TEST=1
!       REFROIDISSEMENT ZM=1 => SI TP < TFEQ : TR=AR+BR*LN(VITESR)
!       AVEC VITESR LA VITESSE AU REFROIDISSEMENT
!       ET SI TP >= TR PAS DE TRANSFORMATION => TEST=1
!
! TEST=0 ON INTEGRE LES EQUATIONS D EVOLUTION
! TEST=1 ZP=1 OU 0
!
    test=0
!
! 3 - RECHERCHE DE L INSTANT CORRESPONDANT A TDEQ ET TFEQ
!
    if (zm .eq. 0.d0) then
        if ((tdeq .ge. tm) .and. (tdeq .le. tp)) then
            if (tp .eq. tm) then
                vip(4)=instp
            else
                dtemp=tp-tm
                vip(4)=instp+(dinst/dtemp)*(tdeq-tp)
            endif
        else
            vip(4)=vim(4)
        endif
!
! 3.1 - CALCUL DE TC
!
        if (tp .gt. tdeq) then
            vitesc=(tp-tdeq)/(instp-vip(4))
            if (vitesc .lt. 0.d0) then
                test=1
            else
                if (vitesc .lt. 0.1d0) then
                    tc=tdeq
                else
                    tc=t1c*(vitesc**t2c)
                endif
                if (tp .le. tc) test=1
            endif
        else
            test = 1
        endif
    endif
!
    if (zm .eq. 1.d0) then
        if ((tfeq .ge. tp) .and. (tfeq .le. tm)) then
            if (tp .eq. tm) then
                vip(4)=instp
            else
                dtemp=tp-tm
                vip(4)=instp+(dinst/dtemp)*(tfeq-tp)
            endif
        else
            vip(4)=vim(4)
        endif
!
! 3.2 - CALCUL DE TR
!
        if (tp .lt. tfeq) then
            vitesr=(tp-tfeq)/(instp-vip(4))
            if (vitesr .gt. 0.d0) then
                test=1
            else
                if (vitesr .eq. 0.d0) then
                    tr=tfeq
                else
                    tr=t1r+t2r*log(abs(vitesr))
                    if (tr .gt. tfeq) tr=tfeq
                endif
                if (tp .ge. tr) test=1
            endif
        else
            test = 1
        endif
    endif
!
    if ((zm.ne.0.d0) .and. (zm.ne.1.d0)) vip(4)=vim(4)
!
! 4 - DETERMINSATION DE ZP
!
    if (test .eq. 1) then
        if (zm .eq. 0.d0) zp=0.d0
        if (zm .eq. 1.d0) zp=1.d0
    else
!
! 4.1 - CAS PARTICULIER
!
        if (cine .eq. chau) then
            if (zeq .gt. 0.99d0) then
                call zevolu(cine, 0.99d0, zm, dinst, tp,&
                            k, n, tdeq, tfeq, coeffc,&
                            m, ar, br, g, dg)
                if (g .lt. 0.d0) then
                    zp=1.d0
                    goto 100
                endif
            endif
        endif
!
! 4.2 - METODE DE NEWTON AVEC BORNES CONTROLEES
! 4.2.1 - MINORANT ET MAJORANT
!
        if (cine .eq. chau) then
            zinf=zm
            zsup=zeq
        else
            zinf=zeq
            zsup=zm
        endif
!
! 4.2.2 - INITIALISATION
!
        zp=zm
        if (zm .eq. 0.d0) zp=zeq/2.d0
        if (zm .eq. 1.d0) zp=(zm+zeq)/2.d0
!
        call zevolu(cine, zp, zm, dinst, tp,&
                    k, n, tdeq, tfeq, coeffc,&
                    m, ar, br, g, dg)
!
! 4.2.3 - RESOLUTION PAR UNE METHODE DE NEWTON ENTRE LES BORNES
!
        do 10 iter = 1, 15
            if (abs(g) .le. 1.d-06) goto 100
!
            if (dg .eq. 0.d0) then
                call utmess('F', 'ALGORITH16_96')
            endif
            zp = zp - g/dg
            if (zp .le. zinf .or. zp .ge. zsup) zp=(zinf+zsup)/2.d0
!
            call zevolu(cine, zp, zm, dinst, tp,&
                        k, n, tdeq, tfeq, coeffc,&
                        m, ar, br, g, dg)
!
            if (g .ge. 0.d0) zsup = zp
            if (g .le. 0.d0) zinf = zp
!
10      continue
        ASSERT(.false.)
100      continue
    endif
!
    vip(3)=tp
    zalphp=1.d0-zp
!
! 6 - CREATION DES DEUX PHASES
!
    if (zp .gt. 0.1d0) then
        vip(1) =0.d0
    else
        vip(1)=10.d0*(zalphp-0.9d0)*zalphp
    endif
    if (vip(1) .le. zero) vip(1) =0.d0
    vip(2)=zalphp-vip(1)
    if (vip(2) .le. zero) vip(2) =0.d0
!
end subroutine
