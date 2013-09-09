subroutine nmtevp(fami, kpg, ksp, ndim, typmod,&
                  imate, compor, crit, instam, instap,&
                  deps, sigm, vim, option, sigp,&
                  vip, dsidep, demu, cinco, iret)
!
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
! person_in_charge: sebastien.fayolle at edf.fr
! aslint: disable=
    implicit none
#include "asterfort/eccook.h"
#include "asterfort/nmcri9.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utlcal.h"
#include "asterfort/utmess.h"
#include "asterfort/verift.h"
#include "asterfort/zerofr.h"
    integer :: ndim, imate, kpg, ksp, iret
!
    real(kind=8) :: crit(6), instam, instap
    real(kind=8) :: deps(6), deuxmu, demu, cinco
    real(kind=8) :: sigm(6), vim(5), sigp(6), vip(5), dsidep(6, 6)
!
    character(len=*) :: fami
    character(len=8) :: typmod(*)
    character(len=16) :: compor(*), option
! ----------------------------------------------------------------------
!     INTEGRATION DE LA LOI DE JOHNSON-COOK
!     ELEMENTS ISOPARAMETRIQUES EN PETITES DEFORMATIONS
!
! IN  KPG,KSP  : NUMERO DU (SOUS)POINT DE GAUSS
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  IMATE   : ADRESSE DU MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT : RELCOM ET DEFORM
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  INSTAM  : INSTANT DU CALCUL PRECEDENT
! IN  INSTAP  : INSTANT DU CALCUL
! IN  DEPS    : INCREMENT DE DEFORMATION
!               SI C_PLAN DEPS(3) EST EN FAIT INCONNU (ICI:0)
!                 =>  ATTENTION LA PLACE DE DEPS(3) EST ALORS UTILISEE.
! IN  SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
! IN  VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
! IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
! OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL
! OUT VIP     : VARIABLES INTERNES A L'INSTANT ACTUEL
! OUT DSIDEP  : MATRICE CARREE (INUTILISE POUR RAPH_MECA)
!
!               ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
!               L'ORDRE :  XX,YY,ZZ,SQRT(2)*XY,SQRT(2)*XZ,SQRT(2)*YZ
! OUT DEUXMU,CINCO : POUR ELEMEMTS INCOMPRESSIBLES
! OUT IRET    : CODE RETOUR DE L'INTEGRATION DE LA LOI DE VOM MISES
!               = 1  => PAS DE PROBLEME
!               = 0  => ECHEC DANS L'INTEGRATION DE LA LOI
!
    common /rconm9/acook,bcook,ccook,npuis,mpuis,&
     &               epsp0,troom,tmelt,tp,dinst,sieleq,deuxmu,rprim,pm
!
    logical :: plasti, inco, dech
!
    integer :: ndimsi
    integer :: k, l, niter, i
    integer :: iret3, iret4, iret0, iret5
!
    real(kind=8) :: depsth(6), valres(8), epsthe, pm, co, dp0, tm, rprim0, precr
    real(kind=8) :: depsmo, sigmmo, e, nu, troisk, rprim, rp
    real(kind=8) :: sieleq, sigeps, seuil, dp, coef, sigy
    real(kind=8) :: kron(6), depsdv(6), sigmdv(6), sigpdv(6), sigdv(6)
    real(kind=8) :: em, num, troikm, deumum, sigmp(6), sigel(6), a
    real(kind=8) :: tp, defam(6), defap(6)
    real(kind=8) :: rac2, dpmax, fmax, coef1
    real(kind=8) :: sigpmo, dinst
    real(kind=8) :: acook, bcook, ccook, npuis, mpuis, epsp0, troom, tmelt
    real(kind=8) :: alpha, dirr, dte, dgdtsg, dkdtsk, khi, tpdsdt, epspet
!
    integer :: codret(8), iter
    character(len=6) :: epsa(6)
    character(len=8) :: nomres(8), materi
    character(len=16) :: meth
!
    data        kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
    data epsa   / 'EPSAXX','EPSAYY','EPSAZZ','EPSAXY','EPSAXZ',&
     &              'EPSAYZ'/
! DEB ------------------------------------------------------------------
!
!     -- 1 INITIALISATIONS :
!     ----------------------
    inco = typmod(2) .eq. 'INCO'
    dech = option(11:14).eq.'ELAS'
    materi = ' '
    if (inco) then
        co = 0.d0
    else
        co = 1.d0
    endif
    ndimsi = 2*ndim
    rac2 = sqrt(2.d0)
!
!     -- 2 RECUPERATION DES CARACTERISTIQUES
!     ---------------------------------------
    nomres(1)='E'
    nomres(2)='NU'
!
    call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                ksp, tm, iret3)
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                ksp, tp, iret4)
!
    do 19 k = 1, 6
        defam(k) = 0.d0
        defap(k) = 0.d0
19  end do
!
    do 20 k = 1, ndimsi
        call rcvarc(' ', epsa(k), '-', fami, kpg,&
                    ksp, defam(k), iret5)
        if (iret5 .ne. 0) defam(k)=0.d0
!
        call rcvarc(' ', epsa(k), '+', fami, kpg,&
                    ksp, defap(k), iret5)
        if (iret5 .ne. 0) defap(k)=0.d0
20  end do
!
! MISE AU FORMAT DES TERMES NON DIAGONAUX
!
    do 105 k = 4, ndimsi
        defam(k) = defam(k)*rac2
        defap(k) = defap(k)*rac2
105  end do
!
    call rcvalb(fami, kpg, ksp, '-', imate,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                2, nomres(1), valres(1), codret(1), 1)
    em = valres(1)
    num = valres(2)
    deumum = em/(1.d0+num)
    if (inco) then
        troikm = deumum
    else
        troikm = em/(1.d0-2.d0*num)
    endif
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                2, nomres(1), valres(1), codret(1), 1)
    e = valres(1)
    nu = valres(2)
!
    if (inco) then
        deuxmu = 2.d0*e/3.d0
        troisk = deuxmu
    else
        deuxmu = e/(1.d0+nu)
        troisk = e/(1.d0-2.d0*nu)
    endif
    call verift(fami, kpg, ksp, 'T', imate,&
                materi, 'ELAS', 1, epsthe, iret0)
!
    if (iret4 .eq. 0) then
        nomres(1)='ALPHA'
        call rcvalb(fami, kpg, ksp, '+', imate,&
                    ' ', 'ELAS', 0, ' ', [0.d0],&
                    1, nomres(1), valres(1), codret(1), 0)
        if (codret(1) .ne. 0) valres(1) = 0.d0
        alpha = valres(1)
    else
        alpha = 0.d0
    endif
!
!     -- 3 RECUPERATION DES CARACTERISTIQUES
!     ---------------------------------------
    plasti=(vim(2).ge.0.5d0)
!
    nomres(1)='A'
    nomres(2)='B'
    nomres(3)='N_PUIS'
    nomres(4)='C'
    nomres(5)='M_PUIS'
    nomres(6)='EPSP0'
    nomres(7)='TROOM'
    nomres(8)='TMELT'
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'ECRO_COOK', 0, ' ', [0.d0],&
                3, nomres, valres, codret, 1)
    acook = valres(1)
    bcook = valres(2)
    npuis = valres(3)
!
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'ECRO_COOK', 0, ' ', [0.d0],&
                5, nomres(4), valres(4), codret(4), 0)
!
    if (codret(4) .ne. 0) then
        valres(4) = 0.d0
        valres(6) = 1.d0
    endif
    ccook = valres(4)
    epsp0 = valres(6)
    if (codret(5) .ne. 0) then
        valres(5) = 0.d0
        valres(7) = -1.d0
        valres(8) = 1.d0
    endif
    mpuis = valres(5)
    troom = valres(7)
    tmelt = valres(8)
    dinst = instap-instam
!
    if (iret3 .ne. 0 .and. iret4 .ne. 0) then
        tm = troom
        tp = troom
    endif
!
    call eccook(acook, bcook, ccook, npuis, mpuis,&
                epsp0, troom, tmelt, tm, vim(4),&
                vim(1), vim(3), rp, rprim)
!
!
    demu = deuxmu
    if (inco) then
        cinco =(1.d0-2.d0*nu)/nu
    endif
!
!     -- 4 CALCUL DE DEPSMO ET DEPSDV :
!     --------------------------------
!
    depsmo = 0.d0
    do 110 k = 1, 3
        depsth(k) = deps(k) -epsthe -(defap(k)-defam(k))
        depsth(k+3) = deps(k+3)-(defap(k+3)-defam(k+3))
        depsmo = depsmo + depsth(k)
110  end do
    depsmo = depsmo/3.d0
    do 115 k = 1, ndimsi
        depsdv(k) = depsth(k) - depsmo * kron(k)*co
115  end do
!
!     -- 5 CALCUL DE SIGMP :
!     ----------------------
    sigmmo = 0.d0
    do 113 k = 1, 3
        sigmmo = sigmmo + sigm(k)
113  end do
    sigmmo = sigmmo /3.d0
    do 114 k = 1, ndimsi
        sigmp(k)=deuxmu/deumum*(sigm(k)-sigmmo*kron(k)) + troisk/&
        troikm*sigmmo*kron(k)
114  end do
!
!     -- 6 CALCUL DE SIGMMO, SIGMDV, SIGEL, SIELEQ ET SEUIL :
!     -------------------------------------------------------
    sigmmo = 0.d0
    do 116 k = 1, 3
        sigmmo = sigmmo + sigmp(k)
116  end do
    sigmmo = sigmmo /3.d0
    sieleq = 0.d0
    do 117 k = 1, ndimsi
        sigmdv(k) = sigmp(k)- sigmmo * kron(k)
        sigel(k) = sigmdv(k) + deuxmu * depsdv(k)
        sieleq = sieleq + sigel(k)**2
117  end do
    sieleq = sqrt(1.5d0*sieleq)
    seuil = sieleq - rp
!
!=======================================================================
!     -- 7 CALCUL DE SIGP,SIGPDV,VIP,DP,RP:
!=======================================================================
!
    dp=0.d0
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
!
!       -- 7.1 CALCUL DE DP :
!
        if (seuil .le. 0.d0) then
            dp = 0.d0
            vip(2) = 0.d0
            vip(3)=dp
            vip(4)=dinst
        else
!
            pm = vim(1)
!
            dp0 = (sieleq - rp)/(1.5d0*deuxmu)
            call eccook(acook, bcook, ccook, npuis, mpuis,&
                        epsp0, troom, tmelt, tp, dinst,&
                        pm, dp0, sigy, rprim0)
            precr = crit(3) * sigy
            niter = nint(crit(1))
!     -------------------------------------------------------
!
! ---        F0 < 0 , ON CHERCHE DPMAX PAS TROP GRAND TEL QUE FMAX < 0
!
            fmax=sigy
            dpmax=dp0
            coef1=2.d0
            if (abs(fmax) .le. precr) then
                dp = dpmax
                iter=1
                goto 50
            else if (fmax.gt.0.d0) then
!               FMAX > 0.
!               VERIFICATION QUE DPMAX N'EST PAS TROP GRAND.BRACKETTING
                do 31 i = 1, niter
                    dpmax = dpmax/coef1
                    fmax=nmcri9(dpmax)
                    if (abs(fmax) .le. precr) then
                        dp = dpmax
                        iter=i
                        goto 50
                    else if (fmax.lt.0.d0) then
!                     ON RECALCULE LA VALEUR PRECEDENTE DE DPMAX
                        dpmax = dpmax*coef1
                        fmax=nmcri9(dpmax)
                        goto 21
                    endif
31              continue
                goto 21
!
            else
!               FMAX <0. On augmente DPMAX jusqu'A ce que F(DPMAX) > 0
                do 32 i = 1, niter
                    fmax=nmcri9(dpmax)
                    if (abs(fmax) .le. precr) then
                        dp = dpmax
                        iter=i
                        goto 50
                    else if (fmax.gt.0.d0) then
                        goto 21
                    else
                        dpmax = dpmax*coef1
                    endif
32              continue
                call utmess('A', 'ALGORITH6_79')
                goto 21
            endif
!
21          continue
!
!
!     -------------------------------------------------------
!            RESOLUTION 1D
!     -------------------------------------------------------
!            RECUPERATION DE L'ALGORITHME DE RESOLUTION 1D
            call utlcal('VALE_NOM', meth, crit(6))
!     -------------------------------------------------------
            call zerofr(2, meth, nmcri9, 0.d0, dpmax,&
                        precr, niter, dp, iret, iter)
!
50          continue
!
            vip(2)=iter
!
            if (iret .eq. 1) goto 9999
!
            call eccook(acook, bcook, ccook, npuis, mpuis,&
                        epsp0, troom, tmelt, tp, dinst,&
                        pm, dp, rp, rprim)
            vip(3)=dp
            vip(4)=dinst
!
        endif
!
        vip(1) = vim(1) + dp
        plasti=(vip(2).ge.0.5d0)
!
!         -- 7.2 CALCUL DE SIGP :
!         -----------------------
!
        sieleq = 0.d0
        sigpmo = sigmmo + co*troisk*depsmo
        do 160 k = 1, ndimsi
            sigpdv(k) = sigmdv(k) + deuxmu * depsdv(k)
            sigpdv(k) = sigpdv(k)*rp/(rp+1.5d0*deuxmu*dp)
            sieleq = sieleq + sigpdv(k)**2
            sigp(k) = sigpdv(k) + sigpmo*kron(k)
160      continue
        sieleq = sqrt(1.5d0*sieleq)
!
!         -- 7.3 CALCUL DES DISSIPATIONS :
!         --------------------------------
!         -- 7.3.1 CALCUL DES DISSIPATIONS IRREVERSIBLES :
!         ------------------------------------------------
!         FACTEUR DE TAYLOR-QUINNEY
        khi = 0.9d0
        dirr = khi * sieleq * dp/dinst
!
!         -- 7.3.2 CALCUL DES DISSIPATIONS THERMOELASTIQUE :
!         --------------------------------------------------
        dte = 0.d0
        if ((e.ne.em) .or. (nu.ne.num)) then
            dgdtsg = (deuxmu-deumum)/(tp-tm)/deuxmu
            dkdtsk = (troisk-troikm)/(tp-tm)/troisk
        else
            dgdtsg = 0.d0
            dkdtsk = 0.d0
        endif
!
        do 180 k = 1, ndimsi
            if (plasti) then
                epspet = (deps(k)-3.d0*sigpdv(k)*dp/(2.d0*sieleq))/ dinst
            else
                epspet = deps(k)/dinst
            endif
            tpdsdt=tp*(dgdtsg*sigp(k) +kron(k)*((dkdtsk-dgdtsg)*&
            sigpmo-troisk*alpha))
            dte = dte + tpdsdt*epspet
180      continue
        vip(5) = dte + dirr
!
    endif
!
!=======================================================================
!     -- 8 CALCUL DE DSIDEP(6,6) :
!=======================================================================
    if (option(1:10) .eq. 'RIGI_MECA_' .or. option(1:9) .eq. 'FULL_MECA') then
!
        if (option(1:10) .eq. 'RIGI_MECA_') then
!         - - OPTION='RIGI_MECA_TANG' => SIGMA(T)
            rp=0.d0
            do 118 k = 1, ndimsi
                sigdv(k) = sigmdv(k)
                rp = rp + sigdv(k)**2
118          continue
            rp = sqrt(1.5d0*rp)
        else
!         - - OPTION='FULL_MECA' => SIGMA(T+DT)
            if (compor(1)(1:5) .eq. 'VMIS_') then
                do 119 k = 1, ndimsi
                    sigdv(k) = sigpdv(k)
119              continue
            endif
        endif
!
!       -- 8.1 PARTIE PLASTIQUE:
        do 100, k=1,ndimsi
        do 101, l=1,ndimsi
        dsidep(k,l) = 0.d0
101      continue
100      continue
!
        a=1.d0
        if (.not.dech) then
            if (compor(1)(1:5) .eq. 'VMIS_') then
                sigeps = 0.d0
                do 170 k = 1, ndimsi
                    sigeps = sigeps + sigdv(k)*depsdv(k)
170              continue
                if (plasti .and. sigeps .ge. 0.d0) then
                    a = 1.d0+1.5d0*deuxmu*dp/rp
                    coef = - (1.5d0 * deuxmu)**2/(1.5d0*deuxmu+rprim)/ rp**2 *(1.d0 - dp*rprim/rp&
                           & )/a
                    do 135 k = 1, ndimsi
                        do 135 l = 1, ndimsi
                            dsidep(k,l) = coef*sigdv(k)*sigdv(l)
135                      continue
                endif
            endif
        endif
!
!       -- 8.2 PARTIE ELASTIQUE:
        do 130 k = 1, 3
            do 131 l = 1, 3
                dsidep(k,l) = dsidep(k,l)+co*(troisk/3.d0-deuxmu/( 3.d0*a))
131          continue
130      continue
        do 120 k = 1, ndimsi
            dsidep(k,k) = dsidep(k,k) + deuxmu/a
120      continue
    endif
!=======================================================================
!
!
9999  continue
! FIN ------------------------------------------------------------------
end subroutine
