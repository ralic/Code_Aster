subroutine nmecmi(fami, kpg, ksp, ndim, typmod,&
                  imate, compor, crit, deps, sigm,&
                  vim, option, sigp, vip, dsidep,&
                  iret)
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
!
! aslint: disable=
    implicit none
#include "jeveux.h"
#include "asterfort/nmcri5.h"
#include "asterfort/radial.h"
#include "asterfort/rcfon2.h"
#include "asterfort/rctrac.h"
#include "asterfort/rctype.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utmess.h"
#include "asterfort/verift.h"
#include "asterfort/zerofr.h"
!
    integer :: kpg, ksp, ndim, imate, iret, iret0, iret1, iret2
    character(len=*) :: fami
    character(len=8) :: typmod(*)
    character(len=16) :: compor(3), option
    real(kind=8) :: crit(10), tp2, line, radi
    real(kind=8) :: deps(6), prec, dx, deuxmu
    real(kind=8) :: sigm(6), vim(8), sigp(6), vip(8), dsidep(6, 6)
! ----------------------------------------------------------------------
!     REALISE LA LOI DE VON MISES ISOTROPE ET ELASTIQUE POUR LES
!     ELEMENTS ISOPARAMETRIQUES EN PETITES DEFORMATIONS
!
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
! OUT IRET    : CODE RETOUR DE  L'INTEGRATION DE LA LOI DE VOM MISES
!                   IRET=0 => PAS DE PROBLEME
!                   IRET=1 => ABSENCE DE CONVERGENCE LORS DE
!                                        L'INTEGRATION DE LA LOI
!
!----- COMMONS NECESSAIRES A VON_MISES ISOTROPE C_PLAN :
!      COMMONS COMMUNS A NMCRI1 ET NMECMI
    common /rconm5/deuxmu,troisk,sigy,rprim,pm,sigel,tp2,line,prag,xm
    common /kconm1/imate2,jprol2,jvale2,nbval2
!
!
!
!
    logical :: cplan, plasti
    real(kind=8) :: depsth(6), valres(3), pm, xp(6), plast, resu
    real(kind=8) :: depsmo, sigmmo, e, nu, troisk, rprim, rp, hp, gp, g1, rpm
    real(kind=8) :: sieleq, sigeps, seuil, dp, coef, dsde, sigy, xm(6)
    real(kind=8) :: sigedv(6)
    real(kind=8) :: kron(6), depsdv(6), sigmdv(6), sigpdv(6), sigdv(6), dum, cc
    real(kind=8) :: em, num, troikm, deumum, rbid, sigmp(6), sigel(6)
    real(kind=8) :: hsg, pp, prag, pragm, precr, tm, tp, epsthe
    integer :: ndimsi, jprolm, jvalem, nbvalm, jprol2, jvale2, nbval2
    integer :: jprolp, jvalep, nbvalp, k, l, niter, imate2, ibid
    integer :: icodre(3)
    character(len=8) :: nomres(3), type, materi
!-----------------------------------------------------------------------
    real(kind=8) :: dp0, xap
!-----------------------------------------------------------------------
    data        kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
! DEB ------------------------------------------------------------------
!
!     -- 1 INITIALISATIONS :
!     ----------------------
    cplan = typmod(1) .eq. 'C_PLAN'
    ndimsi = 2*ndim
    imate2=imate
    iret=0
    jprolp=1
    materi = ' '
!
! MISE AU FORMAT DES CONTRAINTES DE RAPPEL
!
    pm = vim(1)
    plast = vim(2)
    do 11 k = 1, 3
        xm(k) = vim(k+2)
11  end do
    do 10 k = 4, ndimsi
        xm(k) = vim(k+2)*sqrt(2.d0)
10  end do
    dp=0.d0
!
!
    if (.not.( compor(1)(1:9) .eq. 'VMIS_ECMI' )) then
        call utmess('F', 'ALGORITH4_50', sk=compor(1))
    endif
!
!
!
!     -- 2 RECUPERATION DES CARACTERISTIQUES
!     ---------------------------------------
    nomres(1)='E'
    nomres(2)='NU'
    nomres(3)='ALPHA'
!
    if (compor(1)(1:14) .eq. 'VMIS_ECMI_TRAC') then
        call rcvalb(fami, kpg, ksp, '-', imate,&
                    ' ', 'ELAS', 0, ' ', 0.d0,&
                    1, nomres(2), valres(2), icodre(2), 2)
        num = valres(2)
        call rcvalb(fami, kpg, ksp, '+', imate,&
                    ' ', 'ELAS', 0, ' ', 0.d0,&
                    1, nomres(2), valres(2), icodre(2), 2)
        nu = valres(2)
    else
        call rcvalb(fami, kpg, ksp, '-', imate,&
                    ' ', 'ELAS', 0, ' ', 0.d0,&
                    2, nomres(1), valres(1), icodre(1), 2)
        em = valres(1)
        num = valres(2)
        deumum = em/(1.d0+num)
        troikm = em/(1.d0-2.d0*num)
        call rcvalb(fami, kpg, ksp, '+', imate,&
                    ' ', 'ELAS', 0, ' ', 0.d0,&
                    2, nomres(1), valres(1), icodre(1), 2)
        e = valres(1)
        nu = valres(2)
        deuxmu = e/(1.d0+nu)
        troisk = e/(1.d0-2.d0*nu)
    endif
    call verift(fami, kpg, ksp, 'T', imate,&
                materi, 'ELAS', 1, epsthe, iret0)
!
!     -- 3 RECUPERATION DES CARACTERISTIQUES
!     ---------------------------------------
    nomres(1)='C'
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'PRAGER', 0, ' ', 0.d0,&
                1, nomres, valres, icodre, 1)
    prag=valres(1)
    call rcvalb(fami, kpg, ksp, '-', imate,&
                ' ', 'PRAGER', 0, ' ', 0.d0,&
                1, nomres, valres, icodre, 1)
    pragm=valres(1)
    line=0.d0
    if (compor(1)(10:14) .eq. '_LINE') then
        line=1.d0
        nomres(1)='D_SIGM_EPSI'
        nomres(2)='SY'
        call rcvalb(fami, kpg, ksp, '+', imate,&
                    ' ', 'ECRO_LINE', 0, ' ', 0.d0,&
                    2, nomres, valres, icodre, 1)
        dsde=valres(1)
        sigy=valres(2)
        rprim = dsde*e/(e-dsde)-1.5d0*prag
        rpm = rprim*pm +sigy
    else
!
        call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                    ksp, tm, iret2)
        call rctype(imate, 1, 'TEMP', tm, resu,&
                    type)
        if ((type.eq.'TEMP') .and. (iret2.eq.1)) then
            call utmess('F', 'CALCULEL_31')
        endif
        call rctrac(imate, 1, 'SIGM', resu, jprolm,&
                    jvalem, nbvalm, em)
        deumum = em/(1.d0+num)
        troikm = em/(1.d0-2.d0*num)
!
        call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                    ksp, tp, iret1)
        call rctype(imate, 1, 'TEMP', tp, resu,&
                    type)
        if ((type.eq.'TEMP') .and. (iret1.eq.1)) then
            call utmess('F', 'CALCULEL_31')
        endif
        call rctrac(imate, 1, 'SIGM', resu, jprolp,&
                    jvalep, nbvalp, e)
        deuxmu = e/(1.d0+nu)
        troisk = e/(1.d0-2.d0*nu)
!
        call rcfon2('S', jprolp, jvalep, nbvalp, sigy,&
                    dum, dum, dum, dum, dum,&
                    dum, dum, dum)
        call rcfon2('V', jprolp, jvalep, nbvalp, rbid,&
                    rbid, rbid, pm, rpm, rprim,&
                    prag, rbid, rbid)
    endif
!
!     -- 4 CALCUL DE DEPSMO ET DEPSDV :
!     --------------------------------
    coef = epsthe
    if (cplan) deps(3)=-nu/(1.d0-nu)*(deps(1)+deps(2)) +(1.d0+nu)/(1.d0-nu)*coef
    depsmo = 0.d0
    do 110 k = 1, 3
        depsth(k) = deps(k) -coef
        depsmo = depsmo + depsth(k)
110  end do
    depsmo = depsmo/3.d0
    do 111 k = 4, ndimsi
        depsth(k) = deps(k)
111  end do
    do 115 k = 1, ndimsi
        depsdv(k) = depsth(k) - depsmo * kron(k)
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
        if (pragm .ne. 0.d0) then
            xm(k)=xm(k)*prag/pragm
        endif
        sigel(k)=sigmdv(k)+deuxmu*depsdv(k)-xm(k)
        sieleq = sieleq + sigel(k)**2
117  end do
    sieleq = sqrt(1.5d0*sieleq)
    seuil = sieleq - rpm
    hp=1.d0
    gp=1.d0
!
!     -- 7 CALCUL DE SIGP,SIGPDV,VIP,DP,RP:
!     -------------------------------------
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
!
!       -- 7.1 CALCUL DE DP (ET DX SI C_PLAN) :
!       -------------------------------------------
        if (seuil .le. 0.d0) then
            plast = 0.d0
            dp = 0.d0
            rp=rpm
        else
            plast = 1.d0
            if (cplan) then
                prec= crit(3)
                niter=nint(crit(1))
                precr = prec * sigy
!
!             CALCUL DE L'APPROXIMATION : DP SANS CONTRAINTE PLANE
!
                if (compor(1)(10:14) .eq. '_LINE') then
                    dp0 = sieleq - sigy - rprim * pm
                    dp0 = dp0 / (rprim+1.5d0*(deuxmu+prag))
                else
                    jprol2 = jprolp
                    jvale2 = jvalep
                    nbval2 = nbvalp
                    call rcfon2('E', jprolp, jvalep, nbvalp, rbid,&
                                e, nu, pm, rp, rprim,&
                                prag, sieleq, dp0)
                endif
                xap = dp0
                call zerofr(0, 'DEKKER', nmcri5, 0.d0, xap,&
                            precr, niter, dp, iret, ibid)
                if (iret .eq. 1) goto 9999
                if (line .ge. 0.5d0) then
                    rp = sigy +rprim*(pm+dp)
                else
                    call rcfon2('V', jprolp, jvalep, nbvalp, rbid,&
                                rbid, rbid, pm+dp, rp, rprim,&
                                prag, rbid, rbid)
                endif
            else
                if (compor(1)(10:14) .eq. '_LINE') then
                    dp = sieleq - sigy - rprim * pm
                    dp = dp / (rprim+1.5d0*(deuxmu+prag))
                    rp = sigy +rprim*(pm+dp)
                else
                    call rcfon2('E', jprolp, jvalep, nbvalp, rbid,&
                                e, nu, pm, rp, rprim,&
                                prag, sieleq, dp)
                endif
            endif
        endif
        pp = pm + dp
        gp=1.d0+1.5d0*prag*dp/rp
        hp=gp+1.5d0*deuxmu*dp/rp
        plasti=(plast.ge.0.5d0)
!
!         -- 7.2 CALCUL DE SIGP :
!         -----------------------
        if (cplan .and. plasti) then
            hsg=hp/gp
            dx= (hsg-1.d0)* sigel(3)
            dx=dx/( deuxmu/1.5d0 + troisk*hsg/3.d0 )
            depsmo =depsmo +dx/3.d0
            depsdv(1)=depsdv(1)-dx/3.d0
            depsdv(2)=depsdv(2)-dx/3.d0
            depsdv(3)=depsdv(3)+dx*2.d0/3.d0
        endif
        do 160 k = 1, ndimsi
            sigedv(k) = sigmdv(k) + deuxmu * depsdv(k)
            g1=1.5d0*prag*dp/rp/hp
            xp(k)=xm(k)*(1.d0-g1)+g1*sigedv(k)
            sigpdv(k) = sigedv(k)*gp/hp+xm(k)*1.5d0*deuxmu*dp/rp/hp
            sigp(k) = sigpdv(k) + (sigmmo + troisk*depsmo)*kron(k)
160      continue
    endif
!
!     -- 8 CALCUL DE DSIDEP(6,6) :
!     ----------------------------
    if (option(1:14) .eq. 'RIGI_MECA_TANG' .or. option(1:9) .eq. 'FULL_MECA') then
!
        plasti=(plast.ge.0.5d0)
        if (option(1:14) .eq. 'RIGI_MECA_TANG') then
!         - - OPTION='RIGI_MECA_TANG' => SIGMA(T)
            do 118 k = 1, ndimsi
                sigdv(k) = sigmdv(k)-xm(k)
118          continue
            rp = rpm
        else
!         - - OPTION='FULL_MECA' => SIGMA(T+DT)
            do 119 k = 1, ndimsi
                sigdv(k) = sigpdv(k)-xp(k)
119          continue
        endif
!
!       -- 8.1 PARTIE PLASTIQUE:
        do 100 k = 1, ndimsi
            do 101 l = 1, ndimsi
                dsidep(k,l) = 0.d0
101          continue
100      continue
!
        sigeps = 0.d0
        do 170 k = 1, ndimsi
            sigeps = sigeps + sigdv(k)*depsdv(k)
170      continue
        if (plasti .and. sigeps .ge. 0.d0) then
            cc=-(1.5d0*deuxmu)**2/( 1.5d0*(deuxmu+prag)+rprim )/rp**2&
            *(1.d0 - dp*rprim/rp )/hp
            do 135 k = 1, ndimsi
                do 135 l = 1, ndimsi
                    dsidep(k,l) = cc*sigdv(k)*sigdv(l)
135              continue
        endif
!
!       -- 8.2 PARTIE ELASTIQUE:
        do 130 k = 1, 3
            do 131 l = 1, 3
                dsidep(k,l)=dsidep(k,l)+troisk/3.d0-deuxmu/3.d0*gp/hp
131          continue
130      continue
        do 120 k = 1, ndimsi
            dsidep(k,k) = dsidep(k,k) + deuxmu*gp/hp
120      continue
!
!       -- 8.3 CORRECTION POUR LES CONTRAINTES PLANES :
        if (cplan) then
            do 136 k = 1, ndimsi
                if (k .eq. 3) goto 136
                do 137 l = 1, ndimsi
                    if (l .eq. 3) goto 137
                    dsidep(k,l)=dsidep(k,l) - 1.d0/dsidep(3,3)*dsidep(&
                    k,3)*dsidep(3,l)
137              continue
136          continue
        endif
    endif
!
    if (option(1:9) .ne. 'RIGI_MECA') then
        if (crit(10) .gt. 0.d0) then
            call radial(ndimsi, sigm, sigp, vim(2), plast,&
                        1, vim(3), vip(3), radi)
            if (radi .gt. crit(10)) then
                iret=2
            endif
        endif
    endif
!
! MISE AU FORMAT DES CONTRAINTES DE RAPPEL
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        vip(1)=pp
        vip(2)=plast
        do 31 k = 1, 3
            vip(k+2) = xp(k)
31      continue
        do 30 k = 4, ndimsi
            vip(k+2) = xp(k)/sqrt(2.d0)
30      continue
    endif
!
9999  continue
end subroutine
