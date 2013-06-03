subroutine nmisot(fami, kpg, ksp, ndim, typmod,&
                  imate, compor, crit, deps, sigm,&
                  vim, option, sigp, vip, dsidep,&
                  demu, cinco, iret)
! ----------------------------------------------------------------------
! person_in_charge: jean-michel.proix at edf.fr
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
! aslint: disable=W1501
    implicit none
    include 'asterc/r8miem.h'
    include 'asterc/r8prem.h'
    include 'asterfort/ecpuis.h'
    include 'asterfort/nmcri1.h'
    include 'asterfort/nmcri2.h'
    include 'asterfort/radial.h'
    include 'asterfort/rcfonc.h'
    include 'asterfort/rctrac.h'
    include 'asterfort/rctype.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/rcvarc.h'
    include 'asterfort/rupmat.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/verift.h'
    include 'asterfort/zerofr.h'
    integer :: ndim, imate, kpg, ksp, iret
    character(len=*) :: fami
    character(len=8) :: typmod(*)
    character(len=16) :: compor, option
    real(kind=8) :: crit(11), line, radi
    real(kind=8) :: deps(6), dx, deuxmu, demu, cinco
    real(kind=8) :: sigm(6), vim(*), sigp(6), vip(*), dsidep(6, 6)
! ----------------------------------------------------------------------
!     REALISE LA LOI DE VON MISES ISOTROPE ET ELASTIQUE POUR LES
!     ELEMENTS ISOPARAMETRIQUES EN PETITES DEFORMATIONS
!
! IN  KPG,KSP  : NUMERO DU (SOUS)POINT DE GAUSS
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  IMATE   : ADRESSE DU MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT : RELCOM ET DEFORM
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
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
!----- COMMONS NECESSAIRES A VON_MISES ISOTROPE C_PLAN :
!      COMMONS COMMUNS A NMCRI1 ET NMISOT
    common /rconm1/deuxmu,nu,e,sigy,rprim,pm,sigel,line
    common /kconm1/imate2,jprol2,jvale2,nbval2
!----- COMMONS NECESSAIRES A VON_MISES ISOTROPE ECROUISSAGE PUISSANCE :
!      COMMONS COMMUNS A NMCRI2 ET NMISOT
    common /rconm2/alfafa,unsurn,sieleq
!
    logical :: cplan, plasti, inco, dech
    real(kind=8) :: depsth(6), valres(3), epsthe, pm, co
    real(kind=8) :: depsmo, sigmmo, e, nu, troisk, rprim, rp, airerp
    real(kind=8) :: sieleq, sigeps, seuil, dp, coef, dsde, sigy, hydrm, hydrp
    real(kind=8) :: kron(6), depsdv(6), sigmdv(6), sigpdv(6), sigdv(6), dum
    real(kind=8) :: em, num, troikm, deumum, rbid, sigmp(6), sigel(6), a, rbid2
    real(kind=8) :: sechm, sechp, sref, tp, defam(6), defap(6)
    integer :: ndimsi, jprolm, jvalem, nbvalm, jprol2, jvale2, nbval2
    integer :: imate2, jprolp, jvalep, nbvalp, k, l, niter, ibid
    integer :: iret2, iret3, iret4, iret0, iret5
    integer :: icodre(3)
    character(len=6) :: epsa(6)
    character(len=8) :: nomres(3)
    character(len=8) :: nompar(3), type
    real(kind=8) :: valpam(3), valpap(3), resu, valrm(2)
    real(kind=8) :: bendom, bendop, kdessm, kdessp, rac2, xm(6), xp(6)
!-----------------------------------------------------------------------
    integer :: lgpg
    real(kind=8) :: alfafa, coco, dp0, precr, rprim0, tm
    real(kind=8) :: unsurn, xap
!-----------------------------------------------------------------------
    data        kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
    data epsa   / 'EPSAXX','EPSAYY','EPSAZZ','EPSAXY','EPSAXZ',&
     &              'EPSAYZ'/
! DEB ------------------------------------------------------------------
!
!     -- 1 INITIALISATIONS :
!     ----------------------
!     CES VARIABLES SONT RECOPIEES DANS LE COMMON PLUS LOIN,
!     EN LES INITIALISANT A ZERO, ON DEVRAIT VOIR ASSEZ VITE
!     SI NMCRI1 UTILISE LE COMMON SANS QUE L'ON SOIT PASSER PAR RCTRAC.
    nbvalp = 0
    jprolp = 0
    jvalep = 0
!
    cplan = typmod(1) .eq. 'C_PLAN'
    inco = typmod(2) .eq. 'INCO'
    dech = option(11:14).eq.'ELAS'
    if (inco) then
        co = 0.d0
    else
        co = 1.d0
    endif
    ndimsi = 2*ndim
    imate2 = imate
    rac2 = sqrt(2.d0)
!
!
    if (.not.( compor(1:9) .eq. 'VMIS_ISOT' )) then
        call u2mesk('F', 'ALGORITH4_50', 1, compor)
    endif
!
!     -- 2 RECUPERATION DES CARACTERISTIQUES
!     ---------------------------------------
    nomres(1)='E'
    nomres(2)='NU'
!
    nompar(1) = 'TEMP'
    call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                ksp, tm, iret3)
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                ksp, tp, iret4)
    valpam(1) = tm
    valpap(1) = tp
!
    call rcvarc(' ', 'HYDR', '-', fami, kpg,&
                ksp, hydrm, iret2)
    if (iret2 .ne. 0) hydrm=0.d0
    call rcvarc(' ', 'HYDR', '+', fami, kpg,&
                ksp, hydrp, iret2)
    if (iret2 .ne. 0) hydrp=0.d0
    call rcvarc(' ', 'SECH', '-', fami, kpg,&
                ksp, sechm, iret2)
    if (iret2 .ne. 0) sechm=0.d0
    call rcvarc(' ', 'SECH', '+', fami, kpg,&
                ksp, sechp, iret2)
    if (iret2 .ne. 0) sechp=0.d0
    call rcvarc(' ', 'SECH', 'REF', fami, kpg,&
                ksp, sref, iret2)
    if (iret2 .ne. 0) sref=0.d0
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
    if (compor(1:14) .eq. 'VMIS_ISOT_TRAC') then
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
!        CRIT_RUPT
        if ((crit(11).gt.0.d0) .and. (vim(8).gt.0.d0)) then
            lgpg = 8
            call rupmat(fami, kpg, ksp, imate, vim,&
                        lgpg, em, sigm)
        endif
!
        if (inco) then
            troikm = deumum
        else
            troikm = em/(1.d0-2.d0*num)
        endif
        call rcvalb(fami, kpg, ksp, '+', imate,&
                    ' ', 'ELAS', 0, ' ', 0.d0,&
                    2, nomres(1), valres(1), icodre(1), 2)
        e = valres(1)
        nu = valres(2)
!
!        CRIT_RUPT
        if ((crit(11).gt.0.d0) .and. (vim(8).gt.0.d0)) then
            lgpg = 8
            call rupmat(fami, kpg, ksp, imate, vim,&
                        lgpg, e, sigm)
        endif
!
        if (inco) then
            deuxmu = 2.d0*e/3.d0
            troisk = deuxmu
        else
            deuxmu = e/(1.d0+nu)
            troisk = e/(1.d0-2.d0*nu)
        endif
    endif
    call verift(fami, kpg, ksp, 'T', imate,&
                'ELAS', 1, epsthe, iret0)
!
! --- RETRAIT ENDOGENE ET RETRAIT DE DESSICCATION
!
    nomres(1)='B_ENDOGE'
    nomres(2)='K_DESSIC'
    call rcvalb(fami, kpg, ksp, '-', imate,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                1, nomres(1), valres(1), icodre(1), 0)
    if (icodre(1) .ne. 0) valres(1) = 0.d0
    bendom = valres(1)
!
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                1, nomres(1), valres(1), icodre(1), 0)
    if (icodre(1) .ne. 0) valres(1) = 0.d0
    bendop = valres(1)
!
    call rcvalb(fami, kpg, ksp, '-', imate,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                1, nomres(2), valres(2), icodre(2), 0)
    if (icodre(2) .ne. 0) valres(2) = 0.d0
    kdessm = valres(2)
!
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                1, nomres(2), valres(2), icodre(2), 0)
    if (icodre(2) .ne. 0) valres(2) = 0.d0
    kdessp = valres(2)
!
!     -- 3 RECUPERATION DES CARACTERISTIQUES
!     ---------------------------------------
    line=0.d0
    plasti=(vim(2).ge.0.5d0)
    if (compor(10:14) .eq. '_LINE') then
        line=1.d0
        nomres(1)='D_SIGM_EPSI'
        nomres(2)='SY'
        call rcvalb(fami, kpg, ksp, '+', imate,&
                    ' ', 'ECRO_LINE', 0, ' ', 0.d0,&
                    2, nomres, valres, icodre, 2)
        dsde=valres(1)
        sigy=valres(2)
        if ((e-dsde) .lt. r8miem()) then
            valrm(1)=dsde
            valrm(2)=e
            call u2mesg('F', 'COMPOR1_54', 0, ' ', 0,&
                        ibid, 2, valrm)
        else
            rprim = dsde*e/(e-dsde)
        endif
        rp = rprim*vim(1)+sigy
    else if (compor(10:14) .eq. '_PUIS') then
        line=-1.d0
        nomres(1)='SY'
        nomres(2)='A_PUIS'
        nomres(3)='N_PUIS'
        call rcvalb(fami, kpg, ksp, '+', imate,&
                    ' ', 'ECRO_PUIS', 0, ' ', 0.d0,&
                    3, nomres, valres, icodre, 2)
        sigy = valres(1)
        alfafa = valres(2)
        coco = e/alfafa/sigy
        unsurn = 1.d0/valres(3)
        rp = sigy * (coco*vim(1))**unsurn + sigy
        if (vim(1) .gt. r8prem()) then
            rprim = unsurn * sigy * coco * (coco*vim(1))**(unsurn-1)
        else
            rprim = e
        endif
    else if (compor(10:14) .eq. '_TRAC') then
        nompar(2)='SECH'
        valpam(2)=sechm
        nompar(3)='HYDR'
        valpam(3)=hydrm
        call rctype(imate, 3, nompar, valpam, resu,&
                    type)
!
        if ((type.eq.'TEMP') .and. (iret3.eq.1)) call u2mess('F', 'CALCULEL_31')
        call rctrac(imate, 1, 'SIGM', tm, jprolm,&
                    jvalem, nbvalm, em)
!
!        CRIT_RUPT VMIS_ISOT_TRAC
        if ((crit(11).gt.0.d0) .and. (vim(8).gt.0.d0)) then
            lgpg = 8
            call rupmat(fami, kpg, ksp, imate, vim,&
                        lgpg, em, sigm)
        endif
!
        deumum = em/(1.d0+num)
        if (inco) then
            troikm = deumum
        else
            troikm = em/(1.d0-2.d0*num)
        endif
        nompar(2)='SECH'
        valpap(2)=sechp
        nompar(3)='HYDR'
        valpap(3)=hydrp
        call rctype(imate, 3, nompar, valpap, resu,&
                    type)
        if ((type.eq.'TEMP') .and. (iret4.eq.1)) call u2mess('F', 'CALCULEL_31')
        call rctrac(imate, 1, 'SIGM', resu, jprolp,&
                    jvalep, nbvalp, e)
!        CRIT_RUPT VMIS_ISOT_TRAC
        if ((crit(11).gt.0.d0) .and. (vim(8).gt.0.d0)) then
            lgpg = 8
            call rupmat(fami, kpg, ksp, imate, vim,&
                        lgpg, e, sigm)
        endif
!
        call rcfonc('S', 1, jprolp, jvalep, nbvalp,&
                    sigy, dum, dum, dum, dum,&
                    dum, dum, dum, dum)
        call rcfonc('V', 1, jprolp, jvalep, nbvalp,&
                    rbid, rbid, rbid, vim(1), rp,&
                    rprim, airerp, rbid, rbid)
        if (inco) then
            deuxmu = 2.d0*e/3.d0
            troisk = deuxmu
        else
            deuxmu = e/(1.d0+nu)
            troisk = e/(1.d0-2.d0*nu)
        endif
    endif
    demu = deuxmu
    if (inco) then
        cinco =(1.d0-2.d0*nu)/nu
    endif
!
!     -- 4 CALCUL DE DEPSMO ET DEPSDV :
!     --------------------------------
    coef = epsthe - bendop*hydrp + bendom*hydrm - kdessp*(sref-sechp) + kdessm*(sref-sechm)
    if (cplan) deps(3)=-nu/(1.d0-nu)*(deps(1)+deps(2)) +(1.d0+nu)/(1.d0-nu)*coef + nu*(defap(1)-d&
               &efam(1)+defap(2)-defam(2))/(1.d0-nu) + defap(3)-defam(3)
    depsmo = 0.d0
    do 110 k = 1, 3
        depsth(k) = deps(k) -coef -(defap(k)-defam(k))
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
!     -- 7 CALCUL DE SIGP,SIGPDV,VIP,DP,RP:
!     -------------------------------------
    dp=0.d0
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
!
!       -- 7.1 CALCUL DE DP (ET DX SI C_PLAN) :
!       -------------------------------------------
        if (seuil .le. 0.d0) then
            vip(2) = 0.d0
            dp = 0.d0
        else
            vip(2) = 1.d0
            pm = vim(1)
            if (cplan) then
                niter=nint(crit(1))
                jprol2 = jprolp
                jvale2 = jvalep
                nbval2 = nbvalp
                precr = crit(3) * sigy
!
!             CALCUL DE L'APPROXIMATION : DP SANS CONTRAINTE PLANE
!
                if (compor(10:14) .eq. '_LINE') then
                    dp0 = sieleq - sigy - rprim * pm
                    dp0 = dp0 / (rprim+1.5d0*deuxmu)
                else if (compor(10:14) .eq. '_PUIS') then
                    dp0 = (sieleq - rp)/(1.5d0*deuxmu)
                else
                    call rcfonc('E', 1, jprolp, jvalep, nbvalp,&
                                rbid, e, nu, pm, rp,&
                                rprim, airerp, sieleq, dp0)
                endif
                xap = dp0
                call zerofr(0, 'DEKKER', nmcri1, 0.d0, xap,&
                            precr, niter, dp, iret, ibid)
                if (iret .eq. 1) goto 9999
                if (line .gt. 0.5d0) then
                    rp = sigy +rprim*(pm+dp)
                else if (line.lt.-0.5d0) then
                    rp=sigy+sigy*(e*(pm+dp)/alfafa/sigy)**unsurn
                else
                    call rcfonc('V', 1, jprolp, jvalep, nbvalp,&
                                rbid, rbid, rbid, pm+dp, rp,&
                                rbid2, airerp, rbid, rbid)
                endif
                dx = 3.d0*(1.d0-2.d0*nu)*sigel(3)*dp/(e*dp+2.d0* (1.d0-nu)*rp)
            else
                if (compor(10:14) .eq. '_LINE') then
                    dp = sieleq - sigy - rprim * pm
                    dp = dp / (rprim+1.5d0*deuxmu)
                    rp = sigy +rprim*(pm+dp)
                else if (compor(10:14) .eq. '_PUIS') then
                    dp0 = (sieleq - rp)/(1.5d0*deuxmu)
!               AMELIORATION DE LA PREDICTION DE DP EN ESTIMANT
!               RPRIM(PM+DP0)
                    rprim0 = unsurn*sigy*coco * (coco*(pm+dp0))**( unsurn-1)
                    dp0 = dp0 / (1+rprim0/1.5d0/deuxmu)
                    xap = dp0
                    precr = crit(3) * sigy
                    niter = nint(crit(1))
                    call zerofr(0, 'DEKKER', nmcri2, 0.d0, xap,&
                                precr, niter, dp, iret, ibid)
                    if (iret .eq. 1) goto 9999
                    call ecpuis(e, sigy, alfafa, unsurn, pm,&
                                dp, rp, rprim)
!
                else if (compor(10:14) .eq. '_TRAC') then
                    call rcfonc('E', 1, jprolp, jvalep, nbvalp,&
                                rbid, e, nu, vim(1), rp,&
                                rprim, airerp, sieleq, dp)
                endif
            endif
        endif
        vip(1) = vim(1) + dp
        plasti=(vip(2).ge.0.5d0)
!
!         -- 7.2 CALCUL DE SIGP :
!         -----------------------
        if (cplan .and. plasti) then
            depsmo =depsmo +dx/3.d0
            depsdv(1)=depsdv(1)-dx/3.d0
            depsdv(2)=depsdv(2)-dx/3.d0
            depsdv(3)=depsdv(3)+dx*2.d0/3.d0
        endif
        do 160 k = 1, ndimsi
            sigpdv(k) = sigmdv(k) + deuxmu * depsdv(k)
            sigpdv(k) = sigpdv(k)*rp/(rp+1.5d0*deuxmu*dp)
            sigp(k) = sigpdv(k) + (sigmmo + co*troisk*depsmo)*kron(k)
160      continue
!
    endif
!
!     -- 8 CALCUL DE DSIDEP(6,6) :
!     ----------------------------
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
            do 119 k = 1, ndimsi
                sigdv(k) = sigpdv(k)
119          continue
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
            sigeps = 0.d0
            do 170 k = 1, ndimsi
                sigeps = sigeps + sigdv(k)*depsdv(k)
170          continue
            if (plasti .and. sigeps .ge. 0.d0) then
                a = 1.d0+1.5d0*deuxmu*dp/rp
                coef = - (1.5d0 * deuxmu)**2/(1.5d0*deuxmu+rprim)/rp** 2 *(1.d0 - dp*rprim/rp )/a
                do 135 k = 1, ndimsi
                    do 135 l = 1, ndimsi
                        dsidep(k,l) = coef*sigdv(k)*sigdv(l)
135                  continue
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
            call radial(ndimsi, sigm, sigp, vim(2), vip(2),&
                        0, xm, xp, radi)
            if (radi .gt. crit(10)) then
                iret=2
            endif
        endif
    endif
!
9999  continue
! FIN ------------------------------------------------------------------
end subroutine
