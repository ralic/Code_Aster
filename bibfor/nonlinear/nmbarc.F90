subroutine nmbarc(ndim, imate, crit, sat, biot,&
                  tm, tp, deps, sbism, vim,&
                  option, sbisp, vip, dsidep, p1,&
                  p2, dp1, dp2, dsidp1, sipm,&
                  sipp, retcom)
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
! aslint: disable=W1501,W1504
    implicit none
    include 'jeveux.h'
!
    include 'asterc/r8maem.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/mgauss.h'
    include 'asterfort/promat.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rcvala.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    integer :: ndim, imate, retcom
    character(len=16) :: option
    real(kind=8) :: crit(*), tm, tp
    real(kind=8) :: deps(6), deuxmu, biot, sat, p1, p2, dp1, dp2
    real(kind=8) :: sbism(6), vim(5), sbisp(6), vip(5), dsidep(6, 6)
    real(kind=8) :: dsidp1(6)
    real(kind=8) :: sipm, sipp
! ----------------------------------------------------------------------
!     REALISE LA LOI DE BARCELONE DES MILIEUX NON-SATURES
!     EN ELASTOPLASTICITE MECANIQUE ET HYDRIQUE UTILISABLE
!     SEULEMENT DANS UNE MODELISATION HHM ou THHM
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  IMATE   : ADRESSE DU MATERIAU CODE
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  TM      : TEMPERATURE A L'INSTANT PRECEDENT
! IN  TP      : TEMPERATURE A L'INSTANT DU CALCUL
! IN  DEPS    : INCREMENT DE DEFORMATION
! IN  SBISM   : CONTRAINTES DE BISHOP A L'INSTANT DU CALCUL PRECEDENT
! IN  PCRM    : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
! IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
! OUT SBISP   : CONTRAINTES DE BISHOP A L'INSTANT ACTUEL
! OUT VIP    : VARIABLES INTERNES A L'INSTANT ACTUEL
! OUT DSIDEP  : MATRICE CARREE (INUTILISE POUR RAPH_MECA)
! OUT DSIDP1  : MATRICE COLONNE (INUTILISE POUR RAPH_MECA)
!
!               ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
!               L'ORDRE :  XX,YY,ZZ,SQRT(2)*XY,SQRT(2)*XZ,SQRT(2)*YZ
!
!
!
!
    real(kind=8) :: depsth(6), valres(16), alpha
    real(kind=8) :: lambda, kapa, poro, prescr, m, pa, r, beta, kc, pc0ini
    real(kind=8) :: lambs, kapas, lambb, lambbm, alphab, lamp
    real(kind=8) :: depsmo, sigmmo, e, nu, e0, xk0, xk, xk0s, xks, fonc1, fonc2
    real(kind=8) :: mu
    real(kind=8) :: sieleq, simoel, h, a(6), aa(6), ap(6), aap(6), sieqm
    real(kind=8) :: kron(6), depsdv(6), sigmdv(6), sigpdv(6), tplus(6)
    real(kind=8) :: sigpmo, f1, f2, f3, f4, f5, f6, f, fp, coef, poro1, poro2
    real(kind=8) :: deppmo, deltap, deltas(6), spards, hp, xc, xd, xhhc
    real(kind=8) :: xlam, xa, xu, xg, xh, xm, xe, xf, xv, xi, rap
    real(kind=8) :: cc(6, 6), fv(6)
    real(kind=8) :: c(6, 6), ct, xb, v0, seuil
    real(kind=8) :: sigel(6), xinf, xsup, det, tol, ffi(6, 6), ee(6, 6)
    real(kind=8) :: v(6, 6), s(6, 6), t(6, 6), vv(6, 6)
    real(kind=8) :: diff, diff1
    real(kind=8) :: sbarm(6), sbarp(6), pc0m(2), pc0p(2), pcrm(2), pcrp(2)
    real(kind=8) :: p1m, p2m, pcrmp1, par, pcrpp
    real(kind=8) :: psp
    real(kind=8) :: tra, xgg, xz, xdd, hh(6), xj, xhh, ct1, kv(6)
    real(kind=8) :: ssh(6), hhkv(6), vh(6, 6), vhh(6, 6), vvh(6, 6)
    real(kind=8) :: kkh(6), sshh(6), bb, kcp1, kpmax, zero
    real(kind=8) :: fxi1, fxi2, fxi3, fxi
    real(kind=8) :: xinf0, xsup0, xb0, seuil0, f0, fp0, fxi0, signf0, sigfi0
    real(kind=8) :: sieqp
    real(kind=8) :: hhb(6, 6), ses(6, 6), hhbm(6, 6), gg(6, 6), sps(6, 6)
    real(kind=8) :: d1g(6, 6), id2(6, 6), devhyd(6, 6), devhym(6, 6)
    real(kind=8) :: d1ghhm(6, 6)
    real(kind=8) :: un, deux, trois, six, unsde
    integer :: ndimsi, signf, signfi, iret
    integer :: k, l, iter, matr, iadzi, iazk24, umess
    integer :: icodre(16)
    character(len=8) :: nomres(16)
    character(len=8) :: nompar(1)
    real(kind=8) :: epxmax
    character(len=8) :: nomail
    real(kind=8) :: valpam(1)
! ======================================================================
    parameter   ( un     = 1.d0   )
    parameter   ( deux   = 2.d0   )
    parameter   ( trois  = 3.d0   )
    parameter   ( six    = 6.d0   )
    parameter   ( unsde  = 0.5d0   )
    data        kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
    data        tol/1.d-10/zero/0.d0/
! DEB ------------------------------------------------------------------
!
!     -- 1 INITIALISATIONS :
!     ----------------------
    ndimsi = 2*ndim
    epxmax=log(r8maem())
!
!
    retcom = 0
!
!     -- 2 RECUPERATION DES CARACTERISTIQUES
!     ---------------------------------------
    nomres(1)='ALPHA'
    nomres(2)='MU'
    nomres(3)='PORO'
    nomres(4)='KAPA'
    nomres(5)='LAMBDA'
    nomres(6)='M'
    nomres(7)='PRES_CRIT'
    nomres(8)='PA'
    nomres(9)='R'
    nomres(10)='BETA'
    nomres(11)='KC'
    nomres(12)='PC0_INIT'
    nomres(13)='KAPAS'
    nomres(14)='LAMBDAS'
    nomres(15)='ALPHAB'
!
    nompar(1) = 'TEMP'
    valpam(1) = tm
!
!
    call rcvala(imate, ' ', 'ELAS', 1, nompar,&
                valpam, 1, nomres(1), valres(1), icodre(1),&
                0)
    if (icodre(1) .ne. 0) valres(1) = 0.d0
    alpha = valres(1)
!
    call rcvala(imate, ' ', 'BARCELONE', 1, nompar,&
                valpam, 13, nomres(2), valres(2), icodre(2),&
                2)
!
    mu = valres(2)
    poro = valres(3)
    poro1 = poro
    kapa = valres(4)
    lambda = valres(5)
    m = valres(6)
    prescr = valres(7)
    pa = valres(8)
    r = valres(9)
    beta = valres(10)
    kc = valres(11)
    pc0ini = valres(12)
    kapas = valres(13)
    lambs = valres(14)
    call rcvala(imate, ' ', 'BARCELONE', 1, nompar,&
                valpam, 1, nomres(15), valres(15), icodre(15),&
                0)
    if (icodre(15) .ne. 0) then
        valres(15) = m*(m-9.d0)*(m-3.d0)/9.d0/(6.d0-m) *(1.d0/(1.d0- kapa/lambda))
        alphab = valres(15)
    else
        alphab = valres(15)
    endif
    call rcvala(imate, ' ', 'THM_INIT', 1, nompar,&
                valpam, 1, nomres(3), valres(3), icodre(3),&
                2)
    poro = valres(3)
    poro2 = poro
    diff = poro1-poro2
    if (abs(diff) .gt. tol) then
        call u2mess('F', 'ALGORITH6_60')
    else
        poro=poro1
    endif
    deuxmu = deux*mu
    e0=poro/(1.d0-poro)
!--- CALCUL DES COEFFICIENTS K ET K0 DE LA COURBE HYDROSTATIQUE
!--- MECANIQUE
    xk0 = (1.d0+e0)/kapa
!    CALCUL DE LAMBDA COMME DANS LE PAPIER D'ALONSO
    lambb = lambda*((1.d0-r)*exp(-beta*p1)+r)
    xk= (1.d0+e0)/(lambb-kapa)
!    DERIVEE DE LAMBB PAR RAPPORT A P1
    lamp = -beta*lambda*((1.d0-r)*exp(-beta*p1))
!--- CALCUL DES COEFFICIENTS KS ET K0S DE LA COURBE HYDROSTATIQUE
!--- HYDRIQUE
    xk0s = (1.d0+e0)/kapas
    xks= (1.d0+e0)/(lambs-kapas)
!--- ACTUALISATION DE SIP
    sipp=sipm+biot*sat*dp1-biot*dp2
!
!     -- 3 CALCUL DE DEPSMO ET DEPSDV :
!     --------------------------------
    coef = alpha*tp - alpha*tm
    depsmo = 0.d0
    do 110 k = 1, ndimsi
        depsth(k) = deps(k)
110  end do
    do 111 k = 1, 3
        depsth(k) = depsth(k) - coef
        depsmo = depsmo + depsth(k)
111  end do
    depsmo = -depsmo
    do 112 k = 1, ndimsi
        depsdv(k) = depsth(k) + depsmo/3.d0 * kron(k)
112  end do
!     -- 4 CALCUL DES CONTRAINTES DE BARCELONE A PARTIR DES
!     ------------------------------------------------------
!     CONTRAINTES DE BISHOP
!     ---------------------
    p2m=p2-dp2
    p1m=p1-dp1
!
    do 113 k = 1, ndimsi
        sbarm(k) = sbism(k) + (sipm+p2m)*kron(k)
113  end do
!     -- 5 CALCUL DE SIGMMO, SIGMDV, SIGEL,SIMOEL,SIELEQ, SIEQM :
!     -------------------------------------------------------------
    sigmmo = 0.d0
    do 116 k = 1, 3
        sigmmo = sigmmo + sbarm(k)
116  end do
    sigmmo = -sigmmo/3.d0
    if (sigmmo .le. (-0.99d0*kc*p1)) then
        call u2mess('F', 'ALGORITH6_61')
    endif
!
    sieleq = 0.d0
    sieqm = 0.d0
    do 117 k = 1, ndimsi
        sigmdv(k) = sbarm(k) + sigmmo * kron(k)
        sieqm = sieqm + sigmdv(k)**2
        sigel(k) = sigmdv(k) + deuxmu * depsdv(k)
        sieleq = sieleq + sigel(k)**2
117  end do
    sieleq = sqrt(1.5d0*sieleq)
    sieqm = sqrt(1.5d0*sieqm)
!
    if ((-xk0*depsmo) .gt. epxmax) then
        umess = iunifi('MESSAGE')
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3) (1:8)
        write (umess,9001) 'NMBARC_2','EXP EXPLOSE A LA MAILLE: ',&
        nomail
        retcom = 1
        goto 8000
    endif
    if ((-beta*p1m) .gt. epxmax) then
        umess = iunifi('MESSAGE')
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3) (1:8)
        write (umess,9001) 'NMBARC_3','EXP EXPLOSE A LA MAILLE: ',&
        nomail
        retcom = 1
        goto 8000
    endif
!
!
    simoel = sigmmo*exp(xk0*depsmo)/((p1+pa)/(p1m+pa))**(xk0/xk0s)
!
    kcp1 = kc*p1
    kpmax = max (kcp1 , zero)
!    CALCUL DE LAMBDA COMME DANS LE PAPIER D'ALONSO
    lambbm = lambda*((1.d0-r)*exp(-beta*p1m)+r)
!
!
!     -- 6 DEFINITION DES VARIABLES INTERNES
!     ------------------------------------
    pcrm(1) = vim(1)
    pcrm(2) = vim(2)
    pc0m(1) = vim(3)
    pc0m(2) = vim(4)
!
    if (pcrm(1) .eq. 0.d0) then
        pcrmp1 = (pa/2.d0)* (2*prescr/pa)**((lambda-kapa)/(lambb-kapa) )
! ---- ON VERIFIE LA COHERENCE DES DONNEES MECA DE DEPART
        nu = ( trois*(un+e0)*sigmmo-deuxmu*kapa)/ (six*(un+e0)*sigmmo+ deuxmu*kapa )
        e = deuxmu*(un+nu)
!
!
        if ((e.le.zero) .or. (nu.le.zero) .or. (nu.gt.unsde)) then
            call tecael(iadzi, iazk24)
            nomail = zk24(iazk24-1+3) (1:8)
            call u2mesk('A', 'COMPOR1_3', 1, nomail)
        endif
! ----------------------------------------------------------
!
    else
        pcrmp1=(pa/2.d0)* (2*pcrm(1)/pa)**((lambbm-kapa)/(lambb-kapa))
    endif
    if (pc0m(1) .eq. 0.d0) pc0m(1) = pc0ini
!
!     -- 7 CALCUL DU CRITERE MECANIQUE:
!     ----------------------
    fonc1 = sieleq**2+m*m*(simoel+kpmax)*(simoel-2.d0*pcrmp1)
!
!     --  CALCUL DU CRITERE HYDRIQUE:
!     ----------------------
    fonc2 = p1-pc0m(1)
!
!     -- 8  TEST DE PLASTIFICATION ET CALCUL DE PCRP SIGP, SIGPDV :
!     ------------------------------------------------------------
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
!     -- CAS ELASTIQUE
        if ((fonc1.le.0.d0) .and. (fonc2.le.0.d0)) then
            pcrp(1) = pcrmp1
            pcrp(2) = 0.d0
            pc0p(1) = pc0m(1)
            pc0p(2) = 0.d0
            psp = 0.d0
!
            do 118 k = 1, ndimsi
!     -- REACTUALISATION DES CONTRAINTES
                sigpmo = simoel
                sigpdv(k) = sigel(k)
                sbarp(k) = sigel(k)-simoel*kron(k)
!     -- CALCUL DES CONTRAINTES DE BISHOP
                sbisp(k) = sbarp(k)-(sipp+p2)*kron(k)
118          end do
        else
!     -- PLASTIFICATION : CALCUL DE LA DEFORMATION
!     -- VOLUMIQUE PLASTIQUE : DEPPMO
!     -- CRITERE HYDRIQUE EST ATTEINT
            if (fonc2 .gt. 0) then
                pc0p(2) = 1.d0
                pc0p(1) = p1
!
                deppmo = 1/xks*log((pc0p(1)+pa)/(pc0m(1)+pa))
                psp = kc*pc0p(1)
                if ((-xk0*deppmo) .gt. epxmax) then
                    umess = iunifi('MESSAGE')
                    call tecael(iadzi, iazk24)
                    nomail = zk24(iazk24-1+3) (1:8)
                    write (umess,9001) 'NMBARC_4','EXP EXPLOSE A LA MAILLE: ',&
     &                                                           nomail
                    retcom = 1
                    goto 8000
                endif
!
                if ((xk*deppmo) .gt. epxmax) then
                    umess = iunifi('MESSAGE')
                    call tecael(iadzi, iazk24)
                    nomail = zk24(iazk24-1+3) (1:8)
                    write (umess,9001) 'NMBARC_5','EXP EXPLOSE A LA MAILLE: ',&
     &                                                           nomail
                    retcom = 1
                    goto 8000
                endif
!
                sigpmo = simoel*exp(-xk0*deppmo)
                pcrp(1) = pcrmp1*exp(xk*deppmo)
!
                do 114 k = 1, ndimsi
                    sigpdv(k) = sigel(k)
                    sbarp(k) = sigpdv(k)-sigpmo*kron(k)
                    sbisp(k) = sbarp(k)-(sipp+p2)*kron(k)
114              end do
                f1 = simoel*exp(-xk0*deppmo)
                f2 = simoel*exp(-xk0*deppmo)-2.d0*pcrmp1
                f3 = 2.d0*simoel*exp(-xk0*deppmo)-2.d0*pcrmp1+kpmax
                f = sieleq**2/(1.d0+6.d0*deuxmu*deppmo*alphab/2.d0/m/ m/f3)**2 +m**2*(f1+kpmax)*f&
                    &2
                if (f .gt. 0) then
                    pcrp(2) = 1
                else
                    pcrp(2) = 0
                endif
            else
!     -- CRITERE MECANIQUE EST ETTEINT
                if (fonc1 .gt. 0) then
                    pcrp(2) = 1.d0
                    pc0p(2) = 0.d0
!     -- VALEURS DES BORNES INITIALES
                    xinf = 0.d0
!
!     -- NEWTON POUR CALCULER LA BORNE SUP
!
                    xinf0 = -1.d0
                    xsup0 = 1.d0
                    xb0 = xinf0
                    seuil0 = pcrmp1-(kc*p1)/2.d0
!
                    if ((-xk0*xb0) .gt. epxmax) then
                        umess = iunifi('MESSAGE')
                        call tecael(iadzi, iazk24)
                        nomail = zk24(iazk24-1+3) (1:8)
                        write (umess,9001) 'NMBARC_6','EXP EXPLOSE A LA MAILLE: ',&
     &                                                           nomail
                        retcom = 1
                        goto 8000
                    endif
!
                    if ((xk*xb0) .gt. epxmax) then
                        umess = iunifi('MESSAGE')
                        call tecael(iadzi, iazk24)
                        nomail = zk24(iazk24-1+3) (1:8)
                        write (umess,9001) 'NMBARC_7','EXP EXPLOSE A LA MAILLE: ',&
     &                                                           nomail
                        retcom = 1
                        goto 8000
                    endif
!
!
                    f0 = simoel*exp(-xk0*xb0)-pcrmp1*exp(xk*xb0)+(kc* p1)/2.d0
                    fp0 = -xk0*simoel*exp(-xk0*xb0)-xk*pcrmp1*exp(xk* xb0)
!
                    do 210 iter = 1, nint(crit(1))
!     --CRITERE DE CONVERGENCE
!
                        if ((abs(f0/seuil0)) .le. crit(3)) goto 101
!
!     --CONSTRUCTION DU NOUVEL ITERE
                        xb0 = xb0-f0/fp0
!
                        if ((-xk0*xb0) .gt. epxmax) then
                            umess = iunifi('MESSAGE')
                            call tecael(iadzi, iazk24)
                            nomail = zk24(iazk24-1+3) (1:8)
                            write (umess,9001) 'NMBARC_8','EXP EXPLOSE A LA MAILLE: ',&
     &                                                           nomail
                            retcom = 1
                            goto 8000
                        endif
!
                        if ((xk*xb0) .gt. epxmax) then
                            umess = iunifi('MESSAGE')
                            call tecael(iadzi, iazk24)
                            nomail = zk24(iazk24-1+3) (1:8)
                            write (umess,9001) 'NMBARC_9','EXP EXPLOSE A LA MAILLE: ',&
     &                                                           nomail
                            retcom = 1
                            goto 8000
                        endif
!
!     -- DICHOTOMIE
                        if (xb0 .le. xinf0 .or. xb0 .ge. xsup0) v0 = (xinf0+xsup0)/2
!
                        f0 = simoel*exp(-xk0*xb0)-pcrmp1*exp(xk*xb0)+( kc*p1)/2.d0
                        fp0 = -xk0*simoel*exp(-xk0*xb0)-xk*pcrmp1*exp( xk*xb0)
                        if (f0 .gt. zero) signf0 = 1
                        if (f0 .lt. zero) signf0 = -1
!
                        if ((-xk0*xinf0) .gt. epxmax) then
                            umess = iunifi('MESSAGE')
                            call tecael(iadzi, iazk24)
                            nomail = zk24(iazk24-1+3) (1:8)
                            write (umess,9001) 'NMBARC_10','EXP EXPLOSE A LA MAILLE: ',&
     &                                                           nomail
                            retcom = 1
                            goto 8000
                        endif
!
                        if ((xk*xinf0) .gt. epxmax) then
                            umess = iunifi('MESSAGE')
                            call tecael(iadzi, iazk24)
                            nomail = zk24(iazk24-1+3) (1:8)
                            write (umess,9001) 'NMBARC_11','EXP EXPLOSE A LA MAILLE: ',&
     &                                                           nomail
                            retcom = 1
                            goto 8000
                        endif
!
                        fxi0 = simoel*exp(-xk0*xinf0)-pcrmp1*exp(xk* xinf0)+(kc*p1)/2.d0
!
                        if (fxi0 .gt. zero) sigfi0 = 1
                        if (fxi0 .lt. zero) sigfi0 = -1
!
                        if ((signf0*sigfi0) .lt. zero) xsup0 = xb0
                        if ((signf0*sigfi0) .gt. zero) xinf0 = xb0
!
210                  end do
                    call u2mess('F', 'ALGORITH6_62')
101                  continue
                    xb = xb0
                    xsup = xb
!
!     --RESOLUTION AVEC LA METHODE DE NEWTON ENTRE LES BORNES
                    v0 = xinf
                    seuil = m**2*(pcrmp1+kc*p1/2)**2
!
                    if ((-xk0*v0) .gt. epxmax) then
                        umess = iunifi('MESSAGE')
                        call tecael(iadzi, iazk24)
                        nomail = zk24(iazk24-1+3) (1:8)
                        write (umess,9001) 'NMBARC_12','EXP EXPLOSE A LA MAILLE:',&
     &                                                           nomail
                        retcom = 1
                        goto 8000
                    endif
!
                    if ((xk*v0) .gt. epxmax) then
                        umess = iunifi('MESSAGE')
                        call tecael(iadzi, iazk24)
                        nomail = zk24(iazk24-1+3) (1:8)
                        write (umess,9001) 'NMBARC_13','EXP EXPLOSE A LA MAILLE:',&
     &                                                           nomail
                        retcom = 1
                        goto 8000
                    endif
!
                    if ((-2.d0*xk0*v0) .gt. epxmax) then
                        umess = iunifi('MESSAGE')
                        call tecael(iadzi, iazk24)
                        nomail = zk24(iazk24-1+3) (1:8)
                        write (umess,9001) 'NMBARC_14','EXP EXPLOSE A LA MAILLE: ',&
     &                                                           nomail
                        retcom = 1
                        goto 8000
                    endif
!
                    if (((xk-xk0)*v0) .gt. epxmax) then
                        umess = iunifi('MESSAGE')
                        call tecael(iadzi, iazk24)
                        nomail = zk24(iazk24-1+3) (1:8)
                        write (umess,9001) 'NMBARC_15','EXP EXPLOSE A LA MAILLE: ',&
     &                                                           nomail
                        retcom = 1
                        goto 8000
                    endif
!
                    f1 = simoel*exp(-xk0*v0)
                    f2 = simoel*exp(-xk0*v0)-2.d0*pcrmp1*exp(xk*v0)
                    f3 = 2.d0*simoel*exp(-xk0*v0)-2.d0*pcrmp1*exp(xk* v0)+kpmax
                    f = sieleq**2+m**2*(1.d0+6.d0*deuxmu*v0*alphab/ 2.d0/m/m/f3)**2 *(f1+kpmax)*f&
                        &2
!
                    f4 = (1.d0+6.d0*deuxmu*v0*alphab/2.d0/m/m/f3)
                    f5 = -2.d0*xk0*simoel**2*exp(-2.d0*xk0*v0)+ 2.d0*simoel*pcrmp1*exp((xk-xk0)*v&
                         &0)*(xk0-xk) -kpmax*(xk0*simoel*exp(-xk0*v0)+2.d0*xk*pcrmp1* exp(xk*v0))
                    f6 = 2.d0*simoel*(1.d0+v0*xk0)*exp(-xk0*v0)+ 2.d0*pcrmp1*(-1.d0+v0*xk)*exp(xk&
                         &*v0)+ kpmax
                    fp = m**2*f4**2*f5+6.d0*deuxmu*alphab*f4*(f1+ kpmax)*f2*(f6/f3/f3)
!
!
                    do 200 iter = 1, nint(crit(1))
!     --CRITERE DE CONVERGENCE
                        if ((abs(f)/seuil) .le. crit(3)) goto 100
!
!     --CONSTRUCTION DU NOUVEL ITERE
                        v0 = v0-f/fp
                        if (xsup .gt. zero) then
                            if (v0 .le. xinf .or. v0 .ge. xsup) v0 = ( xinf+xsup)/2
                        else
                            if (v0 .le. xsup .or. v0 .ge. xinf) v0 = ( xinf+xsup)/2
                        endif
!
                        if ((-xk0*v0) .gt. epxmax) then
                            umess = iunifi('MESSAGE')
                            call tecael(iadzi, iazk24)
                            nomail = zk24(iazk24-1+3) (1:8)
                            write (umess,9001) 'NMBARC_16','EXP EXPLOSE A LA MAILLE:',&
     &                                                           nomail
                            retcom = 1
                            goto 8000
                        endif
!
                        if ((xk*v0) .gt. epxmax) then
                            umess = iunifi('MESSAGE')
                            call tecael(iadzi, iazk24)
                            nomail = zk24(iazk24-1+3) (1:8)
                            write (umess,9001) 'NMBARC_17','EXP EXPLOSE A LA MAILLE: ',&
     &                                                           nomail
                            retcom = 1
                            goto 8000
                        endif
!
                        if ((-2.d0*xk0*v0) .gt. epxmax) then
                            umess = iunifi('MESSAGE')
                            call tecael(iadzi, iazk24)
                            nomail = zk24(iazk24-1+3) (1:8)
                            write (umess,9001) 'NMBARC_18','EXP EXPLOSE A LA MAILLE: ',&
     &                                                           nomail
                            retcom = 1
                            goto 8000
                        endif
!
                        if ((-xk0*xinf) .gt. epxmax) then
                            umess = iunifi('MESSAGE')
                            call tecael(iadzi, iazk24)
                            nomail = zk24(iazk24-1+3) (1:8)
                            write (umess,9001) 'NMBARC_19','EXP EXPLOSE A LA MAILLE: ',&
     &                                                           nomail
                            retcom = 1
                            goto 8000
                        endif
!
                        if ((xk*xinf) .gt. epxmax) then
                            umess = iunifi('MESSAGE')
                            call tecael(iadzi, iazk24)
                            nomail = zk24(iazk24-1+3) (1:8)
                            write (umess,9001) 'NMBARC_20','EXP EXPLOSE A LA MAILLE: ',&
     &                                                           nomail
                            retcom = 1
                            goto 8000
                        endif
!
!     --CALCUL DE LA FONCTION EN V0 ET DE SA DERIVEE
                        f1 = simoel*exp(-xk0*v0)
                        f2 = simoel*exp(-xk0*v0)-2.d0*pcrmp1*exp(xk* v0)
                        f3 = 2.d0*simoel*exp(-xk0*v0)-2.d0*pcrmp1*exp( xk*v0)+kpmax
                        f = sieleq**2+m**2*(1.d0+6.d0*deuxmu*v0* alphab/2.d0/m/m/f3)**2 *(f1+kpma&
                            &x)*f2
                        if (f .gt. zero) signf = 1
                        if (f .lt. zero) signf = -1
                        f4 = (1.d0+6.d0*deuxmu*v0*alphab/2.d0/m/m/f3)
                        f5 = -2.d0*xk0*simoel**2*exp(-2.d0*xk0*v0)+ 2.d0*simoel*pcrmp1*exp((xk-xk&
                             &0)*v0)*(xk0-xk) -kpmax*(xk0*simoel*exp(-xk0*v0)+2.d0*xk* pcrmp1*exp&
                             &(xk*v0))
                        f6 = 2.d0*simoel*(1.d0+v0*xk0)*exp(-xk0*v0)+ 2.d0*pcrmp1*(-1.d0+v0*xk)*ex&
                             &p(xk*v0)+ kpmax
                        fp = m**2*f4**2*f5+6.d0*deuxmu*alphab*f4*(f1+ kpmax)*f2*(f6/f3/f3)
!
!
                        fxi1 = simoel*exp(-xk0*xinf)
                        fxi2 = simoel*exp(-xk0*xinf)-2.d0*pcrmp1*exp( xk*xinf)
                        fxi3 = 2.d0*simoel*exp(-xk0*xinf)-2.d0*pcrmp1* exp(xk*xinf)+kpmax
                        fxi = sieleq**2+m**2* (1.d0+6.d0*deuxmu*xinf* alphab/2.d0/m/m/fxi3)**2 *(&
                              &fxi1+kpmax)*fxi2
                        if (fxi .gt. zero) signfi = 1
                        if (fxi .lt. zero) signfi = -1
!
                        if ((signf*signfi) .lt. zero) xsup = v0
                        if ((signf*signfi) .gt. zero) xinf = v0
!
!
200                  end do
                    call u2mess('F', 'ALGORITH3_55')
100                  continue
                    deppmo=v0
!
                    if ((xk*deppmo) .gt. epxmax) then
                        umess = iunifi('MESSAGE')
                        call tecael(iadzi, iazk24)
                        nomail = zk24(iazk24-1+3) (1:8)
                        write (umess,9001) 'NMBARC_21','EXP EXPLOSE A LA MAILLE: ',&
     &                                                           nomail
                        retcom = 1
                        goto 8000
                    endif
!
                    if ((xks*deppmo) .gt. epxmax) then
                        umess = iunifi('MESSAGE')
                        call tecael(iadzi, iazk24)
                        nomail = zk24(iazk24-1+3) (1:8)
                        write (umess,9001) 'NMBARC_22','EXP EXPLOSE A LA MAILLE: ',&
     &                                                           nomail
                        retcom = 1
                        goto 8000
                    endif
!
                    if ((xk0*(depsmo-deppmo)) .gt. epxmax) then
                        umess = iunifi('MESSAGE')
                        call tecael(iadzi, iazk24)
                        nomail = zk24(iazk24-1+3) (1:8)
                        write (umess,9001) 'NMBARC_23','EXP EXPLOSE A LA MAILLE: ',&
     &                                                           nomail
                        retcom = 1
                        goto 8000
                    endif
!
!     -- REACTUALISATION DE LA VARIABLE INTERNE MECANIQUE PCR
                    pcrp(1) = pcrmp1*exp(xk*deppmo)
!     -- CALCUL DE LA DERIVEE DE PCRP PAR RAPPORT A P1
                    pcrpp = -log(2.d0*prescr/pa)* ((lambda-kapa)/( lambb-kapa)**2)*lamp*pcrp(1)
!     -- REACTUALISATION DU SEUIL HYDRIQUE
                    pc0p(1) = (pc0m(1)+pa)*exp(xks*deppmo)-pa
!
                    psp = kc*pc0p(1)
!
!     -- REACTUALISATION DES CONTRAINTES NETTES DE BARCELONE
                    sigpmo = sigmmo*exp( xk0*(depsmo-deppmo))/ ((p1+pa) /(p1m+pa))**(xk0/xk0s )
                    call r8inir(6, 0.d0, sigpdv, 1)
                    do 119 k = 1, ndimsi
                        sigpdv(k) = sigel(k)/(1.d0+(6.d0*deuxmu/2.d0* alphab* deppmo)/(m*m*(2.d0*&
                                    &sigpmo-2.d0*pcrp(1) +kpmax)))
                        sbarp(k) = sigpdv(k)-sigpmo*kron(k)
!     -- CALCUL DES CONTRAINTES DE BISHOP
                        sbisp(k) = sbarp(k)-(sipp+p2)*kron(k)
119                  end do
!
                endif
            endif
        endif
        vip(1) = pcrp(1)
        vip(2) = pcrp(2)
        vip(3) = pc0p(1)
        vip(4) = pc0p(2)
        vip(5) = psp
    endif
!
!     -- 9 CALCUL DE L'OPERATEUR TANGENT :
!     --------------------------------
    if (option(1:14) .eq. 'RIGI_MECA_TANG' .or. option(1:9) .eq. 'FULL_MECA') then
!
        if (option(1:14) .eq. 'RIGI_MECA_TANG') then
            if ((pcrm(2).eq.0.d0) .and. (pc0m(2).eq.0.d0)) then
                matr = 0
            else if ((pcrm(2).eq.1.d0).and.(pc0m(2).eq.0.d0)) then
                matr = 1
            else if ((pcrm(2).eq.0.d0).and.(pc0m(2).eq.1.d0)) then
                matr =11
            endif
        endif
        if (option(1:9) .eq. 'FULL_MECA') then
            if ((pcrp(2).eq.1.d0) .and. (pc0p(2).eq.0.d0)) then
                matr = 2
            else if ((pcrp(2).eq.0.d0).and.(pc0p(2).eq.1.d0)) then
                matr = 21
            else if ((pcrp(2).eq.0.d0).and.(pc0p(2).eq.0.d0)) then
                matr = 0
            endif
        endif
!
!     --9.0 INITIALISATION DE L'OPERATEUR TANGENT
!     ---------------------------------------
        do 125 k = 1, 6
            do 126 l = 1, 6
                dsidep(k,l) = 0.d0
                dsidp1(k) = 0.d0
126          end do
125      end do
!
!     -- 9.1 CALCUL DE DSIDEP(6,6)-ELASTIQUE:
!     ---------------------------------------
        if (matr .eq. 0) then
            do 127 k = 1, 3
                do 128 l = 1, 3
                    dsidep(k,l) = xk0*simoel-deuxmu/3.d0
128              end do
127          end do
            do 129 k = 1, ndimsi
                dsidep(k,k) = dsidep(k,k)+deuxmu
129          end do
!
!     -- 9.2 CALCUL DE DSIDP1(6) ELASTIQUE:
!     ---------------------------------------
            do 139 k = 1, ndimsi
                dsidp1(k) = (xk0/xk0s*simoel/(p1+pa)-sat*biot)*kron(k)
139          end do
        endif
!     -- 9.3 CALCUL DE DSIDEP(6,6) CRITERE MECANIQUE ATTEINT
!     ------------------------------------------------------
!     -EN VITESSE :
!     ------------
        if (matr .eq. 1) then
!
!     -- 9.3.1 CALCUL DU MODULE ELASTOPLASTIQUE H
            h = m**4*(2.d0*sigmmo-2.d0*pcrmp1+kpmax)* (xk0*sigmmo*( 2.d0*sigmmo-2.d0*pcrmp1+kpmax&
                &)+2.d0*xk*pcrmp1* (sigmmo+ kpmax))+6.d0*deuxmu*sieqm**2*alphab
!
!     -- 9.3.2 CALCUL D'UN TERME INTERMEDIAIRE
            do 160 k = 1, ndimsi
                a(k) = 0.d0
160          end do
            do 130 k = 1, 3
                a(k) = -xk0*m*m*sigmmo*(2.d0*sigmmo-2.d0*pcrmp1+kpmax) *kron(k)+3.d0*deuxmu*sigmd&
                       &v(k)
                ap(k) = -xk0*m*m*sigmmo*(2.d0*sigmmo-2.d0*pcrmp1+ kpmax) *kron(k)+3.d0*deuxmu*sig&
                        &mdv(k)*alphab
130          end do
            call r8inir(3, 0.d0, aa, 1)
            do 131 k = 4, ndimsi
                aa(k) = 3.d0*deuxmu*sigmdv(k)
                aap(k) = 3.d0*deuxmu*sigmdv(k)*alphab
131          end do
!
!     -- 9.3.3 CALCUL DES TERMES DE DSIDEP
            call r8inir(ndimsi*ndimsi, 0.d0, dsidep, 1)
            do 132 k = 1, 3
                do 133 l = 1, 3
                    dsidep(k,l)=xk0*sigmmo-deuxmu/3.d0 - 1.d0/2.d0/h*(&
                    a(k)*ap(l)+a(l)*ap(k))
133              end do
132          end do
            do 134 k = 1, 3
                do 135 l = 4, ndimsi
                    dsidep(k,l) = -1.d0/2.d0*(a(k)*aap(l)+ap(k)*aa(l))
                    dsidep(k,l) = dsidep(k,l)/h
                    dsidep(l,k) = dsidep(k,l)
135              end do
134          end do
            do 136 k = 4, ndimsi
                do 137 l = 4, ndimsi
                    dsidep(k,l) = -1.d0/2.d0*(aa(k)*aap(l)+aa(l)*aap( k))
                    dsidep(k,l) = dsidep(k,l)/h
137              end do
136          end do
            do 138 k = 1, ndimsi
                dsidep(k,k) = deuxmu + dsidep(k,k)
138          end do
!     -- 9.4 CALCUL DE DSIDP1(6) CRITERE MECANIQUE ATTEINT-
!     -----------------------------------------------------
!     EN VITESSE :
!     -----------
            tra = -3.d0*xk0*m*m*sigmmo*(2.d0*sigmmo-2.d0*pcrmp1+kpmax)
            par = (&
                  kc*(2.d0*pcrmp1-sigmmo)-2.d0*pcrmp1* (sigmmo+kpmax) *log(2.d0*prescr/pa)* ((lam&
                  &bda-kapa)/(lambb-kapa)**2)* lamp&
                  )
            do 161 k = 1, 3
                dsidp1(k) = -ap(k)*tra/3.d0/h/xk0s/(p1+pa) +m*m*par/h* ap(k) +xk0*sigmmo/xk0s/(p1&
                            &+pa) -biot*sat
161          end do
            do 166 k = 4, ndimsi
                dsidp1(k)=-deuxmu*tra*sigmdv(k)*alphab/h/xk0s/(p1+pa)&
                +3.d0*deuxmu*sigmdv(k)*alphab*m*m*par/h
166          end do
        endif
!     -- 9.5 CALCUL DE DSIDEP(6,6)CRITERE HYDRIQUE ATTEINT-EN VITESSE:
!     ---------------------------------------------------------------
        if (matr .eq. 11) then
            do 162 k = 1, 3
                do 163 l = 1, 3
                    dsidep(k,l) = xk0*sigmmo-deuxmu/3.d0
163              end do
162          end do
            do 164 k = 1, ndimsi
                dsidep(k,k) = dsidep(k,k)+deuxmu
164          end do
!     -- 9.6 CALCUL DE DSIDP1(6) CRITERE HYDRIQUE ATTEINT-EN VITESSE:
!     --------------------------------------------------------------
            do 165 k = 1, ndimsi
                dsidp1(k) = ( xk0*sigmmo/(p1+pa)*(1.d0/xks+1.d0/xk0s) -sat*biot)*kron(k )
165          end do
        endif
!
!---OPERATEUR TANGENT EN VITESSE  A LINSTATNT COURANT AU LIEU
!--- DE L OPERATEUR COHERENT (DANS LE DOUTE)
        if (matr .eq. 2) then
!
            sieqp = 0.0d0
            do 4000 k = 1, ndimsi
                sieqp = sieqp + sigpdv(k)**2
4000          continue
            sieqp = sqrt(1.5d0*sieqp)
!     -- 9.3.11 CALCUL DU MODULE ELASTOPLASTIQUE H
            h = m**4*(&
                2.d0*sigpmo-2.d0*pcrp(1)+kpmax)* (xk0*sigpmo*( 2.d0*sigpmo-2.d0*pcrp(1)+kpmax)+2.&
                &d0*xk*pcrp(1)* (sigpmo+ kpmax)&
                )+6.d0*deuxmu*sieqp**2*alphab
!
!     -- 9.3.12 CALCUL D'UN TERME INTERMEDIAIRE
            do 260 k = 1, ndimsi
                a(k) = 0.d0
260          end do
            do 230 k = 1, 3
                a(k) = -xk0*m*m*sigpmo*(&
                       2.d0*sigpmo-2.d0*pcrp(1)+ kpmax) *kron(k)+3.d0*deuxmu*sigpdv(k)
                ap(k) = -xk0*m*m*sigpmo*(&
                        2.d0*sigpmo-2.d0*pcrp(1)+ kpmax) *kron(k)+3.d0*deuxmu*sigpdv(k)*alphab
230          end do
            call r8inir(3, 0.d0, aa, 1)
            do 231 k = 4, ndimsi
                aa(k) = 3.d0*deuxmu*sigpdv(k)
                aap(k) = 3.d0*deuxmu*sigpdv(k)*alphab
231          end do
!
!     -- 9.3.13 CALCUL DES TERMES DE DSIDEP
            call r8inir(ndimsi*ndimsi, 0.d0, dsidep, 1)
            do 232 k = 1, 3
                do 233 l = 1, 3
                    dsidep(k,l)=xk0*sigpmo-deuxmu/3.d0 - 1.d0/2.d0/h*(&
                    a(k)*ap(l)+a(l)*ap(k))
233              end do
232          end do
            do 234 k = 1, 3
                do 235 l = 4, ndimsi
                    dsidep(k,l) = -1.d0/2.d0*(a(k)*aap(l)+ap(k)*aa(l))
                    dsidep(k,l) = dsidep(k,l)/h
                    dsidep(l,k) = dsidep(k,l)
235              end do
234          end do
            do 236 k = 4, ndimsi
                do 237 l = 4, ndimsi
                    dsidep(k,l) = -1.d0/2.d0*(aa(k)*aap(l)+aa(l)*aap( k))
                    dsidep(k,l) = dsidep(k,l)/h
237              end do
236          end do
            do 238 k = 1, ndimsi
                dsidep(k,k) = deuxmu + dsidep(k,k)
238          end do
!     -- 9.3.14 CALCUL DE DSIDP1(6) CRITERE MECANIQUE ATTEINT-
!     -----------------------------------------------------
!     EN VITESSE :
!     -----------
            tra = -3.d0*xk0*m*m*sigpmo*(2.d0*sigpmo-2.d0*pcrp(1)+ kpmax)
            par = (&
                  kc*(&
                  2.d0*pcrp(1)-sigpmo)-2.d0*pcrp(1)* (sigpmo+ kpmax)*log(2.d0*prescr/pa)* ((lambd&
                  &a-kapa)/(lambb-kapa)** 2&
                  )*lamp&
                  )
            do 261 k = 1, 3
                dsidp1(k) = -ap(k)*tra/3.d0/h/xk0s/(p1+pa) +m*m*par/h* ap(k) +xk0*sigpmo/xk0s/(p1&
                            &+pa) -biot*sat
261          end do
            do 266 k = 4, ndimsi
                dsidp1(k)=-deuxmu*tra*sigpdv(k)*alphab/h/xk0s/(p1+pa)&
                +3.d0*deuxmu*sigpdv(k)*alphab*m*m*par/h
266          end do
        endif
!   -- 9.3.15 CALCUL DE DSIDEP(6,6)CRITERE HYDRIQUE ATTEINT-EN VITESSE:
!   ---------------------------------------------------------------
        if (matr .eq. 21) then
            do 262 k = 1, 3
                do 263 l = 1, 3
                    dsidep(k,l) = xk0*sigpmo-deuxmu/3.d0
263              end do
262          end do
            do 264 k = 1, ndimsi
                dsidep(k,k) = dsidep(k,k)+deuxmu
264          end do
!     -- 9.3.16 CALCUL DE DSIDP1(6) CRITERE HYDRIQUE ATTEINT-EN VITESSE:
!     --------------------------------------------------------------
            do 265 k = 1, ndimsi
                dsidp1(k) = ( xk0*sigpmo/(p1+pa)*(1.d0/xks+1.d0/xk0s) -sat*biot)*kron(k )
265          end do
        endif
!
!---------------------------------------------------------------------
!
!     -- 9.7 CALCUL DE DSIDEP(6,6)-MATRICE COHERENTE CRITERE MECANIQUE
!      ATTEINT: MATRICE QUI RELIE LES CONTRAINTES AUX DEFORMATIONS
!     -----------------------------------------------------------------
        if (matr .eq. 3) then
            sieqp = 0.0d0
            do 3000 k = 1, ndimsi
                sieqp = sieqp + sigpdv(k)**2
3000          continue
            sieqp = sqrt(1.5d0*sieqp)
            diff1 = abs((sigpmo-pcrp(1)+kpmax/2)/(pcrp(1)-kpmax/2))
            if (diff1 .lt. crit(3)) then
!
!     -- 9.7.0.1 OPERATEUR TANGENT COHERENT AU POINT CRITIQUE
!     -- TRAITEMENT DE LA PARTIE DEVIATORIQUE
!     -- CALCUL DE Q+
!     -- CALCUL DU TENSEUR HHB QUI MULTIMPLIE LA DEFORMATION
                call r8inir(6*6, 0.d0, ses, 1)
                do 1000 k = 1, ndimsi
                    do 1001 l = 1, ndimsi
                        ses(k,l) = 1.d0/2.d0*( sigpdv(k)*sigel(l)+ sigel(k)*sigpdv(l))
1001                  end do
1000              end do
                call r8inir(6*6, 0.d0, hhb, 1)
                do 3001 k = 1, ndimsi
                    do 3002 l = 1, ndimsi
                        hhb(k,l) = -deuxmu*3.d0*ses(k,l )/2.d0/sieleq/ sieqp/alphab
3002                  continue
3001              continue
                do 3003 k = 1, ndimsi
                    hhb(k,k) = deuxmu+hhb(k,k)
3003              continue
                if (ndim .eq. 2) then
                    hhb(5,5) = 1.d0
                    hhb(6,6) = 1.d0
                endif
!     -- INVERSE DE HHB
                call r8inir(6*6, 0.d0, hhbm, 1)
                do 3004 k = 1, 6
                    hhbm(k,k)=1.d0
3004              continue
                call mgauss('NFVP', hhb, hhbm, 6, 6,&
                            6, det, iret)
!     -- CALCUL DU TENSEUR GG QUI MULTIMPLIE LA CONTRAINTE
                call r8inir(6*6, 0.d0, sps, 1)
                do 1002 k = 1, ndimsi
                    do 1003 l = 1, ndimsi
                        sps(k,l) = sigpdv(k)*sigpdv(l)
1003                  end do
1002              end do
                call r8inir(6*6, 0.d0, gg, 1)
                do 3005 k = 1, ndimsi
                    do 3006 l = 1, ndimsi
                        gg(k,l) = -3.d0*sieleq*sps(k,l)/2.d0/sieqp**3/ alphab
3006                  continue
3005              continue
                do 3007 k = 1, ndimsi
                    gg(k,k) = sieleq/sieqp/alphab+(1.d0-1.d0/alphab)+ gg(k,k)
3007              continue
!     --  MATRICE DE PROJECTION SUR L'ESPACE DES CONTRAINTES
!     -- DEVIATORIQUES
                call r8inir(6*6, 0.d0, v, 1)
                do 3015 k = 1, 3
                    do 3016 l = 1, 3
                        v(k,l) = -1.d0/3.d0
                        v(l,k) = v(k,l)
3016                  continue
3015              continue
                do 3017 k = 1, ndimsi
                    v(k,k) = v(k,k) + 1.d0
3017              continue
!     --  PRODUIT DE LA MATRICE DE PROJECTION SUR L'ESPACE
!     --  DES CONTRAINTES DEVIATORIQUES PAR GG
                call r8inir(6*6, 0.d0, d1g, 1)
                call promat(v, 6, ndimsi, ndimsi, gg,&
                            6, ndimsi, ndimsi, d1g)
!     -- PRODUIT DU RESULTAT PAR L'INVERSE DE HHB
                call r8inir(6*6, 0.d0, d1ghhm, 1)
                call promat(d1g, 6, ndimsi, ndimsi, hhbm,&
                            6, ndimsi, ndimsi, d1ghhm)
!
!     -- 9.7.0.2 TRAITEMENT DE LA PARTIE HYDROSTATIQUE
!     --  PRODUIT DE LA MATRICE DE PROJECTION SUR L'ESPACE
!     --  DES CONTRAINTES HYDROSTATIQUES PAR LA MATRICE IDENTITE
!     --  D'ORDRE 2
                call r8inir(6*6, 0.d0, id2, 1)
                do 3008 k = 1, 3
                    do 3009 l = 1, 3
                        id2(k,l) = -1.d0/3.d0/xk0/sigpmo
3009                  continue
3008              continue
!     -- SOMME DES TERMES DEVIATORIQUE ET HYDROSTATIQUE
                call r8inir(6*6, 0.d0, devhyd, 1)
                do 3010 k = 1, ndimsi
                    do 3011 l = 1, ndimsi
                        devhyd(k,l) = d1ghhm(k,l)/deuxmu + id2(k,l)
3011                  continue
3010              continue
                if (ndim .eq. 2) then
                    devhyd(5,5) = 1.d0
                    devhyd(6,6) = 1.d0
                endif
!     -- INVERSE DE LA SOMME DES TERMES DEVIATORIQUE ET HYDROSTATIQUE
                call r8inir(6*6, 0.d0, devhym, 1)
                do 3012 k = 1, 6
                    devhym(k,k)=1.d0
3012              continue
                call mgauss('NFVP', devhyd, devhym, 6, 6,&
                            6, det, iret)
!     -- TERMES DE L'OPERATEUR TANGENT QUI RELIENT LA CONTRAINTE
!     -- A LA DEFORMATION
                call r8inir(6*6, 0.d0, dsidep, 1)
                do 3013 k = 1, 6
                    do 3014 l = 1, 6
                        dsidep(k,l) = devhym(k,l)
3014                  continue
3013              continue
!     -- 9.7.0.3 CALCUL DE DSIDP1(6) COHERENT AU POINT CRITIQUE:
!    -- MATRICE QUI RELIE LES CONTRAINTES A LA SUCCION
                call r8inir(6*6, 0.d0, dsidp1, 1)
                do 3018 k = 1, ndimsi
                    dsidp1(k) = -kron(k)/xk0s/(p1+pa)-biot*sat*kron(k)
3018              continue
!
            else
!
!      -- 9.7.1 OPERATEUR TANGENT COHERENT DANS LE CAS GENERAL
!      -- CALCUL DES INCREMENTS DE P ET DE S
                deltap = sigpmo - sigmmo
                call r8inir(6, 0.d0, deltas, 1)
                do 140 k = 1, ndimsi
                    deltas(k)=sigpdv(k)-sigmdv(k)
140              end do
!
!     -- 9.7.2 CALCUL DE VECTEURS INTERMEDIAIRES
                spards = 0.d0
                do 141 k = 1, ndimsi
                    spards = spards+deltas(k)*sigpdv(k)
141              end do
                call r8inir(6, 0.d0, tplus, 1)
                do 142 k = 1, ndimsi
                    tplus(k) = sigpdv(k) + deltas(k)
142              end do
!
!      9.7.3-- TERMES NECESSAIRES A LA PARTIE DEVIATORIQUE
                hp = 2.d0*m**4*xk*(sigpmo+kpmax)*pcrp(1)* (2.d0* sigpmo-2.d0*pcrp(1)+kpmax)
!
                xc = 9.d0*spards*alphab/hp
                xd = 3.d0*m*m*(2.d0*sigpmo-2.d0*pcrp(1)+kpmax )*deltap* alphab/hp
                xgg = -3.d0*m**2/hp*kc*(2.d0*pcrp(1)-sigpmo)*dp1* alphab
                xhhc = -6.d0*alphab/hp*m*m*(sigpmo+kpmax)*pcrpp*dp1
                xv = 3.d0*spards + m**2*(&
                     2.d0*sigpmo-2.d0*pcrp(1)+ kpmax)*deltap- m**2*(kc*(2.d0*pcrp(1)-sigpmo)+2.d0&
                     &*( sigpmo+kpmax)*pcrpp&
                     )*dp1
                xlam = xv/hp
                xa = (&
                     xlam*xk*m**4*(sigpmo+kpmax)* (2.d0*sigpmo-4.d0* pcrp(1)+kpmax)+ m**2*deltap+&
                     &m**2*kc*dp1)*m**2* (2.d0*sigpmo-2.d0*pcrp(1)+kpmax)/ (m**2*xlam+(1.d0/ 2.d0&
                     &/xk/pcrp(1))&
                     )
                xi = m**2*(&
                     2.d0*sigpmo-2.d0*pcrp(1)+kpmax)-m**4*xlam* (2.d0*sigpmo-2.d0*pcrp(1)+kpmax)/&
                     & ((1.d0/2.d0/xk/pcrp( 1))+m**2*xlam&
                     )
                rap = xi/(hp+xa)
!
!     9.7.4-- CALCUL DE LA MATRICE CC-SYMETRISATION DE TPLUS.I
!
                call r8inir(6*6, 0.d0, cc, 1)
                do 172 k = 1, 3
                    do 173 l = 1, 3
                        cc(k,l)=(tplus(k)+tplus(l))/2.d0
173                  end do
172              end do
                do 174 k = 1, 3
                    do 175 l = 4, ndimsi
                        cc(k,l)=tplus(l)/2.d0
                        cc(l,k)=cc(k,l)
175                  end do
174              end do
!
!     9.7.5-- CALCUL DES TERMES D'UNE MATRICE INTERMEDIAIRE C
!
                call r8inir(6*6, 0.d0, c, 1)
                do 170 k = 1, ndimsi
                    do 171 l = 1, ndimsi
                        c(k,l) = 9.d0*alphab/2.d0/(hp+xa)*(sigpdv(k)* tplus(l)+ tplus(k)*sigpdv(l&
                                 &))
171                  end do
170              end do
                do 149 k = 1, ndimsi
                    c(k,k) = c(k,k)+1.d0/deuxmu+xc+xd+xgg+xhhc
149              end do
!
!     9.7.6-- ASSEMBLAGE DES TERMES POUR LA PARTIE DEVIATORIQUE
                call r8inir(6*6, 0.d0, ee, 1)
                do 180 k = 1, ndimsi
                    do 181 l = 1, ndimsi
                        ee(k,l) = c(k,l) - rap*cc(k,l)
181                  continue
180              continue
!
!      9.7.7-- TERMES NECESSAIRES A LA PARTIE HYDROSTATIQUE
                xu = 2.d0*m**2*xk*pcrp(1)
                xg = xlam*xu/(1.d0+xlam*xu)
                xh = xu*(2.d0*sigpmo-2.d0*pcrp(1)+kpmax)/2.d0/(1.d0+ xlam*xu)
                xm = 2.d0*sigpmo**2-4.d0*sigpmo*pcrp(1)-4.d0*pcrp(1)* kpmax +3.d0*sigpmo*kpmax+(k&
                     &pmax)**2
                xe = 1.d0+xh*2.d0*m**2*(deltap+kpmax )/hp+xh*2.d0*xk* m**4* xm*xv/hp/hp
                xf = m**2*(&
                     2.d0*sigpmo-2.d0*pcrp(1)+ kpmax+kc*(1.d0- 2.d0*xg)*dp1+2.d0*deltap -2.d0*xg*&
                     &deltap-2.d0*pcrpp* dp1)/hp -2.d0*xk*m**4*xv/hp/hp* ((4.d0*sigpmo-2.d0* pcrp&
                     &(1)+3.d0*kpmax)*pcrp(1)+xg*xm&
                     )
                ct = (&
                     1.d0+m**2*xk0*sigpmo*(&
                     2.d0*xlam-2.d0*xg*xlam- 2.d0* xlam*xf*xh/xe+xf/xe*(&
                     2.d0*sigpmo-2.d0*pcrp(1)+ kpmax))&
                     ) /(xk0*sigpmo&
                     )
!
!     9.7.8--  VECTEUR INTERMEDIAIRE
                call r8inir(6, 0.d0, fv, 1)
                do 190 k = 1, ndimsi
                    fv(k)=3.d0*xf/xe*sigpdv(k)-ct*kron(k)/3.d0
190              end do
!     9.7.9-- SYMMETRISATION DEFV ET SA PROJECTION SUR L'ESPACE
!     -- DES CONTRAINTES HYDROSTATIQUES
                call r8inir(6*6, 0.d0, ffi, 1)
                do 195 k = 1, 3
                    do 196 l = 1, 3
                        ffi(k,l) = -(1.d0/3.d0)*(fv(k)+fv(l))/2.d0
196                  end do
195              end do
                do 197 k = 1, 3
                    do 198 l = 4, ndimsi
                        ffi(k,l) = -(1.d0/3.d0)*fv(l)/2.d0
                        ffi(l,k) = ffi(k,l)
198                  end do
197              end do
!     9.7.10--  MATRICE DE PROJECTION SUR L'ESPACE DES CONTRAINTES
!     -- DEVIATORIQUES
                call r8inir(6*6, 0.d0, v, 1)
                do 185 k = 1, 3
                    do 186 l = 1, 3
                        v(k,l) = -1.d0/3.d0
                        v(l,k) = v(k,l)
186                  end do
185              end do
                do 187 k = 1, ndimsi
                    v(k,k) = v(k,k) + 1.d0
187              end do
!     9.7.11-- PROJECTION DE EE SUR L'ESPACE DES CONTRAINTES
!     -- DEVIATORIQUES
                call r8inir(6*6, 0.d0, s, 1)
                call promat(ee, 6, ndimsi, ndimsi, v,&
                            6, ndimsi, ndimsi, s)
!
!     9.7.12-- COMBINAISON DES DEUX PARTIES DEVIATORIQUE ET
!     -- HYDROSTATIQUE
                call r8inir(6*6, 0.d0, t, 1)
                do 204 k = 1, ndimsi
                    do 205 l = 1, ndimsi
                        t(k,l) = s(k,l)+ ffi(k,l)
205                  end do
204              end do
                if (ndim .eq. 2) then
                    t(5,5) = 1.d0
                    t(6,6) = 1.d0
                endif
!     9.7.13-- INVERSE DE LA MATRICE T
                call r8inir(6*6, 0.d0, vv, 1)
                do 108 k = 1, 6
                    vv(k,k)=1.d0
108              end do
                call mgauss('NFVP', t, vv, 6, 6,&
                            6, det, iret)
!     --  9.7.14 CALCUL DES TERMES DSIDEP L'OPERATEUR TANGENT
                call r8inir(6*6, 0.d0, dsidep, 1)
                do 106 k = 1, 6
                    do 107 l = 1, 6
                        dsidep(k,l) = vv(k,l)
107                  end do
106              end do
!     -- 9.8 CALCUL DE DSIDP1(6) COHERENT CRITERE MECANIQUE ATTEINT:
!    -- MATRICE QUI RELIE LES CONTRAINTES A LA SUCCION
!     -----------------------------------------------------------
!     9.8.1 TERMES NECESSAIRES A LA PARTIE DEVIATORIQUE
                xz = m*m*xlam*(&
                     xk*kc*xlam*m*m*xm/(1.d0/xu+xlam)+ 2.d0*xk*kc*pcrp(1)*m*m* (3.d0*sigpmo-2.d0*&
                     &pcrp(1)+ 2.d0*kpmax))+ (m**4*kc*deltap*xlam+m*m*kc*kc*xlam*dp1) /(1.d0/xu+x&
                     &lam)+ m*m*kc*(2*pcrp(1)-sigpmo-deltap)+ 2.d0*m*m*pcrpp*(sigpmo+kpmax+kc*dp1&
                     )
                xdd = -kc*(xlam*m*m)**2/(m**2*xlam+1.d0/(2.d0*xk*pcrp( 1)))+ m*m*xlam*kc
!     9.8.2 VECTEUR INTERMEDIAIRE MULTIPLIE DP1 EN DEVIATORIQUE
                call r8inir(6, 0.d0, hh, 1)
                do 300 k = 1, ndimsi
                    hh(k) = 3.d0*xz/(hp+xa)*sigpdv(k)- xi*xz/3.d0/(hp+ xa)*kron(k) + xdd/3.d0*kro&
                            &n(k) + kron(k)/3.d0/ xk0s/(p1+pa)
300              continue
!     9.8.3 TERMES NECESSAIRES A LA PARTIE HYDROSTATIQUE
                xj = xu*xlam*kc/2.d0/(1.d0+xu*xlam)
                xhh = m*m/hp*(&
                      -2.d0*xj*deltap-2.d0*xj*kc*dp1+kc* deltap -2.d0*kc*pcrp(1)+kc*sigpmo -2.d0*&
                      &pcrpp*(sigpmo+ kpmax+kc*dp1))- 2.d0*xk*xv*m**4/hp/hp*(xj*xm+kc*pcrp( 1)* (&
                      &3.d0*sigpmo-2.d0*pcrp(1)+2.d0*kpmax)&
                      )
                ct1 = xk0*sigpmo*m**2*(&
                      xhh/xe*(&
                      2.d0*sigpmo-2.d0*pcrp(1)+kpmax) + kc*xlam-2.d0*xh*xhh/xe*xlam-2.d0*xj*xlam&
                      ) + xk0/xk0s*sigpmo/(p1+pa&
                      )
!     9.8.4 VECTEUR INTERMEDIAIRE MULTIPLIE DP1 EN HYDROSTATIQUE
                call r8inir(6, 0.d0, kv, 1)
                do 310 k = 1, ndimsi
                    kv(k) = 3.d0*xhh/xe*sigpdv(k)-ct1*kron(k)/3.d0
310              continue
!     9.8.5 MULTIPLICATION DE VV par H(K)-KV(K)
                call r8inir(6, 0.d0, hhkv, 1)
                do 320 k = 1, ndimsi
                    hhkv(k) = hh(k) - kv(k)
320              continue
                call r8inir(6, 0.d0, ssh, 1)
                call promat(vv, 6, ndimsi, ndimsi, hhkv,&
                            6, ndimsi, 1, ssh)
!     9.8.6 LES TERMES DE L'OPERATEUR TANGENT COHERENT DSIDP1(6)
                call r8inir(6, 0.d0, dsidp1, 1)
                do 330 k = 1, ndimsi
                    dsidp1(k) = ssh(k)-biot*sat*kron(k)
330              continue
            endif
        endif
!     --9.9 CALCUL DE DSIDEP(6,6) CRITERE HYDRIQUE ATTEINT
!     MATRICE QUI RELIE LES CONTRAINTES AUX DEFORMATIONS
!     ----------------------------------------------------
        if (matr .eq. 31) then
!     9.9.1 -- MATRICE QUI RELIE LES CONTRAINTES AUX DEFORMATIONS
            call r8inir(6*6, 0.d0, vh, 1)
            do 340 k = 1, 3
                do 345 l = 1, 3
                    vh(k,l) = -1.d0/3.d0+deuxmu/9.d0/xk0/sigpmo
                    vh(l,k) = vh(k,l)
345              end do
340          end do
            do 350 k = 1, ndimsi
                vh(k,k) = vh(k,k)+1.d0
350          end do
            call r8inir(6*6, 0.d0, vhh, 1)
            do 355 k = 1, ndimsi
                do 360 l = 1, ndimsi
                    vhh(k,l) = 1.d0/deuxmu*vh(k,l)
360              end do
355          end do
            if (ndim .eq. 2) then
                vhh(5,5) = 1.d0
                vhh(6,6) = 1.d0
            endif
!     9.9.2-- INVERSE DE LA MATRICE VHH
            call r8inir(6*6, 0.d0, vvh, 1)
            do 390 k = 1, 6
                vvh(k,k)=1.d0
390          end do
            call mgauss('NFVP', vhh, vvh, 6, 6,&
                        6, det, iret)
!     9.9.3--  LES TERMES DSIDEP L'OPERATEUR TANGENT
            call r8inir(6*6, 0.d0, dsidep, 1)
            do 395 k = 1, 6
                do 400 l = 1, 6
                    dsidep(k,l) = vvh(k,l)
400              end do
395          end do
!     --9.10 CALCUL DE DSIDP1(6) CRITERE HYDRIQUE ATTEINT
!    -- MATRICE QUI RELIE LES CONTRAINTES A LA SUCCION
!     LES TERMES DE L'OPERATEUR TANGENT COHERENT DSIDP1(6)
!    ------------------------------------------------------
            call r8inir(6, 0.d0, kkh, 1)
            do 402 k = 1, 3
                kkh(k) = 1.d0
402          continue
            do 403 k = 4, ndimsi
                kkh(k) = 0.d0
403          continue
            call r8inir(6, 0.d0, sshh, 1)
            call promat(vvh, 6, ndimsi, ndimsi, kkh,&
                        6, ndimsi, 1, sshh)
            bb = 1.d0/3.d0*(1.d0/xk0s/(p1+pa)+1.d0/xks/(p1+pa))
            do 410 k = 1, ndimsi
                dsidp1(k) = sshh(k)*bb-biot*sat*kron(k)
410          continue
        endif
    endif
! ======================================================================
8000  continue
! =====================================================================
    9001 format (a10,2x,a40,2x,a8)
!    FIN ---------------------------------------------------------
end subroutine
