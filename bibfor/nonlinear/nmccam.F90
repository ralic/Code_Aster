subroutine nmccam(ndim, typmod, imate, compor, crit,&
                  instam, instap, tm, tp, tref,&
                  deps, sigm, pcrm, option, sigp,&
                  pcrp, dsidep, retcom)
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
!
!
! ======================================================================
! aslint: disable=W1501
    implicit none
    include 'jeveux.h'
!
    include 'asterc/iisnan.h'
    include 'asterc/r8miem.h'
    include 'asterfort/matini.h'
    include 'asterfort/mgauss.h'
    include 'asterfort/promat.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rcvala.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    integer :: ndim, imate, retcom
    character(len=8) :: typmod(*)
    character(len=16) :: compor(*), option
    real(kind=8) :: crit(3), instam, instap, tm, tp, tref
    real(kind=8) :: deps(6), deuxmu
    real(kind=8) :: sigm(6), pcrm(7), sigp(6), pcrp(7), dsidep(6, 6)
! ----------------------------------------------------------------------
!     REALISE LA LOI DE CAM CLAY ELASTOPLASTIQUE POUR LES
!     ELEMENTS ISOPARAMETRIQUES EN PETITES DEFORMATIONS
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  IMATE   : ADRESSE DU MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT : RELCOM ET DEFORM
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  INSTAM  : INSTANT DU CALCUL PRECEDENT
! IN  INSTAP  : INSTANT DU CALCUL
! IN  TM      : TEMPERATURE A L'INSTANT PRECEDENT
! IN  TP      : TEMPERATURE A L'INSTANT DU CALCUL
! IN  TREF    : TEMPERATURE DE REFERENCE
! IN  DEPS    : INCREMENT DE DEFORMATION
! IN  SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
! IN  PCRM    : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
! IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
! OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL
! OUT PCRP    : VARIABLES INTERNES A L'INSTANT ACTUEL
! OUT DSIDEP  : MATRICE CARREE (INUTILISE POUR RAPH_MECA)
!
!               ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
!               L'ORDRE :  XX,YY,ZZ,SQRT(2)*XY,SQRT(2)*XZ,SQRT(2)*YZ
!
!
!
!
    logical :: cplan
    integer :: iadzi, iazk24, iret
    real(kind=8) :: epxmax
    parameter   (epxmax = 5.d0)
    character(len=8) :: nomail
    real(kind=8) :: valres(10), valpam(3)
    real(kind=8) :: mu, lambda, kapa, poro, prescr, m, kcam, ptrac
    real(kind=8) :: coef, poro1, poro2, young, nu, e0, xk0, xk, fonc
    real(kind=8) :: depsmo, deppmo, depseq
    real(kind=8) :: sigmmo, sigpmo, deltap, sieqm, sieqp, sieleq, simoel, spards
    real(kind=8) :: kron(6), depsdv(6), depsth(6), sigmdv(6), sigpdv(6)
    real(kind=8) :: deltas(6), sigel(6), tplus(6)
    real(kind=8) :: a(6), aa(6), fv(6)
    real(kind=8) :: ffi(6, 6), ee(6, 6), c(6, 6), cc(6, 6)
    real(kind=8) :: v(6, 6), s(6, 6), t(6, 6), vv(6, 6)
    real(kind=8) :: hh(6, 6), ses(6, 6), gg(6, 6), sps(6, 6), hhm(6, 6)
    real(kind=8) :: d1g(6, 6), d1ghhm(6, 6), id2(6, 6), devhyd(6, 6)
    real(kind=8) :: devhym(6, 6)
    real(kind=8) :: f1, f2, f3, f4, f, fp
    real(kind=8) :: f1p, f2p, f3p, f4p
    real(kind=8) :: fxi1, fxi2, fxi3, fxi4, fxi
    real(kind=8) :: h, hp, xc, xd, xlam, xa, xu, xg, xh, xe, xf, xv, xi, rap
    real(kind=8) :: ct, v0, seuil
    real(kind=8) :: xinf, xsup, rbid
    real(kind=8) :: diff, diff2
    real(kind=8) :: zero, un, deux, trois, six, unsde, tol, ptit
    real(kind=8) :: valm, valp
    integer :: ndimsi, signf, signfi
    integer :: i, k, l, iter, matr
    integer :: icodre(9)
    character(len=8) :: nomres(10), nompar(10)
! ======================================================================
    real(kind=8) :: valrm(5)
    integer :: valim
    character(len=16) :: valkm(5)
! ======================================================================
    parameter   ( zero   = 0.d0   )
    parameter   ( un     = 1.d0   )
    parameter   ( deux   = 2.d0   )
    parameter   ( trois  = 3.d0   )
    parameter   ( six    = 6.d0   )
    parameter   ( unsde  = 0.5d0  )
!
!
    data        kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
    data        tol/1.d-6/
! DEB ------------------------------------------------------------------
!
!     -- 1 INITIALISATIONS :
!     ----------------------
    cplan = typmod(1) .eq. 'C_PLAN'
    ndimsi = 2*ndim
    retcom = 0
!
    ptit = r8miem()
!
!     -- 2 RECUPERATION DES CARACTERISTIQUES MATERIAUX
!     -------------------------------------------------
    nomres(1)='ALPHA'
    nomres(2)='MU'
    nomres(3)='PORO'
    nomres(4)='KAPA'
    nomres(5)='LAMBDA'
    nomres(6)='M'
    nomres(7)='PRES_CRIT'
    nomres(8)='KCAM'
    nomres(9)='PTRAC'
!
    nompar(1) = 'TEMP'
    valpam(1) = tm
!
!
    if (compor(1)(1:9) .eq. 'CAM_CLAY ') then
!
        call rcvala(imate, ' ', 'ELAS', 1, nompar,&
                    valpam, 1, nomres(1), valres(1), icodre(1),&
                    0)
!
        if ((iisnan(tp).eq.0) .and. (iisnan(tm).gt.0)) then
            if ((iisnan(tref).gt.0) .or. (icodre(1) .ne.0)) then
                call u2mess('F', 'CALCULEL_31')
            else
                coef = valres(1)*(tp-tref) - valres(1)*(tm-tref)
            endif
        else
            valres(1) = 0.d0
            coef = 0.d0
        endif
        call rcvala(imate, ' ', 'CAM_CLAY ', 1, nompar,&
                    valpam, 8, nomres(2), valres(2), icodre(2),&
                    2)
        mu = valres(2)
        poro = valres(3)
        kapa = valres(4)
        lambda = valres(5)
        m = valres(6)
        prescr = valres(7)
        kcam = valres(8)
        ptrac = valres(9)
!
    endif
    if ((&
        (compor(1)(1:6) .eq. 'KIT_HM') .or. (compor(1)(1:7) .eq. 'KIT_HHM') .or.&
        (compor(1)(1:7) .eq. 'KIT_THM') .or. (compor(1)(1:8) .eq. 'KIT_THHM')&
        )&
        .and. (compor(11)(1:9) .eq. 'CAM_CLAY ')) then
!
        call rcvala(imate, ' ', 'ELAS', 1, nompar,&
                    valpam, 1, nomres(1), valres(1), icodre(1),&
                    0)
        if (icodre(1) .ne. 0) valres(1) = 0.d0
        coef = valres(1)*(tp-tref) - valres(1)*(tm-tref)
!
        call rcvala(imate, ' ', 'CAM_CLAY ', 1, nompar,&
                    valpam, 8, nomres(2), valres(2), icodre(2),&
                    2)
        mu = valres(2)
        poro = valres(3)
        poro1 = poro
        kapa = valres(4)
        lambda = valres(5)
        m = valres(6)
        prescr = valres(7)
        kcam = valres(8)
        ptrac = valres(9)
!
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
    endif
    deuxmu = deux*mu
    e0 = poro/(1.d0-poro)
    xk0 = (1.d0+e0)/kapa
    xk = (1.d0+e0)/(lambda-kapa)
    if ((kcam.ne.zero) .and. (kcam.le.(-xk0*ptrac))) then
        call u2mess('F', 'COMPOR1_42')
    endif
!
!     -- 3 CALCUL DE DEPSMO ET DEPSDV :
!     --------------------------------
    if (cplan) then
        call u2mess('F', 'ALGORITH6_63')
    endif
    depsmo = 0.d0
    do 110 k = 1, ndimsi
        depsth(k) = deps(k)
110  end do
    do 111 k = 1, 3
        depsth(k) = depsth(k) - coef
        depsmo = depsmo + depsth(k)
111  end do
    depsmo = -depsmo
    do 115 k = 1, ndimsi
        depsdv(k) = depsth(k) + depsmo/3.d0 * kron(k)
115  end do
!
!     -- 4 CALCUL DE SIGMMO, SIGMDV, SIGEL,SIMOEL,SIELEQ, SIEQM :
!     -------------------------------------------------------------
    sigmmo = 0.d0
    do 116 k = 1, 3
        sigmmo = sigmmo + sigm(k)
116  end do
    sigmmo = -sigmmo /3.d0
    if (sigmmo .lt. ptrac) then
        call u2mess('F', 'ALGORITH6_64')
    endif
    sieleq = 0.d0
    sieqm = 0.d0
    do 117 k = 1, ndimsi
        sigmdv(k) = sigm(k) + sigmmo * kron(k)
        sieqm = sieqm + sigmdv(k)**2
        sigel(k) = sigmdv(k) + deuxmu * depsdv(k)
        sieleq = sieleq + sigel(k)**2
117  end do
    sieleq = sqrt(1.5d0*sieleq)
    sieqm = sqrt(1.5d0*sieqm)
!
    if (((xk0*depsmo) .gt. epxmax)) then
!
        call tecael(iadzi, iazk24)
        valkm(1) = zk24(iazk24-1+3) (1:8)
        valrm(1) = epxmax
        valkm(2) ='EXP(XK0*DEPSMO)'
        valrm(2) = (xk0*depsmo)
        valkm(3) ='EXP(XK*DEPSMO)'
        valrm(3) = (xk*depsmo)
        call u2mesg('A', 'COMPOR1_41', 3, valkm, 0,&
                    valim, 3, valrm)
        retcom = 1
        goto 30
    endif
    simoel = sigmmo*exp(xk0*depsmo)+kcam/xk0*(exp(xk0*depsmo)-un)
! ---- INITIALISATION A T=0
    if (pcrm(1) .eq. 0.d0) then
!
        pcrm(1) = prescr
        pcrm(3) = simoel
        pcrm(4) = sieleq
        pcrm(5) = 0.d0
        pcrm(6) = 0.d0
        pcrm(7) = e0
!
! ---- ON VERIFIE LA COHERENCE DES DONNEES MECA DE DEPART
        nu = (&
             trois*((un+e0)*sigmmo+kapa*kcam)-deuxmu*kapa)/ (six*(( un+e0)*sigmmo+kapa*kcam&
             )+deuxmu*kapa&
             )
!
        young = deuxmu*(un+nu)
!
        if ((young.le.zero) .or. (nu.le.zero) .or. (nu.gt.unsde)) then
            call tecael(iadzi, iazk24)
            nomail = zk24(iazk24-1+3) (1:8)
            call u2mesk('F', 'COMPOR1_3', 1, nomail)
        endif
!
    endif
!
!
!     -- 5 CALCUL DU CRITERE :
!     ----------------------
    fonc = sieleq**2+m*m*(simoel-ptrac)**2- 2.d0*m*m*(simoel-ptrac)*pcrm(1)
!
!     -- 6  TEST DE PLASTIFICATION ET CALCUL DE PCRP SIGP, SIGPDV :
!     ------------------------------------------------------------
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        if (fonc .le. 0.d0) then
!      -- TRAITEMENT DE L'ELASTICITE
            pcrp(1) = pcrm(1)
            pcrp(2) = 0.d0
            do 118 k = 1, ndimsi
                sigpdv(k) = sigel(k)
                sigp(k) = sigel(k)-simoel*kron(k)
118          end do
!
            pcrp(3) = simoel
            pcrp(4) = sieleq
            pcrp(5) = 0.d0
            pcrp(6) = 0.d0
!
            if ((pcrm(3).ne.zero) .and. ((pcrp(3)/pcrm(3)).gt.zero)) then
!
                pcrp(7) = pcrm(7) - kapa*log(pcrp(3)/pcrm(3))
!
            else
!
                pcrp(7) = pcrm(7)
!
            endif
!
        else
!     -- PLASTIFICATION : CALCUL DE LA DEFORMATION
!     -- VOLUMIQUE PLASTIQUE : DEPPMO
            pcrp(2) = 1.d0
            seuil = m**2*(pcrm(1)-ptrac)**2
!
            xinf = 0.d0
!     -- RECHERCHE DE LA BORNE SUP
!
            if (abs((simoel - pcrm(1) - ptrac)/seuil) .lt. ptit) then
                v0=0.d0
                goto 100
            endif
!
            if (abs(xk0*simoel + kcam + xk*pcrm(1)) .lt. ptit) then
!
                if (( -deux*( simoel-pcrm(1)-ptrac)/ (xk0*simoel+kcam- xk*ptrac) ) .lt.&
                    zero) then
!
                    xsup = 1.d0/(xk+xk0)*log(abs(simoel-ptrac)/pcrm(1) )
!
                else
!       RESULTAT D UN DEVELOPPEMENT LIMITE D ORDRE 2
!
                    if ((simoel-ptrac) .gt. pcrm(1)) then
!
                        xsup = sqrt(&
                               ( -deux*( simoel-pcrm(1)-ptrac)/ (xk0*simoel+kcam-xk*ptrac ) ))
!
                    else
!
                        xsup = -sqrt(&
                               ( -deux*( simoel-pcrm(1)-ptrac)/ (xk0*simoel+kcam-xk*ptrac ) ))
                    endif
!
                endif
            else
!       RESULTAT D UN DEVELOPPEMENT LIMITE D ORDRE 1
                xsup = ( simoel - pcrm(1) - ptrac)/ (xk0*simoel + kcam + xk*pcrm(1) )
!
            endif
!
!     --RESOLUTION AVEC LA METHODE DE NEWTON ENTRE LES BORNES
            v0 = xinf
!
!
            if (((-xk0*v0) .gt. epxmax) .or. ((xk*v0) .gt. epxmax)) then
!
                call tecael(iadzi, iazk24)
                valkm(1) = zk24(iazk24-1+3) (1:8)
                valrm(1) = epxmax
                valkm(2) ='EXP(-XK0*V0)'
                valrm(2) = (-xk0*v0)
                valkm(3) ='EXP(XK*V0)'
                valrm(3) = (xk*v0)
                call u2mesg('A', 'COMPOR1_41', 3, valkm, 0,&
                            valim, 3, valrm)
                retcom = 1
                goto 30
            endif
!
            f1 = (simoel+kcam/xk0)*exp(-xk0*v0)-kcam/xk0-ptrac
            f2 = (simoel+kcam/xk0)*exp(-xk0*v0)-kcam/xk0-ptrac -2.d0*pcrm(1)*exp(xk*v0)
            f3 = (simoel+kcam/xk0)*exp(-xk0*v0)-kcam/xk0-ptrac -pcrm(1)*exp(xk*v0)
            f4 = (1.d0+3.d0*deuxmu*v0/2.d0/m/m/f3)
!
            f =sieleq**2+m**2*f4**2*f1*f2
!
!
            f1p =-(xk0*simoel+kcam)*exp(-xk0*v0)
            f2p =-(xk0*simoel+kcam)*exp(-xk0*v0) -2.d0*xk*pcrm(1)*exp(&
            xk*v0)
            f3p =-(xk0*simoel+kcam)*exp(-xk0*v0) -xk*pcrm(1)*exp(xk*&
            v0)
            f4p = 3.d0*deuxmu/2.d0/(m**2)*(f3-v0*f3p)/f3/f3
!
            fp = m**2*f4**2*(f1p*f2 + f1*f2p) + 2.d0*m**2*f4*f4p*f1* f2
!
!
            do 200 iter = 1, nint(crit(1))
!
!     --CRITERE DE CONVERGENCE
                if ((abs(f)/seuil) .le. crit(3)) goto 100
!
!     --CONSTRUCTION DU NOUVEL ITERE
                v0 = v0-f/fp
                if (xsup .gt. 0.d0) then
                    if (v0 .le. xinf .or. v0 .ge. xsup) v0 = (xinf+xsup)/ 2
                else
                    if (v0 .le. xsup .or. v0 .ge. xinf) v0 = (xinf+xsup)/ 2
                endif
!
!     --CALCUL DE LA FONCTION EN V0 ET DE SA DERIVEE
!
                if (((-xk0*v0) .gt. epxmax) .or. ((xk*v0) .gt. epxmax)) then
!
                    call tecael(iadzi, iazk24)
                    valkm(1) = zk24(iazk24-1+3) (1:8)
                    valrm(1) = epxmax
                    valkm(2) ='EXP(-XK0*V0)'
                    valrm(2) = (-xk0*v0)
                    valkm(3) ='EXP(XK*V0)'
                    valrm(3) = (xk*v0)
                    call u2mesg('A', 'COMPOR1_41', 3, valkm, 0,&
                                valim, 3, valrm)
                    retcom = 1
                    goto 30
                endif
                f1 = (simoel+kcam/xk0)*exp(-xk0*v0)-kcam/xk0-ptrac
                f2 = (simoel+kcam/xk0)*exp(-xk0*v0)-kcam/xk0-ptrac -2.d0*pcrm(1)*exp(xk*v0)
                f3 = (simoel+kcam/xk0)*exp(-xk0*v0)-kcam/xk0-ptrac -pcrm(1)*exp(xk*v0)
                f4 = (1.d0+3.d0*deuxmu*v0/2.d0/m/m/f3)
!
                f =sieleq**2+m**2*f4**2*f1*f2
!
                if (f .gt. zero) signf = 1
                if (f .lt. zero) signf = -1
!
                f1p =-(xk0*simoel+kcam)*exp(-xk0*v0)
                f2p =-(xk0*simoel+kcam)*exp(-xk0*v0) -2.d0*xk*pcrm(1)*&
                exp(xk*v0)
                f3p =-(xk0*simoel+kcam)*exp(-xk0*v0) -xk*pcrm(1)*exp(&
                xk*v0)
                f4p = 3.d0*deuxmu/2.d0/(m**2)*(f3-v0*f3p)/f3/f3
!
                fp = m**2*f4**2*(f1p*f2 + f1*f2p) + 2.d0*m**2*f4*f4p* f1*f2
!
!
                if (((-xk0*xinf) .gt. epxmax) .or. ((xk*xinf) .gt. epxmax)) then
!
                    call tecael(iadzi, iazk24)
                    valkm(1) = zk24(iazk24-1+3) (1:8)
                    valrm(1) = epxmax
                    valkm(2) ='EXP(-XK0*XINF)'
                    valrm(2) = (-xk0*xinf)
                    valkm(3) ='EXP(XK*XINF)'
                    valrm(3) = (xk*xinf)
                    call u2mesg('A', 'COMPOR1_41', 3, valkm, 0,&
                                valim, 3, valrm)
                    retcom = 1
                    goto 30
                endif
!
                fxi1 = (simoel+kcam/xk0)*exp(-xk0*xinf)-kcam/xk0- ptrac
                fxi2 = (simoel+kcam/xk0)*exp(-xk0*xinf)-kcam/xk0- ptrac -2.d0*pcrm(1)*exp(xk*xinf&
                       &)
                fxi3 = (simoel+kcam/xk0)*exp(-xk0*xinf)-kcam/xk0- ptrac -pcrm(1)*exp(xk*xinf)
                fxi4 = (1.d0+3.d0*deuxmu*xinf/2.d0/m/m/fxi3)
!
                fxi=sieleq**2+m**2*fxi4**2*fxi1*fxi2
!
                if (fxi .gt. zero) signfi = 1
                if (fxi .lt. zero) signfi = -1
!
                if ((signf*signfi) .lt. zero) xsup = v0
                if ((signf*signfi) .gt. zero) xinf = v0
!
200          end do
            retcom = 1
            goto 30
100          continue
            deppmo=v0
!
!     -- REACTUALISATION DE LA VARIABLE INTERNE
            if (((xk*deppmo) .gt. epxmax) .or. (xk0*(depsmo-deppmo) .gt. epxmax)) then
!
                call tecael(iadzi, iazk24)
                valkm(1) = zk24(iazk24-1+3) (1:8)
                valrm(1) = epxmax
                valkm(2) ='EXP(XK*DEPPMO)'
                valrm(2) = (xk*deppmo)
                valkm(3) ='EXP(XK0*(DEPSMO-DEPPMO))'
                valrm(3) = (xk0*(depsmo-deppmo))
                call u2mesg('A', 'COMPOR1_41', 3, valkm, 0,&
                            valim, 3, valrm)
                retcom = 1
                goto 30
            endif
!
!
            pcrp(1) = pcrm(1)*exp(xk*deppmo)
!     -- REACTUALISATION DES CONTRAINTES
            sigpmo = (sigmmo+kcam/xk0)*exp(xk0*(depsmo-deppmo))-kcam/ xk0
            call r8inir(6, 0.d0, sigpdv, 1)
            do 119 k = 1, ndimsi
                sigpdv(k) = sigel(k)/(1.d0+(3.d0*deuxmu/2.d0*deppmo)/ (m*m*(sigpmo-pcrp(1)-ptrac)&
                            &))
                sigp(k) = sigpdv(k)-sigpmo*kron(k)
119          end do
!
!
! ---- V(3) CONTRAINTE VOLUMIQUE
            pcrp(3) = sigpmo
!
! ---- V(4) CONTRAINTE EQUIVALENTE
            sieqp = 0.0d0
            do 440 k = 1, ndimsi
                sieqp = sieqp + sigpdv(k)**2.d0
440          continue
            pcrp(4) = sqrt(1.5d0*sieqp)
!
! ---- V(5) DEFORMATION PLASTIQUE VOLUMIQUE
            pcrp(5) = pcrm(5) + deppmo
!
! ---- V(6) DEFORMATION PLASTIQUE EQUIVALENTE
            depseq = 0.0d0
            do 450 k = 1, ndimsi
                depseq = depseq + depsdv(k)*depsdv(k)
450          continue
            depseq = sqrt(2.d0/3.d0*depseq)
            pcrp(6) = pcrm(6) + depseq
!
! ---- V(7) :: INDICE DES VIDES
            if ((pcrm(3).ne.zero) .and. ((pcrp(3)/pcrm(3)).gt.zero) .and.&
                ((pcrp(1)/pcrm(1)).gt.zero) .and. (pcrm(1).ne.zero)) then
!
                pcrp(7) = pcrm(7) - kapa * log(pcrp(3)/pcrm(3)) - (lambda-kapa) * log(pcrp(1)/pcr&
                          &m(1))
!
            else
!
                pcrp(7) = pcrm(7)
!
            endif
!
        endif
!
    endif
!
!
!     -- 7 CALCUL DE L'OPERATEUR TANGENT :
!     --------------------------------
    if (option(1:14) .eq. 'RIGI_MECA_TANG' .or. option(1:9) .eq. 'FULL_MECA') then
!
        if (option(1:14) .eq. 'RIGI_MECA_TANG') then
            if (pcrm(2) .eq. 0.d0) then
                matr = 0
            else
                matr = 1
            endif
        endif
        if (option(1:9) .eq. 'FULL_MECA') then
            if (pcrp(2) .eq. 1.d0) then
                matr = 2
            else
                matr = 0
            endif
        endif
!      INITIALISATION DE L'OPERATEUR TANGENT
!     ---------------------------------------
        call matini(6, 6, 0.d0, dsidep)
!
!     -- 7.1 CALCUL DE DSIDEP(6,6)-ELASTIQUE:
!     ---------------------------------------
        if (matr .eq. 0) then
            do 127 k = 1, 3
                do 128 l = 1, 3
                    dsidep(k,l) = xk0*simoel+kcam-deuxmu/3.d0
128              end do
127          end do
            do 129 k = 1, ndimsi
                dsidep(k,k) = dsidep(k,k)+deuxmu
129          end do
        endif
!
!     -- 7.2 CALCUL DE DSIDEP(6,6)-EN VITESSE :
!     ---------------------------------------
        if (matr .eq. 1) then
!
            call r8inir(6*6, 0.d0, dsidep, 1)
!     -- 7.2.1 CALCUL DU MODULE ELASTOPLASTIQUE H
!
            valm = 0.d0
            do 158 i = 1, ndimsi
                valm = valm + sigmdv(i)**2
158          end do
!
            h = 4.d0*m**4*(sigmmo-ptrac)*(sigmmo-ptrac-pcrm(1))* (xk0*(sigmmo-ptrac-pcrm(1))+xk*p&
                &crm(1))+deuxmu*9.d0*valm
!
!
!     -- 7.2.2 CALCUL D'UN TERME INTERMEDIAIRE
            do 160 k = 1, 3
                a(k) = 0.d0
160          end do
            do 130 k = 1, 3
                a(k)=-deux*xk0*m*m*(sigmmo-ptrac)*(sigmmo-ptrac-pcrm(&
                1)) *kron(k)+ 3.d0*deuxmu*sigmdv(k)
130          end do
            call r8inir(3, 0.d0, aa, 1)
            do 131 k = 4, ndimsi
                aa(k) = 3.d0*deuxmu*sigmdv(k)
131          end do
!
!     -- 7.2.3 CALCUL DES TERMES DE DSIDEP
            call r8inir(ndimsi*ndimsi, 0.d0, dsidep, 1)
            do 132 k = 1, 3
                do 133 l = 1, 3
                    dsidep(k,l)=xk0*(sigmmo-ptrac)-deuxmu/3.d0-a(k)*a(&
                    l)/h
133              end do
132          end do
            do 134 k = 1, 3
                do 135 l = 4, ndimsi
                    dsidep(k,l) = -a(k)*aa(l)
                    dsidep(k,l) = dsidep(k,l)/h
                    dsidep(l,k) = dsidep(k,l)
135              end do
134          end do
            do 136 k = 4, ndimsi
                do 137 l = 4, ndimsi
                    dsidep(k,l) = -aa(k)*aa(l)
                    dsidep(k,l) = dsidep(k,l)/h
137              end do
136          end do
            do 138 k = 1, ndimsi
                dsidep(k,k) = deuxmu + dsidep(k,k)
138          end do
!
        endif
!
        if (matr .eq. 2) then
            call r8inir(6*6, 0.d0, dsidep, 1)
!
!     -- 7.2.1 CALCUL DU MODULE ELASTOPLASTIQUE H
!
            valp = 0.d0
            do 159 i = 1, ndimsi
                valp = valp + sigpdv(i)**2
159          end do
!
            h = 4.d0*m**4*(sigpmo-ptrac)*(sigpmo-ptrac-pcrp(1))* (xk0*(sigpmo-ptrac-pcrp(1))+xk*p&
                &crp(1))+deuxmu*9.d0*valp
!
!     -- 7.2.2 CALCUL D'UN TERME INTERMEDIAIRE
            call r8inir(3, 0.d0, a, 1)
            call r8inir(3, 0.d0, aa, 1)
!
            do 4130 k = 1, 3
                a(k) = -deux*xk0*m*m*(sigpmo-ptrac)*(sigpmo-ptrac- pcrp(1)) *kron(k)+ 3.d0*deuxmu&
                       &*sigpdv(k)
4130          end do
!
            do 4131 k = 4, ndimsi
                aa(k) = 3.d0*deuxmu*sigpdv(k)
4131          end do
!
!     -- 7.2.3 CALCUL DES TERMES DE DSIDEP
            call r8inir(ndimsi*ndimsi, 0.d0, dsidep, 1)
            do 4132 k = 1, 3
                do 4133 l = 1, 3
                    dsidep(k,l)=xk0*(sigpmo-ptrac)-deuxmu/3.d0-a(k)*a(&
                    l)/h
4133              end do
4132          end do
            do 4134 k = 1, 3
                do 4135 l = 4, ndimsi
                    dsidep(k,l) = -a(k)*aa(l)
                    dsidep(k,l) = dsidep(k,l)/h
                    dsidep(l,k) = dsidep(k,l)
4135              end do
4134          end do
            do 4136 k = 4, ndimsi
                do 4137 l = 4, ndimsi
                    dsidep(k,l) = -aa(k)*aa(l)
                    dsidep(k,l) = dsidep(k,l)/h
4137              end do
4136          end do
            do 4138 k = 1, ndimsi
                dsidep(k,k) = deuxmu + dsidep(k,k)
4138          end do
!
        endif
!     -- 7.3 CALCUL DE DSIDEP(6,6)-MATRICE COHERENTE :
!     ----------------------------------------------
        if (matr .eq. 3) then
            sieqp = 0.0d0
            do 300 k = 1, ndimsi
                sieqp = sieqp + sigpdv(k)**2
300          end do
            sieqp = sqrt(1.5d0*sieqp)
            diff2 = abs((pcrp(1)-sigpmo)/pcrp(1))
            if (diff2 .lt. crit(3)) then
!
!     -- 7.3.1 OPERATEUR TANGENT COHERENT AU POINT CRITIQUE
!     -- TRAITEMENT DE LA PARTIE DEVIATORIQUE
!     -- CALCUL DE Q+
!     -- CALCUL DU TENSEUR HH QUI MULTIMPLIE LA DEFORMATION
                call r8inir(6*6, 0.d0, ses, 1)
                do 1000 k = 1, ndimsi
                    do 1001 l = 1, ndimsi
                        ses(k,l) = 1.d0/2.d0*( sigpdv(k)*sigel(l)+ sigel(k)*sigpdv(l))
1001                  end do
1000              end do
                call r8inir(6*6, 0.d0, hh, 1)
                do 301 k = 1, ndimsi
                    do 302 l = 1, ndimsi
                        hh(k,l) = -deuxmu*3.d0*ses(k,l)/2.d0/sieleq/ sieqp
302                  end do
301              end do
                do 303 k = 1, ndimsi
                    hh(k,k) = deuxmu+hh(k,k)
303              end do
                if (ndim .eq. 2) then
                    hh(5,5) = 1.d0
                    hh(6,6) = 1.d0
                endif
!     -- INVERSE DE HH
                call r8inir(6*6, 0.d0, hhm, 1)
                do 304 k = 1, 6
                    hhm(k,k)=1.d0
304              end do
                call mgauss('NFWP', hh, hhm, 6, 6,&
                            6, rbid, iret)
!
!     -- CALCUL DU TENSEUR GG QUI MULTIMPLIE LA CONTRAINTE
                call r8inir(6*6, 0.d0, gg, 1)
                call r8inir(6*6, 0.d0, sps, 1)
                do 1002 k = 1, ndimsi
                    do 1003 l = 1, ndimsi
                        sps(k,l) = sigpdv(k)*sigpdv(l)
1003                  end do
1002              end do
                do 305 k = 1, ndimsi
                    do 306 l = 1, ndimsi
                        gg(k,l) = -3.d0*sieleq*sps(k,l)/2.d0/sieqp**3
306                  end do
305              end do
                do 307 k = 1, ndimsi
                    gg(k,k) = sieleq/sieqp + gg(k,k)
307              end do
!     --  MATRICE DE PROJECTION SUR L'ESPACE DES CONTRAINTES
!     -- DEVIATORIQUES
                call r8inir(6*6, 0.d0, v, 1)
                do 315 k = 1, 3
                    do 316 l = 1, 3
                        v(k,l) = -1.d0/3.d0
                        v(l,k) = v(k,l)
316                  end do
315              end do
                do 317 k = 1, ndimsi
                    v(k,k) = v(k,k) + 1.d0
317              end do
!     --  PRODUIT DE LA MATRICE DE PROJECTION SUR L'ESPACE
!     --  DES CONTRAINTES DEVIATORIQUES PAR GG
                call r8inir(6*6, 0.d0, d1g, 1)
                call promat(v, 6, ndimsi, ndimsi, gg,&
                            6, ndimsi, ndimsi, d1g)
!     -- PRODUIT DU RESULTAT PAR L'INVERSE DE HH
                call r8inir(6*6, 0.d0, d1ghhm, 1)
                call promat(d1g, 6, ndimsi, ndimsi, hhm,&
                            6, ndimsi, ndimsi, d1ghhm)
!
!     -- 7.3.2 TRAITEMENT DE LA PARTIE HYDROSTATIQUE
!     --  PRODUIT DE LA MATRICE DE PROJECTION SUR L'ESPACE
!     --  DES CONTRAINTES DEVIATORIQUES PAR LA MATRICE IDENTITE
!     --  D'ORDRE 2
                call r8inir(6*6, 0.d0, id2, 1)
                do 308 k = 1, 3
                    do 309 l = 1, 3
                        id2(k,l) = -1.d0/3.d0/xk0/sigpmo
309                  end do
308              end do
!     -- SOMME DES TERMES DEVIATORIQUE ET HYDROSTATIQUE
                call r8inir(6*6, 0.d0, devhyd, 1)
                do 310 k = 1, ndimsi
                    do 311 l = 1, ndimsi
                        devhyd(k,l) = d1ghhm(k,l)/deuxmu + id2(k,l)
311                  end do
310              end do
                if (ndim .eq. 2) then
                    devhyd(5,5) = 1.d0
                    devhyd(6,6) = 1.d0
                endif
!     -- INVERSE DE LA SOMME DES TERMES DEVIATORIQUE ET HYDROSTATIQUE
                call r8inir(6*6, 0.d0, devhym, 1)
                do 312 k = 1, 6
                    devhym(k,k)=1.d0
312              end do
                call mgauss('NFWP', devhyd, devhym, 6, 6,&
                            6, rbid, iret)
!     -- TERMES DE L'OPERATEUR TANGENT
                call r8inir(6*6, 0.d0, dsidep, 1)
                do 313 k = 1, 6
                    do 314 l = 1, 6
                        dsidep(k,l) = devhym(k,l)
314                  end do
313              end do
            else
!
!      ---7.4 OPERATEUR TANGENT COHERENT CAS GENERAL
!      -- CALCUL DES INCREMENTS DE P ET DE S
                deltap = sigpmo - sigmmo
                call r8inir(6, 0.d0, deltas, 1)
                do 140 k = 1, ndimsi
                    deltas(k)=sigpdv(k)-sigmdv(k)
140              end do
!
!     --  CALCUL DE VECTEURS INTERMEDIAIRES
                spards = 0.d0
                do 141 k = 1, ndimsi
                    spards = spards+deltas(k)*sigpdv(k)
141              end do
                call r8inir(6, 0.d0, tplus, 1)
                do 142 k = 1, ndimsi
                    tplus(k) = sigpdv(k) + deltas(k)
142              end do
!
!      -- 7.4.1 TERMES NECESSAIRES A LA PARTIE DEVIATORIQUE
                hp = 4.d0*m**4*xk*sigpmo*pcrp(1)*(sigpmo-pcrp(1))
!
                xc = 9.d0*spards/hp
                xd = 6.d0*m*m*(sigpmo-pcrp(1))*deltap/hp
                xv = 3.d0*spards + 2.d0*m**2*(sigpmo-pcrp(1))*deltap
                xlam = xv/hp
                xa = (&
                     4.d0*xlam*xk*m**4*sigpmo*(&
                     sigpmo-2.d0*pcrp(1))+ 2.d0*m**2*deltap)*m**2*(sigpmo-pcrp(1))/(m**2*xlam+ (1&
                     &.d0/2.d0/xk/pcrp(1)&
                     )&
                     )
                xi = 2.d0*m**2*(&
                     sigpmo-pcrp(1))-2.d0*m**4*xlam* (sigpmo-pcrp(1))/((1.d0/2.d0/xk/pcrp(1))+m**&
                     &2*xlam&
                     )
                rap = xi/(hp+xa)
!
!     -- CALCUL DE LA MATRICE CC-SYMETRISATION DE TPLUS.I
!
                call r8inir(6*6, 0.d0, cc, 1)
                do 172 k = 1, ndimsi
                    do 173 l = 1, ndimsi
                        cc(k,l)=(tplus(k)*kron(l)+kron(k)*tplus(l))/&
                        2.d0
173                  end do
172              end do
!          DO 172 K=1,3
!          DO 173 L=1,3
!              CC(K,L)=(TPLUS(K)+TPLUS(L))/2.D0
! 173  CONTINUE
! 172  CONTINUE
!          DO 174 K=1,3
!          DO 175 L=4,NDIMSI
!              CC(K,L)=TPLUS(L)/2.D0
!              CC(L,K)=CC(K,L)
! 175  CONTINUE
! 174  CONTINUE
!
!     -- CALCUL DES TERMES D'UNE MATRICE INTERMEDIAIRE C
!
                call r8inir(6*6, 0.d0, c, 1)
                do 170 k = 1, ndimsi
                    do 171 l = 1, ndimsi
                        c(k,l) = 9.d0/2.d0/(hp+xa)*(sigpdv(k)*tplus(l) + tplus(k)*sigpdv(l))
171                  end do
170              end do
                do 149 k = 1, ndimsi
                    c(k,k) = c(k,k)+1.d0/deuxmu+xc+xd
149              end do
!
!     -- ASSEMBLAGE DES TERMES POUR LA PARTIE DEVIATORIQUE
                call r8inir(6*6, 0.d0, ee, 1)
                do 180 k = 1, ndimsi
                    do 181 l = 1, ndimsi
                        ee(k,l) = c(k,l) - rap*cc(k,l)
181                  continue
180              continue
!
!      -- TERMES NECESSAIRES A LA PARTIE HYDROSTATIQUE
                xu = 2.d0*m**2*xk*pcrp(1)
                xg = xlam*xu/(1.d0+xlam*xu)
                xh = xu*(sigpmo-pcrp(1))/(1.d0+xlam*xu)
                xe = 1.d0+xh*2.d0*m**2*deltap/hp+xh*4.d0*xk*m**4* sigpmo*(sigpmo-2.d0*pcrp(1)&
                     )*xv/hp/hp
                xf = (&
                     2.d0*m**2*(&
                     sigpmo-pcrp(1))+2.d0*m**2*deltap- xg*2.d0*m**2*deltap)/hp-4.d0*xk*m**4*xv/hp&
                     &/hp* ((2.d0*sigpmo-pcrp(1))*pcrp(1)+xg*sigpmo* (sigpmo- 2.d0*pcrp(1)&
                     )&
                     )
                ct = (&
                     1.d0+2.d0*m**2*xk0*sigpmo*(&
                     xlam-xg*xlam- xlam*xf*xh/xe+xf/xe*(sigpmo-pcrp(1))))/(xk0*sigpmo&
                     )
!
!     --  VECTEUR INTERMEDIAIRE
                call r8inir(6, 0.d0, fv, 1)
                do 190 k = 1, ndimsi
                    fv(k)=3.d0*xf/xe*sigpdv(k)-ct*kron(k)/3.d0
190              end do
!     -- SYMMETRISATION DEFV ET SA PROJECTION SUR L'ESPACE
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
!     --  MATRICE DE PROJECTION SUR L'ESPACE DES CONTRAINTES
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
!     -- PROJECTION DE EE SUR L'ESPACE DES CONTRAINTES
!     -- DEVIATORIQUES
                call r8inir(6*6, 0.d0, s, 1)
                call promat(ee, 6, ndimsi, ndimsi, v,&
                            6, ndimsi, ndimsi, s)
!
!C
!     -- COMBINAISON DES DEUX PARTIES DEVIATORIQUE ET
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
!     -- INVERSE DE LA MATRICE T
                call r8inir(6*6, 0.d0, vv, 1)
                do 108 k = 1, 6
                    vv(k,k)=1.d0
108              end do
                call mgauss('NFWP', t, vv, 6, 6,&
                            6, rbid, iret)
!     --  7.3.3 CALCUL DES TERMES DSIDEP L'OPERATEUR TANGENT
                call r8inir(6*6, 0.d0, dsidep, 1)
                do 106 k = 1, 6
                    do 107 l = 1, 6
                        dsidep(k,l) = vv(k,l)
107                  end do
106              end do
!
            endif
        endif
    endif
! ======================================================================
30  continue
! =====================================================================
end subroutine
