subroutine nmvpir(fami, kpg, ksp, ndim, typmod,&
                  imate, compor, crit, instam, instap,&
                  deps, sigm, vim, option, angmas,&
                  nvi, sigp, vip, dsidep, iret)
! ----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! aslint: disable=
    implicit none
#include "asterc/r8t0.h"
#include "asterfort/ggplem.h"
#include "asterfort/granac.h"
#include "asterfort/iunifi.h"
#include "asterfort/lcdevi.h"
#include "asterfort/lcnrts.h"
#include "asterfort/matini.h"
#include "asterfort/nmasse.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utmess.h"
#include "asterfort/verift.h"
#include "asterfort/vpalem.h"
#include "asterfort/zerofr.h"
    integer :: ndim, imate, kpg, ksp, iret, nvi
    character(len=8) :: typmod(*)
    character(len=16) :: compor(*), option
    real(kind=8) :: crit(4), instam, instap, irram, irrap
    real(kind=8) :: deps(6), angmas(3)
    real(kind=8) :: sigm(6), vim(nvi), sigp(6), vip(nvi), dsidep(6, 6)
! ----------------------------------------------------------------------
!     REALISE LES LOIS DE VISCOPLASTICITE SOUS IRRADIATION
!     POUR LES ELEMENTS
!     ISOPARAMETRIQUES EN PETITES DEFORMATIONS
!
! 1/ LEMAITRE MODIFIEE
! 2/ VISC_IRRA_LOG (PROJET PACHYDERME - 2004)
! 3/ LEMA_SEUIL
!
!
! IN  KPG  : NUMERO DU POINT DE GAUSS
! IN  KSP  : NUMERO DU SOUS-POINT DE GAUSS  (OU 1)
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  IMATE   : ADRESSE DU MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT : RELCOM ET DEFORM
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  INSTAM  : INSTANT DU CALCUL PRECEDENT
! IN  INSTAP  : INSTANT DU CALCUL
! IN  TM      : TEMPERATURE A L'INSTANT PRECEDENT
! IN  TP      : TEMPERATURE A L'INSTANT DU CALCUL
! IN  TREF    : TEMPERATURE DE REFERENCE
! IN  DEPS    : INCREMENT DE DEFORMATION
! IN  SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
! IN  VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
! IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
! IN  ANGMAS  : LES TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
! OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL
! OUT VIP     : VARIABLES INTERNES A L'INSTANT ACTUEL
! OUT DSIDEP  : MATRICE CARREE
! OUT IRET    : CODE RETOUR DE LA RECHERCHE DE ZERO DE F(X)=0
!                   IRET=0 => PAS DE PROBLEME
!                   IRET=1 => ECHEC
!
!               ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
!               L'ORDRE :  XX YY ZZ XY XZ YZ
!
!     COMMON POUR LES PARAMETRES DES LOIS VISCOPLASTIQUES
    common / nmpavp / dpc,sieleq,deuxmu,deltat,tschem,prec,theta,niter
    real(kind=8) :: dpc, sieleq, deuxmu, deltat, tschem, prec, theta, niter
!     COMMON POUR LES PARAMETRES DES LOIS DE FLUAGE SOUS IRRADIATION
!     VISC_IRRA_LOG: FLUPHI A      B      CTPS    ENER
! -------------------------------------------------------------
    common / nmpair / fluphi,&
     &                  a,b,ctps,ener
    real(kind=8) :: fluphi, tm, tp
    real(kind=8) :: a, b, ctps, ener
!     COMMON POUR LES PARAMETRES DE LA LOI DE LEMAITRE (NON IRRADIEE)
    common / nmpale / unsurk,unsurm,valden
    real(kind=8) :: unsurk, unsurm, valden
! PARAMETRES MATERIAUX
! ELASTIQUES
    real(kind=8) :: ep, nup, troikp, deumup
    real(kind=8) :: em, num, troikm, deumum
! AUTRES
    integer :: nbclem, nbcvil, nbcint
    parameter (nbclem=7, nbcvil=5, nbcint=2)
    real(kind=8) :: coelem(nbclem), coevil(nbcvil)
    real(kind=8) :: coeint(nbcint)
    character(len=16) :: nomlem(nbclem), nomvil(nbcvil)
    character(len=16) :: nomint(nbcint)
    integer :: codvil(nbcvil), codlem(nbclem), codint(nbcint)
    character(len=*) :: fami
!
    real(kind=8) :: t1, t2, defam(6), defap(6)
    integer :: iulmes,  iret2, iret3, ibid
    real(kind=8) :: rac2, tabs
    integer :: k, l
    integer :: ndimsi
    real(kind=8) :: alpha, beta, caa, saa, cba, sba
    integer :: ndt, ndi
    common /tdim/     ndt , ndi
    real(kind=8) :: depsgr, dp1, dev(6)
    real(kind=8) :: degran(6)
    real(kind=8) :: depsth(6), epsthe
    real(kind=8) :: depsdv(6), sigdv(6), sigel(6), epsmo, sigmo
    real(kind=8) :: sieqm, sieqp, d
    real(kind=8) :: xnumer
    real(kind=8) :: sigmp(6), deltkl, deltp2
    real(kind=8) :: a0, xap, x, fg, fdgdst, fdgdev
    real(kind=8) :: coef1, deltev, coef2
    real(kind=8) :: kron(6)
    character(len=6) :: epsa(6)
    data              kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
    data nomlem / 'N', 'UN_SUR_K', 'UN_SUR_M', 'QSR_K',&
     &              'BETA','PHI_ZERO','L'/
    data nomvil / 'A', 'B', 'CSTE_TPS', 'ENER_ACT', 'FLUX_PHI'/
    data nomint / 'A',         'S'/
    data epsa   / 'EPSAXX','EPSAYY','EPSAZZ','EPSAXY','EPSAXZ',&
     &              'EPSAYZ'/
! DEB ------------------------------------------------------------------
!
    call verift(fami, kpg, ksp, 'T', imate,&
                iret = iret3, epsth=epsthe, temp_prev_out = tm, temp_curr_out = tp)
    theta = crit(4)
! TEMPERATURE AU MILIEU DU PAS DE TEMPS
    if (iret3 .eq. 0) then
        tschem = tm*(1.d0-theta)+tp*theta
    else
        tschem = 0.d0
    endif
!
    t1 = abs(theta-0.5d0)
    t2 = abs(theta-1.d0)
    prec = 0.01d0
    if ((t1.gt.prec) .and. (t2.gt.prec)) then
        call utmess('F', 'ALGORITH6_55')
    endif
    if (compor(1)(5:10) .eq. '_IRRA_') theta=1.d0
!
    if (typmod(1) .eq. 'C_PLAN') then
        iulmes = iunifi('MESSAGE')
        call utmess('F', 'ALGORITH6_92')
        goto 299
    endif
    tabs = r8t0()
    rac2 = sqrt(2.d0)
!
! DEFORMATIONS DE GRANDISSEMENT
    call r8inir(6, 0.d0, degran, 1)
! DEFORMATION PLASTIQUE CUMULEE
    dpc = vim(1)
! INCREMENT DE TEMPS
    deltat = instap - instam
    if (deltat .eq. 0.d0) then
        call utmess('F', 'ALGORITH8_87')
    endif
!
    call matini(6, 6, 0.d0, dsidep)
!
    if (ndim .eq. 2) then
        ndimsi=4
    else
        ndimsi=6
    endif
    if (ndim .eq. 3) then
        ndt = 6
        ndi = 3
    else
        ndt = 4
        ndi = 3
    endif
!
! VARIABLE DE COMMANDE D IRRADIATION ET ANELASTIQUE
    call rcvarc(' ', 'IRRA', '-', fami, kpg,&
                ksp, irram, iret2)
    if (iret2 .gt. 0) irram=0.d0
    call rcvarc(' ', 'IRRA', '+', fami, kpg,&
                ksp, irrap, iret2)
    if (iret2 .gt. 0) irrap=0.d0
!
    do k = 1, ndimsi
        call rcvarc(' ', epsa(k), '-', fami, kpg,&
                    ksp, defam(k), iret2)
        if (iret2 .eq. 1) defam(k)=0.d0
!
        call rcvarc(' ', epsa(k), '+', fami, kpg,&
                    ksp, defap(k), iret2)
        if (iret2 .eq. 1) defap(k)=0.d0
    end do
!
! MISE AU FORMAT DES TERMES NON DIAGONAUX
!
    do k = 4, ndimsi
        defam(k) = defam(k)*rac2
        defap(k) = defap(k)*rac2
    end do
!
! CARACTERISTIQUES ELASTIQUES VARIABLES
!
    call nmasse(fami, kpg, ksp, '-', imate,&
                ' ', instam, em, num, deumum,&
                troikm)
!
!
    call nmasse(fami, kpg, ksp, '+', imate,&
                ' ', instap, ep, nup, deumup,&
                troikp)
!
    deuxmu = deumup
!
! ----------------------------------------------------------------------
! ASSEMBLAGE COMBUSTIBLE
!       LOIS DE COMPORTEMENT DISPONIBLES :
!        - LEMAITRE MODIFIEE POUR L'IRRADIATION AVEC GRANDISSEMENT
!        - VISC_IRRA_LOG AVEC GRANDISSEMENT
! ----------------------------------------------------------------------
!
    if (compor(1)(1:13) .eq. 'LEMAITRE_IRRA') then
!       RECUPERATION DES CARACTERISTIQUES DES LOIS DE FLUAGE
!
        call rcvalb(fami, 1, 1, '+', imate,&
                    ' ', 'LEMAITRE_IRRA', 0, ' ', [0.d0],&
                    7, nomlem, coelem, codlem, 1)
!
!        RAJOUT DEMANDE PAR ROMEO FERNANDES (FICHE 17275)
        irrap = irrap - irram + vim(2)
        irram = vim(2)
!
!          FLUX NEUTRONIQUE
        fluphi = (irrap-irram)/deltat
!
!       TRAITEMENT DES PARAMETRES DE LA LOI DE FLUAGE
        if (coelem(2) .eq. 0.d0) then
            fluphi = 1.d0
        endif
!         PARAMETRES DE LA LOI DE FLUAGE
        valden = coelem(1)
        if (coelem(6) .le. 0.d0) then
            call utmess('F', 'ALGORITH7_80')
        endif
        if (fluphi .lt. 0.d0) then
            call utmess('F', 'ALGORITH6_57')
        endif
        xnumer = exp(-1.d0*coelem(4)/(valden*(tschem+tabs)))
        unsurk = coelem(2)*fluphi/coelem(6) + coelem(7)
        if (unsurk .lt. 0.d0) then
            call utmess('F', 'ALGORITH7_81')
        endif
        if (unsurk .eq. 0.d0) then
            if (coelem(5) .eq. 0.d0) unsurk=1.d0
            if (coelem(5) .lt. 0.d0) then
                call utmess('F', 'ALGORITH7_82')
            endif
        endif
        if (unsurk .gt. 0.d0) then
            unsurk = unsurk**(coelem(5)/valden)
        endif
        unsurk = unsurk * xnumer
        unsurm = coelem(3)
!
    else if (compor(1)(1:10).eq.'VISC_IRRA_') then
!        PARAMETRES DE LA LOI DE FLUAGE
        call rcvalb(fami, 1, 1, '+', imate,&
                    ' ', 'VISC_IRRA_LOG', 1, 'TEMP', [tschem],&
                    5, nomvil(1), coevil(1), codvil, 1)
        a = coevil(1)
        b = coevil(2)
        ctps = coevil(3)
        ener = coevil(4)
        if (coevil(5) .ne. 1.d0) then
            fluphi=coevil(5)
            irrap = fluphi*instap
            irram = fluphi*instam
        else
            irrap = irrap - irram + vim(2)
            irram = vim(2)
        endif
        if (irrap .lt. irram) then
            call utmess('F', 'ALGORITH8_88')
        endif
!
    else if (compor(1)(1:10).eq.'GRAN_IRRA_') then
!        PARAMETRES DE LA LOI DE FLUAGE
        call rcvalb(fami, 1, 1, '+', imate,&
                    ' ', 'GRAN_IRRA_LOG', 1, ' ', [0.d0],&
                    5, nomvil(1), coevil(1), codvil, 1)
        if (coevil(5) .ne. 1.d0) then
            fluphi=coevil(5)
            irrap = fluphi*instap
            irram = fluphi*instam
        else
            irrap = irrap - irram + vim(2)
            irram = vim(2)
        endif
        if (irrap .lt. irram) then
            call utmess('F', 'ALGORITH8_88')
        endif
        a = coevil(1)
        b = coevil(2)
        ctps = coevil(3)
        ener = coevil(4)
!
    else if (compor(1)(1:10).eq.'LEMA_SEUIL') then
        call rcvalb(fami, 1, 1, '+', imate,&
                    ' ', 'LEMA_SEUIL', 1, 'TEMP', [tschem],&
                    2, nomint(1), coeint(1), codint, 1)
        unsurm=0.d0
        valden=1.d0
        fluphi=(irrap-irram)/deltat
        unsurk = (coeint(1)*fluphi*2.d0)/sqrt(3.d0)
        if (unsurk .lt. 0.d0) then
            call utmess('F', 'ALGORITH8_89')
        endif
!
    else if (compor(1)(1:10).eq.'LMARC_IRRA') then
        call utmess('F', 'ALGORITH8_90')
!
    endif
!
!       TRAITEMENT DES PARAMETRES DE LA LOI DE GRANDISSEMENT
!
    call granac(fami, kpg, ksp, imate, '        ',&
                compor(1), irrap, irram, tm, tp,&
                depsgr)
! --- RECUPERATION DU REPERE POUR LE GRANDISSEMENT
!
    if (compor(1)(1:13) .eq. 'LEMAITRE_IRRA' .or. compor(1)(1:13) .eq. 'GRAN_IRRA_LOG') then
        if (ndim .eq. 2) then
            if (angmas(2) .ne. 0.d0) then
                call utmess('F', 'ALGORITH11_82', nr=2, valr=angmas(2))
            endif
        endif
        alpha = angmas(1)
        beta = angmas(2)
        caa = cos(alpha)
        saa = sin(alpha)
        cba = cos(beta)
        sba = sin(beta)
!
! --- DEFORMATIONS DE GRANDISSEMENT DANS LE REPERE
        degran(1) = depsgr*caa*caa*cba*cba
        degran(2) = depsgr*saa*saa*sba*sba
        degran(3) = depsgr*sba*sba
        degran(4) = depsgr*saa*caa*cba*cba*rac2
        degran(5) = -depsgr*caa*sba*cba*rac2
        degran(6) = -depsgr*saa*sba*cba*rac2
    endif
!
    epsmo = 0.d0
!
    do k = 1, 3
        depsth(k) = deps(k) -epsthe -(defap(k)-defam(k))
        depsth(k) = depsth(k) - degran(k)
        depsth(k) = depsth(k) * theta
!
        if ((k.eq.1) .or. (ndimsi.eq.6)) then
            depsth(k+3) = deps(k+3)-(defap(k+3)-defam(k+3))
            depsth(k+3) = depsth(k+3) - degran(k+3)
            depsth(k+3) = depsth(k+3) * theta
        endif
!
        epsmo = epsmo + depsth(k)
    end do
!
    epsmo = epsmo/3.d0
    do k = 1, ndimsi
        depsdv(k) = depsth(k) - epsmo * kron(k)
    end do
    sigmo = 0.d0
    do k = 1, 3
        sigmo = sigmo + sigm(k)
    end do
    sigmo = sigmo /3.d0
!
    sieqm=0.d0
    do k = 1, ndimsi
        sigdv(k) = sigm(k) - sigmo * kron(k)
        sieqm = sieqm + sigdv(k)**2
        sigmp(k)=(theta*deuxmu+(1.d0-theta)*deumum) /deumum*(sigm(k)-&
        sigmo*kron(k))+ (theta*troikp+(1.d0-theta)*troikm)/troikm*&
        sigmo*kron(k)
    end do
    sieqm=sqrt(1.5d0*sieqm)
    sieqm=sieqm*(theta*deuxmu+(1.d0-theta)*deumum)/deumum
    sigmo = 0.d0
    do k = 1, 3
        sigmo = sigmo + sigmp(k)
    end do
    sigmo = sigmo /3.d0
!
!
    if (compor(1)(1:10) .eq. 'LEMA_SEUIL') then
        sieqp=0.d0
        do k = 1, ndimsi
            sigdv(k) = sigmp(k) - sigmo * kron(k)
            sigel(k) = sigdv(k) + deumup * depsdv(k)/theta
            sieqp = sieqp + sigel(k)**2
        end do
        sieqp=sqrt(1.5d0*sieqp)
    endif
!
    sieleq = 0.d0
    do k = 1, ndimsi
        sigdv(k) = sigmp(k) - sigmo * kron(k)
        sigel(k) = sigdv(k) + deumup * depsdv(k)
        sieleq = sieleq + sigel(k)**2
!
    end do
    sieleq = sqrt(1.5d0*sieleq)
!
!----RESOLUTION DE L'EQUATION SCALAIRE----
!
    prec = crit(3)
    niter = crit(1)
!
    a0 = - sieleq
!
!
    if (compor(1)(1:13) .eq. 'LEMAITRE_IRRA') then
        xap = sieleq
        xap = xap - sieleq*1.d-12
        if (abs(a0) .le. prec) then
            x = 0.d0
        else
            call zerofr(0, 'DEKKER2', vpalem, 0.d0, xap,&
                        prec, int(niter), x, iret, ibid)
            if (iret .ne. 0) goto 999
        endif
        call ggplem(x, dpc+(sieleq-x)/(1.5d0*deumup), valden, unsurk, unsurm,&
                    theta, deumup, fg, fdgdst, fdgdev)
    else if (compor(1)(1:10).eq.'LEMA_SEUIL') then
        d=vim(2)+(deltat*(sieqm+sieqp)/(2*coeint(2)))
        xap = sieleq
        xap = xap - sieleq*1.d-12
        if (abs(a0) .le. prec) then
            x = 0.d0
! -----LE COMPORTEMENT EST PUREMENT ELASTIQUE EN DESSOUS DU SEUIL
        else if (d.le.1.d0) then
            x=sieleq
        else
            call zerofr(0, 'DEKKER2', vpalem, 0.d0, xap,&
                        prec, int(niter), x, iret, ibid)
            if (iret .ne. 0) goto 999
        endif
! -----LE COMPORTEMENT EST PUREMENT ELASTIQUE EN DESSOUS DU SEUIL
        if (d .le. 1.d0) then
            fg=0.d0
            fdgdst=0.d0
            fdgdev=0.d0
        else
            call ggplem(x, 1.d0, valden, unsurk, unsurm,&
                        theta, deumup, fg, fdgdst, fdgdev)
!
        endif
!
    endif
!
    if (compor(1)(5:10) .eq. '_IRRA_') then
        dp1=exp(-ener/(tp+273.15d0))
        dp1=dp1*(a*ctps/(1.d0+ctps*irrap)+b)*(irrap-irram)
        coef1=1.d0/(1.d0+1.5d0*deuxmu*dp1)
        x=0.d0
    else
        if (x .ne. 0.d0) then
            coef1 = 1.d0/(1.d0+1.5d0*deuxmu*deltat*fg/x)
        else
            coef1 = 1.d0/(1.d0+1.5d0*deuxmu*deltat*fdgdst)
        endif
    endif
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        deltp2 = 0.d0
        do k = 1, ndimsi
            sigdv(k) = sigel(k) * coef1
            sigp(k) = sigdv(k) + (sigmo + troikp*epsmo)*kron(k)
            sigp(k) = (sigp(k) - sigm(k))/theta + sigm(k)
            deltev = (sigel(k)-sigdv(k))/(deumup*theta)
            deltp2 = deltp2 + deltev**2
        end do
!
        if (compor(1)(5:10) .eq. '_IRRA_') then
            call lcdevi(sigp, dev)
            vip(1)= vim(1) + dp1*lcnrts( dev )
            if (compor(1)(1:10) .eq. 'GRAN_IRRA_') then
                vip(2) = irrap
                vip(3) = vim(3) + depsgr
            endif
        else
            vip(1) = vim(1) + sqrt(2.d0*deltp2/3.d0)
        endif
!
        if (compor(1)(1:10) .eq. 'LEMA_SEUIL') then
            if (d .le. 1.d0) then
                vip(2) = vim(2)+ ((sieqp+sieqm)*deltat)/(2*coeint(2))
            else
                vip(2) = vim(2)+ ((x/theta+sieqm)*deltat)/(2*coeint(2) )
            endif
!
        endif
!
!        RAJOUT DEMANDE PAR ROMEO FERNANDES (FICHE 17275)
        if (compor(1)(1:13) .eq. 'LEMAITRE_IRRA') then
            vip(2) = irrap
            vip(3) = vim(3) + depsgr
        endif
!
    endif
!
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:14) .eq. 'RIGI_MECA_TANG') then
        if (x .ne. 0.d0) then
            coef2=sieleq*(1.d0 - deltat*fdgdev)
            coef2=coef2/(1.d0+1.5d0*deuxmu*deltat*fdgdst)
            coef2=coef2 - x
            coef2=coef2*1.5d0/(sieleq**3)
        else
            coef2 = 0.d0
        endif
        do k = 1, ndimsi
            do l = 1, ndimsi
                deltkl = 0.d0
                if (k .eq. l) deltkl = 1.d0
                dsidep(k,l) = coef1*(deltkl-kron(k)*kron(l)/3.d0)
                dsidep(k,l) = deumup*(dsidep(k,l)+ coef2*sigel(k)* sigel(l))
                dsidep(k,l) = dsidep(k,l) + troikp*kron(k)*kron(l)/ 3.d0
            end do
        end do
    endif
!
299 continue
!
999 continue
!
end subroutine
