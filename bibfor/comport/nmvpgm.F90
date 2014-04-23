subroutine nmvpgm(fami, kpg, ksp, ndim, imate,&
                  compor, crit, typmod, instam, instap,&
                  deps, sigm, vim, option, sigp,&
                  vip, dsidep, iret)
! ----------------------------------------------------------------------
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
    implicit none
#include "asterfort/ggpgmo.h"
#include "asterfort/iunifi.h"
#include "asterfort/matini.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utmess.h"
#include "asterfort/verift.h"
#include "asterfort/vpagm1.h"
#include "asterfort/zerofr.h"
    integer :: ndim, imate, iret, kpg, ksp
    character(len=*) :: fami
    character(len=8) :: typmod(*)
    character(len=16) :: compor(*), option
    real(kind=8) :: crit(4), instam, instap
    real(kind=8) :: deps(6), defam(6), defap(6)
    real(kind=8) :: sigm(6), vim(2), sigp(6), vip(2), dsidep(6, 6)
! ----------------------------------------------------------------------
!     REALISE LA LOI DE VISCOPLASTICITE DE GATT - MONERIE
!  POUR LES ELEMENTS
!     ISOPARAMETRIQUES EN PETITES DEFORMATIONS
!
!
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  IMATE   : ADRESSE DU MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT : RELCOM ET DEFORM
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  INSTAM  : INSTANT DU CALCUL PRECEDENT
! IN  INSTAP  : INSTANT DU CALCUL
! IN  DEPS    : INCREMENT DE DEFORMATION
! IN  SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
! IN  VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
! IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
! IN  DEFAM   : DEFORMATIONS ANELASTIQUES A L'INSTANT PRECEDENT
! IN  DEFAP   : DEFORMATIONS ANELASTIQUES A L'INSTANT DU CALCUL
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
! ----------------------------------------------------------------------
!
!     COMMON POUR LES PARAMETRES DES LOIS VISCOPLASTIQUES
    common / nmpavp / sieleq,deuxmu,troisk,deltat,tschem,prec,theta,&
     &                  niter
    real(kind=8) :: sieleq, deuxmu, troisk, deltat, tschem, prec, theta, niter
!     COMMON POUR LES PARAMETRES DE LA LOI GATT-MONERIE
    common / nmpagm /ak1,ak2,xn1,xn2,expa1,expa2,expab1,expab2,a1,a2,&
     &                 b1,b2,xw,xq,xh,sige,sigh,sigh0,porom,sgd
    real(kind=8) :: ak1, ak2, xn1, xn2, expa1, expa2, expab1, expab2, a1, a2, b1
    real(kind=8) :: b2, xw, xq, xh, sige, sigh, sigh0, porom, sgd
!
    real(kind=8) :: depsth(6), valres(5), epsthe
    real(kind=8) :: depsdv(6), sigdv(6), sigel(6), epsmo, e, nu
    real(kind=8) :: kron(6), valpar(2), rac2, t1, t2
    real(kind=8) :: em, num, troikm, deumum, sigmp(6), sigmo
    real(kind=8) :: deltkl, deltp2
    real(kind=8) :: degran(6)
    integer :: k, l, iret1, iret2, iret4, ibid
    integer :: ndimsi
    real(kind=8) :: a0, xap, tm, tp
    real(kind=8) :: fg, fdgdst, fdgdev
    real(kind=8) :: coef1, coef2, deltev
    integer :: icodre(5)
    character(len=6) :: epsa(6)
    character(len=8) :: nomres(5), nompar(2)
! RMS
    real(kind=8) :: grain, tk, xr, xq1, xq2, dporo, poro, xm1, xm2, xe01, xe02
    real(kind=8) :: fdevpkk, grain0
! DEB ------------------------------------------------------------------
    data              kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
    data epsa   / 'EPSAXX','EPSAYY','EPSAZZ','EPSAXY','EPSAXZ',&
     &              'EPSAYZ'/
!
    call verift(fami, kpg, ksp, 'T', imate,&
                epsth=epsthe)
    call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                ksp, tm, iret1)
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                ksp, tp, iret2)
    theta = crit(4)
    if ((iret1+iret2) .eq. 0) then
        tschem = tm*(1.d0-theta)+tp*theta
    else
        tschem = 0.d0
    endif
    t1 = abs(theta-0.5d0)
    t2 = abs(theta-1.d0)
    prec = 0.01d0
    if ((t1.gt.prec) .and. (t2.gt.prec)) then
        call utmess('F', 'ALGORITH6_55')
    endif
!
    if (typmod(1) .eq. 'C_PLAN') then
        call utmess('F', 'ALGORITH6_92')
        goto 299
    endif
    do k = 1, 6
        degran(k) = 0.d0
    end do
    rac2 = sqrt(2.d0)
    deltat = instap - instam
!
    call matini(6, 6, 0.d0, dsidep)
!
    if (ndim .eq. 2) then
        ndimsi=4
    else
        ndimsi=6
    endif
!
! VARIABLE DE COMMANDE ANELASTIQUE
    do k = 1, ndimsi
        call rcvarc(' ', epsa(k), '-', fami, kpg,&
                    ksp, defam(k), iret4)
        if (iret4 .eq. 1) defam(k)=0.d0
!
        call rcvarc(' ', epsa(k), '+', fami, kpg,&
                    ksp, defap(k), iret4)
        if (iret4 .eq. 1) defap(k)=0.d0
    end do
!
!
! MISE AU FORMAT DES TERMES NON DIAGONAUX
!
    do k = 4, ndimsi
        defam(k) = defam(k)*rac2
        defap(k) = defap(k)*rac2
    end do
!
    nompar(1)='INST'
    valpar(1)=instam
    nomres(1)='E'
    nomres(2)='NU'
    call rcvalb(fami, kpg, ksp, '-', imate,&
                ' ', 'ELAS', 1, nompar, [valpar],&
                2, nomres, valres, icodre, 2)
    em = valres(1)
    num = valres(2)
    deumum = em/(1.d0+num)
    troikm = em/(1.d0-2.d0*num)
    valpar(1)=instap
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'ELAS', 1, nompar, [valpar],&
                2, nomres, valres, icodre, 2)
    e = valres(1)
    nu = valres(2)
    deuxmu = e/(1.d0+nu)
    troisk = e/(1.d0-2.d0*nu)
!
! ---- PARAMETRES LOI VISCOPLASTIQUE GATT - MONERIE -------
!
    nomres(1) = 'D_GRAIN'
    nomres(2) = 'PORO_INIT'
    nomres(3) = 'EPSI_01'
    nomres(4) = 'EPSI_02'
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'GATT_MONERIE', 1, nompar, [valpar],&
                4, nomres, valres, icodre, 2)
    grain = valres(1)
    porom = vim(2)
    if (porom .eq. 0.d0) then
        porom = valres(2)
        if ((vim(1).ne.0.d0) .or. (valres(2).eq.0.d0)) then
            call utmess('F', 'ALGORITH8_86')
        endif
    endif
!
    xe01 = valres(3)
    xe02 = valres(4)
!
    xn1 = 1.d0
    xn2 = 8.0d0
    xm1 = -2.0d0
    xm2 = 2.0d0
    xq1 = 377.0d3
    xq2 = 462.0d3
    tk = tschem + 273.d0
! --- CONSTANTE DES GAZS PARFAITS (J/(K*MOL))
    xr = 8.31d0
!     POTENTIELS DE DISSIPATIONS
!     CONTRAINTES EN MPA, SIG0I = 1 MPA -> XE01*(1.d6**XNI)
!     (ASSURE LA COHERENCE POUR PSII ET SES DERIVEES)
    ak1=xe01*(1.d6**xn1)*(grain**xm1)*exp(-xq1/(xr*tk))
    grain0 = 15.0d-6
    ak2=xe02*(1.d6**xn2)*2.d0*(grain0**xm2)*(1.d0-cos(grain/grain0))&
     &    *exp(-xq2/(xr*tk))
!     PREMIER POTENTIEL
    expa1 = 1.d0/xn1
    expab1= 2.d0*xn1/(xn1+1.d0)
    a1 = (porom**(2.d0/(xn1+1))) *(xn1*(1.d0 - porom**expa1))**(-1.d0*expab1)
    b1 = (1.d0+((2.d0/3.d0)*porom))/((1.d0-porom)**expab1)
!     SECOND POTENTIEL
    expa2 = 1.d0/xn2
    expab2= 2.d0*xn2/(xn2+1.d0)
    a2 = (porom**(2.d0/(xn2+1))) *(xn2*(1.d0 - porom**expa2))**(-1.d0*expab2)
    b2 = (1.d0+((2.d0/3.d0)*porom))/((1.d0-porom)**expab2)
!     FCT COUPLAGE
!     CONTRAINTES EN MPA -> REFORMULATION W*SIGI**Q -> W*(SIGI/SIG0)**Q
!     SIG0 = 1 MPA, W -> W*(1.d6**XQ)
!     (ASSURE LA COHERENCE POUR THETA ET SES DERIVEES)
    xq = -0.189d0
    xw = 47350.4d0*(1.d6**xq)
    xh = 600.d0
! --- FIN PARAMETRES LOI VISCOPLASTIQUE GATT - MONERIE -----
!
    epsmo = 0.d0
    do k = 1, 3
        depsth(k) = deps(k) -epsthe -(defap(k)-defam(k))
        depsth(k) = depsth(k) - degran(k)
        depsth(k) = depsth(k) * theta
        if ((k.eq.1) .or. (ndimsi.eq.6)) then
            depsth(k+3) = deps(k+3)-(defap(k+3)-defam(k+3))
            depsth(k+3) = depsth(k+3) - degran(k+3)
            depsth(k+3) = depsth(k+3) * theta
        endif
        epsmo = epsmo + depsth(k)
    end do
!
    epsmo = epsmo/3.d0
    do k = 1, ndimsi
        depsdv(k) = depsth(k) - epsmo * kron(k)
    end do
!
    sigmo = 0.d0
    do k = 1, 3
        sigmo = sigmo + sigm(k)
    end do
    sigmo = sigmo /3.d0
!
    do k = 1, ndimsi
        sigmp(k)=(theta*deuxmu+(1.d0-theta)*deumum) /deumum*(sigm(k)-&
        sigmo*kron(k))+ (theta*troisk+(1.d0-theta)*troikm)/troikm*&
        sigmo*kron(k)
    end do
    sigmo = 0.d0
    do k = 1, 3
        sigmo = sigmo + sigmp(k)
    end do
    sigmo = sigmo /3.d0
    sieleq = 0.d0
    do k = 1, ndimsi
        sigdv(k) = sigmp(k) - sigmo * kron(k)
        sigel(k) = sigdv(k) + deuxmu * depsdv(k)
        sieleq = sieleq + sigel(k)**2
    end do
    sieleq = sqrt(1.5d0*sieleq)
!
!----RESOLUTION CHAINEE DES DEUX EQUATIONS SCALAIRES----
!----VPAGM1 = FD, VPAGM2 = F (VOIR R5.03.08)
!
    prec = crit(3)
    niter = nint(crit(1))
!---PAS D ECOULEMENT
    sigh0 = sigmo + troisk*epsmo
    if ((sieleq.le.prec) .and. (abs(sigh0).le.prec)) then
        poro = porom
        sige = 0.d0
!---ECOULEMENT
    else
        sgd = 1.d0
        a0 = vpagm1(0.d0)
        xap = abs(porom-a0)
        if (a0 .gt. 0.d0) then
!            WRITE(*,*) 'A0>0, DF EXPLICITE', XAP, POROM, A0
            sgd = -1.d0
            a0 = a0*sgd
            call zerofr(0, 'DEKKER2', vpagm1, 0.d0, xap,&
                        1.d20, int(niter), poro, iret, ibid)
            if (iret .ne. 0) goto 999
        else
            if (xap .ge. 1.d0) then
                xap = porom + (1.d0-porom)/2.d0
            endif
            call zerofr(0, 'DEKKER2', vpagm1, 0.d0, xap,&
                        prec, int(niter), poro, iret, ibid)
            if (iret .ne. 0) goto 999
        endif
    endif
    dporo = poro - porom
    sigh = sigh0 - (troisk/3.d0)*dporo/(1.d0-porom-dporo)
    call ggpgmo(sige, sigh, theta, deuxmu, fg,&
                fdevpkk, fdgdst, fdgdev, tschem)
    epsmo = epsmo - fdevpkk*deltat
!
!-----------------------------------------
    if (sige .ne. 0.d0) then
        coef1 = 1.d0/(1.d0+1.5d0*deuxmu*deltat*fg/sige)
    else
        coef1 = 1.d0/(1.d0+1.5d0*deuxmu*deltat*fdgdst)
    endif
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        deltp2 = 0.d0
        do k = 1, ndimsi
            sigdv(k) = sigel(k) * coef1
            sigp(k) = sigdv(k) + (sigmo + troisk*epsmo)*kron(k)
            sigp(k) = (sigp(k) - sigm(k))/theta + sigm(k)
            deltev = (sigel(k)-sigdv(k))/(deuxmu*theta)
            deltp2 = deltp2 + deltev**2
        end do
        vip(1) = vim(1) + sqrt(2.d0*deltp2/3.d0)
        vip(2) = porom + dporo/theta
    endif
!
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:14) .eq. 'RIGI_MECA_TANG') then
        if (sige .ne. 0.d0) then
            coef2=sieleq*(1.d0 - deltat*fdgdev)
            coef2=coef2/(1.d0+1.5d0*deuxmu*deltat*fdgdst)
            coef2=coef2 - sige
            coef2=coef2*1.5d0/(sieleq**3)
        else
            coef2 = 0.d0
        endif
        do  k = 1, ndimsi
            do l = 1, ndimsi
                deltkl = 0.d0
                if (k .eq. l) deltkl = 1.d0
                dsidep(k,l) = coef1*(deltkl-kron(k)*kron(l)/3.d0)
                dsidep(k,l) = deuxmu*(dsidep(k,l)+coef2*sigel(k)* sigel(l))
                dsidep(k,l) = dsidep(k,l) + troisk*kron(k)*kron(l)/ 3.d0
            end do
        end do
    endif
!
! MISE AU FORMAT DES TERMES NON DIAGONAUX
!
    do k = 4, ndimsi
        defam(k) = defam(k)/rac2
        defap(k) = defap(k)/rac2
    end do
!
299 continue
!
999 continue
!
! FIN ------------------------------------------------------------------
end subroutine
