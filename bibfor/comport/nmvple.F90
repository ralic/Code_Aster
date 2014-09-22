subroutine nmvple(fami, kpg, ksp, ndim, imate,&
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
! aslint: disable=
    implicit none
#include "asterfort/ggplem.h"
#include "asterfort/iunifi.h"
#include "asterfort/matini.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utmess.h"
#include "asterfort/verift.h"
#include "asterfort/vpalem.h"
#include "asterfort/zerofr.h"
    integer :: ndim, imate, iret, kpg, ksp
    character(len=*) :: fami
    character(len=8) :: typmod(*)
    character(len=16) :: compor(*), option
    real(kind=8) :: crit(4), instam, instap
    real(kind=8) :: deps(6)
    real(kind=8) :: sigm(6), vim(1), sigp(6), vip(1), dsidep(6, 6)
! ----------------------------------------------------------------------
!     REALISE LA LOI DE VISCOPLASTICITE DE LEMAITRE
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
    common / nmpavp / dpc,sieleq,deuxmu,deltat,tschem,prec,theta,niter
    real(kind=8) :: dpc, sieleq, deuxmu, deltat, tschem, prec, theta, niter
!     COMMON POUR LES PARAMETRES DE LA LOI DE LEMAITRE (NON IRRADIEE)
    common / nmpale / unsurk,unsurm,valden
    real(kind=8) :: unsurk, unsurm, valden
!
    real(kind=8) :: depsth(6), valres(5), epsthe
    real(kind=8) :: depsdv(6), sigdv(6), sigel(6), epsmo, sigmo, e, nu
    real(kind=8) :: troisk, kron(6), valpar(2), rac2, t1, t2
    real(kind=8) :: em, num, troikm, deumum, sigmp(6)
    real(kind=8) :: deltkl, deltp2
    real(kind=8) :: degran(6)
    integer :: k, l, iret1, ibid
    integer :: ndimsi, iret2
    real(kind=8) :: a0, xap, x, tm, tp
    real(kind=8) :: fg, fdgdst, fdgdev, defam(6), defap(6)
    real(kind=8) :: coef1, coef2, deltev
    integer :: icodre(5)
    character(len=6) :: epsa(6)
    character(len=8) :: nompar(2)
    character(len=16) :: nomres(5)
    data              kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
    data epsa   / 'EPSAXX','EPSAYY','EPSAZZ','EPSAXY','EPSAXZ',&
     &              'EPSAYZ'/
! DEB ------------------------------------------------------------------
!
    call verift(fami, kpg, ksp, 'T', imate,&
                epsth=epsthe)
!
    iret=0
    theta = crit(4)
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
!
    do 90 k = 1, 6
        degran(k) = 0.d0
 90 end do
    rac2 = sqrt(2.d0)
    call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                ksp, tm, iret1)
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                ksp, tp, iret2)
    if ((iret1+iret2) .eq. 0) then
        tschem = tm*(1.d0-theta)+tp*theta
    else
        tschem = 0.d0
    endif
    dpc = vim(1)
    deltat = instap - instam
!
    call matini(6, 6, 0.d0, dsidep)
!
    if (ndim .eq. 2) then
        ndimsi=4
    else
        ndimsi=6
    endif
! VARIABLE DE COMMANDE ANELASTIQUE
!
    do 20 k = 1, ndimsi
        call rcvarc(' ', epsa(k), '-', fami, kpg,&
                    ksp, defam(k), iret2)
        if (iret2 .eq. 1) defam(k)=0.d0
!
        call rcvarc(' ', epsa(k), '+', fami, kpg,&
                    ksp, defap(k), iret2)
        if (iret2 .eq. 1) defap(k)=0.d0
 20 end do
!
!
! MISE AU FORMAT DES TERMES NON DIAGONAUX
!
    do 105 k = 4, ndimsi
        defam(k) = defam(k)*rac2
        defap(k) = defap(k)*rac2
105 end do
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
!
    nomres(1) = 'N'
    nomres(2) = 'UN_SUR_K'
    nomres(3) = 'UN_SUR_M'
    nompar(1) = 'TEMP'
    valpar(1) = tschem
    call rcvalb(fami, 1, 1, '+', imate,&
                ' ', 'LEMAITRE', 1, nompar, [valpar],&
                3, nomres, valres, icodre, 2)
    valden = valres(1)
    unsurk = valres(2)
    unsurm = valres(3)
!
    epsmo = 0.d0
    do 110 k = 1, 3
        depsth(k) = deps(k) -epsthe -(defap(k)-defam(k))
        depsth(k) = depsth(k) - degran(k)
        depsth(k) = depsth(k) * theta
        if ((k.eq.1) .or. (ndimsi.eq.6)) then
            depsth(k+3) = deps(k+3)-(defap(k+3)-defam(k+3))
            depsth(k+3) = depsth(k+3) - degran(k+3)
            depsth(k+3) = depsth(k+3) * theta
        endif
        epsmo = epsmo + depsth(k)
110 end do
!
    epsmo = epsmo/3.d0
    do 115 k = 1, ndimsi
        depsdv(k) = depsth(k) - epsmo * kron(k)
115 end do
!
    sigmo = 0.d0
    do 113 k = 1, 3
        sigmo = sigmo + sigm(k)
113 end do
    sigmo = sigmo /3.d0
!
    do 114 k = 1, ndimsi
        sigmp(k)=(theta*deuxmu+(1.d0-theta)*deumum) /deumum*(sigm(k)-&
        sigmo*kron(k))+ (theta*troisk+(1.d0-theta)*troikm)/troikm*&
        sigmo*kron(k)
114 end do
!
    sigmo = 0.d0
    do 116 k = 1, 3
        sigmo = sigmo + sigmp(k)
116 end do
    sigmo = sigmo /3.d0
    sieleq = 0.d0
    do 117 k = 1, ndimsi
        sigdv(k) = sigmp(k) - sigmo * kron(k)
        sigel(k) = sigdv(k) + deuxmu * depsdv(k)
        sieleq = sieleq + sigel(k)**2
117 end do
    sieleq = sqrt(1.5d0*sieleq)
!
!----RESOLUTION DE L'EQUATION SCALAIRE----
!
    prec = crit(3)
    niter = nint(crit(1))
!
    a0 = - sieleq
!
    xap = sieleq
    xap = xap - sieleq*1.d-12
    if (abs(a0) .le. prec) then
        x = 0.d0
    else
        call zerofr(0, 'DEKKER2', vpalem, 0.d0, xap,&
                    prec, int(niter), x, iret, ibid)
        if (iret .eq. 1) then
            if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
                goto 9999
            else
                x=0.d0
                iret=0
            endif
        endif
    endif
    if (x .ne. 0.d0) call ggplem(x, dpc+(sieleq-x)/(1.5d0*deuxmu), valden, unsurk, unsurm,&
                                 theta, deuxmu, fg, fdgdst, fdgdev)
!
!-----------------------------------------
    if (x .ne. 0.d0) then
        coef1 = 1.d0/(1.d0+1.5d0*deuxmu*deltat*fg/x)
    else
!       COEF1 = 1.d0/(1.d0+1.5d0*DEUXMU*DELTAT*FDGDST)
        coef1 = 1.d0
    endif
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        deltp2 = 0.d0
        do 160 k = 1, ndimsi
            sigdv(k) = sigel(k) * coef1
            sigp(k) = sigdv(k) + (sigmo + troisk*epsmo)*kron(k)
            sigp(k) = (sigp(k) - sigm(k))/theta + sigm(k)
            deltev = (sigel(k)-sigdv(k))/(deuxmu*theta)
            deltp2 = deltp2 + deltev**2
160     continue
        vip(1) = vim(1) + sqrt(2.d0*deltp2/3.d0)
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
        do 135 k = 1, ndimsi
            do 135 l = 1, ndimsi
                deltkl = 0.d0
                if (k .eq. l) deltkl = 1.d0
                dsidep(k,l) = coef1*(deltkl-kron(k)*kron(l)/3.d0)
                dsidep(k,l) = deuxmu*(dsidep(k,l)+coef2*sigel(k)* sigel(l))
                dsidep(k,l) = dsidep(k,l) + troisk*kron(k)*kron(l)/ 3.d0
135         continue
    endif
!
299 continue
9999 continue
!
! FIN ------------------------------------------------------------------
end subroutine
