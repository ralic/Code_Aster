subroutine nmcine(fami, kpg, ksp, ndim, imate,&
                  compor, crit, instam, instap, epsm,&
                  deps, sigm, vim, option, sigp,&
                  vip, dsidep, iret)
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
    implicit none
! aslint: disable=W0104
#include "asterc/r8miem.h"
#include "asterfort/matini.h"
#include "asterfort/radial.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
#include "asterfort/verift.h"
    integer :: kpg, ksp, ndim, imate
    character(len=*) :: fami
    character(len=16) :: compor(*), option
    real(kind=8) :: crit(10), instam, instap, radi
    real(kind=8) :: epsm(6), deps(6)
    real(kind=8) :: sigm(6), vim(7), sigp(6), vip(7), dsidep(6, 6)
! ----------------------------------------------------------------------
!     REALISE LA LOI DE VON MISES CINEMATIQUE POUR LES
!     ELEMENTS ISOPARAMETRIQUES EN PETITES DEFORMATIONS
!
!
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  IMATE   : ADRESSE DU MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT : RELCOM ET DEFORM
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  INSTAM  : INSTANT DU CALCUL PRECEDENT
! IN  INSTAP  : INSTANT DU CALCUL
! IN  EPSM    : DEFORMATIONS A L'INSTANT DU CALCUL PRECEDENT
! IN  DEPS    : INCREMENT DE DEFORMATION
! IN  SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
! IN  VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
! IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
! OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL
! OUT VIP     : VARIABLES INTERNES A L'INSTANT ACTUEL
! OUT DSIDEP  : MATRICE CARREE
!
!               ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
!               L'ORDRE :  XX YY ZZ XY XZ YZ
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: depsth(6), valres(3), lambda, deuxmu, epsthe
    real(kind=8) :: depsdv(6), sigdv(6), sigel(6), epsmo, sigmo, e, nu
    real(kind=8) :: sieleq, sigeps, seuil, dp, coef, dsde, sigy
    real(kind=8) :: troisk, kron(6), valrm(2)
    real(kind=8) :: em, num, troikm, deumum, plasti
    integer :: ndimsi
    integer :: icodre(3)
    character(len=8) :: nomres(3)
    character(len=10) :: phenom
    real(kind=8) :: rac2
!-----------------------------------------------------------------------
    integer :: iret, k, l
    real(kind=8) :: a1, a2, c, cm, dsdem
!-----------------------------------------------------------------------
    data        kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
    rac2 = sqrt(2.d0)
! DEB ------------------------------------------------------------------
!
! MISE AU FORMAT DES CONTRAINTES DE RAPPEL
    ndimsi = ndim*2
    do k = 4, ndimsi
        vim(k) = vim(k)*rac2
    end do
!
    call verift(fami, kpg, ksp, 'T', imate,&
                elas_keyword = 'ELAS', epsth=epsthe)
!
! LECTURE DES CARACTERISTIQUES ELASTIQUES DU MATERIAU (TEMPS - ET +)
!    RCCOMA POUR GERER KIT_DDI (GLRC+VMIS_ISOT)
    call rccoma(imate, 'ELAS', 1, phenom, icodre(1))
    if (phenom .eq. 'ELAS') then
        nomres(1)='E'
        nomres(2)='NU'
    else if (phenom .eq. 'ELAS_GLRC') then
        nomres(1)='E_M'
        nomres(2)='NU_M'
    endif
    nomres(3)='ALPHA'
    call rcvalb(fami, kpg, ksp, '-', imate, ' ', phenom, 0, ' ', [0.d0],&
                2, nomres, valres, icodre, 2)
    em = valres(1)
    num = valres(2)
    deumum = em/(1.d0+num)
    troikm = em/(1.d0-2.d0*num)
!
    call rcvalb(fami, kpg, ksp, '+', imate, ' ', phenom, 0, ' ', [0.d0],&
                2, nomres, valres, icodre, 2)
    e = valres(1)
    nu = valres(2)
    lambda = e*nu/((1.d0-2.d0*nu)*(1.d0+nu))
    deuxmu = e/(1.d0+nu)
    troisk = e/(1.d0-2.d0*nu)
!
! LECTURE DES CARACTERISTIQUES D'ECROUISSAGE
!
!     nomres(1)='D_SIGM_EPSI'
    nomres(1)='D_SIGM_E'
    nomres(2)='SY'
    call rcvalb(fami, kpg, ksp, '-', imate, ' ', 'ECRO_LINE', 0, ' ', [0.d0],&
                2, nomres, valres, icodre, 2)
    dsdem=valres(1)
    if ((em-dsdem) .lt. r8miem()) then
        valrm(1)=dsdem
        valrm(2)=em
        call utmess('F', 'COMPOR1_54', nr=2, valr=valrm)
    else
        cm = 2.d0/3.d0*dsdem/(1.d0-dsdem/em)
    endif
!
!     nomres(1)='D_SIGM_EPSI'
    nomres(1)='D_SIGM_E'
    nomres(2)='SY'
    call rcvalb(fami, kpg, ksp, '+', imate, ' ', 'ECRO_LINE', 0, ' ', [0.d0],&
                2, nomres, valres, icodre, 2)
    dsde=valres(1)
    sigy=valres(2)
    if ((e-dsde) .lt. r8miem()) then
        valrm(1)=dsde
        valrm(2)=e
        call utmess('F', 'COMPOR1_54', nr=2, valr=valrm)
    else
        c = 2.d0/3.d0*dsde/(1.d0-dsde/e)
    endif
!
! CALCUL DES CONTRAINTES ELASTIQUES
    do k = 1, 3
        depsth(k) = deps(k) -epsthe
        depsth(k+3) = deps(k+3)
    end do
    epsmo = (depsth(1)+depsth(2)+depsth(3))/3.d0
    do k = 1, ndimsi
        depsdv(k) = depsth(k) - epsmo * kron(k)
    end do
    sigmo = (sigm(1)+sigm(2)+sigm(3))/3.d0
    sieleq = 0.d0
    do k = 1, ndimsi
        sigdv(k) = sigm(k) - sigmo*kron(k)
        sigdv(k) = deuxmu/deumum*sigdv(k)
        sigel(k) = sigdv(k) + deuxmu * depsdv(k)
        sieleq = sieleq + (sigel(k)-c/cm*vim(k))**2
    end do
    sigmo = troisk/troikm * sigmo
    sieleq = sqrt(1.5d0*sieleq)
    seuil = sieleq - sigy
    dp = 0.d0
    plasti=vim(7)
!
! CALCUL DES CONTRAINTES ELASTO-PLASTIQUES ET DES VARIABLES INTERNES
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        if (seuil .lt. 0.d0) then
            vip(7) = 0.d0
            dp = 0.d0
            sieleq = 1.d0
            a1 = 0.d0
            a2 = 0.d0
        else
            vip(7) = 1.d0
            dp = seuil/(1.5d0*(deuxmu+c))
            a1 = (deuxmu/(deuxmu+c))*(seuil/sieleq)
            a2 = (c /(deuxmu+c))*(seuil/sieleq)
        endif
        plasti=vip(7)
        do k = 1, ndimsi
            sigdv(k) = sigel(k) - a1*(sigel(k)-vim(k)*c/cm)
            sigp(k) = sigdv(k) + (sigmo + troisk*epsmo)*kron(k)
            vip(k) = vim(k)*c/cm + a2*(sigel(k)-vim(k)*c/cm)
        end do
    endif
!
! CALCUL DE LA RIGIDITE TANGENTE
    if (option(1:14) .eq. 'RIGI_MECA_TANG' .or. option(1:9) .eq. 'FULL_MECA') then
        call matini(6, 6, 0.d0, dsidep)
        do k = 1, 6
            dsidep(k,k) = deuxmu
        end do
        if (option(1:14) .eq. 'RIGI_MECA_TANG') then
            do k = 1, ndimsi
                sigdv(k) = sigdv(k) - vim(k)*c/cm
            end do
        else
            do k = 1, ndimsi
                sigdv(k) = sigdv(k) - vip(k)
            end do
        endif
        sigeps = 0.d0
        do k = 1, ndimsi
            sigeps = sigeps + sigdv(k)*depsdv(k)
        end do
        a1 = 1.d0/(1.d0+1.5d0*(deuxmu+c)*dp/sigy)
        a2 = (1.d0+1.5d0*c*dp/sigy)*a1
        if (plasti .ge. 0.5d0 .and. sigeps .ge. 0.d0) then
            coef = -1.5d0*(deuxmu/sigy)**2 / (deuxmu+c) * a1
            do k = 1, ndimsi
                do l = 1, ndimsi
                    dsidep(k,l) = a2 * dsidep(k,l) + coef*sigdv(k)* sigdv(l)
                end do
            end do
            lambda = lambda + deuxmu**2*a1*dp/sigy/2.d0
        endif
        do k = 1, 3
            do l = 1, 3
                dsidep(k,l) = dsidep(k,l) + lambda
            end do
        end do
    endif
!
    iret=0
    if (option(1:9) .ne. 'RIGI_MECA') then
        if (crit(10) .gt. 0.d0) then
            call radial(ndimsi, sigm, sigp, vim(7), vip(7), 1, vim(1), vip(1), radi)
            if (radi .gt. crit(10)) then
                iret=2
            endif
        endif
    endif
!
! MISE AU FORMAT DES CONTRAINTES DE RAPPEL
    do k = 4, ndimsi
        vim(k) = vim(k)/rac2
    end do
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        do k = 4, ndimsi
            vip(k) = vip(k)/rac2
        end do
    endif
!
! FIN ------------------------------------------------------------------
end subroutine
