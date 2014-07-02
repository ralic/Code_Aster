subroutine nmelas(fami, kpg, ksp, ndim, typmod,&
                  imate, deps, sigm, option, sigp,&
                  vip, dsidep, iret)
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
! IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
! OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL
! OUT VIP     : VARIABLES INTERNES A L'INSTANT ACTUEL
! OUT DSIDEP  : MATRICE CARREE (INUTILISE POUR RAPH_MECA)
!
!               ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
!               L'ORDRE :  XX,YY,ZZ,SQRT(2)*XY,SQRT(2)*XZ,SQRT(2)*YZ
! OUT IRET    : CODE RETOUR DE L'INTEGRATION DE LA LOI DE VOM MISES
!               = 1  => PAS DE PROBLEME
!               = 0  => ECHEC DANS L'INTEGRATION DE LA LOI
!
!
    implicit none
#include "asterf_types.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/verift.h"
    aster_logical :: cplan, inco
    integer :: ndim, imate, kpg, ksp, iret, ndimsi
    integer :: k, l, iret2, iret3, iret4, iret5, icodre(3)
    real(kind=8) :: sigm(6), sigp(6), vip(1), dsidep(6, 6)
    real(kind=8) :: deps(6), deuxmu, depsth(6), valres(3), epsthe, co, depsmo
    real(kind=8) :: sigmmo, e, nu, troisk, coef, hydrm, hydrp
    real(kind=8) :: kron(6), depsdv(6), em, num, troikm, deumum, sigmp(6)
    real(kind=8) :: sechm, sechp, sref, tp, defam(6), defap(6)
    real(kind=8) :: bendom, bendop, kdessm, kdessp, rac2, tm
    character(len=*) :: fami
    character(len=6) :: epsa(6)
    character(len=8) :: nomres(3), typmod(*)
    character(len=16) :: option
!-----------------------------------------------------------------------
    data kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
    data epsa/'EPSAXX','EPSAYY','EPSAZZ','EPSAXY','EPSAXZ','EPSAYZ'/
! DEB ------------------------------------------------------------------
!
!     -- 1 INITIALISATIONS :
!     ----------------------
!
    iret=0
!
    cplan = typmod(1) .eq. 'C_PLAN'
    inco = typmod(2) .eq. 'INCO'
!
    if (inco) then
        co = 0.d0
    else
        co = 1.d0
    endif
    ndimsi = 2*ndim
!
    rac2 = sqrt(2.d0)
!
!
!     -- 2 RECUPERATION DES CARACTERISTIQUES
!     ---------------------------------------
    nomres(1)='E'
    nomres(2)='NU'
!
    call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                ksp, tm, iret3)
    if (iret3 .ne. 0) tm=0.d0
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                ksp, tp, iret4)
    if (iret4 .ne. 0) tp=0.d0
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
    do k = 1, 6
        defam(k) = 0.d0
        defap(k) = 0.d0
    end do
!
    do k = 1, ndimsi
        call rcvarc(' ', epsa(k), '-', fami, kpg,&
                    ksp, defam(k), iret5)
        if (iret5 .ne. 0) defam(k)=0.d0
!
        call rcvarc(' ', epsa(k), '+', fami, kpg,&
                    ksp, defap(k), iret5)
        if (iret5 .ne. 0) defap(k)=0.d0
    end do
!
! MISE AU FORMAT DES TERMES NON DIAGONAUX
!
    do k = 4, ndimsi
        defam(k) = defam(k)*rac2
        defap(k) = defap(k)*rac2
    end do
!
    call rcvalb(fami, kpg, ksp, '-', imate,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                2, nomres(1), valres(1), icodre(1), 2)
    em = valres(1)
    num = valres(2)
    deumum = em/(1.d0+num)
!
    if (inco) then
        troikm = deumum
    else
        troikm = em/(1.d0-2.d0*num)
    endif
!
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                2, nomres(1), valres(1), icodre(1), 2)
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
!
    call verift(fami, kpg, ksp, 'T', imate,&
                epsth=epsthe)
!
! --- RETRAIT ENDOGENE ET RETRAIT DE DESSICCATION
!
    nomres(1)='B_ENDOGE'
    nomres(2)='K_DESSIC'
    call rcvalb(fami, kpg, ksp, '-', imate,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                1, nomres(1), valres(1), icodre(1), 0)
    if (icodre(1) .ne. 0) valres(1) = 0.d0
    bendom = valres(1)
!
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                1, nomres(1), valres(1), icodre(1), 0)
    if (icodre(1) .ne. 0) valres(1) = 0.d0
    bendop = valres(1)
!
    call rcvalb(fami, kpg, ksp, '-', imate,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                1, nomres(2), valres(2), icodre(2), 0)
    if (icodre(2) .ne. 0) valres(2) = 0.d0
    kdessm = valres(2)
!
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                1, nomres(2), valres(2), icodre(2), 0)
    if (icodre(2) .ne. 0) valres(2) = 0.d0
    kdessp = valres(2)
!
!
!     -- 4 CALCUL DE DEPSMO ET DEPSDV :
!     --------------------------------
    coef = epsthe - bendop*hydrp + bendom*hydrm - kdessp*(sref-sechp) + kdessm*(sref-sechm)
    if (cplan) deps(3)=-nu/(1.d0-nu)*(deps(1)+deps(2)) +(1.d0+nu)/(1.d0-nu)*coef + nu*(defap(1)-d&
               &efam(1)+defap(2)-defam(2))/(1.d0-nu) + defap(3)-defam(3)
    depsmo = 0.d0
    do k = 1, 3
        depsth(k) = deps(k) -coef -(defap(k)-defam(k))
        depsth(k+3) = deps(k+3)-(defap(k+3)-defam(k+3))
        depsmo = depsmo + depsth(k)
    end do
    depsmo = depsmo/3.d0
    do k = 1, ndimsi
        depsdv(k) = depsth(k) - depsmo * kron(k)*co
    end do
!
!     -- 5 CALCUL DE SIGMP :
!     ----------------------
    sigmmo = 0.d0
    do k = 1, 3
        sigmmo = sigmmo + sigm(k)
    end do
    sigmmo = sigmmo /3.d0
    do k = 1, ndimsi
        sigmp(k)=deuxmu/deumum*(sigm(k)-sigmmo*kron(k)) + troisk/&
        troikm*sigmmo*kron(k)
    end do
!
!     -- 6 CALCUL DE SIGMMO, SIGMDV, SIGEL, SIELEQ ET SEUIL :
!     -------------------------------------------------------
    sigmmo = 0.d0
    do k = 1, 3
        sigmmo = sigmmo + sigmp(k)
    end do
    sigmmo = sigmmo /3.d0
!
!     -- 7 CALCUL DE SIGP,SIGPDV,VIP,DP,RP:
!     -------------------------------------
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA' .or. option(1:16)&
        .eq. 'RIGI_MECA_IMPLEX') then
!
!
        do k = 1, ndimsi
            sigp(k) = sigmp(k)+deuxmu*depsdv(k)+co*troisk*depsmo*kron( k)
        end do
!
        vip(1) = 0.d0
!
    endif
!
!     -- 8 CALCUL DE DSIDEP(6,6) :
!     ----------------------------
    if (option(1:10) .eq. 'RIGI_MECA_' .or. option(1:9) .eq. 'FULL_MECA') then
!
        do k = 1, ndimsi
            do l = 1, ndimsi
                dsidep(k,l) = 0.d0
            end do
        end do
!
        do k = 1, 3
            do l = 1, 3
                dsidep(k,l) = dsidep(k,l)+co*(troisk/3.d0-deuxmu/3.d0)
            end do
        end do
        do k = 1, ndimsi
            dsidep(k,k) = dsidep(k,k) + deuxmu
        end do
!
!       -- 8.3 CORRECTION POUR LES CONTRAINTES PLANES :
        if (cplan) then
            do k = 1, ndimsi
                if (k .ne. 3) then
                    do l = 1, ndimsi
                        if (l .ne. 3) then
                            dsidep(k,l)=dsidep(k,l) - 1.d0/dsidep(3,3)*dsidep(k,3)*dsidep(3,l)
                        endif
                    end do
                endif
            end do
        endif
!
    endif
!
end subroutine
