subroutine nzedga(fami, kpg, ksp, ndim, imat,&
                  compor, crit, instam, instap, epsm,&
                  deps, sigm, vim, option, sigp,&
                  vip, dsidep, iret)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/matini.h"
#include "asterfort/nzcalc.h"
#include "asterfort/rcfonc.h"
#include "asterfort/rctrac.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utmess.h"
#include "asterfort/verift.h"
#include "asterfort/get_meta_id.h"
#include "asterfort/get_meta_phasis.h"
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=*), intent(in) :: fami
    integer, intent(in) :: kpg
    integer, intent(in) :: ksp
    integer, intent(in) :: ndim
    integer, intent(in) :: imat
    character(len=16), intent(in) :: compor(*)
    real(kind=8), intent(in) :: crit(*)
    real(kind=8), intent(in) :: instam
    real(kind=8), intent(in) :: instap
    real(kind=8), intent(in) :: epsm(*)
    real(kind=8), intent(in) :: deps(*)
    real(kind=8), intent(in) :: sigm(*)
    real(kind=8), intent(in) :: vim(5)
    character(len=16), intent(in) :: option
    real(kind=8), intent(out) :: sigp(*)
    real(kind=8), intent(out) :: vip(5)
    real(kind=8), intent(out) :: dsidep(6, 6)
    integer, intent(out) :: iret
!
! --------------------------------------------------------------------------------------------------
!
! Comportment
!
! META_P_I* / META_V_I* for small strains and zirconium metallurgy
!
! --------------------------------------------------------------------------------------------------
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  IMAT    : ADRESSE DU MATERIAU CODE
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
!     IRET    : CODE RETOUR DE LA RESOLUTION DE L'EQUATION SCALAIRE
!               (NZCALC)
!                              IRET=0 => PAS DE PROBLEME
!                              IRET=1 => ECHEC
!
!               ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
!               L'ORDRE :  XX YY ZZ XY XZ YZ
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jprol, jvale, nbval(3), maxval, nb_phasis, meta_id
    integer :: ndimsi, i, j, k, mode, iret1
    real(kind=8) :: phase(5), phasm(5), zalpha
    real(kind=8) :: temp, dt
    real(kind=8) :: epsth, e, deuxmu, deumum, troisk
    real(kind=8) :: fmel(1), sy(3), h(3), hmoy, hplus(3), r(3), rmoy
    real(kind=8) :: theta(4), epsthe(3)
    real(kind=8) :: eta(5), n(3), unsurn(5), c(3), m(3), cmoy, mmoy, cr
    real(kind=8) :: dz(2), dz1(2), dz2(2), vi(3), dvin, vimoy, ds
    real(kind=8) :: trans, kpt(2), zvarim, zvarip, deltaz
    real(kind=8) :: trepsm, trdeps, trsigm, trsigp
    real(kind=8) :: dvdeps(6), dvsigm(6), dvsigp(6)
    real(kind=8) :: sigel(6), sig0(6), sieleq, sigeps
    real(kind=8) :: plasti, dp, seuil
    real(kind=8) :: coef1, coef2, coef3, dv, n0(3), b
    real(kind=8) :: rbid
    real(kind=8) :: valres(12)
    character(len=1) :: poum
    integer :: icodre(12), test
    character(len=16) :: nomres(12)
    character(len=8) :: nomcle(3)
    aster_logical :: resi, rigi
    real(kind=8), parameter :: kron(6) = (/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/)
!
! --------------------------------------------------------------------------------------------------
!
    do i = 1, 2*ndim
        sigp(i) = 0.d0
    end do
    vip(1:5)        = 0.d0
    dsidep(1:6,1:6) = 0.d0
    iret            = 0
    ndimsi          = 2*ndim
    resi            = option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL'
    rigi            = option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL'
    dt              = instap-instam
!
! - Get metallurgy type
!
    call get_meta_id(meta_id, nb_phasis)
    ASSERT(meta_id.eq.2)
    ASSERT(nb_phasis.eq.3)
!
! - Get phasis
!
    if (resi) then
        poum = '+'
        call get_meta_phasis(fami     , '+'  , kpg   , ksp , meta_id,&
                             nb_phasis, phase, zcold_ = zalpha)
        call get_meta_phasis(fami     , '-'  , kpg   , ksp , meta_id,&
                             nb_phasis, phasm)
    else
        poum = '-'
        call get_meta_phasis(fami     , '-'  , kpg   , ksp , meta_id,&
                             nb_phasis, phase, zcold_ = zalpha)
    endif
!
! - Compute thermic strain
!
    call rcvarc(' ', 'TEMP', poum, fami, kpg,&
                ksp, temp, iret1)
    call verift(fami, kpg, ksp, poum, imat,&
                vepsth=epsthe)
!
! ****************************************
! 2 - RECUPERATION DES CARACTERISTIQUES
! ****************************************
!
! 2.1 - ELASTIQUE ET THERMIQUE
!
    nomres(1)='E'
    nomres(2)='NU'
    nomres(3)='F_ALPHA'
    nomres(4)='C_ALPHA'
    nomres(5)='PHASE_REFE'
    nomres(6)='EPSF_EPSC_TREF'
!
    call rcvalb(fami, kpg, ksp, '-', imat,&
                ' ', 'ELAS_META', 0, ' ', [0.d0],&
                6, nomres, valres, icodre, 2)
    deumum = valres(1)/(1.d0+valres(2))
!
    call rcvalb(fami, kpg, ksp, poum, imat,&
                ' ', 'ELAS_META', 0, ' ', [0.d0],&
                6, nomres, valres, icodre, 2)
    epsth = phase(nb_phasis)*(epsthe(1)-(1.d0-valres(5))*valres(6)) +&
            zalpha*(epsthe(2) + valres(5)*valres(6))
    e = valres(1)
    deuxmu = e/(1.d0+valres(2))
    troisk = e/(1.d0-2.d0*valres(2))
!
    plasti=vim(4)
!
! 2.2 - LOI DES MELANGES
!
    if (compor(1)(1:6) .eq. 'META_P') then
        nomres(1) ='F1_SY'
        nomres(2) ='F2_SY'
        nomres(3) ='C_SY'
        nomres(4) ='SY_MELAN'
    endif
!
    if (compor(1)(1:6) .eq. 'META_V') then
        nomres(1) ='F1_S_VP'
        nomres(2) ='F2_S_VP'
        nomres(3) ='C_S_VP'
        nomres(4) ='S_VP_MEL'
    endif
!
    call rcvalb(fami, 1, 1, '+', imat,&
                ' ', 'ELAS_META', 1, 'META', [zalpha],&
                1, nomres(4), fmel, icodre(4), 0)
    if (icodre(4) .ne. 0) fmel(1) = zalpha
!
! 2.3 - LIMITE D ELASTICITE
!
    call rcvalb(fami, kpg, ksp, poum, imat,&
                ' ', 'ELAS_META', 0, ' ', [0.d0],&
                3, nomres, sy, icodre, 2)
!
    if (resi) then
!
! 2.4 - RESTAURATION D ECROUISSAGE
!
        if (compor(1)(1:12) .eq. 'META_P_IL_RE' .or. compor(1)(1:15) .eq.&
            'META_P_IL_PT_RE' .or. compor(1)(1:12) .eq. 'META_V_IL_RE' .or.&
            compor(1)(1:15) .eq. 'META_V_IL_PT_RE' .or. compor(1)(1:13) .eq.&
            'META_P_INL_RE' .or. compor(1)(1:16) .eq. 'META_P_INL_PT_RE' .or.&
            compor(1)(1:13) .eq. 'META_V_INL_RE' .or. compor(1)(1:16) .eq.&
            'META_V_INL_PT_RE') then
            nomres(1) ='C_F1_THETA'
            nomres(2) ='C_F2_THETA'
            nomres(3) ='F1_C_THETA'
            nomres(4) ='F2_C_THETA'
            call rcvalb(fami, kpg, ksp, poum, imat,&
                        ' ', 'META_RE', 0, '  ', [0.d0],&
                        4, nomres, theta, icodre, 2)
        else
            do i = 1, 4
                theta(i)=1.d0
            end do
        endif
!
! 2.5 - VISCOSITE
!
        if (compor(1)(1:6) .eq. 'META_V') then
            nomres(1) = 'F1_ETA'
            nomres(2) = 'F2_ETA'
            nomres(3) = 'C_ETA'
            nomres(4) = 'F1_N'
            nomres(5) = 'F2_N'
            nomres(6) = 'C_N'
            nomres(7) ='F1_C'
            nomres(8) ='F2_C'
            nomres(9) ='C_C'
            nomres(10) = 'F1_M'
            nomres(11) = 'F2_M'
            nomres(12) = 'C_M'
            call rcvalb(fami, kpg, ksp, poum, imat,&
                        ' ', 'META_VISC', 0, ' ', [0.d0],&
                        6, nomres, valres, icodre, 2)
            call rcvalb(fami, kpg, ksp, poum, imat,&
                        ' ', 'META_VISC', 0, ' ', [0.d0],&
                        6, nomres(7), valres(7), icodre(7), 0)
            do k = 1, nb_phasis
                eta(k) = valres(k)
                n(k) = valres(nb_phasis+k)
                unsurn(k)=1/n(k)
                if (icodre(2*nb_phasis+k) .ne. 0) valres(2*nb_phasis+k)=0.d0
                c(k) =valres(2*nb_phasis+k)
                if (icodre(3*nb_phasis+k) .ne. 0) valres(3*nb_phasis+k)=20.d0
                m(k) = valres(3*nb_phasis+k)
            end do
        else
            do k = 1, nb_phasis
                eta(k) = 0.d0
                n(k)= 20.d0
                unsurn(k)= 1.d0
                c(k) = 0.d0
                m(k) = 20.d0
            end do
        endif
!
! 2.6 - CALCUL DE VIM+DG-DS ET DE RMOY
!
        do  k = 1, nb_phasis-1
            dz(k)= phase(k)-phasm(k)
            if (dz(k) .ge. 0.d0) then
                dz1(k)=dz(k)
                dz2(k)=0.d0
            else
                dz1(k)=0.d0
                dz2(k)=-dz(k)
            endif
        end do
!
        if (phase(nb_phasis) .gt. 0.d0) then
            dvin=0.d0
            do k = 1, nb_phasis-1
                dvin=dvin+dz2(k)*(theta(2+k)*vim(k)-vim(nb_phasis))/&
                phase(nb_phasis)
            end do
            vi(nb_phasis)=vim(nb_phasis)+dvin
            vimoy=phase(nb_phasis)*vi(nb_phasis)
        else
            vi(nb_phasis) = 0.d0
            vimoy=0.d0
        endif
!
        do k = 1, nb_phasis-1
            if (phase(k) .gt. 0.d0) then
                dvin=dz1(k)*(theta(k)*vim(nb_phasis)-vim(k))/phase(k)
                vi(k)=vim(k)+dvin
                vimoy=vimoy+phase(k)*vi(k)
            else
                vi(k)=0.d0
            endif
        end do
!
! 2.7 - RESTAURATION D ORIGINE VISQUEUSE
!
        cmoy=0.d0
        mmoy=0.d0
        do k = 1, nb_phasis
            cmoy=cmoy+phase(k)*c(k)
            mmoy=mmoy+phase(k)*m(k)
        end do
!
        cr=cmoy*vimoy
        if (cr .le. 0.d0) then
            ds=0.d0
        else
            ds= dt*(cr**mmoy)
        endif
!
        do k = 1, nb_phasis
            if (phase(k) .gt. 0.d0) then
                vi(k)=vi(k)-ds
                if (vi(k) .le. 0.d0) vi(k)=0.d0
            endif
        end do
!
! 2.8 - PLASTICITE DE TRANSFORMATION
!
        trans = 0.d0
        if (compor(1)(1:12) .eq. 'META_P_IL_PT' .or. compor(1)(1: 13) .eq.&
            'META_P_INL_PT' .or. compor(1)(1:15) .eq. 'META_P_IL_PT_RE' .or.&
            compor(1)(1:16) .eq. 'META_P_INL_PT_RE' .or. compor(1)(1:12) .eq.&
            'META_V_IL_PT' .or. compor(1)(1:13) .eq. 'META_V_INL_PT' .or.&
            compor(1)(1:15) .eq. 'META_V_IL_PT_RE' .or. compor(1) (1:16) .eq.&
            'META_V_INL_PT_RE') then
            nomres(1) = 'F1_K'
            nomres(2) = 'F2_K'
            nomres(3) = 'F1_D_F_META'
            nomres(4) = 'F2_D_F_META'
            call rcvalb(fami, kpg, ksp, poum, imat,&
                        ' ', 'META_PT', 0, ' ', [0.d0],&
                        2, nomres, valres, icodre, 2)
            do k = 1, nb_phasis-1
                kpt (k) = valres(k)
                zvarim = phasm(k)
                zvarip = phase(k)
                deltaz = (zvarip - zvarim)
                if (deltaz .gt. 0.d0) then
                    j = 2+k
                    call rcvalb(fami, 1, 1, '+', imat,&
                                ' ', 'META_PT', 1, 'META', [zalpha],&
                                1, nomres(j), valres(j), icodre( j), 2)
                    trans = trans + kpt(k)*valres(j)*(zvarip- zvarim)
                endif
            end do
        endif
    else
        do k = 1, nb_phasis
            vi(k)=vim(k)
        end do
    endif
!
! 2.9 - CALCUL DE HMOY ET RMOY (ON INCLUE LE SIGY)
!
    if (compor(1)(1:9) .eq. 'META_P_IL' .or. compor(1)(1:9) .eq. 'META_V_IL') then
        nomres(1) ='F1_D_SIGM_EPSI'
        nomres(2) ='F2_D_SIGM_EPSI'
        nomres(3) ='C_D_SIGM_EPSI'
        call rcvalb(fami, kpg, ksp, poum, imat,&
                    ' ', 'META_ECRO_LINE', 0, ' ', [0.d0],&
                    3, nomres, h, icodre, 2)
        h(1)=h(1)*e/(e-h(1))
        h(2)=h(2)*e/(e-h(2))
        h(3)=h(3)*e/(e-h(3))
        do k = 1, nb_phasis
            r(k)=h(k)*vi(k)+sy(k)
        end do
    endif
!
    if (compor(1)(1:10) .eq. 'META_P_INL' .or. compor(1)(1:10) .eq. 'META_V_INL') then
        nomcle(1)='SIGM_F1'
        nomcle(2)='SIGM_F2'
        nomcle(3)='SIGM_C'
        if (iret1 .eq. 1) then
            call utmess('F', 'COMPOR5_40',sk='SIGM_*')
        endif
        do k = 1, nb_phasis
            call rctrac(imat, 2, nomcle(k), temp, jprol,&
                        jvale, nbval( k), rbid)
            call rcfonc('V', 2, jprol, jvale, nbval(k),&
                        p = vi(k), rp = r(k), rprim = h(k))
            r(k) = r(k) + sy(k)
        end do
        maxval = max(nbval(1),nbval(2),nbval(3))
    endif
!
    if (zalpha .gt. 0.d0) then
        rmoy=phase(1)*r(1)+phase(2)*r(2)
        rmoy = rmoy/zalpha
        hmoy=phase(1)*h(1)+phase(2)*h(2)
        hmoy = hmoy/zalpha
    else
        rmoy = 0.d0
        hmoy = 0.d0
    endif
    rmoy =(1.d0-fmel(1))*r(nb_phasis)+fmel(1)*rmoy
    hmoy = (1.d0-fmel(1))*h(nb_phasis)+fmel(1)*hmoy
!

!
! ********************************
! 3 - DEBUT DE L ALGORITHME
! ********************************
!
    trdeps = (deps(1)+deps(2)+deps(3))/3.d0
    trepsm = (epsm(1)+epsm(2)+epsm(3))/3.d0
    trsigm = (sigm(1)+sigm(2)+sigm(3))/3.d0
    trsigp = troisk*(trepsm+trdeps)-troisk*epsth
!
    do i = 1, ndimsi
        dvdeps(i) = deps(i) - trdeps * kron(i)
        dvsigm(i) = sigm(i) - trsigm * kron(i)
    end do
!
    sieleq = 0.d0
    do i = 1, ndimsi
        sigel(i) = deuxmu*dvsigm(i)/deumum + deuxmu*dvdeps(i)
        sieleq = sieleq + sigel(i)**2
    end do
    sieleq = sqrt(1.5d0*sieleq)
!
    if (sieleq .gt. 0.d0) then
        do i = 1, ndimsi
            sig0(i) = sigel(i)/sieleq
        end do
    else
        do i = 1, ndimsi
            sig0(i) = 0.d0
        end do
    endif
!
! ************************
! 4 - RESOLUTION
! ************************
!
    if (resi) then
!
! 4.2.1 - CALCUL DE DP
!
        seuil= sieleq-(1.5d0*deuxmu*trans+1.d0)*rmoy
!
        if (seuil .lt. 0.d0) then
            vip(4) = 0.d0
            dp = 0.d0
        else
            vip(4) = 1.d0
            call nzcalc(crit, phase, nb_phasis, fmel(1), seuil,&
                        dt, trans, hmoy, deuxmu, eta,&
                        unsurn, dp, iret)
            if (iret .eq. 1) goto 999
!
! DANS LE CAS NON LINEAIRE
! VERIFICATION QU ON EST DANS LE BON INTERVALLE
!
            if (compor(1)(1:10) .eq. 'META_P_INL' .or. compor(1)(1: 10) .eq.&
                'META_V_INL') then
!
                do j = 1, maxval
                    test=0
                    do k = 1, nb_phasis
                        if (phase(k) .gt. 0.d0) then
                            vip(k)=vi(k)+dp
                            hplus(k)=h(k)
                            call rctrac(imat, 2, nomcle(k), temp, jprol,&
                                        jvale, nbval(k), rbid)
                            call rcfonc('V', 2, jprol, jvale, nbval(k),&
                                        p = vip(k), rp = r(k), rprim = h(k))
                            r(k) = r(k) + sy(k)
                            if (abs(h(k)-hplus(k)) .gt. r8prem()) test= 1
                        endif
                    end do
                    if (test .eq. 0) goto 600
!
                    hmoy=0.d0
                    rmoy=0.d0
                    if (zalpha .gt. 0.d0) then
                        do k = 1, nb_phasis-1
                            if (phase(k) .gt. 0.d0) then
                                rmoy = rmoy + phase(k)*(r(k)-h(k)* dp)
                                hmoy = hmoy + phase(k)*h(k)
                            endif
                        end do
                        rmoy=fmel(1)*rmoy/zalpha
                        hmoy=fmel(1)*hmoy/zalpha
                    endif
                    if (phase(nb_phasis) .gt. 0.d0) then
                        rmoy = (1.d0-fmel(1))*(r(nb_phasis)-h(nb_phasis)*dp)+rmoy
                        hmoy = (1.d0-fmel(1))*h(nb_phasis)+hmoy
                    endif
                    seuil= sieleq - (1.5d0*deuxmu*trans + 1.d0)*&
                    rmoy
                    call nzcalc(crit, phase, nb_phasis, fmel(1), seuil,&
                                dt, trans, hmoy, deuxmu, eta,&
                                unsurn, dp, iret)
                    if (iret .eq. 1) goto 999
                end do
                ASSERT((test.ne.1).or.(j.ne.maxval))
600             continue
            endif
        endif
!
! 4.2.2 - CALCUL DE SIGMA
!
        plasti = vip(4)
!
        do i = 1, ndimsi
            dvsigp(i) = sigel(i) - 1.5d0*deuxmu*dp*sig0(i)
            dvsigp(i) = dvsigp(i)/(1.5d0*deuxmu*trans + 1.d0)
            sigp(i) = dvsigp(i) + trsigp*kron(i)
        end do
!
! 4.2.3 - CALCUL DE VIP ET RMOY
!
        do k = 1, nb_phasis
            if (phase(k) .gt. 0.d0) then
                vip(k)=vi(k)+dp
            else
                vip(k)=0.d0
            endif
        end do
!
        vip(5)=0.d0
        if (phase(nb_phasis) .gt. 0.d0) then
            if (compor(1)(1:9) .eq. 'META_P_IL' .or. compor(1)(1:9) .eq. 'META_V_IL') then
                vip(5)=vip(5)+(1-fmel(1))*h(nb_phasis)*vip(nb_phasis)
            endif
            if (compor(1)(1:10) .eq. 'META_P_INL' .or. compor(1)(1:10) .eq.'META_V_INL') then
                vip(5)=vip(5)+(1-fmel(1))*(r(nb_phasis)-sy(nb_phasis))
            endif
        endif
!
        if (zalpha .gt. 0.d0) then
            do k = 1, nb_phasis-1
                if (compor(1)(1:9) .eq. 'META_P_IL' .or. compor(1)(1: 9) .eq.&
                    'META_V_IL') then
                    vip(5)=vip(5)+fmel(1)*phase(k)*h(k)*vip(k)/&
                    zalpha
                endif
                if (compor(1)(1:10) .eq. 'META_P_INL' .or. compor(1)( 1:10) .eq.&
                    'META_V_INL') then
                    vip(5)=vip(5)+fmel(1)*phase(k)*(r(k)-sy(k))/&
                    zalpha
                endif
            end do
        endif
    endif
!
! *******************************
! 5 - MATRICE TANGENTE DSIGDF
! *******************************
!
    if (rigi) then
!
        mode=2
        if (compor(1)(1:6) .eq. 'META_V') mode=1
!
! 5.1 - MATRICE ELASTIQUE
!
        call matini(6, 6, 0.d0, dsidep)
!
        do i = 1, ndimsi
            dsidep(i,i) =1.d0
        end do
!
        do i = 1, 3
            do j = 1, 3
                dsidep(i,j) = dsidep(i,j)-1.d0/3.d0
            end do
        end do
!
        if (option(1:9) .eq. 'FULL_MECA') then
            coef1=(1.5d0*deuxmu*trans+1.d0)
        else
            coef1=1.d0
        endif
!
        do i = 1, ndimsi
            do j = 1, ndimsi
                dsidep(i,j)=dsidep(i,j)*deuxmu/coef1
            end do
        end do
!
! 5.2 - PARTIE PLASTIQUE
!
        b=1.d0
        coef2 =0.d0
        coef3=0.d0

        if (plasti .ge. 0.5d0) then
            if (option(1:9) .eq. 'FULL_MECA') then
                sigeps = 0.d0
                do i = 1, ndimsi
                    sigeps = sigeps + dvsigp(i)*dvdeps(i)
                end do
                if ((mode.eq.1) .or. ((mode .eq. 2) .and. ( sigeps.ge.0.d0))) then
                    b = 1.d0-(1.5d0*deuxmu*dp/sieleq)
                    dv=0.d0
                    if (mode .eq. 1) then
                        do k = 1, nb_phasis
                            n0(k) = (1-n(k))/n(k)
                        end do
                        dv = (1-fmel(1))*phase(nb_phasis)*(eta(nb_phasis)/n(nb_phasis)/dt) *&
                             ((dp/dt)**n0(nb_phasis))
                        if (zalpha .gt. 0.d0) then
                            do k = 1, nb_phasis-1
                                if (phase(k) .gt. 0.d0) then
                                    dv = dv+ fmel(1)*( phase(k)/zalpha) * (eta(k)/ n(k)/dt)*&
                                         ((dp/dt)**n0(k))
                                endif
                            end do
                        endif
                    endif
                    coef2 = hmoy +dv
                    coef2 = (1.5d0*deuxmu*trans+1.d0)*coef2
                    coef2 = (1.5d0*deuxmu)+coef2
                    coef2 = 1/coef2 - dp/sieleq
                    coef2 =((1.5d0*deuxmu)**2)*coef2
                endif
            endif
!
            if (option(1:14) .eq. 'RIGI_MECA_TANG') then
                if (mode .eq. 2) coef2 = ( (1.5d0*deuxmu)**2 )/( 1.5d0*deuxmu+hmoy )
            endif
!
            coef3 = coef2/coef1
!
        endif
!
        do i = 1, ndimsi
            do j = 1, ndimsi
                dsidep(i,j) = dsidep(i,j)*b
            end do
        end do
!
        do i = 1, 3
            do j = 1, 3
                dsidep(i,j) = dsidep(i,j)+troisk/3.d0
            end do
        end do
!
        do i = 1, ndimsi
            do j = 1, ndimsi
                dsidep(i,j) = dsidep(i,j)- coef3*sig0(i)*sig0(j)
            end do
        end do
!
    endif
!
999 continue
!
end subroutine
