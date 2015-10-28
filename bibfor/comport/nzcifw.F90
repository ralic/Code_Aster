subroutine nzcifw(fami, kpg, ksp, ndim, imat,&
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
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
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
! aslint: disable=W1501
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
    real(kind=8), intent(in) :: vim(37)
    character(len=16), intent(in) :: option
    real(kind=8), intent(out) :: sigp(*)
    real(kind=8), intent(out) :: vip(37)
    real(kind=8), intent(out) :: dsidep(6, 6)
    integer, intent(out) :: iret
!
! --------------------------------------------------------------------------------------------------
!
! Comportment
!
! META_P_C* / META_V_C* for small strains and steel metallurgy
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
    integer :: nb_phasis, meta_id
    integer :: ndimsi, i, j, k, l, mode
    real(kind=8) :: phase(5), phasm(5), zalpha
    real(kind=8) :: dt
    real(kind=8) :: epsth, e, deuxmu, deumum, troisk
    real(kind=8) :: fmel(1), sy(5), symoy, h(5), hmoy, rprim
    real(kind=8) :: theta(8)
    real(kind=8) :: eta(5), n(5), unsurn(5), c(5), m(5), cmoy, mmoy, cr
    real(kind=8) :: dz(4), dz1(4), dz2(4), vi(30), dvin, vimt(30)
    real(kind=8) :: xmoy(6), ds(6), xmoyeq
    real(kind=8) :: trans, kpt(4), zvarim, zvarip, deltaz
    real(kind=8) :: trepsm, trdeps, trsigm, trsigp
    real(kind=8) :: dvdeps(6), dvsigm(6), dvsigp(6)
    real(kind=8) :: sigel(6), sigel2(6), sig0(6), sieleq, sigeps
    real(kind=8) :: plasti, dp, seuil
    real(kind=8) :: coef1, coef2, coef3, dv, n0(5), b
    real(kind=8) :: valres(20), epsth_meta(2)
    character(len=1) :: poum
    integer :: icodre(20)
    character(len=16) :: nomres(20)
    aster_logical :: resi, rigi
    real(kind=8), parameter :: kron(6) = (/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/)
    real(kind=8), parameter :: rac2 = sqrt(2.d0)
!
! --------------------------------------------------------------------------------------------------
!
    do i = 1, 2*ndim
        sigp(i) = 0.d0
    end do
    vip(1:37)       = 0.d0
    dsidep(1:6,1:6) = 0.d0
    ndimsi          = 2*ndim
    iret            = 0
    resi            = option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL'
    rigi            = option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL'
    dt              = instap-instam
!
! - Get metallurgy type
!
    call get_meta_id(meta_id, nb_phasis)
    ASSERT(meta_id.eq.1)
    ASSERT(nb_phasis.eq.5)
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
    call verift(fami, kpg, ksp, poum, imat,&
                epsth_meta_=epsth_meta)
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
    call rcvalb(fami, kpg, ksp, '-', imat,&
                ' ', 'ELAS_META', 0, ' ', [0.d0],&
                6, nomres, valres, icodre, 2)
    deumum = valres(1)/(1.d0+valres(2))
    call rcvalb(fami, kpg, ksp, poum, imat,&
                ' ', 'ELAS_META', 0, ' ', [0.d0],&
                6, nomres, valres, icodre, 2)
    epsth = phase(nb_phasis)*(epsth_meta(1)-(1.d0-valres(5))*valres(6)) +&
            zalpha*(epsth_meta(2) + valres(5)*valres(6))
    e = valres(1)
    deuxmu = e/(1.d0+valres(2))
    troisk = e/(1.d0-2.d0*valres(2))
    plasti=vim(37)
!
! 2.2 - LOI DES MELANGES
!
    if (compor(1)(1:6) .eq. 'META_P') then
        nomres(1) ='F1_SY'
        nomres(2) ='F2_SY'
        nomres(3) ='F3_SY'
        nomres(4) ='F4_SY'
        nomres(5) ='C_SY'
        nomres(6) ='SY_MELAN'
    endif
    if (compor(1)(1:6) .eq. 'META_V') then
        nomres(1) ='F1_S_VP'
        nomres(2) ='F2_S_VP'
        nomres(3) ='F3_S_VP'
        nomres(4) ='F4_S_VP'
        nomres(5) ='C_S_VP'
        nomres(6) ='S_VP_MEL'
    endif
    call rcvalb(fami, 1, 1, '+', imat,&
                ' ', 'ELAS_META', 1, 'META', [zalpha],&
                1, nomres(6), fmel, icodre(6), 0)
    if (icodre(6) .ne. 0) fmel(1) = zalpha
!
! 2.3 - LIMITE D ELASTICITE
!
    call rcvalb(fami, kpg, ksp, poum, imat,&
                ' ', 'ELAS_META', 0, ' ', [0.d0],&
                5, nomres, sy, icodre, 2)
!
! 2.4 - PENTE D ECROUISSAGE
!
    nomres(1) ='F1_D_SIGM_EPSI'
    nomres(2) ='F2_D_SIGM_EPSI'
    nomres(3) ='F3_D_SIGM_EPSI'
    nomres(4) ='F4_D_SIGM_EPSI'
    nomres(5) ='C_D_SIGM_EPSI'
    call rcvalb(fami, kpg, ksp, poum, imat,&
                ' ', 'META_ECRO_LINE', 0, ' ', [0.d0],&
                5, nomres, h, icodre, 2)
    h(1)=(2.d0/3.d0)*h(1)*e/(e-h(1))
    h(2)=(2.d0/3.d0)*h(2)*e/(e-h(2))
    h(3)=(2.d0/3.d0)*h(3)*e/(e-h(3))
    h(4)=(2.d0/3.d0)*h(4)*e/(e-h(4))
    h(5)=(2.d0/3.d0)*h(5)*e/(e-h(5))
    hmoy=0.d0
    do k = 1, nb_phasis
        hmoy=hmoy+phase(k)*h(k)
    end do
!
    if (resi) then
!
! 2.5 - RESTAURATION D ECROUISSAGE
!
        if (compor(1)(1:15) .eq. 'META_P_CL_PT_RE' .or. compor(1)( 1:12) .eq.&
            'META_P_CL_RE' .or. compor(1)(1:12) .eq. 'META_V_CL_RE' .or.&
            compor(1)(1:15) .eq. 'META_V_CL_PT_RE') then
            nomres(1) ='C_F1_THETA'
            nomres(2) ='C_F2_THETA'
            nomres(3) ='C_F3_THETA'
            nomres(4) ='C_F4_THETA'
            nomres(5) ='F1_C_THETA'
            nomres(6) ='F2_C_THETA'
            nomres(7) ='F3_C_THETA'
            nomres(8) ='F4_C_THETA'
            call rcvalb(fami, kpg, ksp, poum, imat,&
                        ' ', 'META_RE', 0, '  ', [0.d0],&
                        8, nomres, theta, icodre, 2)
        else
            do i = 1, 8
                theta(i)=1.d0
            end do
        endif
!
! 2.6 - VISCOSITE
!
        if (compor(1)(1:6) .eq. 'META_V') then
            nomres(1) = 'F1_ETA'
            nomres(2) = 'F2_ETA'
            nomres(3) = 'F3_ETA'
            nomres(4) = 'F4_ETA'
            nomres(5) = 'C_ETA'
            nomres(6) = 'F1_N'
            nomres(7) = 'F2_N'
            nomres(8) = 'F3_N'
            nomres(9) = 'F4_N'
            nomres(10) = 'C_N'
            nomres(11) ='F1_C'
            nomres(12) ='F2_C'
            nomres(13) ='F3_C'
            nomres(14) ='F4_C'
            nomres(15) ='C_C'
            nomres(16) = 'F1_M'
            nomres(17) = 'F2_M'
            nomres(18) = 'F3_M'
            nomres(19) = 'F4_M'
            nomres(20) = 'C_M'
            call rcvalb(fami, kpg, ksp, poum, imat,&
                        ' ', 'META_VISC', 0, ' ', [0.d0],&
                        10, nomres, valres, icodre, 2)
            call rcvalb(fami, kpg, ksp, poum, imat,&
                        ' ', 'META_VISC', 0, ' ', [0.d0],&
                        10, nomres(11), valres(11), icodre(11), 0)
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
! 2.7 - CALCUL DE VIM+DG
!
        do k = 1, nb_phasis-1
            dz(k)= phase(k)-phasm(k)
            if (dz(k) .ge. 0.d0) then
                dz1(k)=dz(k)
                dz2(k)=0.d0
            else
                dz1(k)=0.d0
                dz2(k)=-dz(k)
            endif
        end do
        if (phase(nb_phasis) .gt. 0.d0) then
            do i = 1, ndimsi
                dvin=0.d0
                do k = 1, nb_phasis-1
                    l=i+(k-1)*6
                    dvin = dvin + dz2(k)*(theta(4+k)*vim(l)-vim( 24+i))/phase(nb_phasis)
                end do
                vi(24+i) = vim(24+i)+dvin
                if ((vi(24+i)*vim(24+i)) .lt. 0.d0) vi(24+i)=0.d0
            end do
        else
            do i = 1, ndimsi
                vi(24+i)=0.d0
            end do
        endif
        do k = 1, nb_phasis-1
            do i = 1, ndimsi
                l=i+(k-1)*6
                if (phase(k) .gt. 0.d0) then
                    dvin = dz1(k)*(theta(k)*vim(24+i)-vim(l))/ phase(k)
                    vi(l) = vim(l)+dvin
                    if ((vi(l)*vim(l)) .lt. 0.d0) vi(l)=0.d0
                else
                    vi(l) = 0.d0
                endif
            end do
        end do
!
!
!    -  MISE AU FORMAT DES CONTRAINTES DE RAPPEL
!
        do i = 4, ndimsi
            do k = 1, nb_phasis
                l=i+(k-1)*6
                vi(l)=vi(l)*rac2
            end do
        end do
!
! 2.8 - RESTAURATION D ORIGINE VISQUEUSE
!
        do i = 1, ndimsi
            xmoy(i)=0.d0
            do k = 1, nb_phasis
                l=i+(k-1)*6
                xmoy(i)=xmoy(i)+phase(k)*h(k)*vi(l)
            end do
        end do
        xmoyeq = 0.d0
        do i = 1, ndimsi
            xmoyeq=xmoyeq+xmoy(i)**2.d0
        end do
        xmoyeq= sqrt(1.5d0*xmoyeq)
        cmoy=0.d0
        mmoy=0.d0
        do k = 1, nb_phasis
            cmoy=cmoy+phase(k)*c(k)
            mmoy=mmoy+phase(k)*m(k)
        end do
        cr=cmoy*xmoyeq
        if (xmoyeq .gt. 0.d0) then
            do i = 1, ndimsi
                ds(i)= 3.d0*dt*(cr**mmoy)*xmoy(i)/(2.d0*xmoyeq)
            end do
        else
            do i = 1, ndimsi
                ds(i)= 0.d0
            end do
        endif
        do k = 1, nb_phasis
            do i = 1, ndimsi
                l=i+(k-1)*6
                if (phase(k) .gt. 0.d0) then
                    vimt(l)=vi(l)
                    vi(l)=vi(l)-ds(i)
                    if ((vi(l)*vimt(l)) .lt. 0.d0) vi(l)=0.d0
                endif
            end do
        end do
!
! 2.9 - PLASTICITE DE TRANSFORMATION
!
        trans = 0.d0
        if (compor(1)(1:12) .eq. 'META_P_CL_PT' .or. compor(1)(1: 15) .eq.&
            'META_P_CL_PT_RE' .or. compor(1)(1:12) .eq. 'META_V_CL_PT' .or.&
            compor(1)(1:15) .eq. 'META_V_CL_PT_RE') then
            nomres(1) = 'F1_K'
            nomres(2) = 'F2_K'
            nomres(3) = 'F3_K'
            nomres(4) = 'F4_K'
            nomres(5) = 'F1_D_F_META'
            nomres(6) = 'F2_D_F_META'
            nomres(7) = 'F3_D_F_META'
            nomres(8) = 'F4_D_F_META'
            call rcvalb(fami, kpg, ksp, poum, imat,&
                        ' ', 'META_PT', 0, ' ', [0.d0],&
                        4, nomres, valres, icodre, 2)
            do k = 1, nb_phasis-1
                kpt (k) = valres(k)
                zvarim = phasm(k)
                zvarip = phase(k)
                deltaz = (zvarip - zvarim)
                if (deltaz .gt. 0.d0) then
                    j = 4+k
                    call rcvalb(fami, 1, 1, '+', imat,&
                                ' ', 'META_PT', 1, 'META', [zalpha],&
                                1, nomres(j), valres(j), icodre( j), 2)
                    trans = trans + kpt(k)*valres(j)*(zvarip- zvarim)
                endif
            end do
        endif
    else
!
!           MISE AU FORMAT DES CONTRAINTES DE RAPPEL
!
        do k = 1, nb_phasis
            do i = 1, ndimsi
                l=i+(k-1)*6
                vi(l)=vim(l)
                if (i .gt. 3) then
                    vi(l)=vi(l)*rac2
                end if
            end do
        end do
        trans=0.d0
        do i = 1, ndimsi
            xmoy(i)=0.d0
            do k = 1, nb_phasis
                l=i+(k-1)*6
                xmoy(i)=xmoy(i)+phase(k)*h(k)*vi(l)
            end do
        end do
    endif
!
! 2.10 - CALCUL DE SYMOY
!
    if (zalpha .gt. 0.d0) then
        symoy = phase(1)*sy(1)+phase(2)*sy(2)+phase(3)*sy(3) + phase(4)*sy(4)
        symoy = symoy/zalpha
    else
        symoy = 0.d0
    endif
    symoy =(1.d0-fmel(1))*sy(nb_phasis)+fmel(1)*symoy
!
! ********************************
! 3 - DEBUT DE L ALGORITHME
! ********************************
!
    trdeps = (deps(1)+deps(2)+deps(3))/3.d0
    trepsm = (epsm(1)+epsm(2)+epsm(3))/3.d0
    trsigm = (sigm(1)+sigm(2)+sigm(3))/3.d0
    trsigp = troisk*(trepsm+trdeps)-troisk*epsth
    do i = 1, ndimsi
        dvdeps(i) = deps(i) - trdeps * kron(i)
        dvsigm(i) = sigm(i) - trsigm * kron(i)
    end do
!
    sieleq = 0.d0
    do i = 1, ndimsi
        sigel(i) = deuxmu*dvsigm(i)/deumum + deuxmu*dvdeps(i)
        sigel2(i)= sigel(i)-(1.5d0*deuxmu*trans+1.d0)*xmoy(i)
        sieleq = sieleq + sigel2(i)**2
    end do
    sieleq = sqrt(1.5d0*sieleq)
    if (sieleq .gt. 0.d0) then
        do i = 1, ndimsi
            sig0(i) = sigel2(i)/sieleq
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
        seuil= sieleq-(1.5d0*deuxmu*trans+1.d0)*symoy
!
        if (seuil .lt. 0.d0) then
            vip(37) = 0.d0
            dp = 0.d0
        else
            vip(37) = 1.d0
            rprim=3.d0*hmoy/2.d0
            if (compor(1)(1:6) .eq. 'META_P') then
                dp=seuil/(1.5d0*deuxmu+(1.5d0*deuxmu*trans+1.d0)*rprim)
            else
                call nzcalc(crit, phase, nb_phasis, fmel(1), seuil,&
                            dt, trans, rprim, deuxmu, eta,&
                            unsurn, dp, iret)
                if (iret .eq. 1) goto 999
            endif
        endif
!
! 4.2.2 - CALCUL DE SIGMA
!
        plasti=vip(37)
!
        do i = 1, ndimsi
            dvsigp(i) = sigel(i) - 1.5d0*deuxmu*dp*sig0(i)
            dvsigp(i) = dvsigp(i)/(1.5d0*deuxmu*trans + 1.d0)
            sigp(i) = dvsigp(i) + trsigp*kron(i)
        end do
!
! 4.2.3 - CALCUL DE VIP ET XMOY
!
        do k = 1, nb_phasis
            do i = 1, ndimsi
                l=i+(k-1)*6
                if (phase(k) .gt. 0.d0) then
                    vip(l) = vi(l)+3.d0*dp*sig0(i)/2.d0
                    if (i .gt. 3) then
                        vip(l) = vip(l)/rac2
                    end if
                else
                    vip(l) = 0.d0
                endif
            end do
        end do
        do i = 1, ndimsi
            vip(30+i)= xmoy(i)+3.d0*hmoy*dp*sig0(i)/2.d0
            if (i .gt. 3) then
                vip(30+i) = vip(30+i)/rac2
            end if
        end do
    endif
!
! *******************************
! 5 - MATRICE TANGENTE DSIGDF
! *******************************
!
    if (rigi) then
        mode=2
        if (compor(1)(1:6) .eq. 'META_V') mode=1
        call matini(6, 6, 0.d0, dsidep)
        do i = 1, ndimsi
            dsidep(i,i) = 1.d0
        end do
        do i = 1, 3
            do j = 1, 3
                dsidep(i,j) = dsidep(i,j)-1.d0/3.d0
            end do
        end do
        if (option(1:9) .eq. 'FULL_MECA') then
            coef1 = (1.5d0*deuxmu*trans+1.d0)
        else
            coef1 = 1.d0
        endif
        do i = 1, ndimsi
            do j = 1, ndimsi
                dsidep(i,j) = dsidep(i,j)*deuxmu/coef1
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
                    dvsigp(i)=dvsigp(i)-xmoy(i)
                    sigeps = sigeps + dvsigp(i)*dvdeps(i)
                end do
                if ((mode .eq.1) .or. ((mode .eq. 2) .and. (sigeps.ge.0.d0))) then
                    b = 1.d0-(1.5d0*deuxmu*dp/sieleq)
                    dv = 0.d0
                    if (mode .eq. 1) then
                        do k = 1, nb_phasis
                            n0(k) = (1-n(k))/n(k)
                        end do
                        dv = (1-fmel(1))*phase(nb_phasis)*(eta(nb_phasis)/n(nb_phasis)/dt) *&
                             ((dp/dt)**n0(nb_phasis))
                        if (zalpha .gt. 0.d0) then
                            do k = 1, nb_phasis-1
                                if (phase(k) .gt. 0.d0) dv = dv+ fmel(1)*( phase(k)/zalpha) *&
                                                             & (eta(k)/ n(k)/dt)*((dp/dt)**n0&
                                                             &(k) )
                            end do
                        endif
                    endif
                    coef2 = 3.d0*hmoy/2.d0 + dv
                    coef2 = (1.5d0*deuxmu*trans+1.d0)*coef2
                    coef2 = (1.5d0*deuxmu)+coef2
                    coef2 = 1/coef2 - dp/sieleq
                    coef2 =((1.5d0*deuxmu)**2)*coef2
                endif
            endif
            if (option(1:14) .eq. 'RIGI_MECA_TANG') then
                if (mode .eq. 2) coef2 = ( (1.5d0*deuxmu)**2 )/( 1.5d0*deuxmu+1.5d0*hmoy )
            endif
            coef3 = coef2/coef1
        endif
        do i = 1, ndimsi
            do j = 1, ndimsi
                dsidep(i,j) = dsidep(i,j)*b
            end do
        end do
        do i = 1, 3
            do j = 1, 3
                dsidep(i,j) = dsidep(i,j)+troisk/3.d0
            end do
        end do
        do i = 1, ndimsi
            do j = 1, ndimsi
                dsidep(i,j) = dsidep(i,j)- coef3*sig0(i)*sig0(j)
            end do
        end do
    endif
!
999 continue
!
end subroutine
