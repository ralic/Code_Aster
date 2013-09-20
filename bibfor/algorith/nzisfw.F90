subroutine nzisfw(fami, kpg, ksp, ndim, imat,&
                  compor, crit, instam, instap, epsm,&
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
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
    integer :: ndim, imat, iret, kpg, ksp
    character(len=16) :: compor(*), option
    character(len=*) :: fami
    real(kind=8) :: crit(3), instam, instap
    real(kind=8) :: epsm(6), deps(6)
    real(kind=8) :: sigm(6), vim(7), sigp(6), vip(7), dsidep(6, 6)
!
! ----------------------------------------------------------------------
!     REALISE LA LOI DE VON MISES ISOTROPE ET ELASTIQUE POUR LES
!     ELEMENTS METALLURGIQUES EN PETITES DEFORMATIONS
!
!     AVEC OU SANS PLASTICITE DE TRANSFORMATION
!     AVEC OU SANS RESTAURATION ECROUISSAGE METALLURGIQUE
!     RELATION DE COMPORTEMENT ELASTO-PLASTIQUE OU
!                               ELASTO-VISCOPLASTIQUE
!     ECROUISSAGE ISOTROPE LINEAIRE OU NON LINEAIRE
!
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
!.......................................................................
!
    integer :: jprol, jvale, nbval(5), maxval, nz
    integer :: ndimsi, i, j, k, mode, ire2, iret1, iret2
!
    real(kind=8) :: phase(5), phasm(5), zalpha
    real(kind=8) :: temp, dt
!
    real(kind=8) :: epsth, e, deuxmu, deumum, troisk
    real(kind=8) :: fmel(1), sy(5), h(5), hmoy, hplus(5), r(5), rmoy
    real(kind=8) :: theta(8)
    real(kind=8) :: eta(5), n(5), unsurn(5), c(5), m(5), cmoy, mmoy, cr
    real(kind=8) :: dz(4), dz1(4), dz2(4), vi(5), dvin, vimoy, ds
    real(kind=8) :: trans, kpt(4), zvarim, zvarip, deltaz
!
    real(kind=8) :: trepsm, trdeps, trsigm, trsigp
    real(kind=8) :: dvdeps(6), dvsigm(6), dvsigp(6)
    real(kind=8) :: sigel(6), sig0(6), sieleq, sigeps
!
    real(kind=8) :: plasti, dp, seuil
!
    real(kind=8) :: coef1, coef2, coef3, dv, n0(5), b
!
    real(kind=8) :: rbid, precr
    real(kind=8) :: kron(6)
    real(kind=8) :: valres(20), epsthe(2)
!
    character(len=1) :: c1
    integer :: icodre(20), test
    character(len=8) :: nomres(20), nomcle(5), acier(4), materi
!
    logical :: resi, rigi
!
    data         kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
!
    data acier /'PFERRITE','PPERLITE','PBAINITE','PMARTENS'/
!
! *******************
! 1 - INITIALISATION
! *******************
!
    resi = option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL'
    rigi = option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL'
!
    if (ndim .eq. 2) then
        ndimsi=4
    else
        ndimsi=6
    endif
!
    dt = instap-instam
    materi = ' '
!
! 1.1 - NOMBRE DE PHASES
!
    nz=5
!
! 1.2 - RECUPERATION DES PHASES METALLURGIQUES
!
    if (resi) then
!
        c1='+'
        do 5 k = 1, nz-1
            call rcvarc(' ', acier(k), c1, fami, kpg,&
                        ksp, phase(k), ire2)
            if (ire2 .eq. 1) phase(k)=0.d0
            call rcvarc(' ', acier(k), '-', fami, kpg,&
                        ksp, phasm(k), ire2)
            if (ire2 .eq. 1) phasm(k)=0.d0
  5     continue
!
    else
!
        c1='-'
        do 10 k = 1, nz-1
            call rcvarc(' ', acier(k), c1, fami, kpg,&
                        ksp, phase(k), ire2)
            if (ire2 .eq. 1) phase(k)=0.d0
 10     continue
!
    endif
!
    call rcvarc(' ', 'TEMP', c1, fami, kpg,&
                ksp, temp, iret2)
    call verift(fami, kpg, ksp, c1, imat,&
                materi, 'ELAS_META', iret1, ndim=2, vepsth=epsthe)
    zalpha=phase(1)+phase(2)+phase(3)+phase(4)
    phase(nz)=1.d0-zalpha
!
! 1.3 - TEST SUR LES PHASES
!
    precr=r8prem()
    do 15 k = 1, nz
        if (phase(k) .le. precr) phase(k)=0.d0
        if (phase(k) .ge. 1.d0) phase(k)=1.d0
 15 continue
    if (zalpha .le. precr) zalpha=0.d0
    if (zalpha .ge. 1.d0) zalpha=1.d0
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
    call rcvalb(fami, kpg, ksp, c1, imat,&
                ' ', 'ELAS_META', 0, ' ', [0.d0],&
                6, nomres, valres, icodre, 2)
!
    epsth = phase(nz)*(epsthe(1)-(1.d0-valres(5))*valres(6)) + zalpha*(epsthe(2) + valres(5)*valr&
            &es(6))
    e = valres(1)
    deuxmu = e/(1.d0+valres(2))
    troisk = e/(1.d0-2.d0*valres(2))
!
    if (compor(1)(1:4) .eq. 'META') then
!
        plasti=vim(6)
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
!
        if (compor(1)(1:6) .eq. 'META_V') then
            nomres(1) ='F1_S_VP'
            nomres(2) ='F2_S_VP'
            nomres(3) ='F3_S_VP'
            nomres(4) ='F4_S_VP'
            nomres(5) ='C_S_VP'
            nomres(6) ='S_VP_MEL'
        endif
!
        call rcvalb(fami, 1, 1, '+', imat,&
                    ' ', 'ELAS_META', 1, 'META', [zalpha],&
                    1, nomres(6), fmel, icodre(6), 0)
        if (icodre(6) .ne. 0) fmel(1) = zalpha
!
! 2.3 - LIMITE D ELASTICITE
!
        call rcvalb(fami, kpg, ksp, c1, imat,&
                    ' ', 'ELAS_META', 0, ' ', [0.d0],&
                    5, nomres, sy, icodre, 2)
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
!
                nomres(1) ='C_F1_THETA'
                nomres(2) ='C_F2_THETA'
                nomres(3) ='C_F3_THETA'
                nomres(4) ='C_F4_THETA'
                nomres(5) ='F1_C_THETA'
                nomres(6) ='F2_C_THETA'
                nomres(7) ='F3_C_THETA'
                nomres(8) ='F4_C_THETA'
!
                call rcvalb(fami, kpg, ksp, c1, imat,&
                            ' ', 'META_RE', 0, '  ', [0.d0],&
                            8, nomres, theta, icodre, 2)
!
            else
!
                do 20 i = 1, 8
                    theta(i)=1.d0
 20             continue
!
            endif
!
! 2.5 - VISCOSITE
!
            if (compor(1)(1:6) .eq. 'META_V') then
!
                nomres(1) = 'F1_ETA'
                nomres(2) = 'F2_ETA'
                nomres(3) = 'F3_ETA'
                nomres(4) = 'F4_ETA'
                nomres(5) = 'C_ETA'
!
                nomres(6) = 'F1_N'
                nomres(7) = 'F2_N'
                nomres(8) = 'F3_N'
                nomres(9) = 'F4_N'
                nomres(10) = 'C_N'
!
                nomres(11) ='F1_C'
                nomres(12) ='F2_C'
                nomres(13) ='F3_C'
                nomres(14) ='F4_C'
                nomres(15) ='C_C'
!
                nomres(16) = 'F1_M'
                nomres(17) = 'F2_M'
                nomres(18) = 'F3_M'
                nomres(19) = 'F4_M'
                nomres(20) = 'C_M'
!
                call rcvalb(fami, kpg, ksp, c1, imat,&
                            ' ', 'META_VISC', 0, ' ', [0.d0],&
                            10, nomres, valres, icodre, 2)
!
                call rcvalb(fami, kpg, ksp, c1, imat,&
                            ' ', 'META_VISC', 0, ' ', [0.d0],&
                            10, nomres(11), valres(11), icodre(11), 0)
!
                do 25 k = 1, nz
                    eta(k) = valres(k)
                    n(k) = valres(nz+k)
                    unsurn(k)=1/n(k)
                    if (icodre(2*nz+k) .ne. 0) valres(2*nz+k)=0.d0
                    c(k) =valres(2*nz+k)
                    if (icodre(3*nz+k) .ne. 0) valres(3*nz+k)=20.d0
                    m(k) = valres(3*nz+k)
 25             continue
!
            else
!
                do 30 k = 1, nz
                    eta(k) = 0.d0
                    n(k)= 20.d0
                    unsurn(k)= 1.d0
                    c(k) = 0.d0
                    m(k) = 20.d0
 30             continue
!
            endif
!
! 2.6 - CALCUL DE VIM+DG-DS ET DE RMOY
!
            do 35 k = 1, nz-1
                dz(k)= phase(k)-phasm(k)
                if (dz(k) .ge. 0.d0) then
                    dz1(k)=dz(k)
                    dz2(k)=0.d0
                else
                    dz1(k)=0.d0
                    dz2(k)=-dz(k)
                endif
 35         continue
!
            if (phase(nz) .gt. 0.d0) then
                dvin=0.d0
                do 40 k = 1, nz-1
                    dvin=dvin+dz2(k)*(theta(4+k)*vim(k)-vim(nz))/&
                    phase(nz)
 40             continue
                vi(nz)=vim(nz)+dvin
                vimoy=phase(nz)*vi(nz)
            else
                vi(nz) = 0.d0
                vimoy=0.d0
            endif
!
            do 45 k = 1, nz-1
                if (phase(k) .gt. 0.d0) then
                    dvin=dz1(k)*(theta(k)*vim(nz)-vim(k))/phase(k)
                    vi(k)=vim(k)+dvin
                    vimoy=vimoy+phase(k)*vi(k)
                else
                    vi(k)=0.d0
                endif
 45         continue
!
! 2.7 - RESTAURATION D ORIGINE VISQUEUSE
!
            cmoy=0.d0
            mmoy=0.d0
            do 50 k = 1, nz
                cmoy=cmoy+phase(k)*c(k)
                mmoy=mmoy+phase(k)*m(k)
 50         continue
!
            cr=cmoy*vimoy
            if (cr .le. 0.d0) then
                ds=0.d0
            else
                ds= dt*(cr**mmoy)
            endif
!
            do 55 k = 1, nz
                if (phase(k) .gt. 0.d0) then
                    vi(k)=vi(k)-ds
                    if (vi(k) .le. 0.d0) vi(k)=0.d0
                endif
 55         continue
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
!
                nomres(1) = 'F1_K'
                nomres(2) = 'F2_K'
                nomres(3) = 'F3_K'
                nomres(4) = 'F4_K'
                nomres(5) = 'F1_D_F_META'
                nomres(6) = 'F2_D_F_META'
                nomres(7) = 'F3_D_F_META'
                nomres(8) = 'F4_D_F_META'
!
                call rcvalb(fami, kpg, ksp, c1, imat,&
                            ' ', 'META_PT', 0, ' ', [0.d0],&
                            4, nomres, valres, icodre, 2)
!
                do 60 k = 1, nz-1
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
 60             continue
!
            endif
!
        else
!
            do 65 k = 1, nz
                vi(k)=vim(k)
 65         continue
!
        endif
!
! 2.9 - CALCUL DE HMOY ET RMOY (ON INCLUE LE SIGY)
!
        if (compor(1)(1:9) .eq. 'META_P_IL' .or. compor(1)(1:9) .eq. 'META_V_IL') then
!
            nomres(1) ='F1_D_SIGM_EPSI'
            nomres(2) ='F2_D_SIGM_EPSI'
            nomres(3) ='F3_D_SIGM_EPSI'
            nomres(4) ='F4_D_SIGM_EPSI'
            nomres(5) ='C_D_SIGM_EPSI'
!
            call rcvalb(fami, kpg, ksp, c1, imat,&
                        ' ', 'META_ECRO_LINE', 0, ' ', [0.d0],&
                        5, nomres, h, icodre, 2)
!
            h(1)=h(1)*e/(e-h(1))
            h(2)=h(2)*e/(e-h(2))
            h(3)=h(3)*e/(e-h(3))
            h(4)=h(4)*e/(e-h(4))
            h(5)=h(5)*e/(e-h(5))
!
            do 70 k = 1, nz
                r(k)=h(k)*vi(k)+sy(k)
 70         continue
!
        endif
!
        if (compor(1)(1:10) .eq. 'META_P_INL' .or. compor(1)(1:10) .eq. 'META_V_INL') then
!
            nomcle(1)='SIGM_F1'
            nomcle(2)='SIGM_F2'
            nomcle(3)='SIGM_F3'
            nomcle(4)='SIGM_F4'
            nomcle(5)='SIGM_C'
!
            if (iret1 .eq. 1) then
                call utmess('F', 'CALCULEL_31')
            endif
            do 75 k = 1, nz
                call rctrac(imat, 2, nomcle(k), temp, jprol,&
                            jvale, nbval( k), rbid)
                call rcfonc('V', 2, jprol, jvale, nbval(k),&
                            rbid, rbid, rbid, vi(k), r(k),&
                            h(k), rbid, rbid, rbid)
                r(k) = r(k) + sy(k)
 75         continue
!
            maxval = max(nbval(1),nbval(2),nbval(3),nbval(4),nbval(5))
!
        endif
!
        if (zalpha .gt. 0.d0) then
            rmoy=phase(1)*r(1)+phase(2)*r(2)+phase(3)*r(3)+phase(4)*r(&
            4)
            rmoy = rmoy/zalpha
            hmoy=phase(1)*h(1)+phase(2)*h(2)+phase(3)*h(3)+phase(4)*h(&
            4)
            hmoy = hmoy/zalpha
        else
            rmoy = 0.d0
            hmoy = 0.d0
        endif
        rmoy =(1.d0-fmel(1))*r(nz)+fmel(1)*rmoy
        hmoy = (1.d0-fmel(1))*h(nz)+fmel(1)*hmoy
!
    else
!
        trans =0.d0
!
    endif
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
    do 80 i = 1, ndimsi
        dvdeps(i) = deps(i) - trdeps * kron(i)
        dvsigm(i) = sigm(i) - trsigm * kron(i)
 80 end do
!
    sieleq = 0.d0
    do 85 i = 1, ndimsi
        sigel(i) = deuxmu*dvsigm(i)/deumum + deuxmu*dvdeps(i)
        sieleq = sieleq + sigel(i)**2
 85 end do
    sieleq = sqrt(1.5d0*sieleq)
!
    if (sieleq .gt. 0.d0) then
        do 90 i = 1, ndimsi
            sig0(i) = sigel(i)/sieleq
 90     continue
    else
        do 95 i = 1, ndimsi
            sig0(i) = 0.d0
 95     continue
    endif
!
! ************************
! 4 - RESOLUTION
! ************************
!
    if (resi) then
!
! 4.1 - COMPORTEMENT ELASTIQUE - CALCUL DE SIGMA
!
        if (compor(1)(1:4) .eq. 'ELAS') then
!
            do 100 i = 1, ndimsi
                sigp(i) = sigel(i)+trsigp*kron(i)
100         continue
!
! 4.2 - COMPORTEMENT PLASTIQUE
! 4.2.1 - CALCUL DE DP
!
        else if (compor(1)(1:4) .eq. 'META') then
!
            seuil= sieleq-(1.5d0*deuxmu*trans+1.d0)*rmoy
!
            if (seuil .lt. 0.d0) then
                vip(6) = 0.d0
                dp = 0.d0
            else
                vip(6) = 1.d0
                call nzcalc(crit, phase, nz, fmel(1), seuil,&
                            dt, trans, hmoy, deuxmu, eta,&
                            unsurn, dp, iret)
                if (iret .eq. 1) goto 9999
!
! DANS LE CAS NON LINEAIRE
! VERIFICATION QU ON EST DANS LE BON INTERVALLE
!
                if (compor(1)(1:10) .eq. 'META_P_INL' .or. compor(1)(1: 10) .eq.&
                    'META_V_INL') then
!
                    do 105 j = 1, maxval
                        test=0
                        do 110 k = 1, nz
                            if (phase(k) .gt. 0.d0) then
                                vip(k)=vi(k)+dp
                                hplus(k)=h(k)
                                call rctrac(imat, 2, nomcle(k), temp, jprol,&
                                            jvale, nbval(k), rbid)
                                call rcfonc('V', 2, jprol, jvale, nbval(k),&
                                            rbid, rbid, rbid, vip(k), r(k),&
                                            h(k), rbid, rbid, rbid)
                                r(k) = r(k) + sy(k)
                                if (abs(h(k)-hplus(k)) .gt. precr) test= 1
                            endif
110                     continue
                        if (test .eq. 0) goto 600
!
                        hmoy=0.d0
                        rmoy=0.d0
                        if (zalpha .gt. 0.d0) then
                            do 115 k = 1, nz-1
                                if (phase(k) .gt. 0.d0) then
                                    rmoy = rmoy + phase(k)*(r(k)-h(k)* dp)
                                    hmoy = hmoy + phase(k)*h(k)
                                endif
115                         continue
                            rmoy=fmel(1)*rmoy/zalpha
                            hmoy=fmel(1)*hmoy/zalpha
                        endif
                        if (phase(nz) .gt. 0.d0) then
                            rmoy = (1.d0-fmel(1))*(r(nz)-h(nz)*dp)+rmoy
                            hmoy = (1.d0-fmel(1))*h(nz)+hmoy
                        endif
                        seuil= sieleq - (1.5d0*deuxmu*trans + 1.d0)*&
                        rmoy
                        call nzcalc(crit, phase, nz, fmel(1), seuil,&
                                    dt, trans, hmoy, deuxmu, eta,&
                                    unsurn, dp, iret)
                        if (iret .eq. 1) goto 9999
105                 continue
                    ASSERT((test.ne.1).or.(j.ne.maxval))
600                 continue
                endif
            endif
!
! 4.2.2 - CALCUL DE SIGMA
!
            plasti = vip(6)
!
            do 120 i = 1, ndimsi
                dvsigp(i) = sigel(i) - 1.5d0*deuxmu*dp*sig0(i)
                dvsigp(i) = dvsigp(i)/(1.5d0*deuxmu*trans + 1.d0)
                sigp(i) = dvsigp(i) + trsigp*kron(i)
120         continue
!
! 4.2.3 - CALCUL DE VIP ET RMOY
!
            do 125 k = 1, nz
                if (phase(k) .gt. 0.d0) then
                    vip(k)=vi(k)+dp
                else
                    vip(k)=0.d0
                endif
125         continue
!
            vip(7)=0.d0
            if (phase(nz) .gt. 0.d0) then
!
                if (compor(1)(1:9) .eq. 'META_P_IL' .or. compor(1)(1:9) .eq. 'META_V_IL') then
                    vip(7)=vip(7)+(1-fmel(1))*h(nz)*vip(nz)
                endif
!
                if (compor(1)(1:10) .eq. 'META_P_INL' .or. compor(1)(1:10) .eq.&
                    'META_V_INL') then
                    vip(7)=vip(7)+(1-fmel(1))*(r(nz)-sy(nz))
                endif
!
            endif
!
            if (zalpha .gt. 0.d0) then
                do 130 k = 1, nz-1
!
                    if (compor(1)(1:9) .eq. 'META_P_IL' .or. compor(1)(1: 9) .eq.&
                        'META_V_IL') then
                        vip(7)=vip(7)+fmel(1)*phase(k)*h(k)*vip(k)/&
                        zalpha
                    endif
!
                    if (compor(1)(1:10) .eq. 'META_P_INL' .or. compor(1)( 1:10) .eq.&
                        'META_V_INL') then
                        vip(7)=vip(7)+fmel(1)*phase(k)*(r(k)-sy(k))/&
                        zalpha
                    endif
!
130             continue
            endif
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
        do 140 i = 1, ndimsi
            dsidep(i,i) =1.d0
140     continue
!
        do 145 i = 1, 3
            do 150 j = 1, 3
                dsidep(i,j) = dsidep(i,j)-1.d0/3.d0
150         continue
145     continue
!
        if (option(1:9) .eq. 'FULL_MECA') then
            coef1=(1.5d0*deuxmu*trans+1.d0)
        else
            coef1=1.d0
        endif
!
        do 155 i = 1, ndimsi
            do 160 j = 1, ndimsi
                dsidep(i,j)=dsidep(i,j)*deuxmu/coef1
160         continue
155     continue
!
! 5.2 - PARTIE PLASTIQUE
!
        b=1.d0
        coef2 =0.d0
        coef3=0.d0
        if (compor(1)(1:4) .eq. 'META') then
!
            if (plasti .ge. 0.5d0) then
!
                if (option(1:9) .eq. 'FULL_MECA') then
!
                    sigeps = 0.d0
                    do 165 i = 1, ndimsi
                        sigeps = sigeps + dvsigp(i)*dvdeps(i)
165                 continue
!
                    if ((mode.eq.1) .or. ((mode .eq. 2) .and. ( sigeps.ge.0.d0))) then
!
                        b = 1.d0-(1.5d0*deuxmu*dp/sieleq)
                        dv=0.d0
                        if (mode .eq. 1) then
                            do 170 k = 1, nz
                                n0(k) = (1-n(k))/n(k)
170                         continue
                            dv = (1-fmel(1))*phase(nz)*(eta(nz)/n(nz)/dt) * ((dp/dt)**n0(nz))
                            if (zalpha .gt. 0.d0) then
                                do 175 k = 1, nz-1
                                    if (phase(k) .gt. 0.d0) dv = dv+ fmel(1)*( phase(k)/zalpha) *&
                                                                 & (eta(k)/ n(k)/dt)*((dp/dt)**n0&
                                                                 &(k) )
175                             continue
                            endif
                        endif
!
                        coef2 = hmoy +dv
                        coef2 = (1.5d0*deuxmu*trans+1.d0)*coef2
                        coef2 = (1.5d0*deuxmu)+coef2
                        coef2 = 1/coef2 - dp/sieleq
                        coef2 =((1.5d0*deuxmu)**2)*coef2
!
                    endif
!
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
        endif
!
        do 180 i = 1, ndimsi
            do 185 j = 1, ndimsi
                dsidep(i,j) = dsidep(i,j)*b
185         continue
180     continue
!
        do 190 i = 1, 3
            do 195 j = 1, 3
                dsidep(i,j) = dsidep(i,j)+troisk/3.d0
195         continue
190     continue
!
        do 200 i = 1, ndimsi
            do 205 j = 1, ndimsi
                dsidep(i,j) = dsidep(i,j)- coef3*sig0(i)*sig0(j)
205         continue
200     continue
!
    endif
!
9999 continue
!
end subroutine
