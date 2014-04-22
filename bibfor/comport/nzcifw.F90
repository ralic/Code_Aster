subroutine nzcifw(fami, kpg, ksp, ndim, imat,&
                  compor, crit, instam, instap, epsm,&
                  deps, sigm, vim, option, sigp,&
                  vip, dsidep, iret)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterc/r8prem.h"
#include "asterfort/matini.h"
#include "asterfort/nzcalc.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/verift.h"
    integer :: ndim, imat, iret, kpg, ksp
    character(len=16) :: compor(*), option
    character(len=*) :: fami
    real(kind=8) :: crit(3), instam, instap
    real(kind=8) :: epsm(6), deps(6)
    real(kind=8) :: sigm(6), vim(37), sigp(6), vip(37), dsidep(6, 6)
! ----------------------------------------------------------------------
!     REALISE LA LOI DE VON MISES CINEMATIQUE POUR LES
!     ELEMENTS METALLURGIQUES EN PETITES DEFORMATIONS
!
!     AVEC OU SANS PLASTICITE DE TRANSFORMATION
!     AVEC OU SANS RESTAURATION ECROUISSAGE METALLURGIQUE
!     RELATION DE COMPORTEMENT ELASTO-PLASTIQUE OU
!                               ELASTO-VISCOPLASTIQUE
!     ECROUISSAGE CINEMATIQUE LINEAIRE
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
    integer :: nz
    integer :: ndimsi, i, j, k, l, mode, ire2
!
    real(kind=8) :: phase(5), phasm(5), zalpha
    real(kind=8) :: dt
!
    real(kind=8) :: epsth, e, deuxmu, deumum, troisk
    real(kind=8) :: fmel(1), sy(5), symoy, h(5), hmoy, rprim
    real(kind=8) :: theta(8)
    real(kind=8) :: eta(5), n(5), unsurn(5), c(5), m(5), cmoy, mmoy, cr
    real(kind=8) :: dz(4), dz1(4), dz2(4), vi(30), dvin, vimt(30)
    real(kind=8) :: xmoy(6), ds(6), xmoyeq
    real(kind=8) :: trans, kpt(4), zvarim, zvarip, deltaz
!
    real(kind=8) :: trepsm, trdeps, trsigm, trsigp
    real(kind=8) :: dvdeps(6), dvsigm(6), dvsigp(6)
    real(kind=8) :: sigel(6), sigel2(6), sig0(6), sieleq, sigeps
!
    real(kind=8) :: plasti, dp, seuil
!
    real(kind=8) :: coef1, coef2, coef3, dv, n0(5), b
!
    real(kind=8) :: precr, rac2
    real(kind=8) :: kron(6)
    real(kind=8) :: valres(20), epsthe(2)
!
    character(len=1) :: c1
    integer :: icodre(20)
    character(len=8) :: nomres(20), acier(4)
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
            call rcvarc(' ', acier(k), '+', fami, kpg,&
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
            call rcvarc(' ', acier(k), '-', fami, kpg,&
                        ksp, phase(k), ire2)
            if (ire2 .eq. 1) phase(k)=0.d0
 10     continue
!
    endif
!
    call verift(fami, kpg, ksp, c1, imat,&
                elas_keyword = 'ELAS_META', ndim=2, vepsth=epsthe)
!
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
! 1.4 - MISE AU FORMAT DES CONTRAINTES DE RAPPEL
!
    if (compor(1)(1:4) .eq. 'META') then
!
        rac2 = sqrt(2.d0)
        do 20 i = 4, ndimsi
            do 25 k = 1, nz
                l=i+(k-1)*6
                vim(l)=vim(l)*rac2
 25         continue
 20     continue
!
    endif
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
    epsth = phase(nz)*(epsthe(1)-(1.d0-valres(5))*valres(6)) + zalpha*(epsthe(2) + valres(5)*valr&
            &es(6))
    e = valres(1)
    deuxmu = e/(1.d0+valres(2))
    troisk = e/(1.d0-2.d0*valres(2))
!
    if (compor(1)(1:4) .eq. 'META') then
!
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
! 2.4 - PENTE D ECROUISSAGE
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
        h(1)=(2.d0/3.d0)*h(1)*e/(e-h(1))
        h(2)=(2.d0/3.d0)*h(2)*e/(e-h(2))
        h(3)=(2.d0/3.d0)*h(3)*e/(e-h(3))
        h(4)=(2.d0/3.d0)*h(4)*e/(e-h(4))
        h(5)=(2.d0/3.d0)*h(5)*e/(e-h(5))
!
        hmoy=0.d0
        do 30 k = 1, nz
            hmoy=hmoy+phase(k)*h(k)
 30     continue
!
        if (resi) then
!
! 2.5 - RESTAURATION D ECROUISSAGE
!
            if (compor(1)(1:15) .eq. 'META_P_CL_PT_RE' .or. compor(1)( 1:12) .eq.&
                'META_P_CL_RE' .or. compor(1)(1:12) .eq. 'META_V_CL_RE' .or.&
                compor(1)(1:15) .eq. 'META_V_CL_PT_RE') then
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
                do 35 i = 1, 8
                    theta(i)=1.d0
 35             continue
!
            endif
!
! 2.6 - VISCOSITE
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
                do 40 k = 1, nz
                    eta(k) = valres(k)
                    n(k) = valres(nz+k)
                    unsurn(k)=1/n(k)
                    if (icodre(2*nz+k) .ne. 0) valres(2*nz+k)=0.d0
                    c(k) =valres(2*nz+k)
                    if (icodre(3*nz+k) .ne. 0) valres(3*nz+k)=20.d0
                    m(k) = valres(3*nz+k)
 40             continue
!
            else
!
                do 45 k = 1, nz
                    eta(k) = 0.d0
                    n(k)= 20.d0
                    unsurn(k)= 1.d0
                    c(k) = 0.d0
                    m(k) = 20.d0
 45             continue
!
            endif
!
! 2.7 - CALCUL DE VIM+DG
!
            do 50 k = 1, nz-1
                dz(k)= phase(k)-phasm(k)
                if (dz(k) .ge. 0.d0) then
                    dz1(k)=dz(k)
                    dz2(k)=0.d0
                else
                    dz1(k)=0.d0
                    dz2(k)=-dz(k)
                endif
 50         continue
!
            if (phase(nz) .gt. 0.d0) then
                do 55 i = 1, ndimsi
                    dvin=0.d0
                    do 60 k = 1, nz-1
                        l=i+(k-1)*6
                        dvin = dvin + dz2(k)*(theta(4+k)*vim(l)-vim( 24+i))/phase(nz)
 60                 continue
                    vi(24+i) = vim(24+i)+dvin
                    if ((vi(24+i)*vim(24+i)) .lt. 0.d0) vi(24+i)=0.d0
 55             continue
            else
                do 65 i = 1, ndimsi
                    vi(24+i)=0.d0
 65             continue
            endif
!
            do 70 k = 1, nz-1
                do 75 i = 1, ndimsi
                    l=i+(k-1)*6
                    if (phase(k) .gt. 0.d0) then
                        dvin = dz1(k)*(theta(k)*vim(24+i)-vim(l))/ phase(k)
                        vi(l) = vim(l)+dvin
                        if ((vi(l)*vim(l)) .lt. 0.d0) vi(l)=0.d0
                    else
                        vi(l) = 0.d0
                    endif
 75             continue
 70         continue
!
! 2.8 - RESTAURATION D ORIGINE VISQUEUSE
!
            do 80 i = 1, ndimsi
                xmoy(i)=0.d0
                do 85 k = 1, nz
                    l=i+(k-1)*6
                    xmoy(i)=xmoy(i)+phase(k)*h(k)*vi(l)
 85             continue
 80         continue
!
            xmoyeq = 0.d0
            do 90 i = 1, ndimsi
                xmoyeq=xmoyeq+xmoy(i)**2.d0
 90         continue
            xmoyeq= sqrt(1.5d0*xmoyeq)
!
            cmoy=0.d0
            mmoy=0.d0
            do 100 k = 1, nz
                cmoy=cmoy+phase(k)*c(k)
                mmoy=mmoy+phase(k)*m(k)
100         continue
!
            cr=cmoy*xmoyeq
            if (xmoyeq .gt. 0.d0) then
                do 105 i = 1, ndimsi
                    ds(i)= 3.d0*dt*(cr**mmoy)*xmoy(i)/(2.d0*xmoyeq)
105             continue
            else
                do 110 i = 1, ndimsi
                    ds(i)= 0.d0
110             continue
            endif
!
            do 115 k = 1, nz
                do 120 i = 1, ndimsi
                    l=i+(k-1)*6
                    if (phase(k) .gt. 0.d0) then
                        vimt(l)=vi(l)
                        vi(l)=vi(l)-ds(i)
                        if ((vi(l)*vimt(l)) .lt. 0.d0) vi(l)=0.d0
                    endif
120             continue
115         continue
!
! 2.9 - PLASTICITE DE TRANSFORMATION
!
            trans = 0.d0
            if (compor(1)(1:12) .eq. 'META_P_CL_PT' .or. compor(1)(1: 15) .eq.&
                'META_P_CL_PT_RE' .or. compor(1)(1:12) .eq. 'META_V_CL_PT' .or.&
                compor(1)(1:15) .eq. 'META_V_CL_PT_RE') then
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
                do 125 k = 1, nz-1
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
125             continue
!
            endif
!
        else
!
            do 130 k = 1, nz
                do 135 i = 1, ndimsi
                    l=i+(k-1)*6
                    vi(l)=vim(l)
135             continue
130         continue
!
!         INITIALISATION DE TRANS
            trans=0.d0
!
            do 140 i = 1, ndimsi
                xmoy(i)=0.d0
                do 145 k = 1, nz
                    l=i+(k-1)*6
                    xmoy(i)=xmoy(i)+phase(k)*h(k)*vi(l)
145             continue
140         continue
!
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
        symoy =(1.d0-fmel(1))*sy(nz)+fmel(1)*symoy
!
    else
!
        trans=0.d0
        do 150 i = 1, ndimsi
            xmoy(i)=0.d0
150     continue
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
    do 155 i = 1, ndimsi
        dvdeps(i) = deps(i) - trdeps * kron(i)
        dvsigm(i) = sigm(i) - trsigm * kron(i)
155 end do
!
    sieleq = 0.d0
    do 160 i = 1, ndimsi
        sigel(i) = deuxmu*dvsigm(i)/deumum + deuxmu*dvdeps(i)
        sigel2(i)= sigel(i)-(1.5d0*deuxmu*trans+1.d0)*xmoy(i)
        sieleq = sieleq + sigel2(i)**2
160 end do
    sieleq = sqrt(1.5d0*sieleq)
!
    if (sieleq .gt. 0.d0) then
        do 165 i = 1, ndimsi
            sig0(i) = sigel2(i)/sieleq
165     continue
    else
        do 170 i = 1, ndimsi
            sig0(i) = 0.d0
170     continue
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
            do 175 i = 1, ndimsi
                sigp(i) = sigel(i)+trsigp*kron(i)
175         continue
!
! 4.2 - COMPORTEMENT PLASTIQUE
! 4.2.1 - CALCUL DE DP
!
        else if (compor(1)(1:4) .eq. 'META') then
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
                    dp=seuil/(1.5d0*deuxmu+(1.5d0*deuxmu*trans+1.d0)*&
                    rprim)
                else
                    call nzcalc(crit, phase, nz, fmel(1), seuil,&
                                dt, trans, rprim, deuxmu, eta,&
                                unsurn, dp, iret)
                    if (iret .eq. 1) goto 9999
                endif
            endif
!
! 4.2.2 - CALCUL DE SIGMA
!
            plasti=vip(37)
!
            do 180 i = 1, ndimsi
                dvsigp(i) = sigel(i) - 1.5d0*deuxmu*dp*sig0(i)
                dvsigp(i) = dvsigp(i)/(1.5d0*deuxmu*trans + 1.d0)
                sigp(i) = dvsigp(i) + trsigp*kron(i)
180         continue
!
! 4.2.3 - CALCUL DE VIP ET XMOY
!
            do 185 k = 1, nz
                do 190 i = 1, ndimsi
                    l=i+(k-1)*6
                    if (phase(k) .gt. 0.d0) then
                        vip(l) = vi(l)+3.d0*dp*sig0(i)/2.d0
                    else
                        vip(l) = 0.d0
                    endif
190             continue
185         continue
!
            do 195 i = 1, ndimsi
                vip(30+i)= xmoy(i)+3.d0*hmoy*dp*sig0(i)/2.d0
195         continue
!
        endif
!
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
        do 210 i = 1, ndimsi
            dsidep(i,i) = 1.d0
210     continue
!
        do 215 i = 1, 3
            do 220 j = 1, 3
                dsidep(i,j) = dsidep(i,j)-1.d0/3.d0
220         continue
215     continue
!
        if (option(1:9) .eq. 'FULL_MECA') then
            coef1 = (1.5d0*deuxmu*trans+1.d0)
        else
            coef1 = 1.d0
        endif
!
        do 225 i = 1, ndimsi
            do 230 j = 1, ndimsi
                dsidep(i,j) = dsidep(i,j)*deuxmu/coef1
230         continue
225     continue
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
                    do 235 i = 1, ndimsi
                        dvsigp(i)=dvsigp(i)-xmoy(i)
                        sigeps = sigeps + dvsigp(i)*dvdeps(i)
235                 continue
!
                    if ((mode .eq.1) .or. ((mode .eq. 2) .and. (sigeps.ge.0.d0))) then
!
                        b = 1.d0-(1.5d0*deuxmu*dp/sieleq)
                        dv = 0.d0
                        if (mode .eq. 1) then
                            do 240 k = 1, nz
                                n0(k) = (1-n(k))/n(k)
240                         continue
                            dv = (1-fmel(1))*phase(nz)*(eta(nz)/n(nz)/dt) * ((dp/dt)**n0(nz))
                            if (zalpha .gt. 0.d0) then
                                do 245 k = 1, nz-1
                                    if (phase(k) .gt. 0.d0) dv = dv+ fmel(1)*( phase(k)/zalpha) *&
                                                                 & (eta(k)/ n(k)/dt)*((dp/dt)**n0&
                                                                 &(k) )
245                             continue
                            endif
                        endif
!
                        coef2 = 3.d0*hmoy/2.d0 + dv
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
                    if (mode .eq. 2) coef2 = ( (1.5d0*deuxmu)**2 )/( 1.5d0*deuxmu+1.5d0*hmoy )
                endif
!
                coef3 = coef2/coef1
!
            endif
!
        endif
!
        do 250 i = 1, ndimsi
            do 255 j = 1, ndimsi
                dsidep(i,j) = dsidep(i,j)*b
255         continue
250     continue
!
        do 260 i = 1, 3
            do 265 j = 1, 3
                dsidep(i,j) = dsidep(i,j)+troisk/3.d0
265         continue
260     continue
!
        do 270 i = 1, ndimsi
            do 275 j = 1, ndimsi
                dsidep(i,j) = dsidep(i,j)- coef3*sig0(i)*sig0(j)
275         continue
270     continue
!
    endif
!
! 6 - MISE AU FORMAT CONTRAINTE DE RAPPEL
!
    if (compor(1)(1:4) .eq. 'META') then
!
        do 280 i = 4, ndimsi
            do 285 k = 1, nz
                l=i+(k-1)*6
                vim(l)=vim(l)/rac2
                if (resi) vip(l)=vip(l)/rac2
285         continue
            if (resi) vip(30+i)= vip(30+i)/rac2
280     continue
!
    endif
!
9999 continue
!
end subroutine
