subroutine nzgdzi(fami, kpg, ksp, ndim, imat,&
                  compor, crit, instam, instap, fm,&
                  df, sigm, vim, option, sigp,&
                  vip, dsigdf, iret)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/nzcalc.h"
#include "asterfort/rcfonc.h"
#include "asterfort/rctrac.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utmess.h"
#include "asterfort/verift.h"
#include "asterfort/zerop3.h"
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
    real(kind=8), intent(in) :: fm(3, 3)
    real(kind=8), intent(in) :: df(3, 3)
    real(kind=8), intent(in) :: sigm(*)
    real(kind=8), intent(in) :: vim(6)
    character(len=16), intent(in) :: option
    real(kind=8), intent(out) :: sigp(*)
    real(kind=8), intent(out) :: vip(6)
    real(kind=8), intent(out) :: dsigdf(6, 3, 3)
    integer, intent(out) :: iret
!
! --------------------------------------------------------------------------------------------------
!
! Comportment
!
! META_P_I* / META_V_I* for SIMO_MIEHE strains and zirconium metallurgy
!
! --------------------------------------------------------------------------------------------------
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  IMATE   : ADRESSE DU MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT
! IN  INSTAM  : INSTANT DU CALCUL PRECEDENT
! IN  INSTAP  : INSTANT DU CALCUL
! IN  DF      : INCREMENT DU GRADIENT DE LA TRANSFORMATION
! IN  FM      : GRADIENT DE LA TRANSFORMATION A L INSTANT PRECEDENT
! IN  SIGM    : CONTRAINTES DE CAUCHY A L INSTANT PTECEDENT
! IN  VIM     : VARIABLES INTERNES A L INSTANT DU CALCUL PRECEDENT
! IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
! OUT SIGP    : CONTRAINTES DE CAUCHY A L'INSTANT ACTUEL
! OUT VIP     : VARIABLES INTERNES A L'INSTANT ACTUEL
! OUT DSIGDF  : DERIVEE DE SIGMA PAR RAPPORT A F
!
!          ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
!          L'ORDRE :  XX,YY,ZZ,XY,XZ,YZ
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jprol, jvale, nbval(3), maxval, nb_phasis, meta_id
    integer :: i, j, k, l, mode
    integer :: ind(3, 3), nbr, iret1
    real(kind=8) :: phase(5), phasm(5), zalpha
    real(kind=8) :: temp, dt
    real(kind=8) :: epsth, e, nu, mu, mum, troisk
    real(kind=8) :: fmel(1), sy(3), h(3), hmoy, hplus(3), r(3), rmoy
    real(kind=8) :: theta(4)
    real(kind=8) :: eta(5), n(3), unsurn(5), c(3), m(3), cmoy, mmoy, cr
    real(kind=8) :: dz(2), dz1(2), dz2(2), vi(3), dvin, vimoy, ds
    real(kind=8) :: trans, kpt(2), zvarim, zvarip, deltaz
    real(kind=8) :: jm, jp, dj, dfb(3, 3)
    real(kind=8) :: taum(6), dvtaum(6), trtaum, eqtaum
    real(kind=8) :: taup(6), dvtaup(6), trtaup
    real(kind=8) :: tau(6), dvtau(6), trtau, eqtau
    real(kind=8) :: bem(6), bel(6), dvbel(6), trbel
    real(kind=8) :: dvtel(6), eqtel
    real(kind=8) :: plasti, dp, seuil, mutild
    real(kind=8) :: je2, je3, xm, xp, sol(3)
    real(kind=8) :: coeff1, coeff2, coeff3, coeff4, coeff5, coeff6, coeff7
    real(kind=8) :: coeff8, coeff9, dv, rb, n0(3)
    real(kind=8) :: mat0(3, 3), mat1(3, 3), mat2(6, 3, 3), mat3(3, 3)
    real(kind=8) :: rbid
    real(kind=8) :: valres(12), epsthe(3)
    character(len=1) :: poum
    integer :: icodre(12), test
    character(len=16) :: nomres(12)
    character(len=8) :: nomcle(3)
    real(kind=8), parameter :: kr(6) = (/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/)
    real(kind=8), parameter :: pdtsca(6) = (/1.d0,1.d0,1.d0,2.d0,2.d0,2.d0/)
    aster_logical :: resi, rigi
!
    data ind   /1,4,5,&
                4,2,6,&
                5,6,3/
!
!  FB(I,J)=(J**(-1/3))*F(I,J)
!  GPM(I,J) : DEFORMATION PLASTIQUE LAGRANGIENNE A L'INSTANT PRECEDENT
!  B(I,J)=FB(I,K)*GPM(K,L)*FB(J,L)
!  BETR(1)=B(1,1), BETR(2)=B(2,2),...,BETR(6)=B(2,3)=B(3,2)
!
! TENSEUR DE DEFORMATION ELASTIQUE EULERIEN : BE
! TENSEUR DE DEFORMATION PLASTIQUE LAGRANGIEN : GP
! TAU : TENSEUR DE KIRSHHOFF
!
! --------------------------------------------------------------------------------------------------
!
    do i = 1, 2*ndim
        sigp(i) = 0.d0
    end do
    vip(1:6)            = 0.d0
    dsigdf(1:6,1:3,1:3) = 0.d0
    iret                = 0
    resi                = option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL'
    rigi                = option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL'
    dt                  = instap-instam
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
                iret = iret1, vepsth=epsthe)
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
    mum=valres(1)/(2.d0*(1.d0+valres(2)))
!
    call rcvalb(fami, kpg, ksp, poum, imat,&
                ' ', 'ELAS_META', 0, ' ', [0.d0],&
                6, nomres, valres, icodre, 2)
    epsth = phase(nb_phasis)*(epsthe(1)-(1.d0-valres(5))*valres(6)) +&
            zalpha*(epsthe(2)+valres(5)*valres(6))
    e=valres(1)
    nu=valres(2)
    mu=e/(2.d0*(1.d0+nu))
    troisk = e/(1.d0-2.d0*nu)
!
    plasti=vim(4)
!
! 2.2 - LOI DES MELANGES
!
    if (compor(1)(1:6) .eq. 'META_P') then
        nomres(1) ='F1_SY'
        nomres(2) ='F2_SY'
        nomres(3) ='C_SY'
        nomres(4) ='SY_MELANGE'
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
! 2.6 - CALCUL DE VIM+DG-DS
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
            dvin=0.d0
            do k = 1, nb_phasis-1
                dvin=dvin+dz2(k)*(theta(2+k)*vim(k)-vim(nb_phasis))/phase(nb_phasis)
            end do
            vi(nb_phasis)=vim(nb_phasis)+dvin
            vimoy=phase(nb_phasis)*vi(nb_phasis)
        else
            vi(nb_phasis) = 0.d0
            vimoy=0.d0
        endif
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
        cr=cmoy*vimoy
        if (cr .le. 0.d0) then
            ds=0.d0
        else
            ds= dt*(cr**mmoy)
        endif
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
!
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
        trans=0.d0
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
            r(k) = h(k)*vi(k)+sy(k)
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
! ********************************
! 3 - DEBUT DE L ALGORITHME
! ********************************
!
! 3.1 - JM=DET(FM),DJ=DET(DF),J=JM*DJ ET DFB
!
    jm = fm(1,1)*(fm(2,2)*fm(3,3)-fm(2,3)*fm(3,2))&
        -fm(2,1)*(fm(1,2)*fm(3,3)-fm(1,3)*fm(3,2))&
        +fm(3,1)*(fm(1,2)*fm(2,3)-fm(1,3)*fm(2,2))
    dj = df(1,1)*(df(2,2)*df(3,3)-df(2,3)*df(3,2))&
        -df(2,1)*(df(1,2)*df(3,3)-df(1,3)*df(3,2))&
        +df(3,1)*(df(1,2)*df(2,3)-df(1,3)*df(2,2))
    jp=jm*dj
    do i = 1, 3
        do j = 1, 3
            if (dj .le. 0.d0) then
                iret = 1
                goto 999
            else
                dfb(i,j)=(dj**(-1.d0/3.d0))*df(i,j)
            endif
        end do
    end do
!
! 3.2 - CONTRAINTES DE KIRSHHOFF A L INSTANT PRECEDENT
!
    taum(5)=0.d0
    taum(6)=0.d0
    do i = 1, 2*ndim
        taum(i)=jm*sigm(i)
    end do
!
    trtaum=taum(1)+taum(2)+taum(3)
    eqtaum=0.d0
    do i = 1, 6
        dvtaum(i)=taum(i)-kr(i)*trtaum/3.d0
        eqtaum=eqtaum+pdtsca(i)*(dvtaum(i)**2.d0)
    end do
    eqtaum=sqrt(1.5d0*eqtaum)
!
! 3.3 - DEFORMATIONS ELASTIQUES A L INSTANT PRECEDENT :
! BEM=DVTAUM/MUM+KR*TRBEM/3.D0
!
    if (compor(1)(1:4) .eq. 'ELAS') then
        xm = (jm**(-2.d0/3.d0))*(1.d0-2.d0*vim(1)/3.d0)
    else
        xm = (jm**(-2.d0/3.d0))*(1.d0-2.d0*vim(6)/3.d0)
    endif
    do i = 1, 6
        bem(i)=dvtaum(i)/mum+kr(i)*xm
    end do
!
! 3.4 - BEL(I,J)=DFB(I,K)*BEM(K,L)*DFB(J,L)
!
    do i = 1, 3
        do j = 1, 3
            bel(ind(i,j))=0.d0
            do k = 1, 3
                do l = 1, 3
                    bel(ind(i,j))=bel(ind(i,j)) +dfb(i,k)*bem(ind(k,l)&
                    )*dfb(j,l)
                end do
            end do
        end do
    end do
!
! 3.5 - TRACE ET PARTIE DEVIATORIQUE DU TENSEUR BEL
!
    trbel=bel(1)+bel(2)+bel(3)
    do i = 1, 6
        dvbel(i)=bel(i)-kr(i)*trbel/3.d0
    end do
!
! 3.6 - CONTRAINTE ELASTIQUE (PARTIE DEVIATORIQUE)
!
    do i = 1, 6
        dvtel(i)=mu*dvbel(i)
    end do
!
! 3.7 - CONTRAINTE EQUIVALENTE ELASTIQUE ET SEUIL
!
    eqtel=0.d0
    do i = 1, 6
        eqtel=eqtel+pdtsca(i)*dvtel(i)*dvtel(i)
    end do
    eqtel=sqrt(1.5d0*eqtel)
!
! 3.8 - TRACE DU TENSEUR DE KIRSHHOFF (CONNUE CAR NE DEPEND QUE DE J)
!
    trtaup=(troisk*((jp*jp)-1.d0)/6.d0)-(troisk*epsth*(jp+(1.d0/jp))/2.d0)
    dp=0.d0
!
! ************************
! 4 - RESOLUTION
! ************************
!
    if (resi) then
!
! 4.2.1 - CALCUL DE DP
!
        seuil=eqtel-(1.d0+mu*trans*trbel)*rmoy
        if (seuil .lt. 0.d0) then
            vip(4)=0.d0
            dp=0.d0
        else
            vip(4)=1.d0
            mutild=2.d0*mu*trbel/3.d0
            call nzcalc(crit, phase, nb_phasis, fmel(1), seuil,&
                        dt, trans, hmoy, mutild, eta,&
                        unsurn, dp, iret)
            if (iret .eq. 1) goto 999
!
! DANS LE CAS NON LINEAIRE
! VERIFICATION QU ON EST DANS LE BON INTERVALLE
!
            if (compor(1)(1:10) .eq. 'META_P_INL' .or. compor(1)(1: 10) .eq.'META_V_INL') then
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
                            r(k)=r(k)+sy(k)
                            if (abs(h(k)-hplus(k)) .gt. r8prem()) test= 1
                        endif
                    end do
                    if (test .eq. 0) goto 600
                    hmoy=0.d0
                    rmoy=0.d0
                    if (zalpha .gt. 0.d0) then
                        do k = 1, nb_phasis-1
                            if (phase(k) .gt. 0.d0) then
                                rmoy = rmoy + phase(k)*(r(k)-h(k)*dp)
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
                    seuil=eqtel-(1.d0+mu*trans*trbel)*rmoy
                    call nzcalc(crit, phase, nb_phasis, fmel(1), seuil,&
                                dt, trans, hmoy, mutild, eta,&
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
        plasti=vip(4)
        do i = 1, 6
            if (eqtel .gt. 0.d0) then
                dvtaup(i)=dvtel(i)-mu*dp*trbel*dvtel(i)/eqtel
                dvtaup(i)=dvtaup(i)/(1.d0+mu*trans*trbel)
            else
                dvtaup(i)=dvtel(i)/(1.d0+mu*trans*trbel)
            endif
            taup(i)=dvtaup(i)+kr(i)*trtaup
        end do
        do i = 1, 2*ndim
            sigp(i)=taup(i)/jp
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
        vip(5)=0.d0
        if (phase(nb_phasis) .gt. 0.d0) then
            if (compor(1)(1:9) .eq. 'META_P_IL' .or. compor(1)(1:9) .eq. 'META_V_IL') then
                vip(5)=vip(5)+(1-fmel(1))*h(nb_phasis)*vip(nb_phasis)
            endif
            if (compor(1)(1:10) .eq. 'META_P_INL' .or. compor(1)(1:10) .eq.'META_V_INL') then
                vip(5)=vip(5)+(1-fmel(1))*(r(nb_phasis)-sy(nb_phasis))
            endif
        endif
        if (zalpha .gt. 0.d0) then
            do k = 1, nb_phasis-1
                if (compor(1)(1:9) .eq. 'META_P_IL' .or. compor(1)(1: 9) .eq.'META_V_IL') then
                    vip(5)=vip(5)+fmel(1)*phase(k)*h(k)*vip(k)/zalpha
                endif
                if (compor(1)(1:10) .eq. 'META_P_INL' .or. compor(1)( 1:10) .eq.'META_V_INL') then
                    vip(5)=vip(5)+fmel(1)*phase(k)*(r(k)-sy(k))/zalpha
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
        if (option(1:14) .eq. 'RIGI_MECA_TANG') then
            do i = 1, 6
                tau(i)=taum(i)
            end do
        else
            do i = 1, 6
                tau(i)=taup(i)
            end do
        endif
        mat0(1,1)=df(2,2)*df(3,3)-df(2,3)*df(3,2)
        mat0(2,2)=df(1,1)*df(3,3)-df(1,3)*df(3,1)
        mat0(3,3)=df(1,1)*df(2,2)-df(1,2)*df(2,1)
        mat0(1,2)=df(3,1)*df(2,3)-df(2,1)*df(3,3)
        mat0(2,1)=df(1,3)*df(3,2)-df(1,2)*df(3,3)
        mat0(1,3)=df(2,1)*df(3,2)-df(3,1)*df(2,2)
        mat0(3,1)=df(1,2)*df(2,3)-df(1,3)*df(2,2)
        mat0(2,3)=df(3,1)*df(1,2)-df(1,1)*df(3,2)
        mat0(3,2)=df(2,1)*df(1,3)-df(1,1)*df(2,3)
        do i = 1, 3
            do j = 1, 3
                mat1(i,j)=0.d0
                do k = 1, 3
                    mat1(i,j)=mat1(i,j)+dfb(i,k)*bem(ind(k,j))
                end do
            end do
        end do
        do i = 1, 3
            do j = 1, 3
                do k = 1, 3
                    do l = 1, 3
                        mat2(ind(i,j),k,l)=kr(ind(i,k))*mat1(j,l)+kr(ind(j,k))*mat1(i,l)
                    end do
                end do
            end do
        end do
        eqtau=0.d0
        trtau=(tau(1)+tau(2)+tau(3))/3.d0
        do i = 1, 6
            dvtau(i)=tau(i)-trtau*kr(i)
            eqtau=eqtau+pdtsca(i)*(dvtau(i)**2.d0)
        end do
        eqtau=sqrt(1.5d0*eqtau)
        if (eqtau .gt. 0.d0) then
            coeff1=1.d0+mu*trans*trbel+mu*dp*trbel/eqtau
        else
            coeff1=1.d0+mu*trans*trbel
        endif
        coeff2=(dj**(-1.d0/3.d0))*mu/(coeff1*jp)
        coeff3=-2.d0*mu/(3.d0*coeff1*jp*dj)
        coeff4=(troisk*jp/3.d0)-troisk*epsth *(1.d0-(jp**(-2.d0)))/2.d0
        coeff4=coeff4/dj
        do i = 1, 6
            do j = 1, 3
                do k = 1, 3
                    dsigdf(i,j,k)=mat2(i,j,k)-2.d0*kr(i)*mat1(j,k)/3.d0
                    dsigdf(i,j,k)= coeff2*dsigdf(i,j,k) + coeff3*&
                    dvbel(i)*mat0(j,k) + coeff4*kr(i)*mat0(j,k)&
                    - tau(i)*mat0(j,k)/(jp*dj)
                end do
            end do
        end do
        if (plasti .le. 0.5d0) then
            coeff5=-2.d0*trans*coeff2
            coeff6=2.d0*trans*mu*trbel/(3.d0*jp*dj*coeff1)
            do i = 1, 6
                do j = 1, 3
                    do k = 1, 3
                        dsigdf(i,j,k) = dsigdf(i,j,k) + coeff5*dvtau( i)*mat1(j,k) +&
                                        coeff6*dvtau(i)*mat0(j,k)
                    end do
                end do
            end do
        else
            mode=2
            if (compor(1)(1:6) .eq. 'META_V') mode=1
            if (mode .eq. 1) then
                if (dp .gt. 0.d0) then
                    do i = 1, nb_phasis
                        n0(i) = (1-n(i))/n(i)
                    end do
                    dv = (1-fmel(1))*phase(nb_phasis)*(eta(nb_phasis)/n(nb_phasis)/dt) *&
                         ((dp/dt)**n0(nb_phasis))
                    if (zalpha .gt. 0.d0) then
                        do i = 1, nb_phasis-1
                            if (phase(i) .gt. 0.d0) then
                                dv = dv+fmel(1)*(phase(i)/zalpha)*(eta(i)/n(i)/dt)*((dp/dt)**n0(i))
                            endif
                        end do
                    endif
                else
                    dv =0.d0
                endif
                if (option(1:9) .eq. 'FULL_MECA') then
                    rb=hmoy+dv
                else
                    rb=0.d0
                endif
            else
                rb=hmoy
            endif
            if ((option(1:9).eq.'FULL_MECA') .or.&
                ((option(1:14) .eq.'RIGI_MECA_TANG').and. (mode.eq.2))) then
                coeff5=mu*trbel+rb*(1.d0+mu*trans*trbel)
                coeff6=-3.d0*mu*trbel*(eqtau-rb*dp)/((eqtau**3.d0)*coeff5)
                coeff6=coeff6*coeff2
                coeff7=-2.d0*coeff1*rb*(eqtau*trans+dp)/(eqtau*coeff5)
                coeff7=coeff7*coeff2
                coeff8=0.d0
                do i = 1, 6
                    coeff8=coeff8+pdtsca(i)*dvtau(i)*dvbel(i)
                end do
                coeff9=coeff1*rb*(eqtau*trans+dp)/eqtau+&
                       3.d0*mu*coeff8*(eqtau-rb*dp)/(2.d0*(eqtau**3.d0))
                coeff9=-coeff9*coeff3*trbel/coeff5
                do i = 1, 3
                    do j = 1, 3
                        mat3(i,j)=0.d0
                        do k = 1, 3
                            mat3(i,j)=mat3(i,j)+dvtau(ind(i,k))*mat1(k,j)
                        end do
                    end do
                end do
                do i = 1, 6
                    do j = 1, 3
                        do k = 1, 3
                            dsigdf(i,j,k) = dsigdf(i,j,k) +&
                                            coeff6*dvtau(i)*mat3(j,k) +&
                                            coeff7*dvtau(i)*mat1(j,k) +&
                                            coeff9*dvtau(i)*mat0(j,k)
                        end do
                    end do
                end do
            endif
        endif
    endif
!
! *******************************
! 6 - CORRECTION SUR TRBE
! *******************************
!
    if (resi) then
        trtau=taup(1)+taup(2)+taup(3)
        eqtau=0.d0
        do i = 1, 6
            dvtau(i)=taup(i)-kr(i)*trtau/3.d0
            eqtau=eqtau+pdtsca(i)*(dvtau(i)**2.d0)
        end do
        eqtau = sqrt(1.5d0*eqtau)
        je2   = (eqtau**2.d0)/(3.d0*(mu**2.d0))
        je3   = dvtau(1)*(dvtau(2)*dvtau(3)-dvtau(6)*dvtau(6)) -&
                dvtau(4)*(dvtau(4)*dvtau(3)-dvtau(5)*dvtau(6)) +&
                dvtau(5)*(dvtau(4)*dvtau(6)-dvtau(5)*dvtau(2))
        je3   = je3/(mu**3.d0)
        call zerop3(0.d0, -je2, je3-1.d0, sol, nbr)
        if (nbr .le. 1) then
            xp=sol(1)
        else
            xp=sol(1)
            do i = 2, nbr
                if ((abs(sol(i)-xm)) .lt. (abs(sol(i-1)-xm))) xp=sol(i)
            end do
        endif
        vip(6) = 3.d0*(1.d0-(jp**(2.d0/3.d0))*xp)/2.d0
    endif
!
999 continue
!
end subroutine
