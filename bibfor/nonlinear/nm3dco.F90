subroutine nm3dco(fami, kpg, ksp, ndim, option,&
                  imate, sigm, deps, vim, sigp,&
                  vip, dsidep, crildc, codret)
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
! ----------------------------------------------------------------------
!
    implicit none
! ----------------------------------------------------------------------
!          LOI DE L'ACIER SOUMIS A LA CORROSION 3D
!
! IN  NDIM    : DIMENSION
! IN  OPTION  : OPTION DE CALCUL
! IN  IMATE   : POINTEUR MATERIAU
! IN  SIGM    : CONTRAINTE AU TEMPS MOINS
! IN  EPSM    : DEFORMATION TOTALE AU TEMPS MOINS
! IN  DEPS    : DEFORMATION  TOTALE PLUS - DEFORMATION TOTALE MOINS
! IN VIM      : VARIABLE INTERNES AU TEMPS MOINS
! IN CRILDC   : 1 ITERMAX, 3 RESI_INTE
!
! OUT SIGP     : CONTRAINTES PLUS
! OUT VIP       : VARIABLE INTERNES PLUS
! OUT DSIDEP    : DSIG/DEPS
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
#include "asterfort/matini.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/u2mess.h"
    real(kind=8) :: sigm(6), deps(6), vim(*)
    real(kind=8) :: sigp(6), vip(*), dsidep(6, 6), crildc(3)
    character(len=16) :: option
    character(len=*) :: fami
    integer :: ndim, imate, codret, kpg, ksp
!
    real(kind=8) :: young, nu, kcoef, mcoef, coefdc, limit
    real(kind=8) :: valres(4)
    integer :: codres(4)
    character(len=8) :: nomres(4)
!
    integer :: iter, itemax, ndimsi, i, k, l, m, itd, ibid
!
    real(kind=8) :: resi, ecum, ecumm, dcoef, plas
    real(kind=8) :: defe, defc, nuetoi, defpc(3), ecumc, ecumd
    real(kind=8) :: coef1, coef2, j2, rini, crit, crit0
    logical :: dconv, pconv, premd, mtang, melas
    real(kind=8) :: terme1, terme2(6), terme4(6), terme5, ter11
    real(kind=8) :: deltap, dp
    real(kind=8) :: criten, crit2, crit0d, treps
!
    real(kind=8) :: sigfi(6), sigd(6), rbid, drdp, hp
    real(kind=8) :: kci, lamda, deumu, acoef, bcoef, ccoef, corrm
!
    ndimsi=2*ndim
    nomres(1) = 'D_CORR'
    nomres(2) = 'ECRO_K'
    nomres(3) = 'ECRO_M'
    nomres(4) = 'SY'
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'CORR_ACIER', 0, ' ', 0.d0,&
                4, nomres, valres, codres, 1)
!
    coefdc = valres(1)
    kcoef = valres(2)
    mcoef = valres(3)
    limit = valres(4)
    nomres(1) = 'NU'
    nomres(2) = 'E'
!
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                2, nomres, valres, codres, 1)
!
    nu = valres(1)
    young = valres(2)
!
    ecumm = vim(1)
    dcoef = vim(2)
    plas = vim(3)
!
! --- PARAMETRES DE CONVERGENCE
    resi = crildc(3)
    itemax = nint(crildc(1))
!
!
!     CALCUL DE LA DEFORMATION CRITIQUE
    defe = limit/young
    call rcvarc('F', 'CORR', '-', fami, kpg,&
                ksp, corrm, ibid)
    if (corrm .le. 15.d0) then
        defc = 2.345d-01-(1.11d-02*corrm)
    else
        defc = 5.1d-02-(6.d-04*corrm)
    endif
    nuetoi=0.5d0-(defe/defc)*(0.5d0-nu)
    defpc(1) = defc
    defpc(2)=-1.d0*nuetoi*defc
    defpc(3)=defpc(2)
!
!     CALCUL DE LA DEFORMATION PLAST EQUIV CRITIQUE
    ecumc = defpc(1) * defpc(1)
    ecumc = ecumc + defpc(2) * defpc(2)
    ecumc = ecumc + defpc(3) * defpc(3)
    ecumc = (2.d0 / 3.d0) * ecumc
    ecumc = ecumc ** 0.5d0
!
!     CALCUL DE DEFORMATION PLASTIQUE EQUIV DE DEBUT D'ENDOMMAGMENT
    ecumd = 0.8d0*ecumc
!
!     CALCUL DE TRIAXIALITE
!
!     PARAMETRES COEF2-LAMBDA ET COEF1-2MU
    coef1 = (young / (1.d0 + nu))
    coef2 = (nu * young) / ( (1.d0 + nu) * (1.d0 - (2.d0 * nu)) )
!
!     DES INITIALISATIONS POUR MATRICE TANGENTE ?
    do 50 i = 1, ndimsi
        sigp(i) = sigm(i)
        sigfi(i)= sigp(i)
50  end do
!
!     DEFORMATION PLASTIQUE EQUIV A L'INSTANT M
    ecum = ecumm
!
!     CALCUL DES CONTRAINTES ELASTIQUES
    treps = deps(1)+deps(2)+deps(3)
    do 85 i = 1, ndimsi
        sigp(i) = sigp(i)+coef1*deps(i)
85  end do
    do 90 i = 1, 3
        sigp(i) = sigp(i)+coef2*treps
90  end do
!
    dp = 0.d0
    dconv=.false.
    iter = 0
    itd=0
    premd=.true.
!
!
!   999=RETOUR ENDO
999  continue
!
    if (.not. dconv) then
!
!      CALCUL DE J2(SIG)
        j2 = 0.d0
        do 52 i = 1, ndimsi
            j2 = j2 + (sigp(i)** 2)
52      continue
        j2 = j2 - ((1.d0 / 3.d0) * ((sigp(1) + sigp(2) + sigp(3)) ** 2))
        j2 = ( 3.d0 / 2.d0 * j2) ** 0.5d0
!
!      CALCUL D'ECROUISSAGE
        rini = kcoef*(ecum**(1.d0/mcoef))
!
!       SURFACE SEUIL
        crit0 = ( (j2/(1.d0-dcoef)) - rini - limit )
        crit=crit0
!
        if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
!
889          continue
!
            if (crit0 .lt. 0.d0) then
                plas=0.d0
                vip(3)=plas
                dconv = .true.
                dp = 0.d0
! ON SORT COMPLETEMENT
                goto 999
            else
                plas=1.d0
                vip(3)=plas
                pconv = .false.
!
!     PLASTICITE  (888 = RETOUR PLASTICITE)
888              continue
                if (.not. pconv) then
                    iter = iter + 1
                    if (iter .eq. itemax) then
                        call u2mess('A', 'MODELISA5_42')
                        codret=1
                        do 157 i = 1, ndimsi
                            sigp(i)=0.d0
157                      continue
                        vip(1)=vim(1)
                        vip(2)=vim(2)
                        vip(3)=vim(3)
                        goto 9999
                    endif
!     TERME1 : F(SIG,R)
                    terme1 = (j2/(1.d0-dcoef)) - rini - limit
!
!     TERME2(*) : DF(SIG,X,R) / DSIG
                    rbid=1.d0 / (j2*(1.d0-dcoef))
                    terme2(1)=rbid*(sigp(1)-0.5d0*sigp(2)-0.5d0*sigp(&
                    3))
                    terme2(2)=rbid*(sigp(2)-0.5d0*sigp(1)-0.5d0*sigp(&
                    3))
                    terme2(3)=rbid*(sigp(3)-0.5d0*sigp(1)-0.5d0*sigp(&
                    2))
                    do 54 i = 4, ndimsi
                        terme2(i) = rbid * 1.5d0 * sigp(i)
54                  continue
!
!     TERME3(*) : DF(SIG,X,R) / DSIG = DF(SIG,X,R) / DSIG
!
!     TERME4(*) : KE * TERME2
                    rbid=coef2*(terme2(1) + terme2(2) + terme2(3))
                    do 55 i = 1, ndimsi
                        terme4(i) = coef1 * terme2(i)
55                  continue
                    do 555 i = 1, 3
                        terme4(i) = terme4(i) + rbid
555                  continue
!
!     TERME5 = TERME2 : TERME4
                    terme5 = 0.d0
                    do 56 i = 1, ndimsi
                        terme5 = terme5 + terme2(i)*terme4(i)
56                  continue
!
!     TER11 : DF/DR*COEFFIC
                    ter11 = limit/kcoef
                    ter11 = j2/(kcoef*(1.d0-dcoef)) - ter11
                    ter11 = ter11**(1.d0-mcoef)
                    ter11 = kcoef/mcoef * ter11
                    ter11 = -1.d0 * ter11
!
!     DETERMINATION DE DELTAP
                    deltap = terme1 / ( terme5 - ter11)
!
!      CALCUL  DE TOUTES LES VARIABLES INTERNES :
                    do 95 i = 1, ndimsi
                        sigp(i) = sigp(i) - (deltap * terme4(i))
95                  continue
!
!     DETERMINATION DE LA DEFORMATION PLASTIQUE ET P
                    dp=deltap/(1.d0-dcoef)
                    ecum = ecum + dp
!
!     CALCUL DE J2(SIG)
                    j2 = 0.d0
                    do 58 i = 1, ndimsi
                        j2 = j2 + (sigp(i)** 2)
58                  continue
                    j2 = j2 - ( (1.d0 / 3.d0) * ((sigp(1) + sigp(2) + sigp(3)) ** 2))
                    j2 = ( 3.d0 / 2.d0 * j2) ** 0.5d0
!
!     DETERMINATION DE L'ECROUISSAGE
                    rini=kcoef*(ecum**(1.d0/mcoef))
!
!     SURFACE SEUIL
                    crit = (j2/(1.d0-dcoef))- rini - limit
                    pconv = (abs(crit/crit0) .le. resi)
! FIN IF PCONV
                    goto 888
                endif
            endif
!
!     CRITERE D'ENDOMMAGEMENT
            criten = ecum - ecumd
            if (criten .le. 0.d0) then
                dconv = .true.
            else
!     COEFFICIENT D'ENDOMMAGEMENT
                dcoef = coefdc*(ecum-ecumd)/(ecumc-ecumd)
                if (dcoef .gt. 0.99d0) then
                    dconv = .true.
                    dcoef = 0.99d0
                    do 105 i = 1, ndimsi
                        sigp(i) = 0.d0
105                  continue
                endif
!
                crit2 = (j2/(1.d0-dcoef))- rini - limit
                if (premd) then
                    crit0d = crit2
                    premd=.false.
                endif
                dconv = (abs(crit2/crit0d) .le. resi)
!
! SI PAS CONVERGENCE EN ENDO, RETOUR A LA PLASTICITE
                if (.not.dconv) then
                    pconv=.false.
                    itd=itd+1
                    goto 889
                endif
            endif
!
! FIN IF OPTION
        endif
! FIN IF NOT DCONV
    endif
    vip(1) = ecum
    vip(2) = dcoef
!
!     CALCUL DE LA MATRICE TANGENTE OU ELAS OU SECANTE DECHARGE
    mtang=(option.eq.'RIGI_MECA_TANG').or.&
     &      (option.eq.'FULL_MECA')
    melas=(option.eq.'RIGI_MECA_ELAS').or.&
     &      (option.eq.'FULL_MECA_ELAS')
!
!     ELASTIQUE
    if ((option.eq.'RIGI_MECA') .or. melas .or. (mtang.and.(plas.lt.0.5d0))) then
!
        call matini(6, 6, 0.d0, dsidep)
!
        do 160 k = 1, 6
            dsidep(k,k) = coef1
160      continue
        do 170 k = 1, 3
            do 170 l = 1, 3
                dsidep(k,l) = dsidep(k,l) + coef2
170          continue
    endif
!
!     PLASTICITE
    if (mtang .and. (plas.ge.0.5d0)) then
        kci = 1.d0
        if (option(1:14) .eq. 'RIGI_MECA_TANG') then
            rbid = sigfi(1) + sigfi(2) + sigfi(3)
            do 175 k = 1, 3
                sigd(k) = sigfi(k)- rbid * (1.d0/3.d0)
175          continue
            do 176 k = 4, ndimsi
                sigd(k)= sigfi(k)
176          continue
        else
            rbid = sigp(1) + sigp(2) + sigp(3)
            do 177 k = 1, 3
                sigd(k) = sigp(k)- rbid*(1.d0 / 3.d0)
177          continue
            do 178 k = 4, ndimsi
                sigd(k)= sigp(k)
178          continue
        endif
        drdp = (ecum**((1.d0/mcoef)-1.d0))
        drdp = (kcoef/mcoef)*drdp
!
        if (dcoef .le. 0.d0) then
!          PLASTICITE SANS ENDOMMAGEMENT
            hp = 1.d0+((3.d0/2.d0)*coef1*kci*dp)/((rini+limit))
            lamda = coef2+((coef1/3.d0)*(1.d0-(1.d0/hp)))
            deumu = coef1/hp
            bcoef = 1.d0- (((drdp*dp)/(rini+limit)))
            bcoef = kci*((9.d0*(coef1**2))/(4.d0*hp))*bcoef
            ccoef = drdp+((3.d0/2.d0)*coef1)
            call matini(6, 6, 0.d0, dsidep)
            do 210 k = 1, ndimsi
                dsidep(k,k) = deumu
210          continue
            do 220 k = 1, 3
                do 220 m = 1, 3
                    dsidep(k,m) = dsidep(k,m) + lamda
220              continue
            do 230 k = 1, ndimsi
                do 230 m = 1, ndimsi
                    dsidep(k,m) = dsidep(k,m) -((bcoef/ccoef)* ((sigd(k)/(rini+limit))*(sigd(m)/(&
                                  &rini+limit))))
230              continue
!
        else
!          PLASTICITE ET ENDOMMAGEMENT
            hp = (1.d0+((3.d0/2.d0)*coef1*kci*dp)/ ((1.d0-dcoef)*( rini+limit)) )
            lamda = coef2+((coef1/3.d0)*(1.d0-(1.d0/hp)))
            deumu = coef1/hp
            acoef = (coefdc/(ecumc-ecumd))
            bcoef = (&
                    1.d0- (&
                    dp*( ( (1.d0-dcoef)*drdp) - (rini*acoef))/(( 1.d0-dcoef)*(rini+limit ) ))&
                    )
            bcoef = kci*((9.d0*(coef1**2))/(4.d0*hp))*bcoef
            ccoef = (((1.d0-dcoef)*drdp)+((3.d0/2.d0)*coef1) -(rini* acoef))
!
            call matini(6, 6, 0.d0, dsidep)
!
            do 250 k = 1, ndimsi
                dsidep(k,k) = deumu
250          continue
            do 260 k = 1, 3
                do 260 m = 1, 3
                    dsidep(k,m) = dsidep(k,m) + lamda
260              continue
            do 270 k = 1, ndimsi
                do 270 m = 1, ndimsi
                    dsidep(k,m) = (&
                                  dsidep(k,m) -((bcoef/ccoef)* ((sigd(k)/((1.d0-dcoef)*(rini+limi&
                                  &t))) *(sigd(m)/( (1.d0-dcoef)*(rini+limit)))))&
                                  )
270              continue
        endif
    endif
!
!     CAS RIGI_MECA_ELAS ET FULL_MECA_ELAS AVEC ENDOMMAGEMENT
    if (melas .and. (dcoef.ge.0.d0)) then
        do 327 k = 1, ndimsi
            do 327 m = 1, ndimsi
                dsidep(k,m)=(1.d0-dcoef)*dsidep(k,m)
327          continue
    endif
!
9999  continue
end subroutine
