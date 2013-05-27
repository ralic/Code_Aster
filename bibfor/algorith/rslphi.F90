subroutine rslphi(fami, kpg, ksp, loi, imat,&
                  troisk, troimu, depsmo, rigdmo, rieleq,&
                  pi, d, s1, ann, theta,&
                  acc, f, df, sig0, eps0,&
                  mexpo, dt, phi, phip, rigeq,&
                  rigm, p, overfl)
    implicit none
!       ======================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_21
!       ======================================================
!       CALCUL DE LA FONCTION A ANNULER ET SA DERIVE POUR
!          CALCULER L'INCREMENT DF POUR LA LOI DE ROUSSELIER
!          PHI(DF)=RIGEQ(DF)-R(P(DF))+D*S1*F(DF)*EXP(RIGM(DF)/S1)=0
!
!       IN  IMAT   :  ADRESSE DU MATERIAU CODE
!           FAMI   :  FAMILLE DU POINT DE GAUSS
!           KPG    :  POINT DE GAUSS
!           KSG    :  SOUS-POINT DE GAUSS
!           TROISK :  ELASTICITE : PARTIE MOYENNE
!           TROIMU :  ELASTICITE : 2/3*PARTIE DEVIATORIQUE
!           DEPSMO :  INCREMENT DEFORMATION : PARTIE MOYENNE
!           RIGDMO :  CONTRAINTE REDUITE INITIALE : PARTIE MOYENNE
!           RIELEQ :  (SIGFDV/RHO+2MU*DEPSDV)     : PARTIE EQUIVALENTE
!           PI     :  PLASTICITE CUMULE INITIALE
!           F      :  INCONNU (POROSITE)
!           DF     :  INCREMENT D INCONNU
!           DT     :  INTERVALLE DE TEMPS DT
!       OUT PHI    :  FONCTION A ANNULE
!           PHIP   :  DERIVE DE PHI / INCONNU
!           RIGEQ  :  CONTRAINTE EQUIVALENTE
!           RIGM   :  CONTRAINTE MOYENNE
!           P      :  PLASTICITE CUMULE
!       -------------------------------------------------------------
    include 'asterc/r8miem.h'
    include 'asterfort/rsliso.h'
    integer :: imat, kpg, ksp
!
    character(len=16) :: loi
    character(len=*) :: fami
!
    real(kind=8) :: troisk, troimu, deux, coeffa, dt
    real(kind=8) :: rieleq, rigeq, rigm, depsmo, rigdmo, coeffb
    real(kind=8) :: pi, p, dp, f, df, phi, phip, d, s1, discri
    real(kind=8) :: rp, drdp, d13, un, unmf, unex, dunex
    real(kind=8) :: drigm, ddp, acc, expo, dexpo, ptheta
    real(kind=8) :: coeffc, theta, ftheta, ftot, ann, zero
    real(kind=8) :: sig0, eps0, mexpo, puiss
    real(kind=8) :: seuil, dseuil, dpuiss, asinh, lv1, lv2, lv3
!
    logical :: overfl
!
    parameter       ( un     = 1.d0  )
    parameter       ( zero   = 0.d0  )
    parameter       ( deux   = 2.d0  )
    parameter       ( d13    = .33333333333333D0 )
!
!      -------------------------------------------------------------
!
    ftheta = f - (un - theta)*df
    unmf = (un-ftheta)
!
! ----- CALCUL DE RIGM ET DRIGM/DDF---------------
    rigm = rigdmo + troisk*theta*(depsmo - d13*df/unmf/acc)
    drigm = -troisk*d13*theta*(unmf+theta*df)/(unmf**2)/acc
    expo = d*exp(rigm/s1)
    dexpo = expo*(drigm/s1)
    unex = unmf*expo*acc
    dunex = (-theta*expo + unmf*dexpo)*acc
!
! ----- CALCUL DE DP ET DDP/DDF-----------
! ----- ABSENCE DE GERMINATION---------------
    if (ann .eq. zero) then
        dp = df/(ftheta*unex)
        ddp = (un - df*dunex/unex -df*theta/ftheta)/(ftheta*unex)
! ----- AN NON NUL---------------
    else
        coeffa = deux*ann*theta
        coeffb = ftheta + ann*pi
        coeffc = df/unex
        discri = coeffb**2 + deux*coeffa*coeffc
        if (coeffc .le. r8miem()) then
            dp=0.d0
        else
            dp=(-coeffb+sqrt(discri))/coeffa
        endif
        ddp=((un-df*dunex/unex )/unex -theta*dp)/(coeffa*dp+coeffb)
    endif
    p = pi+dp
    ptheta= pi +theta*dp
! --- RESTONS DANS LES LIMITES DU RAISONNABLE ----
    if (p .gt. 1.d100) then
        overfl = .true.
        goto 9999
    else
        overfl = .false.
    endif
!
! ----- CALCUL DE RIGEQ ---------------
    rigeq = rieleq - troimu*theta*dp
!
! ----- CALCUL DE R(P) ET DR/DP(P) ----
    call rsliso(fami, kpg, ksp, '+', imat,&
                ptheta, rp, drdp)
!
    ftot = ftheta + ann*ptheta
!
! ----- CALCUL DE PHI -----------------
!
    phi = rigeq - rp + s1*ftot*expo
!
!
! ----- CALCUL DE DPHI/DDF -------------
!
    phip = s1*(ftot*dexpo + (theta + ann*theta*ddp)*expo) -(troimu+drdp )*theta*ddp
!
!
    if (loi(1:10) .eq. 'ROUSS_VISC') then
        seuil = phi
        dseuil = phip
!          PUISS = (DP/(DT*EPS0))**(UN/MEXPO)
!          DPUISS = ( (DP/(DT*EPS0))**(UN/MEXPO-UN) )/( MEXPO*DT*EPS0 )
        if (dp .eq. 0.d0) then
            puiss = 0.d0
            dpuiss = 0.d0
        else
            lv1 = dp / (dt*eps0)
            lv2 = un / mexpo - un
            lv3 = mexpo * dt * eps0
            puiss = ( lv1 )**(un/mexpo)
            dpuiss = ( lv1**lv2 ) / lv3
        endif
!
        asinh = log(puiss + sqrt(un + puiss**2))
        phi = seuil - sig0*asinh
        phip = dseuil - sig0*ddp*dpuiss/sqrt(un+puiss**2)
    endif
!
! ----- ET C EST FINI -------------
9999  continue
end subroutine
