function nmchcr(dp)
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!.======================================================================
! person_in_charge: jean-michel.proix at edf.fr
    implicit none
!
!      NMCHCR   -- CETTE ROUTINE CONCERNE L'INTEGRATION DE LA LOI
!                  DE COMPORTEMENT 'VISC_CINX_CHAB' OU 'VMIS_CINX_CHAB'
!                  RESOLUTION DE L'EQUATION SCALAIRE NON LINEAIRE EN DP
!                  (INCREMENT DE DEFORMATION PLASTIQUE CUMULEE) :
!
!  (RP/RPPMDP)*||SIGEDV- 2/3+MP1*ALPHAM1-2/3+MP1*ALPHAM2)|| = RP
!
!                  CETTE EQUATION EST RELATIVE AU MODELE DE CHABOCHE
!                  A UN OU DEUX TENSEURS CINEMATIQUES
!                  ET ELLE EST RESOLUE PAR UNE METHODE DE SECANTES
!
!   ARGUMENT        E/S  TYPE         ROLE
!    MAT(6+2*NBVAR) IN    R       TABLEAU DES COEFFICIENTS
!                                 D'ECROUISSAGE DU MATERIAU
!    DP             IN    R       INCREMENT DE DEFORMATION PLASTIQUE
!                                 CUMULEE
!    PM             IN    R       DEFORMATION PLASTIQUE CUMULEE A
!                                 L'INSTANT DU CALCUL PRECEDENT
!    NDIMSI         IN    I       DIMENSION DU VECTEUR DES CONTRAINTES
!                                 I.E. 4 EN 2D ET 6 EN 3D
!    SIGEDV(6)       IN    R       VECTEUR DES CONTRAINTES D'ESSAI, I.E.
!                                 SIGEDV = MU/(MU-)*SIGM +2MU*DELTA_EPS
!    NBVAR          IN    R       NOMBRE DE TENSEURS DE RAPPEL
!    ALFAM(6)       IN    R       LE TENSEUR DE RAPPEL XM A L'INSTANT
!    ALFA2M(6)                     DU CALCUL PRECEDENT EST RELIE
!                                 AU TENSEUR ALFAM PAR XM = 2/3*C*ALFAM
!    DEUXMU         IN    R       COEFFICIENT DE LAME :2*MU
!    VISC           IN    I       INDICATEUR DE VISCOSITE
!    MEMO           IN    I       INDICATERU EFFET MEMOIRE
!    RM             IN    R       R(INSTM)
!    RP             IN    R       R(INSTP)=RM+DR
!    QM             IN    R       Q(PM)
!    QP             OUT   R       Q(PM+DP)
!    KSIM           IN    R       KSI(PM)
!    KSIP           OUT   R       KSI(PM+DP)
!    DT             IN    R       VALEUR DE L'INCREMENT DE TEMPS DELTAT
!    F              OUT   R       VALEUR DU CRITERE DE PLASTICITE
!                                 POUR LA VALEUR DP
!
    include 'asterc/r8miem.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    include 'blas/ddot.h'
    include 'blas/dscal.h'
    integer :: ndimsi, nbvar, visc, memo, i, idelta
    real(kind=8) :: nmchcr, dp, critme, dq, dksi(6), gq
    real(kind=8) :: epspp(6), mat(18), pm, sigedv(6), alfam(6), deuxmu
    real(kind=8) :: epspm(6), f, alfa2m(6), dt, rm, rp, qm, q, ksim(6), ksi(6)
    real(kind=8) :: r0, rinf, b, cinf, k, w, gamma0, ainf, c2inf, gamm20
    real(kind=8) :: zero, un, deux, trois, c2p, gamm2p, m2p, delta1, delta2, n1
    real(kind=8) :: n2
    real(kind=8) :: pp, cp, gammap, mp, rppmdp, seq, s(6), grjeps, norm(6)
    real(kind=8) :: mumem, valden, kvi, etam, q0mem, qmmem, dr, depsp(6)
    real(kind=8) :: rpp, coef, denom, sdenom(6), beta1, beta2
    common/fchab/mat,pm,sigedv,epspm,alfam,alfa2m,deuxmu,rm,rp,&
     &    qm,q,ksim,ksi,dt,n1,n2,depsp,&
     &    beta1,beta2,ndimsi,nbvar,visc,memo,idelta
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! --- INITIALISATIONS :
!     ===============
    zero = 0.0d0
    un = 1.0d0
    deux = 2.0d0
    trois = 3.0d0
!
! --- COEFFICIENTS D'ECROUISSAGE DU MATERIAU :
!     --------------------------------------
    r0 = mat(1)
    rinf = mat(2)
    b = mat(3)
    cinf = mat(4)
    k = mat(5)
    w = mat(6)
    gamma0 = mat(7)
    ainf = mat(8)
    if (nbvar .eq. 2) then
        c2inf =mat(9)
        gamm20=mat(10)
    endif
    if (visc .eq. 1) then
        valden=mat(11)
        kvi =mat(12)
    endif
    if (memo .eq. 1) then
        etam=mat(13)
        q0mem=mat(14)
        qmmem=mat(15)
        mumem=mat(16)
    endif
    if (idelta .gt. 0) then
        delta1 = mat(17)
        delta2 = mat(18)
    else
        delta1=1.d0
        delta2=1.d0
    endif
    beta1=0.d0
    beta2=0.d0
!
! --- CALCUL DES DIFFERENTS TERMES INTERVENANT DANS LE CRITERE
! --- DE PLASTICITE :
!     =============
    pp = pm + dp
    cp = cinf * (un+(k-un)*exp(-w*pp))
    gammap = gamma0 * (ainf + (un-ainf)*exp(-b*pp))
    mp = cp/(un+gammap*dp*delta1)
    if (nbvar .eq. 2) then
        c2p = c2inf * (un+(k-un)*exp(-w*pp))
        gamm2p = gamm20 * (ainf + (un-ainf)*exp(-b*pp))
        m2p = c2p/(un+gamm2p*dp*delta2)
    else
        c2p=zero
        gamm2p=zero
        m2p=zero
    endif
!
! CALCUL DE LA NORMALE
    seq = zero
    do 10 i = 1, ndimsi
        if (nbvar .eq. 1) then
            s(i) = sigedv(i) -deux/trois*mp*alfam(i)
        else if (nbvar.eq.2) then
            s(i) = sigedv(i) -deux/trois*mp*alfam(i) -deux/trois*m2p* alfa2m(i)
        endif
        seq = seq + s(i)*s(i)
10  end do
    seq = sqrt(trois/deux*seq)
    do 20 i = 1, ndimsi
        norm(i)=sqrt(1.5d0)*s(i)/seq
20  end do
!
!     R(P) SANS EFFET DE MEMOIRE
    if (memo .eq. 0) then
        rpp = rinf + (r0-rinf)*exp(-b*pp)
    endif
!
    call dcopy(ndimsi, norm, 1, depsp, 1)
    call dscal(ndimsi, dp*sqrt(1.5d0), depsp, 1)
!
    if (memo .eq. 1) then
!
! --- DETERMINATION DE L'INCREMENT DES DEFORMATIONS PLASTIQUES
!
        call dcopy(ndimsi, epspm, 1, epspp, 1)
        call daxpy(ndimsi, 1.d0, depsp, 1, epspp,&
                   1)
!
        grjeps=0.0d0
        do 122 i = 1, ndimsi
            grjeps=grjeps+(epspp(i)-ksim(i))**2
122      continue
        grjeps=sqrt(grjeps*1.5d0)
        critme=grjeps/1.5d0-qm
        if (critme .le. 0.0d0) then
            dq=0.0d0
            do 123 i = 1, ndimsi
                dksi(i)=0.0d0
123          continue
        else
            dq=etam*critme
            coef=etam*qm+dq
            do 124 i = 1, ndimsi
                if (coef .gt. r8miem()) then
                    dksi(i)=(1.d0-etam)*dq*(epspp(i)-ksim(i))/coef
                else
                    dksi(i)=0.d0
                endif
124          continue
!            test partie positive de <n:n*>. Utilité ?
!            NNE=0.D0
!            DO I=1,NDIMSI
!            NNE=NNE+DEPSP(I)*DKSI(I)
!            ENDDO
!            IF (NNE.LT.0.D0) THEN
!             DQ=0
!             DO i=1,NDIMSI
!             DKSI(I)=0.D0
!             KSI(I)=KSIM(I)
!             ENDDO
!            ENDIF
        endif
        q=qm+dq
        do 125 i = 1, ndimsi
            ksi(i)=ksim(i)+dksi(i)
125      continue
        gq=qmmem+(q0mem-qmmem)*exp(-2.d0*mumem*q)
        dr=b*(gq-rm)*dp/(1.d0+b*dp)
        rp = rm + dr
        rpp = r0 + rp
    endif
!
!
    n1=1.d0
    n2=1.d0
    if (idelta .gt. 0) then
!        CALCUL DES BETA - N1, N2 - EFFET NON RADIAL
        beta1=ddot(ndimsi,alfam,1,norm,1)/sqrt(1.5d0)
        beta2=ddot(ndimsi,alfa2m,1,norm,1)/sqrt(1.5d0)
        if ((idelta.eq.1) .or. (idelta.eq.3)) then
            n1=(1.d0+gammap*delta1*dp-gammap*(1.d0-delta1)*beta1)
            n1=n1/(1.d0+gammap*dp)
        endif
        if ((idelta.eq.2) .or. (idelta.eq.3)) then
            n2=(1.d0+gamm2p*delta2*dp-gamm2p*(1.d0-delta2)*beta2)
            n2=n2/(1.d0+gamm2p*dp)
        endif
    endif
!
!
! POUR NORMER L'EQUATION
    denom = zero
    do 30 i = 1, ndimsi
        if (nbvar .eq. 1) then
            sdenom(i) = sigedv(i) -deux/trois*cinf*alfam(i)
        else if (nbvar.eq.2) then
            sdenom(i) = sigedv(i) -deux/trois*cinf*alfam(i) -deux/ trois*c2inf*alfa2m(i)
        endif
        denom=denom+sdenom(i)*sdenom(i)
30  end do
    denom = sqrt(trois/deux*denom)
!
    rppmdp = rpp + (trois/deux*deuxmu+mp*n1+m2p*n2)*dp
!
    if (visc .eq. 1) then
        rppmdp = rppmdp + kvi*((dp/dt)**(un/valden))
    endif
    if (denom .le. r8miem()) then
        f = seq - rppmdp
    else
        f = (seq - rppmdp)/denom
    endif
!
    nmchcr=-f
!
end function
