      FUNCTION NMCHCR (DP)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/12/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C.======================================================================
C RESPONSABLE PROIX J.M.PROIX
      IMPLICIT NONE
C
C      NMCHCR   -- CETTE ROUTINE CONCERNE L'INTEGRATION DE LA LOI
C                  DE COMPORTEMENT 'VISC_CINX_CHAB' OU 'VMIS_CINX_CHAB'
C                  RESOLUTION DE L'EQUATION SCALAIRE NON LINEAIRE EN DP
C                  (INCREMENT DE DEFORMATION PLASTIQUE CUMULEE) :
C
C  (RP/RPPMDP)*||SIGEDV- 2/3+MP1*ALPHAM1-2/3+MP1*ALPHAM2)|| = RP
C
C                  CETTE EQUATION EST RELATIVE AU MODELE DE CHABOCHE
C                  A UN OU DEUX TENSEURS CINEMATIQUES
C                  ET ELLE EST RESOLUE PAR UNE METHODE DE SECANTES
C
C   ARGUMENT        E/S  TYPE         ROLE
C    MAT(6+2*NBVAR) IN    R       TABLEAU DES COEFFICIENTS
C                                 D'ECROUISSAGE DU MATERIAU
C    DP             IN    R       INCREMENT DE DEFORMATION PLASTIQUE
C                                 CUMULEE
C    PM             IN    R       DEFORMATION PLASTIQUE CUMULEE A
C                                 L'INSTANT DU CALCUL PRECEDENT
C    NDIMSI         IN    I       DIMENSION DU VECTEUR DES CONTRAINTES
C                                 I.E. 4 EN 2D ET 6 EN 3D
C    SIGEDV(6)       IN    R       VECTEUR DES CONTRAINTES D'ESSAI, I.E.
C                                 SIGEDV = MU/(MU-)*SIGM +2MU*DELTA_EPS
C    NBVAR          IN    R       NOMBRE DE TENSEURS DE RAPPEL
C    ALFAM(6)       IN    R       LE TENSEUR DE RAPPEL XM A L'INSTANT
C    ALFA2M(6)                     DU CALCUL PRECEDENT EST RELIE
C                                 AU TENSEUR ALFAM PAR XM = 2/3*C*ALFAM
C    DEUXMU         IN    R       COEFFICIENT DE LAME :2*MU
C    VISC           IN    I       INDICATEUR DE VISCOSITE
C    MEMO           IN    I       INDICATERU EFFET MEMOIRE
C    RM             IN    R       R(INSTM)
C    RP             IN    R       R(INSTP)=RM+DR
C    QM             IN    R       Q(PM)
C    QP             OUT   R       Q(PM+DP)
C    KSIM           IN    R       KSI(PM)
C    KSIP           OUT   R       KSI(PM+DP)
C    DT             IN    R       VALEUR DE L'INCREMENT DE TEMPS DELTAT
C    F              OUT   R       VALEUR DU CRITERE DE PLASTICITE
C                                 POUR LA VALEUR DP
C
      INTEGER NDIMSI,NBVAR,VISC,MEMO,I,IDELTA
      REAL*8 NMCHCR,DP,CRITME,DQ,DKSI(6),GQ
      REAL*8 EPSPP(6),MAT(18),PM,SIGEDV(6),ALFAM(6),DEUXMU
      REAL*8 EPSPM(6), F,ALFA2M(6),DT,RM,RP,QM,Q,KSIM(6),KSI(6)
      REAL*8 R0,RINF,B,CINF,K,W,GAMMA0,AINF,C2INF ,GAMM20
      REAL*8 ZERO,UN,DEUX,TROIS,C2P,GAMM2P,M2P,DELTA1,DELTA2,N1,N2
      REAL*8 PP,CP,GAMMAP,MP,RPPMDP,SEQ,S(6),R8MIEM,GRJEPS,DDOT,NORM(6)
      REAL*8 MUMEM,VALDEN,KVI,ETAM,Q0MEM,QMMEM,DR,DEPSP(6)
      REAL*8 RPP,COEF,DENOM,SDENOM(6),BETA1,BETA2
      COMMON/FCHAB/MAT,PM,SIGEDV,EPSPM,ALFAM,ALFA2M,DEUXMU,RM,RP,
     &    QM,Q,KSIM,KSI,DT,N1,N2,DEPSP,
     &    BETA1,BETA2,NDIMSI,NBVAR,VISC,MEMO,IDELTA
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     ===============
      ZERO   =  0.0D0
      UN     =  1.0D0
      DEUX   =  2.0D0
      TROIS  =  3.0D0
C
C --- COEFFICIENTS D'ECROUISSAGE DU MATERIAU :
C     --------------------------------------
      R0     = MAT(1)
      RINF   = MAT(2)
      B      = MAT(3)
      CINF   = MAT(4)
      K      = MAT(5)
      W      = MAT(6)
      GAMMA0 = MAT(7)
      AINF   = MAT(8)
      IF (NBVAR.EQ.2) THEN
         C2INF =MAT(9)
         GAMM20=MAT(10)
      ENDIF
      IF (VISC.EQ.1) THEN
         VALDEN=MAT(11)
         KVI =MAT(12)
      ENDIF
      IF (MEMO.EQ.1) THEN
         ETAM=MAT(13)
         Q0MEM=MAT(14)
         QMMEM=MAT(15)
         MUMEM=MAT(16)
      ENDIF
      IF (IDELTA.GT.0) THEN
         DELTA1 = MAT(17)
         DELTA2 = MAT(18)
      ELSE
         DELTA1=1.D0
         DELTA2=1.D0
      ENDIF
      BETA1=0.D0
      BETA2=0.D0
C
C --- CALCUL DES DIFFERENTS TERMES INTERVENANT DANS LE CRITERE
C --- DE PLASTICITE :
C     =============
      PP     = PM + DP
      CP     = CINF * (UN+(K-UN)*EXP(-W*PP))
      GAMMAP = GAMMA0 * (AINF + (UN-AINF)*EXP(-B*PP))
      MP     = CP/(UN+GAMMAP*DP*DELTA1)
      IF (NBVAR.EQ.2) THEN
         C2P = C2INF  * (UN+(K-UN)*EXP(-W*PP))
         GAMM2P = GAMM20 * (AINF + (UN-AINF)*EXP(-B*PP))
         M2P     = C2P/(UN+GAMM2P*DP*DELTA2)
       ELSE
         C2P=ZERO
         GAMM2P=ZERO
         M2P=ZERO
      ENDIF

C CALCUL DE LA NORMALE
      SEQ = ZERO
      DO 10 I = 1, NDIMSI
        IF (NBVAR.EQ.1) THEN
            S(I) = SIGEDV(I) -DEUX/TROIS*MP*ALFAM(I)
        ELSEIF (NBVAR.EQ.2) THEN
            S(I) = SIGEDV(I) -DEUX/TROIS*MP*ALFAM(I)
     &                       -DEUX/TROIS*M2P*ALFA2M(I)
        ENDIF
        SEQ  = SEQ + S(I)*S(I)
  10  CONTINUE
      SEQ = SQRT(TROIS/DEUX*SEQ)
      DO 20 I=1,NDIMSI
         NORM(I)=SQRT(1.5D0)*S(I)/SEQ
  20  CONTINUE

C     R(P) SANS EFFET DE MEMOIRE
      IF (MEMO.EQ.0) THEN
         RPP  = RINF + (R0-RINF)*EXP(-B*PP)
      ENDIF

      CALL DCOPY(NDIMSI,NORM,1,DEPSP,1)
      CALL DSCAL(NDIMSI,DP*SQRT(1.5D0),DEPSP,1)
      
      IF (MEMO.EQ.1) THEN

C --- DETERMINATION DE L'INCREMENT DES DEFORMATIONS PLASTIQUES

         CALL DCOPY(NDIMSI,EPSPM,1,EPSPP,1)
         CALL DAXPY(NDIMSI,1.D0,DEPSP,1,EPSPP,1)
  
         GRJEPS=0.0D0
         DO 122 I=1,NDIMSI
             GRJEPS=GRJEPS+(EPSPP(I)-KSIM(I))**2
  122    CONTINUE
         GRJEPS=SQRT(GRJEPS*1.5D0)
         CRITME=GRJEPS/1.5D0-QM
         IF (CRITME.LE.0.0D0) THEN
            DQ=0.0D0
            DO 123 I=1,NDIMSI
               DKSI(I)=0.0D0
  123       CONTINUE
         ELSE
            DQ=ETAM*CRITME
            COEF=ETAM*QM+DQ
            DO 124 I=1,NDIMSI
               IF (COEF.GT.R8MIEM()) THEN
                  DKSI(I)=(1.D0-ETAM)*DQ*(EPSPP(I)-KSIM(I))/COEF
               ELSE
                  DKSI(I)=0.D0
               ENDIF
  124       CONTINUE
C            test partie positive de <n:n*>. Utilité ?
C            NNE=0.D0
C            DO I=1,NDIMSI
C            NNE=NNE+DEPSP(I)*DKSI(I)
C            ENDDO
C            IF (NNE.LT.0.D0) THEN
C             DQ=0
C             DO i=1,NDIMSI
C             DKSI(I)=0.D0
C             KSI(I)=KSIM(I)
C             ENDDO
C            ENDIF
         ENDIF
         Q=QM+DQ
         DO 125 I=1,NDIMSI
            KSI(I)=KSIM(I)+DKSI(I)
  125    CONTINUE
         GQ=QMMEM+(Q0MEM-QMMEM)*EXP(-2.D0*MUMEM*Q)
         DR=B*(GQ-RM)*DP/(1.D0+B*DP)
         RP = RM + DR
         RPP = R0 + RP
      ENDIF

      
      N1=1.D0
      N2=1.D0
      IF (IDELTA.GT.0) THEN
C        CALCUL DES BETA - N1, N2 - EFFET NON RADIAL
         BETA1=DDOT(NDIMSI,ALFAM,1,NORM,1)/SQRT(1.5D0)
         BETA2=DDOT(NDIMSI,ALFA2M,1,NORM,1)/SQRT(1.5D0)
         IF ((IDELTA.EQ.1).OR.(IDELTA.EQ.3)) THEN
            N1=(1.D0+GAMMAP*DELTA1*DP-GAMMAP*(1.D0-DELTA1)*BETA1)
            N1=N1/(1.D0+GAMMAP*DP)
         ENDIF
         IF ((IDELTA.EQ.2).OR.(IDELTA.EQ.3)) THEN
            N2=(1.D0+GAMM2P*DELTA2*DP-GAMM2P*(1.D0-DELTA2)*BETA2)
            N2=N2/(1.D0+GAMM2P*DP)
         ENDIF
      ENDIF

  
C POUR NORMER L'EQUATION
      DENOM = ZERO
      DO 30 I = 1, NDIMSI
        IF (NBVAR.EQ.1) THEN
            SDENOM(I) = SIGEDV(I) -DEUX/TROIS*CINF*ALFAM(I)
        ELSEIF (NBVAR.EQ.2) THEN
            SDENOM(I) = SIGEDV(I) -DEUX/TROIS*CINF*ALFAM(I)
     &                            -DEUX/TROIS*C2INF*ALFA2M(I)
        ENDIF
        DENOM=DENOM+SDENOM(I)*SDENOM(I)
  30  CONTINUE
      DENOM = SQRT(TROIS/DEUX*DENOM)
      
      RPPMDP = RPP + (TROIS/DEUX*DEUXMU+MP*N1+M2P*N2)*DP
      
      IF (VISC.EQ.1) THEN
         RPPMDP = RPPMDP + KVI*((DP/DT)**(UN/VALDEN))
      ENDIF
      IF (DENOM.LE.R8MIEM()) THEN
         F = SEQ - RPPMDP
      ELSE
         F = (SEQ - RPPMDP)/DENOM
      ENDIF

      NMCHCR=-F
      
      END
