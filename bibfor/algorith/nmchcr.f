      SUBROUTINE NMCHCR (MAT,DP,PM,NDIMSI,SIGEDV,NBVAR,EPSPM,ALFAM,
     &  ALFA2M,DEUXMU,VISC,MEMO,RM,RP,QM,Q,KSIM,KSI,DT,F)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/01/2009   AUTEUR PROIX J-M.PROIX 
C TOLE CRP_21
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE JMBHH01 J.M.PROIX
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
C -----  ARGUMENTS
          INTEGER             NDIMSI,NBVAR,VISC,MEMO
           REAL*8      EPSPP(6),MAT(16),PM,SIGEDV(6),ALFAM(6),DEUXMU,DP
           REAL*8    EPSPM(6), F,ALFA2M(6),DT,RM,RP,QM,Q,KSIM(6),KSI(6)
C -----  VARIABLES LOCALES
           INTEGER     I
           REAL*8      R0,RINF,B,CINF,K,W,GAMMA0,AINF,C2INF ,GAMM20
           REAL*8      ZERO,UN,DEUX,TROIS,C2P,GAMM2P,M2P,XN,VI(6)
           REAL*8      PP,CP,GAMMAP,MP,RPPMDP,SEQ,S(6),R8MIEM,GRJEPS
           REAL*8      MUMEM,VALDEN,KVI,ETAM,Q0MEM,QMMEM,DR,DEPSP(6)
           REAL*8      CRITME,DQ,DKSI(6),XXN,PETIN(6),PETIN2(6),GQ
           REAL*8      DEPPEQ,RPP,DQ1,NORME2,COEF,DENOM,SDENOM(6)
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
C
C --- CALCUL DES DIFFERENTS TERMES INTERVENANT DANS LE CRITERE
C --- DE PLASTICITE :
C     =============
      PP     = PM + DP
      CP     = CINF * (UN+(K-UN)*EXP(-W*PP))
      GAMMAP = GAMMA0 * (AINF + (UN-AINF)*EXP(-B*PP))
      MP     = CP/(UN+GAMMAP*DP)
      IF (NBVAR.EQ.2) THEN
         C2P = C2INF  * (UN+(K-UN)*EXP(-W*PP))
         GAMM2P = GAMM20 * (AINF + (UN-AINF)*EXP(-B*PP))
         M2P     = C2P/(UN+GAMM2P*DP)
       ELSE
         C2P=ZERO
         GAMM2P=ZERO
         M2P=ZERO
      ENDIF

      IF (MEMO.EQ.1) THEN
      
C --- DETERMINATION DE L'INCREMENT DES DEFORMATIONS PLASTIQUES
         DEPPEQ=0.D0
         DO 120 I = 1, NDIMSI
            DEPSP(I)=SIGEDV(I)
     &            - (MP*DP*ALFAM(I)-M2P*DP*ALFA2M(I))/1.5D0
            DEPPEQ=DEPPEQ+DEPSP(I)*DEPSP(I) 
  120    CONTINUE
         DEPPEQ=SQRT(DEPPEQ*1.5D0)
         DO 121 I = 1, NDIMSI
            DEPSP(I)=1.5D0*DP*DEPSP(I)/DEPPEQ
            EPSPP(I)=EPSPM(I)+DEPSP(I)
  121    CONTINUE
         GRJEPS=0.0D0
         DO 17 I=1,NDIMSI
             GRJEPS=GRJEPS+(EPSPP(I)-KSIM(I))**2
   17    CONTINUE
         GRJEPS=SQRT(GRJEPS*1.5D0)
         CRITME=GRJEPS/1.5D0-QM
         IF (CRITME.LE.0.0D0) THEN
            DQ=0.0D0
            DO 18 I=1,NDIMSI
               DKSI(I)=0.0D0
   18       CONTINUE
         ELSE
            DQ=ETAM*CRITME
            COEF=ETAM*QM+DQ
            DO 19 I=1,NDIMSI
               IF (COEF.GT.R8MIEM()) THEN
                  DKSI(I)=(1.D0-ETAM)*DQ*(EPSPP(I)-KSIM(I))/COEF
               ELSE
                  DKSI(I)=0.D0
               ENDIF
               KSI(I)=KSIM(I)+DKSI(I)
   19       CONTINUE
         ENDIF   
         Q=QM+DQ
         GQ=QMMEM+(Q0MEM-QMMEM)*EXP(-2.D0*MUMEM*Q)
         DR=B*(GQ-RM)*DP/(1.D0+B*DP)
         RP = RM + DR
         RPP = R0 + RP 
      ELSEIF (MEMO.EQ.0) THEN
         RPP     = RINF + (R0-RINF)*EXP(-B*PP)
      ENDIF   
      
      SEQ = ZERO
C POUR NORMER L'EQUATION      
      DENOM = ZERO
C
      DO 10 I = 1, NDIMSI
C
        IF (NBVAR.EQ.1) THEN
            S(I) = SIGEDV(I) -DEUX/TROIS*MP*ALFAM(I)
            SDENOM(I) = SIGEDV(I) -DEUX/TROIS*CINF*ALFAM(I)
        ELSEIF (NBVAR.EQ.2) THEN
            S(I) = SIGEDV(I) -DEUX/TROIS*MP*ALFAM(I)
     &                       -DEUX/TROIS*M2P*ALFA2M(I)
            SDENOM(I) = SIGEDV(I) -DEUX/TROIS*CINF*ALFAM(I)
     &                            -DEUX/TROIS*CINF*ALFA2M(I)
        ENDIF
        SEQ  = SEQ + S(I)*S(I)
        DENOM=DENOM+SDENOM(I)*SDENOM(I)
C
  10  CONTINUE
      SEQ = SQRT(TROIS/DEUX*SEQ)
      DENOM = SQRT(TROIS/DEUX*DENOM)
      RPPMDP = RPP + (TROIS/DEUX*DEUXMU+MP+M2P)*DP
      IF (VISC.EQ.1) THEN
         RPPMDP = RPPMDP + KVI*((DP/DT)**(UN/VALDEN))
      ENDIF
      IF (DENOM.LE.R8MIEM()) THEN   
         F = SEQ - RPPMDP         
      ELSE                        
         F = (SEQ - RPPMDP)/DENOM      
      ENDIF

      END
