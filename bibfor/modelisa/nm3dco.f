      SUBROUTINE NM3DCO(FAMI,KPG,KSP,NDIM,OPTION,IMATE,TM,TP,SIGM,
     &             EPSM,DEPS,VIM,SIGP,VIP,DSIDEP,CRILDC,CODRET)
C MODIF MODELISA  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C TOLE CRP_20
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION

C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ----------------------------------------------------------------------

      IMPLICIT NONE
C ----------------------------------------------------------------------
C          LOI DE L'ACIER SOUMIS A LA CORROSION 3D

C IN  NDIM    : DIMENSION
C IN  OPTION  : OPTION DE CALCUL
C IN  IMATE   : POINTEUR MATERIAU
C IN  TM      : TEMPERATURE MOINS
C IN  TP      : TEMPERATURE PLUS
C IN  SIGM    : CONTRAINTE AU TEMPS MOINS
C IN  EPSM    : DEFORMATION TOTALE AU TEMPS MOINS
C IN  DEPS    : DEFORMATION  TOTALE PLUS - DEFORMATION TOTALE MOINS
C IN VIM      : VARIABLE INTERNES AU TEMPS MOINS
C IN CRILDC   : 1 ITERMAX, 3 RESI_INTE
C
C OUT SIGP     : CONTRAINTES PLUS
C OUT VIP       : VARIABLE INTERNES PLUS
C OUT DSIDEP    : DSIG/DEPS
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
      REAL*8 TP,TM
      REAL*8 SIGM(6),DEPS(6),VIM(*),EPSM(6)
      REAL*8 SIGP(6),VIP(*),DSIDEP(6,6),CRILDC(3)
      CHARACTER*16 OPTION
      CHARACTER*(*) FAMI
      INTEGER NDIM,IMATE,CODRET,KPG,KSP

      REAL*8 YOUNG,NU,KCOEF,MCOEF,COEFDC,LIMIT
      REAL*8 VALPAR
      CHARACTER*2  FB2,CODRES
      CHARACTER*8  NOMPAR
      INTEGER NBPAR

      INTEGER ITER,ITEMAX,NDIMSI,I,J,K,L,M,ITD,IBID

      REAL*8 RESI,ECUM,ECUMM,DCOEF,PLAS
      REAL*8 DEFE,DEFC,NUETOI,DEFPC(3),ECUMC,ECUMD
      REAL*8 COEF1,COEF2,J2,RINI,CRIT,CRIT0
      LOGICAL DCONV,PCONV,PREMD,MTANG,MELAS
      REAL*8 TERME1,TERME2(6),TERME4(6),TERME5,TER11
      REAL*8 DELTAP,DP
      REAL*8 CRITEN,CRIT2,CRIT0D,TREPS

      REAL*8 SIGFI(6),SIGD(6),RBID,DRDP,HP
      REAL*8 KCI,LAMDA,DEUMU,ACOEF,BCOEF,CCOEF,CORRM

2327  FORMAT(A4,6(2X,D12.6))

      NDIMSI=2*NDIM

      VALPAR = TP
      CALL RCVALA(IMATE,' ','CORR_ACIER',NBPAR,NOMPAR,VALPAR,1,'D_CORR',
     &              COEFDC,CODRES,FB2)
      CALL RCVALA(IMATE,' ','CORR_ACIER',NBPAR,NOMPAR,VALPAR,1,'ECRO_K',
     &              KCOEF,CODRES,FB2)
      CALL RCVALA(IMATE,' ','CORR_ACIER',NBPAR,NOMPAR,VALPAR,1,'ECRO_M',
     &              MCOEF,CODRES,FB2)
      CALL RCVALA(IMATE,' ','CORR_ACIER',NBPAR,NOMPAR,VALPAR,1,'SY',
     &              LIMIT,CODRES,FB2)
      CALL RCVALA(IMATE,' ','ELAS',NBPAR,NOMPAR,VALPAR,1,'NU',
     &              NU,CODRES,FB2)
       CALL RCVALA(IMATE,' ','ELAS',NBPAR,NOMPAR,VALPAR,1,'E',
     &              YOUNG,CODRES,FB2)


      ECUMM = VIM(1)
      DCOEF = VIM(2)
      PLAS  = VIM(3)

C --- PARAMETRES DE CONVERGENCE
      RESI = CRILDC(3)
      ITEMAX = NINT(CRILDC(1))


C     CALCUL DE LA DEFORMATION CRITIQUE
      DEFE = LIMIT/YOUNG
      CALL RCVARC('F','CORR','-',FAMI,KPG,KSP,CORRM,IBID)
      IF (CORRM .LE. 15.D0)  THEN
        DEFC = 2.345D-01-(1.11D-02*CORRM)
      ELSE
        DEFC = 5.1D-02-(6.D-04*CORRM)
      END IF
      NUETOI=0.5D0-(DEFE/DEFC)*(0.5D0-NU)
      DEFPC(1) = DEFC
      DEFPC(2)=-1.D0*NUETOI*DEFC
      DEFPC(3)=DEFPC(2)

C     CALCUL DE LA DEFORMATION PLAST EQUIV CRITIQUE
      ECUMC = DEFPC(1) * DEFPC(1)
      ECUMC = ECUMC + DEFPC(2) * DEFPC(2)
      ECUMC = ECUMC + DEFPC(3) * DEFPC(3)
      ECUMC = (2.D0 / 3.D0) * ECUMC
      ECUMC = ECUMC ** 0.5D0

C     CALCUL DE DEFORMATION PLASTIQUE EQUIV DE DEBUT D'ENDOMMAGMENT
      ECUMD = 0.8D0*ECUMC

C     CALCUL DE TRIAXIALITE

C     PARAMETRES COEF2-LAMBDA ET COEF1-2MU
      COEF1 = (YOUNG / (1.D0 + NU))
      COEF2 = (NU * YOUNG) / ( (1.D0 + NU) * (1.D0 - (2.D0 * NU)) )

C     DES INITIALISATIONS POUR MATRICE TANGENTE ?
      DO 50 I =1,NDIMSI
        SIGP(I) = SIGM(I)
        SIGFI(I)= SIGP(I)
 50   CONTINUE

C     DEFORMATION PLASTIQUE EQUIV A L'INSTANT M
      ECUM = ECUMM

C     CALCUL DES CONTRAINTES ELASTIQUES
      TREPS = DEPS(1)+DEPS(2)+DEPS(3)
      DO 85 I=1,NDIMSI
        SIGP(I) = SIGP(I)+COEF1*DEPS(I)
 85   CONTINUE
      DO 90 I=1,3
        SIGP(I) = SIGP(I)+COEF2*TREPS
 90   CONTINUE

      DP = 0.D0
      DCONV=.FALSE.
      ITER = 0
      ITD=0
      PREMD=.TRUE.


C   999=RETOUR ENDO
999   CONTINUE

      IF (.NOT. DCONV) THEN

C      CALCUL DE J2(SIG)
        J2 = 0.D0
        DO 52 I = 1,NDIMSI
          J2 = J2 + (SIGP(I)** 2)
 52     CONTINUE
        J2 = J2 - ((1.D0 / 3.D0) *
     &           ((SIGP(1) + SIGP(2) + SIGP(3)) ** 2))
        J2 = ( 3.D0 / 2.D0  *  J2) ** 0.5D0

C      CALCUL D'ECROUISSAGE
        RINI = KCOEF*(ECUM**(1.D0/MCOEF))

C       SURFACE SEUIL
        CRIT0 = ( (J2/(1.D0-DCOEF)) - RINI - LIMIT )
        CRIT=CRIT0

        IF ( OPTION(1:9).EQ.'FULL_MECA' .OR.
     &      OPTION(1:9).EQ.'RAPH_MECA' ) THEN

 889      CONTINUE

          IF (CRIT0.LT.0.D0 ) THEN
              PLAS=0.D0
              VIP(3)=PLAS
              DCONV = .TRUE.
              DP = 0.D0
C ON SORT COMPLETEMENT
              GOTO 999
              ELSE
              PLAS=1.D0
              VIP(3)=PLAS
              PCONV = .FALSE.

C     PLASTICITE  (888 = RETOUR PLASTICITE)
 888          CONTINUE
              IF (.NOT. PCONV)  THEN
                ITER = ITER + 1
                IF (ITER . EQ. ITEMAX) THEN
                   CALL U2MESS('A','MODELISA5_42')
                   CODRET=1
                   DO 157 I=1,NDIMSI
                      SIGP(I)=0.D0
 157               CONTINUE
                   VIP(1)=VIM(1)
                   VIP(2)=VIM(2)
                   VIP(3)=VIM(3)
                   GOTO 9999
                END IF
C     TERME1 : F(SIG,R)
                TERME1 = (J2/(1.D0-DCOEF)) - RINI - LIMIT

C     TERME2(*) : DF(SIG,X,R) / DSIG
                RBID=1.D0 / (J2*(1.D0-DCOEF))
                TERME2(1)=RBID*(SIGP(1)-0.5D0*SIGP(2)-0.5D0*SIGP(3))
                TERME2(2)=RBID*(SIGP(2)-0.5D0*SIGP(1)-0.5D0*SIGP(3))
                TERME2(3)=RBID*(SIGP(3)-0.5D0*SIGP(1)-0.5D0*SIGP(2))
                DO 54 I = 4,NDIMSI
                  TERME2(I) = RBID * 1.5D0 * SIGP(I)
 54             CONTINUE

C     TERME3(*) : DF(SIG,X,R) / DSIG = DF(SIG,X,R) / DSIG

C     TERME4(*) : KE * TERME2
                RBID=COEF2*(TERME2(1) + TERME2(2) + TERME2(3))
                DO 55 I = 1,NDIMSI
                  TERME4(I) = COEF1 * TERME2(I)
 55             CONTINUE
                DO 555 I = 1,3
                  TERME4(I) = TERME4(I) + RBID
 555             CONTINUE

C     TERME5 = TERME2 : TERME4
                TERME5 = 0.D0
                DO 56 I = 1,NDIMSI
                  TERME5 = TERME5 + TERME2(I)*TERME4(I)
 56             CONTINUE

C     TER11 : DF/DR*COEFFIC
                TER11 = LIMIT/KCOEF
                TER11 = J2/(KCOEF*(1.D0-DCOEF)) - TER11
                TER11 = TER11**(1.D0-MCOEF)
                TER11 = KCOEF/MCOEF * TER11
                TER11 = -1.D0 * TER11

C     DETERMINATION DE DELTAP
                DELTAP = TERME1 / ( TERME5 - TER11)

C      CALCUL  DE TOUTES LES VARIABLES INTERNES :
                DO 95 I = 1,NDIMSI
                  SIGP(I) = SIGP(I) - (DELTAP * TERME4(I))
 95             CONTINUE

C     DETERMINATION DE LA DEFORMATION PLASTIQUE ET P
                DP=DELTAP/(1.D0-DCOEF)
                ECUM = ECUM + DP

C     CALCUL DE J2(SIG)
                J2 = 0.D0
                DO 58 I = 1,NDIMSI
                  J2 = J2 + (SIGP(I)** 2)
 58             CONTINUE
                J2 = J2 - ((1.D0 / 3.D0) *
     &             ((SIGP(1) + SIGP(2) + SIGP(3)) ** 2))
                J2 = ( 3.D0 / 2.D0 * J2) ** 0.5D0

C     DETERMINATION DE L'ECROUISSAGE
                RINI=KCOEF*(ECUM**(1.D0/MCOEF))

C     SURFACE SEUIL
                CRIT = (J2/(1.D0-DCOEF))- RINI - LIMIT
                PCONV = (ABS(CRIT/CRIT0) .LE. RESI)
C FIN IF PCONV
                GOTO 888
            END IF
          END IF

C     CRITERE D'ENDOMMAGEMENT
          CRITEN = ECUM - ECUMD
           IF (CRITEN.LE.0.D0) THEN
               DCONV = .TRUE.
           ELSE
C     COEFFICIENT D'ENDOMMAGEMENT
             DCOEF = COEFDC*(ECUM-ECUMD)/(ECUMC-ECUMD)
             IF (DCOEF .GT. 0.99D0) THEN
               DCONV = .TRUE.
               DCOEF = 0.99D0
               DO 105 I = 1,NDIMSI
                 SIGP(I) = 0.D0
 105           CONTINUE
             END IF

             CRIT2 = (J2/(1.D0-DCOEF))- RINI - LIMIT
             IF (PREMD) THEN
                 CRIT0D = CRIT2
                 PREMD=.FALSE.
             ENDIF
             DCONV = (ABS(CRIT2/CRIT0D) .LE. RESI)

C SI PAS CONVERGENCE EN ENDO, RETOUR A LA PLASTICITE
             IF(.NOT.DCONV)THEN
                PCONV=.FALSE.
                ITD=ITD+1
                GOTO 889
             ENDIF
           END IF

C FIN IF OPTION
       END IF
C FIN IF NOT DCONV
      END IF
      VIP(1) = ECUM
      VIP(2) = DCOEF

C     CALCUL DE LA MATRICE TANGENTE OU ELAS OU SECANTE DECHARGE
      MTANG=(OPTION.EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION.EQ.'FULL_MECA')
      MELAS=(OPTION.EQ.'RIGI_MECA_ELAS').OR.
     &      (OPTION.EQ.'FULL_MECA_ELAS')

C     ELASTIQUE
      IF((OPTION.EQ.'RIGI_MECA').OR.MELAS.OR.
     &   (MTANG.AND.(PLAS.LT.0.5D0)))THEN
         DO 150 K=1,6
         DO 150 L=1,6
           DSIDEP(K,L) = 0.D0
 150     CONTINUE
         DO 160 K=1,6
           DSIDEP(K,K) = COEF1
 160     CONTINUE
         DO 170 K=1,3
         DO 170 L=1,3
           DSIDEP(K,L) = DSIDEP(K,L) + COEF2
 170     CONTINUE
      ENDIF

C     PLASTICITE
      IF(MTANG.AND.(PLAS.GE.0.5D0))THEN
         KCI = 1.D0
         IF (OPTION(1:14).EQ.'RIGI_MECA_TANG') THEN
           RBID = SIGFI(1) + SIGFI(2) + SIGFI(3)
           DO 175 K=1,3
             SIGD(K) = SIGFI(K)- RBID * (1.D0/3.D0)
175        CONTINUE
           DO 176 K =4,NDIMSI
             SIGD(K)= SIGFI(K)
176        CONTINUE
         ELSE
           RBID = SIGP(1) + SIGP(2) + SIGP(3)
           DO 177 K=1,3
             SIGD(K) = SIGP(K)- RBID*(1.D0 / 3.D0)
177        CONTINUE
           DO 178 K =4,NDIMSI
             SIGD(K)= SIGP(K)
178        CONTINUE
         END IF
         DRDP = (ECUM**((1.D0/MCOEF)-1.D0))
         DRDP = (KCOEF/(MCOEF))*DRDP

         IF(DCOEF.LE.0.D0)THEN
C          PLASTICITE SANS ENDOMMAGEMENT
           HP = 1.D0+((3.D0/2.D0)*COEF1*KCI*DP)/((RINI+LIMIT))
           LAMDA = COEF2+((COEF1/3.D0)*(1.D0-(1.D0/HP)))
           DEUMU = COEF1/HP
           BCOEF = 1.D0- (((DRDP*DP)/(RINI+LIMIT)))
           BCOEF = KCI*((9.D0*(COEF1**2))/(4.D0*HP))*BCOEF
           CCOEF = DRDP+((3.D0/2.D0)*COEF1)
           DO 200 K=1,NDIMSI
           DO 200 M=1,NDIMSI
              DSIDEP(K,M) = 0.D0
 200       CONTINUE
           DO 210 K=1,NDIMSI
              DSIDEP(K,K) = DEUMU
 210       CONTINUE
           DO 220 K=1,3
           DO 220 M=1,3
              DSIDEP(K,M) = DSIDEP(K,M) + LAMDA
 220       CONTINUE
           DO 230 K=1,NDIMSI
           DO 230 M=1,NDIMSI
              DSIDEP(K,M) = DSIDEP(K,M) -((BCOEF/CCOEF)*
     &        ((SIGD(K)/(RINI+LIMIT))*(SIGD(M)/(RINI+LIMIT))))
 230       CONTINUE

         ELSE
C	   PLASTICITE ET ENDOMMAGEMENT
           HP = (1.D0+((3.D0/2.D0)*COEF1*KCI*DP)/
     &                     ((1.D0-DCOEF)*(RINI+LIMIT)))
           LAMDA = COEF2+((COEF1/3.D0)*(1.D0-(1.D0/HP)))
           DEUMU = COEF1/HP
           ACOEF = ((COEFDC)/(ECUMC-ECUMD))
           BCOEF = (1.D0- (DP*(((1.D0-DCOEF)*DRDP) -
     &          (RINI*ACOEF))/((1.D0-DCOEF)*(RINI+LIMIT))))
           BCOEF = KCI*((9.D0*(COEF1**2))/(4.D0*HP))*BCOEF
           CCOEF = (((1.D0-DCOEF)*DRDP)+((3.D0/2.D0)*COEF1)
     &                            -(RINI*ACOEF))
           DO 240 K=1,NDIMSI
           DO 240 M=1,NDIMSI
              DSIDEP(K,M) = 0.D0
 240       CONTINUE
           DO 250 K=1,NDIMSI
              DSIDEP(K,K) = DEUMU
 250       CONTINUE
           DO 260 K=1,3
           DO 260 M=1,3
              DSIDEP(K,M) = DSIDEP(K,M) + LAMDA
 260       CONTINUE
           DO 270 K=1,NDIMSI
           DO 270 M=1,NDIMSI
              DSIDEP(K,M) = (DSIDEP(K,M) -((BCOEF/CCOEF)*
     &        ((SIGD(K)/((1.D0-DCOEF)*(RINI+LIMIT)))
     &            *(SIGD(M)/((1.D0-DCOEF)*(RINI+LIMIT))))))
 270       CONTINUE
        ENDIF
      ENDIF

C     CAS RIGI_MECA_ELAS ET FULL_MECA_ELAS AVEC ENDOMMAGEMENT
      IF(MELAS.AND.(DCOEF.GE.0.D0))THEN
        DO 327 K=1,NDIMSI
        DO 327 M=1,NDIMSI
           DSIDEP(K,M)=(1.D0-DCOEF)*DSIDEP(K,M)
 327    CONTINUE
      ENDIF

9999  CONTINUE
      END
