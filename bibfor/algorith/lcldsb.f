      SUBROUTINE LCLDSB (NDIM, TYPMOD, IMATE, EPSM,
     &                   DEPS, VIM, OPTION, SIG, VIP,  DSIDEP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/05/2001   AUTEUR JMBHH01 J.M.PROIX 
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
      IMPLICIT NONE
      CHARACTER*8        TYPMOD(2)
      CHARACTER*16       OPTION
      INTEGER            NDIM, IMATE
      REAL*8             EPSM(6), DEPS(6), VIM(2)
      REAL*8             SIG(6), VIP(2), DSIDEP(6,6)
C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT ELASTIQUE FRAGILE (EN DELOCALISE)
C
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  IMATE   : NATURE DU MATERIAU
C IN  TEMP    : TEMPERATURE EN T+
C IN  TREF    : TEMPERATURE DE REFERENCE
C IN  EPSM    : DEFORMATION EN T-
C IN  DEPS    : INCREMENT DE DEFORMATION
C IN  VIM     : VARIABLES INTERNES EN T-
C IN  OPTION  : OPTION DEMANDEE
C                 RIGI_MECA_TANG ->     DSIDEP
C                 FULL_MECA      -> SIG DSIDEP VIP
C                 RAPH_MECA      -> SIG        VIP
C OUT SIG     : CONTRAINTE
C OUT VIP     : VARIABLES INTERNES
C                 1   -> VALEUR DE L'ENDOMMAGEMENT
C OUT DSIDEP  : MATRICE TANGENTE
C ----------------------------------------------------------------------
C LOC EDFRC1  COMMON CARACTERISTIQUES DU MATERIAU (AFFECTE DANS EDFRMA)
      LOGICAL     RIGI, RESI,ELAS,MTG
      INTEGER     NDIMSI, K, L, I, J, M, N, P,T(3,3)
      INTEGER     NPERM, NITJAC, TTRIJ, OTRIJ
      REAL*8      EPS(6),  TREPS, SIGEL(6)
      REAL*8      RAC2,COEF
      REAL*8      RIGMIN, FD, D, ENER, TROISK, G
      REAL*8      TR(6), TU(6), RTEMP2
      REAL*8      EPSP(3), VECP(3,3), DSPDEP(3,3),VECP2(3,3)
      REAL*8      DEUMUD(3), LAMBDD, SIGP(3),RTEMP,RTEMP3,RTEMP4
      REAL*8      E, NU, ALPHA, LAMBDA, DEUXMU, GAMMA, SEUIL
      REAL*8      TOL,TOLDYN,JACAUX(3)
      CHARACTER*2 CODRET(3)
      CHARACTER*8 NOMRES(3)
      REAL*8      VALRES(3)
      REAL*8      R8DOT
      PARAMETER  (RIGMIN = 1.D-3)
      DATA   NPERM ,TOL,TOLDYN    /12,1.D-10,1.D-2/
      DATA   TTRIJ,OTRIJ  /2,2/
C ----------------------------------------------------------------------
C ======================================================================
C                            INITIALISATION
C ======================================================================
C -- OPTION ET MODELISATION
      RIGI  = (OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL')
      RESI  = (OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL')
      NDIMSI = 2*NDIM
      RAC2=SQRT(2.D0)
      T(1,1)=1
      T(1,2)=4
      T(1,3)=5
      T(2,1)=4
      T(2,2)=2
      T(2,3)=6
      T(3,1)=5
      T(3,2)=6
      T(3,3)=3
C    LECTURE DES CARACTERISTIQUES DU MATERIAU
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      CALL RCVALA ( IMATE,'ELAS',0,' ',0.D0,2,
     &              NOMRES,VALRES,CODRET, 'FM')
      E     = VALRES(1)
      NU    = VALRES(2)
      LAMBDA = E * NU / (1.D0+NU) / (1.D0 - 2.D0*NU)
      DEUXMU = E/(1.D0+NU)
C    LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
      NOMRES(1) = 'SY'
      NOMRES(2) = 'D_SIGM_EPSI'
      CALL RCVALA(IMATE,'ECRO_LINE',0,' ',0.D0,2,
     &            NOMRES,VALRES,CODRET,'FM')
      GAMMA  = - E/VALRES(2)
      SEUIL  = VALRES(1)**2 * (1.D0+GAMMA) / (2.D0*E)
C    CALCUL DES CONTRAINTES (DANS CAS 'RAPH' OU 'FULL')
      IF (RESI) THEN
C      MISE A JOUR DES DEFORMATIONS MECANIQUES
        DO 10 K = 1, NDIMSI
          EPS(K) = EPSM(K) + DEPS(K)
 10     CONTINUE
      ELSE
        DO 40 K=1,NDIMSI
          EPS(K)=EPSM(K)
40      CONTINUE
        D=VIM(1)
        FD  = (1 - D) / (1 + GAMMA*D)
        IF (FD.LT.RIGMIN) FD = RIGMIN
        ELAS=((NINT(VIM(2)).EQ.0).OR.(NINT(VIM(2)).EQ.2))
      ENDIF
C - ON MET DANS EPS LES DEFORMATIONS REELLES
      DO 45 K=4,NDIMSI
        EPS(K) = EPS(K)/RAC2
45    CONTINUE
      IF (NDIMSI.LT.6) THEN
        DO 46 K=NDIMSI+1,6
          EPS(K)=0.D0
46      CONTINUE
      ENDIF
C     MATRICE TR = (XX XY XZ YY YZ ZZ) (POUR JACOBI)
C
      TR(1) = EPS(1)
      TR(2) = EPS(4)
      TR(3) = EPS(5)
      TR(4) = EPS(2)
      TR(5) = EPS(6)
      TR(6) = EPS(3)
C
C     MATRICE UNITE = (1 0 0 1 0 1) (POUR JACOBI)
C
      TU(1) = 1.D0
      TU(2) = 0.D0
      TU(3) = 0.D0
      TU(4) = 1.D0
      TU(5) = 0.D0
      TU(6) = 1.D0
C - ON PASSE EN REPERE PROPRE DE EPS
C
      CALL JACOBI(3,NPERM,TOL,TOLDYN,TR,TU,VECP,EPSP,JACAUX,
     &       NITJAC,TTRIJ,OTRIJ)
C -   CALCUL DES CONTRAINTES ELASTIQUES
      TREPS = EPS(1)+EPS(2)+EPS(3)
      IF (TREPS.GT.0.D0) THEN
        DO 60 K=1,3
          SIGEL(K) = LAMBDA*TREPS
 60     CONTINUE
      ELSE
        DO 61 K=1,3
          SIGEL(K) = 0.D0
 61     CONTINUE
      ENDIF
      DO 15 K=1,3
        IF (EPSP(K).GT.0.D0) THEN
          SIGEL(K) = SIGEL(K) + DEUXMU*EPSP(K)
        ENDIF
15    CONTINUE
      ENER = 0.5D0 * R8DOT(3,EPSP,1,SIGEL,1)
      IF (RESI) THEN
        ELAS = .FALSE.
C      CALCUL DE L'ETAT D'ENDOMMAGEMENT
        D = (SQRT((1+GAMMA)/SEUIL * ENER) - 1) / GAMMA
        IF (D.LT.VIM(1)) THEN
          D = VIM(1)
          ELAS = .TRUE.
        ELSE IF (D .GT. 1) THEN
          D = 1
          ELAS = .TRUE.
        END IF
        FD  = (1 - D) / (1 + GAMMA*D)
        IF (FD.LT.RIGMIN) FD = RIGMIN
        VIP(1) = D
        IF (ELAS) THEN
          VIP(2) = 0
          IF (FD.EQ.RIGMIN) VIP(2) = 2
        ELSE
          VIP(2) = 1
        END IF
      ENDIF
C -   CALCUL DES CONTRAINTES
      IF ((EPSP(1)+EPSP(2)+EPSP(3)).GT.0.D0) THEN
        LAMBDD=LAMBDA * FD
      ELSE
        LAMBDD=LAMBDA
      ENDIF
      IF (EPSP(1).GT.0.D0) THEN
        DEUMUD(1)=DEUXMU*FD
      ELSE
        DEUMUD(1)=DEUXMU
      ENDIF
      IF (EPSP(2).GT.0.D0) THEN
        DEUMUD(2)=DEUXMU*FD
      ELSE
        DEUMUD(2)=DEUXMU
      ENDIF
      IF (EPSP(3).GT.0.D0) THEN
        DEUMUD(3)=DEUXMU*FD
      ELSE
        DEUMUD(3)=DEUXMU
      ENDIF
      TREPS=EPSP(1)+EPSP(2)+EPSP(3)
      SIGP(1)=LAMBDD*TREPS+DEUMUD(1)*EPSP(1)
      SIGP(2)=LAMBDD*TREPS+DEUMUD(2)*EPSP(2)
      SIGP(3)=LAMBDD*TREPS+DEUMUD(3)*EPSP(3)
      IF (RESI) THEN
C      ON REPASSE DANS LE REPERE INITIAL LES CONTRAINTES
        TR(1) = SIGP(1)
        TR(2) = SIGP(2)
        TR(3) = SIGP(3)
        TR(4) = 0.D0
        TR(5) = 0.D0
        TR(6) = 0.D0
        CALL BPTOBG(TR,SIG,VECP)
        DO 18 K=4,NDIMSI
          SIG(K)=RAC2*SIG(K)
18      CONTINUE
      ENDIF
      IF (RIGI) THEN
        TR(1) = SIGEL(1)
        TR(2) = SIGEL(2)
        TR(3) = SIGEL(3)
        TR(4) = 0.D0
        TR(5) = 0.D0
        TR(6) = 0.D0
        CALL BPTOBG(TR,SIGEL,VECP)
        DO 28 K=4,NDIMSI
          SIGEL(K)=RAC2*SIGEL(K)
28      CONTINUE
C -- CONTRIBUTION ELASTIQUE
        MTG = .TRUE.
        RTEMP=ABS(EPSP(1))
        IF (ABS(EPSP(2)).GT.RTEMP) RTEMP=ABS(EPSP(2))
        IF (ABS(EPSP(3)).GT.RTEMP) RTEMP=ABS(EPSP(3))
        DO 500 I=1,2
          DO 501 J=(I+1),3
            IF (ABS(EPSP(I)-EPSP(J)).LT.1D-12) THEN
              EPSP(I)=EPSP(I)+RIGMIN*RTEMP
              EPSP(J)=EPSP(J)-RIGMIN*RTEMP
              MTG = .FALSE.
            ENDIF
501       CONTINUE
500     CONTINUE
        IF (.NOT. MTG) THEN
          SIGP(1)=LAMBDD*TREPS+DEUMUD(1)*EPSP(1)
          SIGP(2)=LAMBDD*TREPS+DEUMUD(2)*EPSP(2)
          SIGP(3)=LAMBDD*TREPS+DEUMUD(3)*EPSP(3)
        ENDIF
        MTG = .TRUE.
        CALL R8INIR(9, 0.D0, DSPDEP, 1)
        CALL R8INIR(36, 0.D0, DSIDEP, 1)
C        IF (.NOT. ELAS) THEN
        DO 100 K = 1,3
          DO 110 L = 1,3
            DSPDEP(K,L) = LAMBDD
 110      CONTINUE
 100    CONTINUE
        DO 120 K = 1,3
          DSPDEP(K,K) = DSPDEP(K,K) + DEUMUD(K)
 120    CONTINUE
        DO 20 I=1,3
          DO 21 J=I,3
            DO 22 K=1,3
              DO 23 L=1,3
                DO 24 M=1,3
                  DO 25 N=1,3
                    IF (I.EQ.J) THEN
                      RTEMP3=1.D0
                    ELSE
                      RTEMP3=RAC2
                    ENDIF
                    IF (K.EQ.L) THEN
                      RTEMP4=1.D0
                    ELSE
                      RTEMP4=1.D0/RAC2
                    ENDIF
        DSIDEP(T(I,J),T(K,L))=DSIDEP(T(I,J),T(K,L))+VECP(K,M)*
     &        VECP(I,N)*VECP(J,N)*VECP(L,M)*DSPDEP(N,M)*RTEMP3*RTEMP4
        RTEMP=ABS(EPSP(M)-EPSP(N))
                   IF ((M.NE.N)) THEN
                     IF ((RTEMP.GT.1.D-12)) THEN
       RTEMP2=(VECP(K,M)*VECP(L,N))/(EPSP(N)-EPSP(M))
       RTEMP2=RTEMP2*VECP(I,M)*VECP(J,N)*SIGP(N)*RTEMP3*RTEMP4
       DSIDEP(T(I,J),T(K,L))=DSIDEP(T(I,J),T(K,L))+RTEMP2
       RTEMP2=(VECP(K,N)*VECP(L,M))/(EPSP(N)-EPSP(M))
       RTEMP2=RTEMP2*VECP(J,M)*VECP(I,N)*SIGP(N)*RTEMP3*RTEMP4
       DSIDEP(T(I,J),T(K,L))=DSIDEP(T(I,J),T(K,L))+RTEMP2
                      ELSE
                        MTG= .FALSE.
                      ENDIF
                    ENDIF
25                CONTINUE
24              CONTINUE
23            CONTINUE
22          CONTINUE
21        CONTINUE
20      CONTINUE
       IF (.NOT.MTG) THEN
         DO 70 K=1,6
           DO 71 L=1,6
             DSIDEP(K,L)=0.D0
71         CONTINUE
70       CONTINUE
         DSIDEP(1,1)=LAMBDA+DEUXMU
         DSIDEP(2,2)=LAMBDA+DEUXMU
         DSIDEP(3,3)=LAMBDA+DEUXMU
         DSIDEP(1,2)=LAMBDA
         DSIDEP(2,1)=LAMBDA
         DSIDEP(1,3)=LAMBDA
         DSIDEP(3,1)=LAMBDA
         DSIDEP(2,3)=LAMBDA
         DSIDEP(3,2)=LAMBDA
         DSIDEP(4,4)=DEUXMU
         DSIDEP(5,5)=DEUXMU
         DSIDEP(6,6)=DEUXMU
       ENDIF
C -- CONTRIBUTION DISSIPATIVE
        IF ((.NOT. ELAS).AND.(ENER.GT.0.D0)) THEN
          COEF = (1+GAMMA)/(2*GAMMA*(1+GAMMA*D)*ENER)
          DO 200 K = 1,NDIMSI
            DO 210 L = 1, NDIMSI
              DSIDEP(K,L) = DSIDEP(K,L) - COEF * SIGEL(K) * SIGEL(L)
 210        CONTINUE
 200      CONTINUE
        END IF
      END IF
      END
