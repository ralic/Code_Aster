      SUBROUTINE LCLDSB (NDIM, TYPMOD, IMATE, COMPOR, EPSM, DEPS,
     &                   VIM, TM,TP,TREF,OPTION, SIG, VIP,  DSIDEP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/09/2003   AUTEUR DURAND C.DURAND 
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
      CHARACTER*16       COMPOR(*),OPTION
      INTEGER            NDIM, IMATE
      REAL*8             EPSM(6), DEPS(6), VIM(2), TM, TP, TREF
      REAL*8             SIG(6), VIP(2), DSIDEP(6,6)
C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT D'ENDOMMAGEMENT (EN LOCAL)
C
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  IMATE   : NATURE DU MATERIAU
C IN  EPSM    : DEFORMATION EN T-
C IN  DEPS    : INCREMENT DE DEFORMATION
C IN  VIM     : VARIABLES INTERNES EN T-
C IN  TM      : TEMPERATURE EN T-
C IN  TP      : TEMPERATURE EN T+
C IN  TREF    : TEMPERATURE DE REFERENCE
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
      REAL*8      EPS(6),  TREPS, SIGEL(6), KRON(6)
      REAL*8      RAC2,COEF
      REAL*8      RIGMIN, FD, D, ENER, TROISK, G
      REAL*8      TR(6), RTEMP2
      REAL*8      EPSP(3), VECP(3,3), DSPDEP(3,3),VECP2(3,3)
      REAL*8      DEUMUD(3), LAMBDD, SIGP(3),RTEMP,RTEMP3,RTEMP4
      REAL*8      E, NU, ALPHA, LAMBDA, DEUXMU, GAMMA, SEUIL,TREPSM
      REAL*8      K0,K1,SICR
      CHARACTER*2 CODRET(3)
      CHARACTER*8 NOMRES(3)
      REAL*8      VALRES(3)
      REAL*8      R8DOT
      REAL*8      TPS(6)
      PARAMETER  (RIGMIN = 1.D-3)
      DATA        KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
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
      
      IF ((.NOT.( COMPOR(1)(1:15) .EQ. 'ENDO_ISOT_BETON')).AND.
     &   (.NOT.( COMPOR(1)(1:6) .EQ. 'KIT_HM')).AND.
     &   (.NOT.( COMPOR(1)(1:7) .EQ. 'KIT_HHM')).AND.
     &   (.NOT.( COMPOR(1)(1:7) .EQ. 'KIT_THM')).AND.
     &   (.NOT.( COMPOR(1)(1:8) .EQ. 'KIT_THHM'))) THEN
            CALL UTMESS('F','ENDO_ISOT_BETON_01',
     &           ' COMPORTEMENT INATTENDU : '//COMPOR(1))
      ENDIF
C    LECTURE DES CARACTERISTIQUES DU MATERIAU
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      NOMRES(3) = 'ALPHA'
      IF ((((COMPOR(1)(1:6) .EQ. 'KIT_HM') .OR. 
     &     (COMPOR(1)(1:7) .EQ. 'KIT_HHM') .OR.
     &     (COMPOR(1)(1:7) .EQ. 'KIT_THM') .OR.
     &     (COMPOR(1)(1:8) .EQ. 'KIT_THHM')).AND.
     &     (COMPOR(11)(1:15) .EQ. 'ENDO_ISOT_BETON')).OR.
     &     (COMPOR(1)(1:15) .EQ. 'ENDO_ISOT_BETON')) THEN
      CALL RCVALA ( IMATE,'ELAS',0,' ',0.D0,2,
     &              NOMRES,VALRES,CODRET, 'FM')
      CALL RCVALA ( IMATE,'ELAS',3,' ',0.D0,1,
     &              NOMRES(3),VALRES(3),CODRET(3), ' ')
      IF ( CODRET(3) .NE. 'OK' ) VALRES(3) = 0.D0
      E     = VALRES(1)
      NU    = VALRES(2)
      ALPHA = VALRES(3)
      LAMBDA = E * NU / (1.D0+NU) / (1.D0 - 2.D0*NU)
      DEUXMU = E/(1.D0+NU)
C    LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
      NOMRES(1) = 'D_SIGM_EPSI'
      NOMRES(2) = 'SYT'
      NOMRES(3) = 'SYC'
      CALL RCVALA(IMATE,'BETON_ECRO_LINE',0,' ',0.D0,3,
     &            NOMRES,VALRES,CODRET,' ')
      GAMMA  = - E/VALRES(1)
      K0=VALRES(2)**2 *(1.D0+GAMMA)/(2.D0*E)
     &               *(1.D0+NU-2.D0*NU**2)/(1.D0+NU)
      IF (NU.EQ.0) THEN
        IF (CODRET(3).EQ.'OK') THEN
          CALL UTMESS('F','LCLDSB',' SYC NE DOIT PAS ETRE
     & VALORISE POUR NU NUL DANS DEFI_MATERIAU')
        ELSE
          SEUIL=K0
        ENDIF
      ELSE
        SICR=SQRT((1.D0+NU-2.D0*NU**2)/(2.D0*NU**2))*VALRES(2)
        IF (CODRET(3).EQ.'NO') THEN
          SEUIL=K0
        ELSE
          IF (VALRES(3).LT.SICR) THEN        
            CALL UTMESS('F','LCLDSB',' SYC DOIT ETRE
     &  SUPERIEUR A SQRT((1+NU-2*NU*NU)/(2.D0*NU*NU))*SYT
     &  DANS DEFI_MATERIAU POUR PRENDRE EN COMPTE LE 
     &  CONFINEMENT')
          ELSE
            K1=VALRES(3)*(1.D0+GAMMA)*NU**2/(1.D0+NU)/(1.D0-2.D0*NU)
     &        -K0*E/(1.D0-2.D0*NU)/VALRES(3)
            TREPSM=0.D0
            DO 1 I=1,NDIM
              TREPSM=TREPSM+EPSM(I)
 1          CONTINUE
            IF (TREPSM.GT.0.D0) THEN
              TREPSM=0.D0
            ENDIF
            SEUIL  = K0-K1*TREPSM
          ENDIF
        ENDIF
      ENDIF
      ENDIF

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
C     MATRICE TR = (XX XY XZ YY YZ ZZ) 
C
C    PRISE EN COMPTE DE LA TEMPERATURE
C    POUR L'INSTANT LES COEFFICIENTS MATERIAUX
C    NE DEPENDENT PAS DE LA TEMPERATURE
C    SI DEVELOPPEMENT INTRODUIRE TMAX
      IF (RESI) THEN
      DO 30 K=1,NDIMSI
        EPS(K) = EPS(K) - ALPHA * (TP - TREF) * KRON(K)
30    CONTINUE 
      ELSE
      DO 35 K=1,NDIMSI
        EPS(K) = EPS(K) - ALPHA * (TM - TREF) * KRON(K)
35    CONTINUE 
      ENDIF
      
      TR(1) = EPS(1)
      TR(2) = EPS(4)
      TR(3) = EPS(5)
      TR(4) = EPS(2)
      TR(5) = EPS(6)
      TR(6) = EPS(3)
C - ON PASSE EN REPERE PROPRE DE EPS
C
      CALL DIAGP3(TR,VECP,EPSP)
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
        CALL R8INIR(6,0.D0,SIG,1)
        DO 1010 I=1,3
          RTEMP=SIGP(I)
          SIG(1)=SIG(1)+VECP(1,I)*VECP(1,I)*RTEMP
          SIG(2)=SIG(2)+VECP(2,I)*VECP(2,I)*RTEMP
          SIG(3)=SIG(3)+VECP(3,I)*VECP(3,I)*RTEMP
          SIG(4)=SIG(4)+VECP(1,I)*VECP(2,I)*RTEMP
          SIG(5)=SIG(5)+VECP(1,I)*VECP(3,I)*RTEMP
          SIG(6)=SIG(6)+VECP(2,I)*VECP(3,I)*RTEMP
1010    CONTINUE
        DO 18 K=4,NDIMSI
          SIG(K)=RAC2*SIG(K)
18      CONTINUE
      ENDIF
      IF (RIGI) THEN
        TR(1) = SIGEL(1)
        TR(2) = SIGEL(2)
        TR(3) = SIGEL(3)
        CALL R8INIR(6,0.D0,SIGEL,1)
        DO 1020 I=1,3
          RTEMP=TR(I)
          SIGEL(1)=SIGEL(1)+VECP(1,I)*VECP(1,I)*RTEMP
          SIGEL(2)=SIGEL(2)+VECP(2,I)*VECP(2,I)*RTEMP
          SIGEL(3)=SIGEL(3)+VECP(3,I)*VECP(3,I)*RTEMP
          SIGEL(4)=SIGEL(4)+VECP(1,I)*VECP(2,I)*RTEMP
          SIGEL(5)=SIGEL(5)+VECP(1,I)*VECP(3,I)*RTEMP
          SIGEL(6)=SIGEL(6)+VECP(2,I)*VECP(3,I)*RTEMP
1020    CONTINUE
        DO 28 K=4,NDIMSI
          SIGEL(K)=RAC2*SIGEL(K)
28      CONTINUE
C -- CONTRIBUTION ELASTIQUE
        MTG = .TRUE.
        RTEMP=ABS(EPSP(1))
        IF (ABS(EPSP(2)).GT.RTEMP) RTEMP=ABS(EPSP(2))
        IF (ABS(EPSP(3)).GT.RTEMP) RTEMP=ABS(EPSP(3))
        IF ((RTEMP*RIGMIN).LT.1.D-12) MTG=.FALSE. 
        IF (MTG) THEN
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
           IF (I.EQ.J) THEN
             RTEMP3=1.D0
           ELSE
             RTEMP3=RAC2
           ENDIF
            DO 22 K=1,3
              DO 23 L=1,3
                IF (T(I,J).GE.T(K,L)) THEN
                IF (K.EQ.L) THEN
                  RTEMP4=RTEMP3
                ELSE
                  RTEMP4=RTEMP3/RAC2
                ENDIF
                RTEMP2=0.D0                
                DO 24 M=1,3
                  DO 25 N=1,3
        RTEMP2=RTEMP2+VECP(K,M)*
     &        VECP(I,N)*VECP(J,N)*VECP(L,M)*DSPDEP(N,M)
        RTEMP=EPSP(N)-EPSP(M)
                   IF ((M.NE.N)) THEN
       RTEMP=SIGP(N)/RTEMP
       RTEMP2=RTEMP2+(VECP(K,M)*VECP(L,N))*VECP(I,M)*VECP(J,N)*RTEMP
       RTEMP2=RTEMP2+(VECP(K,N)*VECP(L,M))*VECP(J,M)*VECP(I,N)*RTEMP
                    ENDIF
25                CONTINUE
24              CONTINUE
               DSIDEP(T(I,J),T(K,L))=DSIDEP(T(I,J),T(K,L))+RTEMP2*RTEMP4
               ENDIF
23            CONTINUE
22          CONTINUE
21        CONTINUE
20      CONTINUE
        DO 26 I=1,6 
          DO 27 J=I+1,6
            DSIDEP(I,J)=DSIDEP(J,I)
27        CONTINUE
26      CONTINUE
        ELSE
          DO 70 K=1,6
            DO 71 L=1,6
              DSIDEP(K,L)=0.D0
71          CONTINUE
70        CONTINUE
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
