      SUBROUTINE NUAINR(METHOD,NP1,NX1,NC1,IC1,NUAX1,NUAL1,NUAV1,
     &           X2,DREF,VAL2)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 22/06/2005   AUTEUR REZETTE C.REZETTE 
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
      CHARACTER*(*) METHOD
      INTEGER NX1,NP1,IC1,NC1
      REAL*8 NUAX1(*),NUAV1(*),X2(NX1),DREF,DREF2,VAL2
      LOGICAL NUAL1(*)

C  BUT : INTERPOLER LA VALEUR VAL2 DU NUAGE NUAG1 SUR LE POINT DE
C        COORDONNEES X2
C
C IN  METHOD   : METHODE D'INTERPOLATION: 'NUAGE_DEG_0' OU 'NUAGE_DEG_1'
C IN  NP1      : NOMBRE DE POINTS DU NUAGE NUAG1
C IN  NX1      : NOMBRE DE COORDONNEES DES POINTS DE NUAG1 (ET X2)
C IN  NC1      : NOMBRE DE CMPS DES POINTS DE NUAG1
C IN  IC1      : NUMERO DE LA CMP A INTERPOLER DANS NUAG1
C IN  NUAX1    : OBJET .NUAX  DE NUAG1
C IN  NUAL1    : OBJET .NUAL  DE NUAG1
C IN  NUAV1    : OBJET .NUAV  DE NUAG1
C IN  X2       : COORDONNEES DU POINT QU L'ON CHERCHE A INTERPOLER
C IN  DREF     : DISTANCE DE REFERENCE POUR LA METHODE D'INTERPOLATION
C OU  VAL2     : VALEUR INTERPOLEE

C VARIABLES LOCALES :
      INTEGER IP1,IX1,I,J,IRET
      REAL*8 K0(1,1),K1(2,2),K2(3,3),K3(4,4),F(4)
      REAL*8 W,X1,Y1,Z1,V1,D,DET
      CHARACTER*1 TRANS,KSTOP
      CHARACTER*16 METH2

C PARAMETRES DE L'EXPONENTIELLE DONNANT LE POIDS DES POINTS :
      REAL*8 ALPHA,BETA
      DATA ALPHA,BETA/.2D0,0.75D0/

C -DEB
C
C     -- MISE A ZERO DE K ET F :
C     -------------------------
      IF (NX1.EQ.1) THEN
        DO 52,I = 1,NX1+1
          DO 53,J = 1,NX1+1
            K1(I,J)= 0.D0
   53     CONTINUE
   52   CONTINUE
      ELSE IF (NX1.EQ.2) THEN
        DO 54,I = 1,NX1+1
          DO 55,J = 1,NX1+1
            K2(I,J)= 0.D0
   55     CONTINUE
   54   CONTINUE
      ELSE IF (NX1.EQ.3) THEN
        DO 56,I = 1,NX1+1
          DO 57,J = 1,NX1+1
            K3(I,J)= 0.D0
   57     CONTINUE
   56   CONTINUE
      END IF

      DO 58,I = 1,NX1+1
        F(I)= 0.D0
   58 CONTINUE

      DREF2=DREF*ALPHA

      IF (METHOD.EQ.'NUAGE_DEG_0') THEN
C     ------------------------
        K0(1,1) = 0.D0
        F(1) = 0.D0

        IF (NX1.EQ.1) THEN
          DO 1,IP1 = 1,NP1
            IF (.NOT.NUAL1((IP1-1)*NC1+IC1)) GO TO 1
C           W = NUAPOI(NX1,IP1,NUAX1,X2,DREF)
            D=(NUAX1((IP1-1)*1+1)-X2(1))**2
            W=EXP(-(D/DREF2)**BETA)
            V1 = NUAV1((IP1-1)*NC1+IC1)
            K0(1,1) = K0(1,1) + W
            F(1) = F(1) + W*V1
    1     CONTINUE

        ELSE IF (NX1.EQ.2) THEN
          DO 2,IP1 = 1,NP1
            IF (.NOT.NUAL1((IP1-1)*NC1+IC1)) GO TO 2
C           W = NUAPOI(NX1,IP1,NUAX1,X2,DREF)
            D=(NUAX1((IP1-1)*2+1)-X2(1))**2
     &       +(NUAX1((IP1-1)*2+2)-X2(2))**2
            W=EXP(-(D/DREF2)**BETA)
            V1 = NUAV1((IP1-1)*NC1+IC1)
            K0(1,1) = K0(1,1) + W
            F(1) = F(1) + W*V1
    2     CONTINUE

        ELSE IF (NX1.EQ.3) THEN
          DO 3,IP1 = 1,NP1
            IF (.NOT.NUAL1((IP1-1)*NC1+IC1)) GO TO 3
C           W = NUAPOI(NX1,IP1,NUAX1,X2,DREF)
            D=(NUAX1((IP1-1)*3+1)-X2(1))**2
     &       +(NUAX1((IP1-1)*3+2)-X2(2))**2
     &       +(NUAX1((IP1-1)*3+3)-X2(3))**2
            W=EXP(-(D/DREF2)**BETA)
            V1 = NUAV1((IP1-1)*NC1+IC1)
            K0(1,1) = K0(1,1) + W
            F(1) = F(1) + W*V1
    3     CONTINUE
        END IF


        VAL2 = F(1)/K0(1,1)


      ELSE IF (METHOD.EQ.'NUAGE_DEG_1') THEN
C     -----------------------------
        IF (NX1.EQ.1) THEN
C       --------------
          DO 11,IP1 = 1,NP1
            IF (.NOT.NUAL1((IP1-1)*NC1+IC1)) GO TO 11
C           W = NUAPOI(NX1,IP1,NUAX1,X2,DREF)
            D=(NUAX1((IP1-1)*1+1)-X2(1))**2
            W=EXP(-(D/DREF2)**BETA)

            X1 = NUAX1((IP1-1)*1+1)
            V1 = NUAV1((IP1-1)*NC1+IC1)
            K1(1,1) = K1(1,1)   +W
            K1(1,2) = K1(1,2)   +W*X1
            K1(2,2) = K1(2,2)   +W*X1*X1
            F(1) = F(1) + W*V1
            F(2) = F(2) + W*V1*X1
   11     CONTINUE
          K1(2,1)=K1(1,2)

        ELSE IF (NX1.EQ.2) THEN
C       ------------------
          DO 21,IP1 = 1,NP1
            IF (.NOT.NUAL1((IP1-1)*NC1+IC1)) GO TO 21
C           W = NUAPOI(NX1,IP1,NUAX1,X2,DREF)
            D=(NUAX1((IP1-1)*2+1)-X2(1))**2
     &       +(NUAX1((IP1-1)*2+2)-X2(2))**2
            W=EXP(-(D/DREF2)**BETA)

            X1 = NUAX1((IP1-1)*2+1)
            Y1 = NUAX1((IP1-1)*2+2)
            V1 = NUAV1((IP1-1)*NC1+IC1)
            K2(1,1) = K2(1,1) + W
            K2(1,2) = K2(1,2) + W*X1
            K2(1,3) = K2(1,3) + W*Y1
            K2(2,2) = K2(2,2) + W*X1*X1
            K2(2,3) = K2(2,3) + W*X1*Y1
            K2(3,3) = K2(3,3) + W*Y1*Y1
            F(1) = F(1) + W*V1
            F(2) = F(2) + W*V1*X1
            F(3) = F(3) + W*V1*Y1
   21     CONTINUE
          K2(2,1) = K2(1,2)
          K2(3,1) = K2(1,3)
          K2(3,2) = K2(2,3)

        ELSE IF (NX1.EQ.3) THEN
C       ------------------
          DO 31,IP1 = 1,NP1
            IF (.NOT.NUAL1((IP1-1)*NC1+IC1)) GO TO 31
C           W = NUAPOI(NX1,IP1,NUAX1,X2,DREF)
            D=(NUAX1((IP1-1)*3+1)-X2(1))**2
     &       +(NUAX1((IP1-1)*3+2)-X2(2))**2
     &       +(NUAX1((IP1-1)*3+3)-X2(3))**2
            W=EXP(-(D/DREF2)**BETA)

            X1 = NUAX1((IP1-1)*3+1)
            Y1 = NUAX1((IP1-1)*3+2)
            Z1 = NUAX1((IP1-1)*3+3)
            V1 = NUAV1((IP1-1)*NC1+IC1)
            K3(1,1) = K3(1,1) + W
            K3(1,2) = K3(1,2) + W*X1
            K3(1,3) = K3(1,3) + W*Y1
            K3(1,4) = K3(1,4) + W*Z1
            K3(2,2) = K3(2,2) + W*X1*X1
            K3(2,3) = K3(2,3) + W*X1*Y1
            K3(2,4) = K3(2,4) + W*X1*Z1
            K3(3,3) = K3(3,3) + W*Y1*Y1
            K3(3,4) = K3(3,4) + W*Y1*Z1
            K3(4,4) = K3(4,4) + W*Z1*Z1
            F(1) = F(1) + W*V1
            F(2) = F(2) + W*V1*X1
            F(3) = F(3) + W*V1*Y1
            F(4) = F(4) + W*V1*Z1
   31     CONTINUE
          K3(2,1) = K3(1,2)
          K3(3,1) = K3(1,3)
          K3(3,2) = K3(2,3)
          K3(4,1) = K3(1,4)
          K3(4,2) = K3(2,4)
          K3(4,3) = K3(3,4)
        END IF
        TRANS=' '
        KSTOP='S'
        IF (NX1.EQ.1) THEN
          CALL MGAUSS(TRANS,KSTOP,K1,F,2,2,1,DET,IRET)
        ELSE IF (NX1.EQ.2) THEN
          CALL MGAUSS(TRANS,KSTOP,K2,F,3,3,1,DET,IRET)
        ELSE IF (NX1.EQ.3) THEN
          CALL MGAUSS(TRANS,KSTOP,K3,F,4,4,1,DET,IRET)
        END IF

        VAL2 = F(1)
        DO 50,IX1 = 1,NX1
          VAL2 = VAL2 + F(IX1+1)*X2(IX1)
   50   CONTINUE

      ELSE
        METH2 = METHOD
        CALL UTMESS('F','NUAINR','METHODE INCONNUE : '//METH2)
      END IF


      END
