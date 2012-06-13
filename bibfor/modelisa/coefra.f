      SUBROUTINE COEFRA(IPAS,IRES,X,XSI0,CK)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C TOLE CRP_20
C-----------------------------------------------------------------------
C   CALCUL DU COEFFICIENT DE RAIDEUR AJOUTEE CK EN FONCTION DE LA
C   VITESSE REDUITE  (FAISCEAU DE TUBES SOUS ECOULEMENT TRANSVERSE)
C-----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
C  IN   : IPAS      : TYPE DE PAS
C  IN   : IRES      : TYPE DE RESEAU DU POINT COURANT
C  IN   : X         : VITESSE REDUITE
C  IN   : NBORCK :
C  IN   : BORNCK  :
C  IN   : COEFCK   :
C  OUT  : CK        : COEFFICIENT DE MASSE AJOUTEE
C-----------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'
      INTEGER     IPAS, IRES
      INTEGER     JBORNE, JCOEFF, JVIRED
      REAL*8      CK,   XSI0
C
      INTEGER      NBORCK, NCKMAX, IRET
      REAL*8       ZERO, BORNCK(20),COEFCK(20,11)
      CHARACTER*24 NOM1, NOM2, NOM3
C
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      X = DBLE(ABS(X))
      NCKMAX = 11
      ZERO = 0.0D0
C
      NOM1 = '&&COEFRA.CKI'
      NOM2 = '&&COEFRA.CKR1'
      NOM3 = '&&COEFRA.CKR2'
C
      IF (IRES .EQ. 0) THEN
         CK = ZERO
         GO TO 1000
      ENDIF
C
C --- ON TESTE L'EXISTENCE DU VECTEUR DE COEFFICIENTS
C     POUR LA CORRELATION RELATIVE A IPAS ET IRES
C     ===============================================
      CALL JEEXIN (NOM2,IRET)
      IF (IRET .EQ. 0) THEN
C
C --- LECTURE DU FICHIER DE DONNEES
C     =============================
         CALL COEFRL(NOM1,NOM2,NOM3,NCKMAX,IPAS,IRES,BORNCK,
     &               NBORCK,COEFCK,IPAS1,IRES1)
      ELSE
         CALL JEVEUO(NOM1,'L',JBORNE)
         CALL JEVEUO(NOM2,'L',JCOEFF)
         CALL JEVEUO(NOM3,'L',JVIRED)
         IPAS1 = ZI(JBORNE-1+1)
         IRES1 = ZI(JBORNE-1+2)
         NBORCK = ZI(JBORNE-1+3)
         IF(IPAS1 .EQ. IPAS .AND. IRES1 .EQ. IRES) THEN
            K = 1
            DO 100 I = 1,NBORCK
               BORNCK(I) = ZR(JCOEFF + I - 1)
               DO 110 J = 1,NCKMAX
                 COEFCK(I,J) = ZR(JCOEFF + NBORCK + K - 1 )
                 K = K + 1
  110           CONTINUE
  100       CONTINUE
         ELSE
           CALL JEDETR(NOM1)
           CALL JEDETR(NOM2)
           CALL JEDETR(NOM3)
           CALL COEFRL(NOM1,NOM2,NOM3,NCKMAX,IPAS,IRES,BORNCK,
     &                 NBORCK,COEFCK,IPAS1,IRES1)
         ENDIF
      ENDIF
      IF(IPAS1 .NE. IPAS .OR. IRES1 .NE. IRES) THEN
         CALL U2MESS('F','MODELISA4_31')
      ENDIF
C
C **********************************************************************
C ***                  FAISCEAU EN PAS CARRE LIGNE                   ***
C **********************************************************************
C
      IF ( IPAS .EQ. 1 ) THEN
C
         IF ( IRES.GE.1.AND.IRES.LE.1000 ) THEN
C
            IF( X.LT.BORNCK(1) ) THEN
               CK = 0.D0
            ELSE
               IF( X.LT.BORNCK(NBORCK) ) THEN
                  DO 130 I = 2 , NBORCK
                    IF ( X.GE.BORNCK(I-1).AND.X.LT.BORNCK(I) ) THEN
                       CK = COEFCK(I-1,1)/(X*X*X*X*X*X*X) +
     &                      COEFCK(I-1,2)/(X*X*X*X*X*X) +
     &                      COEFCK(I-1,3)/(X*X*X*X*X) +
     &                      COEFCK(I-1,4)/(X*X*X*X) +
     &                      COEFCK(I-1,5)/(X*X*X) +
     &                      COEFCK(I-1,6)/(X*X) +
     &                      COEFCK(I-1,7)/(X) +
     &                      COEFCK(I-1,8)   +
     &                      COEFCK(I-1,9)*(X)  +
     &                      COEFCK(I-1,10)*(X*X) +
     &                      COEFCK(I-1,11)*(X*X*X)
                       GO TO 140
                    ENDIF
  130              CONTINUE
  140             CONTINUE
               ELSE
                  CK = COEFCK(NBORCK,1)/(X*X*X*X*X*X*X) +
     &                 COEFCK(NBORCK,2)/(X*X*X*X*X*X) +
     &                 COEFCK(NBORCK,3)/(X*X*X*X*X) +
     &                 COEFCK(NBORCK,4)/(X*X*X*X) +
     &                 COEFCK(NBORCK,5)/(X*X*X) +
     &                 COEFCK(NBORCK,6)/(X*X) +
     &                 COEFCK(NBORCK,7)/(X) +
     &                 COEFCK(NBORCK,8)   +
     &                 COEFCK(NBORCK,9)*(X) +
     &                 COEFCK(NBORCK,10)*(X*X) +
     &                 COEFCK(NBORCK,11)*(X*X*X)
               ENDIF
            ENDIF
C
C --- CELLULE DE TUBES VIBRANTS EN DEBUT DE FAISCEAU VISCACHE1.
C
         ELSE IF ( IRES .EQ. 1001 ) THEN
C
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF ( X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE IF ( X .LT. BORNCK(3) ) THEN
               CK = COEFCK(2,8) +
     &              COEFCK(2,9)*X
            ELSE IF ( X .LT. BORNCK(4) ) THEN
               CK = COEFCK(3,8) +
     &              COEFCK(3,7)/X  +
     &              COEFCK(3,6)/(X*X) +
     &              COEFCK(3,5)/(X*X*X)
            ELSE
               CK = COEFCK(4,8)
            END IF
C
C --- CELLULE DE TUBES VIBRANTS EN MILIEU DE FAISCEAU CLOTAIRE.
C     (PROFIL DE VITESSE REEL)
C
         ELSE IF ( IRES .EQ. 1002 ) THEN
C
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF ( X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE IF ( X .LT. BORNCK(3) ) THEN
               CK = COEFCK(2,8) +
     &              COEFCK(2,7)/X  +
     &              COEFCK(2,6)/(X*X) +
     &              COEFCK(2,5)/(X*X*X)
            ELSE IF ( X .LT. BORNCK(4) ) THEN
               CK = COEFCK(3,8) +
     &              COEFCK(3,7)/X  +
     &              COEFCK(3,6)/(X*X) +
     &              COEFCK(3,5)/(X*X*X)
            ELSE IF ( X .LT. BORNCK(5) ) THEN
               CK = COEFCK(4,8) +
     &              COEFCK(4,9)*X
            ELSE
               CK = COEFCK(5,8)
            END IF
C
C --- CELLULE DE TUBES VIBRANTS EN MILIEU DE FAISCEAU CLOTAIRE.
C     (PROFIL DE VITESSE UNIFORME)
C
         ELSE IF ( IRES .EQ. 1003 ) THEN
C
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF ( X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE IF ( X .LT. BORNCK(3) ) THEN
               CK = COEFCK(2,8) +
     &              COEFCK(2,7)/X  +
     &              COEFCK(2,6)/(X*X) +
     &              COEFCK(2,5)/(X*X*X)
            ELSE IF ( X .LT. BORNCK(4) ) THEN
               CK = COEFCK(3,8) +
     &              COEFCK(3,7)/X  +
     &              COEFCK(3,6)/(X*X) +
     &              COEFCK(3,5)/(X*X*X)
            ELSE IF ( X .LT. BORNCK(5) ) THEN
               CK = COEFCK(4,8) +
     &              COEFCK(4,9)*X
            ELSE
               CK = COEFCK(5,8)
            END IF
C
C --- TUBE UNIQUE VIBRANT EN MILIEU DE FAISCEAU RIGIDE VISCACHE1.
C
         ELSE IF ( IRES .EQ. 1004 ) THEN
C
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF ( X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE IF ( X .LT. BORNCK(3) ) THEN
               CK = COEFCK(2,8) +
     &              COEFCK(2,7)/X  +
     &              COEFCK(2,6)/(X*X)
            ELSE IF ( X .LT. BORNCK(4) ) THEN
               CK = COEFCK(3,8) +
     &              COEFCK(3,9)*X
            ELSE
               CK = COEFCK(4,8)
            ENDIF
C
C --- TUBE UNIQUE VIBRANT EN DEBUT DE FAISCEAU RIGIDE VISCACHE1.
C
         ELSE IF ( IRES .EQ. 1005 ) THEN
C
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF (  X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE IF (  X .LT. BORNCK(3) ) THEN
               CK = COEFCK(2,8) +
     &              COEFCK(2,7)/X  +
     &              COEFCK(2,6)/(X*X) +
     &              COEFCK(2,5)/(X*X*X)
            ELSE IF (  X .LT. BORNCK(4) ) THEN
               CK = COEFCK(3,8) +
     &              COEFCK(3,7)/X  +
     &              COEFCK(3,6)/(X*X) +
     &              COEFCK(3,5)/(X*X*X)
            ELSE
               CK = COEFCK(4,8) +
     &              COEFCK(4,7)/X  +
     &              COEFCK(4,6)/(X*X) +
     &              COEFCK(4,5)/(X*X*X)
            ENDIF
C
C --- TUBE ROMPU
C
         ELSE IF ( IRES .EQ. 1006 ) THEN
C
           IF (X.LE. BORNCK(1)) THEN
           CK = 0.0D0
           ELSE
           CALL CKATRC(X,XSI0,COEFCK,CK)
           ENDIF
C
C --- TUBE UNIQUE VIBRANT EN MILIEU DE FAISCEAU RIGIDE TANAKA.
C
         ELSE IF ( IRES .EQ. 1007 ) THEN
C
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE
               CK = COEFCK(1,8) +
     &              COEFCK(1,7)/X  +
     &              COEFCK(1,6)/(X*X) +
     &              COEFCK(1,5)/(X*X*X)
            ENDIF
C
C --- TUBE UNIQUE VIBRANT EN MILIEU DE FAISCEAU RIGIDE DIVA EAU.
C
         ELSE IF ( IRES .EQ. 1008 ) THEN
C
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF ( X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE IF ( X .LT. BORNCK(3) ) THEN
               CK = COEFCK(2,8) +
     &              COEFCK(2,7)/X
            ELSE IF ( X .LT. BORNCK(4) ) THEN
               CK = COEFCK(3,8) +
     &              COEFCK(3,7)/X  +
     &              COEFCK(3,6)/(X*X) +
     &              COEFCK(3,5)/(X*X*X)
            ELSE
               CK = COEFCK(4,8) +
     &              COEFCK(4,7)/X  +
     &              COEFCK(4,6)/(X*X) +
     &              COEFCK(4,5)/(X*X*X)
            ENDIF
C
C
C --- COEFFICIENT VISCACHE 2 CFD 90%
C
         ELSE IF ( IRES .EQ. 1101 ) THEN
C
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF ( X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE IF ( X .LT. BORNCK(3) ) THEN
               CK = COEFCK(2,8) +
     &              COEFCK(2,7)/X  +
     &              COEFCK(2,6)/(X*X)
            ELSE
               CK = COEFCK(3,8) +
     &              COEFCK(3,7)/X  +
     &              COEFCK(3,6)/(X*X) +
     &              COEFCK(3,5)/(X*X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 CFD 85 %
C
         ELSE IF ( IRES .EQ. 1102 ) THEN
C
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF ( X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE
               CK = COEFCK(2,8) +
     &              COEFCK(2,7)/X  +
     &              COEFCK(2,6)/(X*X) +
     &              COEFCK(2,5)/(X*X*X) +
     &              COEFCK(2,4)/(X*X*X*X) +
     &              COEFCK(2,3)/(X*X*X*X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 CFD 80 %
C
         ELSE IF ( IRES .EQ. 1103 ) THEN
C
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF ( X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE IF ( X .LT. BORNCK(3) ) THEN
               CK = COEFCK(2,8) +
     &              COEFCK(2,7)/X  +
     &              COEFCK(2,6)/(X*X) +
     &              COEFCK(2,5)/(X*X*X)
            ELSE
               CK = COEFCK(3,8) +
     &              COEFCK(3,7)/X  +
     &              COEFCK(3,6)/(X*X) +
     &              COEFCK(3,5)/(X*X*X) +
     &              COEFCK(3,4)/(X*X*X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 CFD 50 %
C
         ELSE IF ( IRES .EQ. 1104 ) THEN
C
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF ( X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE
               CK = COEFCK(2,8) +
     &              COEFCK(2,7)/X  +
     &              COEFCK(2,6)/(X*X) +
     &              COEFCK(2,5)/(X*X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 CFD 20 %
C
         ELSE IF ( IRES .EQ. 1105 ) THEN
C
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF ( X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE
               CK = COEFCK(2,8) +
     &              COEFCK(2,7)/X  +
     &              COEFCK(2,6)/(X*X) +
     &              COEFCK(2,5)/(X*X*X) +
     &              COEFCK(2,4)/(X*X*X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 CFD 10 %
C
         ELSE IF ( IRES .EQ. 1106 ) THEN
C
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF ( X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE
               CK = COEFCK(2,8) +
     &              COEFCK(2,7)/X  +
     &              COEFCK(2,6)/(X*X) +
     &              COEFCK(2,5)/(X*X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 TUM 90 %
C
         ELSE IF ( IRES .EQ. 1201 ) THEN
C
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF ( X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE
               CK = COEFCK(2,8) +
     &              COEFCK(2,7)/X  +
     &              COEFCK(2,6)/(X*X) +
     &              COEFCK(2,5)/(X*X*X) +
     &              COEFCK(2,4)/(X*X*X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 TUM 86 %
C
         ELSE IF ( IRES .EQ. 1202 ) THEN
C
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF ( X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE
               CK = COEFCK(2,8) +
     &              COEFCK(2,7)/X  +
     &              COEFCK(2,6)/(X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 TUM 82 %
C
         ELSE IF ( IRES .EQ. 1203 ) THEN
C
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF ( X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE
               CK = COEFCK(2,8) +
     &              COEFCK(2,7)/X  +
     &              COEFCK(2,6)/(X*X) +
     &              COEFCK(2,5)/(X*X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 TUM 50 %
C
         ELSE IF ( IRES .EQ. 1204 ) THEN
C
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF ( X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE
               CK = COEFCK(2,8) +
     &              COEFCK(2,7)/X  +
     &              COEFCK(2,6)/(X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 TUM 20 %
C
         ELSE IF ( IRES .EQ. 1205 ) THEN
C
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF ( X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE
               CK = COEFCK(2,8) +
     &              COEFCK(2,7)/X  +
     &              COEFCK(2,6)/(X*X) +
     &              COEFCK(2,5)/(X*X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 TUM 10 %
C
         ELSE IF ( IRES .EQ. 1206 ) THEN
C
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF ( X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE
               CK = COEFCK(2,8) +
     &              COEFCK(2,7)/X  +
     &              COEFCK(2,6)/(X*X) +
     &              COEFCK(2,5)/(X*X*X)
            END IF
C
         ELSE
C
            CALL U2MESS('F','MODELISA4_32')
C
         END IF
C
C **********************************************************************
C ***               FAISCEAU EN PAS TRIANGULAIRE LIGNE               ***
C **********************************************************************
C
      ELSE IF ( IPAS .EQ. 2 ) THEN
C
         IF ( IRES.GE.1.AND.IRES.LE.1000 ) THEN
C
            IF( X.LT.BORNCK(1) ) THEN
               CK = 0.D0
            ELSE
               IF( X.LT.BORNCK(NBORCK) ) THEN
                  DO 150 I = 2 , NBORCK
                    IF ( X.GE.BORNCK(I-1).AND.X.LT.BORNCK(I) ) THEN
                       CK = COEFCK(I-1,1)/(X*X*X*X*X*X*X) +
     &                      COEFCK(I-1,2)/(X*X*X*X*X*X) +
     &                      COEFCK(I-1,3)/(X*X*X*X*X) +
     &                      COEFCK(I-1,4)/(X*X*X*X) +
     &                      COEFCK(I-1,5)/(X*X*X) +
     &                      COEFCK(I-1,6)/(X*X) +
     &                      COEFCK(I-1,7)/(X) +
     &                      COEFCK(I-1,8)   +
     &                      COEFCK(I-1,9)*(X)  +
     &                      COEFCK(I-1,10)*(X*X) +
     &                      COEFCK(I-1,11)*(X*X*X)
                       GO TO 160
                    ENDIF
  150             CONTINUE
  160             CONTINUE
               ELSE
                  CK = COEFCK(NBORCK,1)/(X*X*X*X*X*X*X) +
     &                 COEFCK(NBORCK,2)/(X*X*X*X*X*X) +
     &                 COEFCK(NBORCK,3)/(X*X*X*X*X) +
     &                 COEFCK(NBORCK,4)/(X*X*X*X) +
     &                 COEFCK(NBORCK,5)/(X*X*X) +
     &                 COEFCK(NBORCK,6)/(X*X) +
     &                 COEFCK(NBORCK,7)/(X) +
     &                 COEFCK(NBORCK,8)   +
     &                 COEFCK(NBORCK,9)*(X) +
     &                 COEFCK(NBORCK,10)*(X*X) +
     &                 COEFCK(NBORCK,11)*(X*X*X)
               ENDIF
            ENDIF
C
C --- CELLULE DE TUBES VIBRANTS EN DEBUT DE FAISCEAU VISCACHE1.
C
         ELSE IF ( IRES .EQ. 1001 ) THEN
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF ( X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE IF ( X .LT. BORNCK(3) ) THEN
               CK = COEFCK(2,8) +
     &              COEFCK(2,9)*X  +
     &              COEFCK(2,10)*(X*X) +
     &              COEFCK(2,11)*(X*X*X)
            ELSE IF ( X .LT. BORNCK(4) ) THEN
               CK = COEFCK(3,8) +
     &              COEFCK(3,7)/X  +
     &              COEFCK(3,6)/(X*X)
            ELSE
               CK = COEFCK(4,8)
            END IF
C
C --- CELLULE DE TUBES VIBRANTS EN MILIEU DE FAISCEAU VISCACHE1.
C
         ELSE IF ( IRES .EQ. 1002 ) THEN
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF ( X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE IF ( X .LT. BORNCK(3) ) THEN
               CK = COEFCK(2,8) +
     &              COEFCK(2,7)/X  +
     &              COEFCK(2,6)/(X*X) +
     &              COEFCK(2,5)/(X*X*X)
            ELSE IF ( X .LT. BORNCK(4) ) THEN
               CK = COEFCK(3,8) +
     &              COEFCK(3,7)/X  +
     &              COEFCK(3,6)/(X*X)
            ELSEIF ( X .LT. BORNCK(5) ) THEN
               CK = COEFCK(4,8) +
     &              COEFCK(4,7)/X  +
     &              COEFCK(4,6)/(X*X) +
     &              COEFCK(4,5)/(X*X*X)
            ELSEIF ( X .LT. BORNCK(6) ) THEN
               CK = COEFCK(5,8) +
     &              COEFCK(5,9)*X
            ELSE
               CK = COEFCK(6,8)
            END IF
C
C --- TUBE UNIQUE VIBRANT EN MILIEU DE FAISCEAU RIGIDE VISCACHE1.
C
         ELSE IF ( IRES .EQ. 1003 ) THEN
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF ( X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE IF ( X .LT. BORNCK(3) ) THEN
               CK = COEFCK(2,8) +
     &              COEFCK(2,7)/X
            ELSE IF ( X .LT. BORNCK(4) ) THEN
               CK = COEFCK(3,8) +
     &              COEFCK(3,7)/X  +
     &              COEFCK(3,6)/(X*X) +
     &              COEFCK(3,5)/(X*X*X)
            ELSE IF ( X .LT. BORNCK(5) ) THEN
               CK = COEFCK(4,8) +
     &              COEFCK(4,7)/X  +
     &              COEFCK(4,6)/(X*X)
            ELSEIF ( X .LT. BORNCK(6) ) THEN
               CK = COEFCK(5,8) +
     &              COEFCK(5,7)/X
            ELSEIF ( X .LT. BORNCK(7) ) THEN
               CK = COEFCK(6,8) +
     &              COEFCK(6,9)*X
            ELSE
               CK = COEFCK(7,8)
            END IF
C
C --- TUBE UNIQUE VIBRANT EN DEBUT DE FAISCEAU RIGIDE VISCACHE1.
C
         ELSE IF ( IRES .EQ. 1004 ) THEN
            IF ( X .LT. BORNCK(1) ) THEN
               CK = 0.D0
            ELSE IF ( X .LT. BORNCK(2) ) THEN
               CK = COEFCK(1,8) +
     &              COEFCK(1,9)*X
            ELSE IF ( X .LT. BORNCK(3) ) THEN
               CK = COEFCK(2,8) +
     &              COEFCK(2,7)/X  +
     &              COEFCK(2,6)/(X*X)
            ELSE IF ( X .LT. BORNCK(4) ) THEN
               CK = COEFCK(3,8) +
     &              COEFCK(3,9)*X
            ELSE IF ( X .LT. BORNCK(5) ) THEN
               CK = COEFCK(4,8) +
     &              COEFCK(4,7)/X  +
     &              COEFCK(4,6)/(X*X) +
     &              COEFCK(4,5)/(X*X*X)
            ELSE IF ( X .LT. BORNCK(6) ) THEN
               CK = COEFCK(5,8) +
     &              COEFCK(5,9)*X
            ELSE
               CK = COEFCK(6,8)
            END IF
C
         END IF
C
      END IF
C
C --- INVERSION DE CK POUR CONVENTION DE SIGNE ET FIN DE COEFR.
C
      CK = -CK
C
 1000 CONTINUE
      CALL JEDEMA()
      END
