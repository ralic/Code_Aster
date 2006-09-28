      SUBROUTINE COEFAM(IPAS,IRES,X,XSI0,CD)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C TOLE CRP_20
C-----------------------------------------------------------------------
C   CALCUL DU COEFFICIENT D AMORTISSEMENT AJOUTE CD EN FONCTION
C   DE LA VITESSE REDUITE (FAISCEAU DE TUBES SOUS ECOULEMENT TRANSVERSE)
C-----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
C  IN    : IPAS      : TYPE DE PAS
C  IN    : IRES      : TYPE DE RESEAU DU POINT COURANT
C  IN    : X         : VITESSE REDUITE
C  OUT   : CD        : COEFFICIENT D AMORTISSEMENT AJOUTE
C-----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER      IPAS, IRES
      REAL*8       CD,   XSI0
C
      INTEGER      NBORCD, NCDMAX, IRET
      INTEGER      JBORNE, JCOEFF, JVIRED
      REAL*8       ZERO, BORNCD(20),COEFCD(20,11)
      CHARACTER*24 NOM1, NOM2, NOM3
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      X = DBLE(ABS(X))
C
      NCDMAX = 11
      ZERO = 0.0D0
C
      NOM1 = '&&COEFAM.CDI'
      NOM2 = '&&COEFAM.CDR1'
      NOM3 = '&&COEFAM.CDR2'
C
      IF (IRES .EQ. 0) THEN
         CD = ZERO
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
         CALL COEFAL(NOM1,NOM2,NOM3,NCDMAX,IPAS,IRES,BORNCD,
     &               NBORCD,COEFCD,IPAS1,IRES1)
      ELSE
         CALL JEVEUO(NOM1,'L',JBORNE)
         CALL JEVEUO(NOM2,'L',JCOEFF)
         CALL JEVEUO(NOM3,'L',JVIRED)
         IPAS1 = ZI(JBORNE-1+1)
         IRES1 = ZI(JBORNE-1+2)
         NBORCD = ZI(JBORNE-1+3)
         IF(IPAS1 .EQ. IPAS .AND. IRES1 .EQ. IRES) THEN
            K = 1
            DO 100 I = 1,NBORCD
               BORNCD(I) = ZR(JCOEFF + I - 1)
               DO 110 J = 1,NCDMAX
                 COEFCD(I,J) = ZR(JCOEFF + NBORCD + K - 1 )
                 K = K + 1
  110          CONTINUE
  100       CONTINUE
         ELSE
            CALL JEDETR(NOM1)
            CALL JEDETR(NOM2)
            CALL JEDETR(NOM3)
            CALL COEFAL(NOM1,NOM2,NOM3,NCDMAX,IPAS,IRES,BORNCD,
     &                  NBORCD,COEFCD,IPAS1,IRES1)
         ENDIF
      ENDIF
      IF(IPAS1 .NE. IPAS .OR. IRES1 .NE. IRES) THEN
         CALL U2MESS('F','MODELISA4_29')
      ENDIF
C
C **********************************************************************
C ***                  FAISCEAU EN PAS CARRE LIGNE                   ***
C **********************************************************************
C
C
      IF ( IPAS .EQ. 1 ) THEN
C
         IF ( IRES.GE.1.AND.IRES.LE.1000 ) THEN
C
            IF( X.LT.BORNCD(1) ) THEN
               CD = 0.D0
            ELSE
               IF( X.LT.BORNCD(NBORCD) ) THEN
                  DO 130 I = 2 , NBORCD
                    IF ( X.GE.BORNCD(I-1).AND.X.LT.BORNCD(I) ) THEN
                       CD = COEFCD(I-1,1)/(X*X*X*X*X*X*X) +
     &                      COEFCD(I-1,2)/(X*X*X*X*X*X) +
     &                      COEFCD(I-1,3)/(X*X*X*X*X) +
     &                      COEFCD(I-1,4)/(X*X*X*X) +
     &                      COEFCD(I-1,5)/(X*X*X) +
     &                      COEFCD(I-1,6)/(X*X) +
     &                      COEFCD(I-1,7)/(X) +
     &                      COEFCD(I-1,8)   +
     &                      COEFCD(I-1,9)*(X)  +
     &                      COEFCD(I-1,10)*(X*X) +
     &                      COEFCD(I-1,11)*(X*X*X)
                       GO TO 140
                    ENDIF
  130              CONTINUE
  140             CONTINUE
               ELSE
                  CD = COEFCD(NBORCD,1)/(X*X*X*X*X*X*X) +
     &                 COEFCD(NBORCD,2)/(X*X*X*X*X*X) +
     &                 COEFCD(NBORCD,3)/(X*X*X*X*X) +
     &                 COEFCD(NBORCD,4)/(X*X*X*X) +
     &                 COEFCD(NBORCD,5)/(X*X*X) +
     &                 COEFCD(NBORCD,6)/(X*X) +
     &                 COEFCD(NBORCD,7)/(X) +
     &                 COEFCD(NBORCD,8)   +
     &                 COEFCD(NBORCD,9)*(X) +
     &                 COEFCD(NBORCD,10)*(X*X) +
     &                 COEFCD(NBORCD,11)*(X*X*X)
               ENDIF
            ENDIF
C
C
C --- CELLULE DE TUBES VIBRANTS EN DEBUT DE FAISCEAU VISCACHE1.
C
         ELSE IF( IRES.EQ.1001 ) THEN
C
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE IF ( X .LT. BORNCD(2) ) THEN
               CD = COEFCD(1,8) +
     &              COEFCD(1,9)*X
            ELSE IF ( X .LT. BORNCD(3) ) THEN
               CD = COEFCD(2,8) +
     &              COEFCD(2,9)*X
            ELSE IF ( X .LT. BORNCD(4) ) THEN
               CD = COEFCD(3,8) +
     &              COEFCD(3,7)/X +
     &              COEFCD(3,6)/(X*X)
            ELSE IF ( X .LT. BORNCD(5) ) THEN
               CD = COEFCD(4,8)
            ELSE IF ( X .LT. BORNCD(6) ) THEN
               CD = COEFCD(5,11)*X*X*X  +
     &              COEFCD(5,10)*X*X  +
     &              COEFCD(5,9)*X  +
     &              COEFCD(5,8)
            ELSE
               CD = COEFCD(6,9)*X
            END IF
C
C --- CELLULE DE TUBES VIBRANTS EN MILIEU DE FAISCEAU CLOTAIRE.
C     (PROFIL DE VITESSE REEL)
C
         ELSE IF ( IRES .EQ. 1002 ) THEN
C
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE IF ( X .LT. BORNCD(2) ) THEN
               CD = COEFCD(1,9)*X + COEFCD(1,8)
            ELSE IF ( X .LT. BORNCD(3) ) THEN
               CD = COEFCD(2,8) +
     &             COEFCD(2,7)/X +
     &             COEFCD(2,6)/(X*X) +
     &             COEFCD(2,5)/(X*X*X)
            ELSE IF ( X .LT. BORNCD(4) ) THEN
               CD = COEFCD(3,9)*X +
     &             COEFCD(3,8)
            ELSE IF ( X .LT. BORNCD(5) ) THEN
               CD = COEFCD(4,8)
            ELSE
               CD = COEFCD(5,9)*X
            END IF
C
C --- CELLULE DE TUBES VIBRANTS EN MILIEU DE FAISCEAU CLOTAIRE.
C     (PROFIL DE VITESSE UNIFORME)
C
         ELSE IF ( IRES .EQ. 1003 ) THEN
C
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE IF ( X .LT. BORNCD(2) ) THEN
               CD = COEFCD(1,9)*X +
     &              COEFCD(1,8)
            ELSE IF ( X .LT. BORNCD(3) ) THEN
               CD = COEFCD(2,8)  +
     &              COEFCD(2,7)/X  +
     &              COEFCD(2,6)/(X*X) +
     &              COEFCD(2,5)/(X*X*X)
            ELSE IF ( X .LT. BORNCD(4) ) THEN
               CD = COEFCD(3,9)*X +
     &              COEFCD(3,8)
            ELSE IF ( X .LT. BORNCD(5) ) THEN
               CD = COEFCD(4,8)
            ELSE
               CD = COEFCD(5,9)*X
            END IF
C
C --- TUBE UNIQUE VIBRANT EN MILIEU DE FAISCEAU RIGIDE VISCACHE1.
C
         ELSE IF ( IRES .EQ. 1004 ) THEN
C
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.0D0
            ELSE IF (X .LT. BORNCD(2)) THEN
               CD = COEFCD(1,9)*X  +
     &              COEFCD(1,8)
            ELSE IF ( X .LT. BORNCD(3) ) THEN
               CD = COEFCD(2,8)  +
     &              COEFCD(2,7)/X  +
     &              COEFCD(2,6)/(X*X) +
     &              COEFCD(2,5)/(X*X*X) +
     &              COEFCD(2,4)/(X*X*X*X)
            ELSE
               CD = COEFCD(3,8) * (X ** 2.1337D0)
            END IF
C
C --- TUBE UNIQUE VIBRANT EN DEBUT DE FAISCEAU RIGIDE VISCACHE1.
C
         ELSE IF ( IRES .EQ. 1005 ) THEN
C
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE IF ( X .LT. BORNCD(2) ) THEN
               CD = COEFCD(1,9)*X  +
     &              COEFCD(1,8)
            ELSE IF ( X .LT. BORNCD(3) ) THEN
               CD = COEFCD(2,8)  +
     &              COEFCD(2,7)/X  +
     &              COEFCD(2,6)/(X*X) +
     &              COEFCD(2,5)/(X*X*X)
            ELSE IF ( X .LT. BORNCD(4) ) THEN
               CD = COEFCD(3,9)*X  +
     &              COEFCD(3,8)
            ELSE IF ( X .LT. BORNCD(5) ) THEN
               CD = COEFCD(4,8)  +
     &              COEFCD(4,7)/X  +
     &              COEFCD(4,6)/(X*X) +
     &              COEFCD(4,5)/(X*X*X)
            ELSE IF (X .LT. BORNCD(6)) THEN
               CD = COEFCD(5,8)  +
     &              COEFCD(5,7)/X  +
     &              COEFCD(5,6)/(X*X)
            ELSE
               CD = COEFCD(6,8) * (X ** 1.539D0)
            ENDIF
C
C --- TUBE ROMPU
C
         ELSE IF ( IRES .EQ. 1006 ) THEN
C
            IF ( X .LE. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE
            CALL CDATRC(X,XSI0,COEFCD,CD)
            ENDIF
C
C --- TUBE UNIQUE VIBRANT EN MILIEU DE FAISCEAU RIGIDE TANAKA.
C
         ELSE IF ( IRES .EQ. 1007 ) THEN
C
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE
               CD = COEFCD(1,1)/(X*X*X*X*X*X*X) +
     &              COEFCD(1,2)/(X*X*X*X*X*X) +
     &              COEFCD(1,3)/(X*X*X*X*X) +
     &              COEFCD(1,4)/(X*X*X*X) +
     &              COEFCD(1,5)/(X*X*X) +
     &              COEFCD(1,6)/(X*X) +
     &              COEFCD(1,7)/X   +
     &              COEFCD(1,8)   +
     &              COEFCD(1,9)*X
            ENDIF
C
C --- TUBE UNIQUE VIBRANT EN MILIEU DE FAISCEAU RIGIDE DIVA EAU.
C
         ELSE IF ( IRES .EQ. 1008 ) THEN
C
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE IF ( X .LT. BORNCD(2) ) THEN
               CD = COEFCD(1,9)*X  +
     &              COEFCD(1,8)
            ELSE IF ( X .LT. BORNCD(3) ) THEN
               CD = COEFCD(2,8)  +
     &              COEFCD(2,7)/X  +
     &              COEFCD(2,6)/(X*X)
            ELSE IF ( X .LT. BORNCD(4) ) THEN
               CD = COEFCD(3,8)  +
     &              COEFCD(3,7)/X  +
     &              COEFCD(3,6)/(X*X) +
     &              COEFCD(3,5)/(X*X*X)
            ELSE IF ( X .LT. BORNCD(5) ) THEN
               CD = COEFCD(4,9)*X  +
     &              COEFCD(4,8)
            ELSE IF ( X .LT. BORNCD(6) ) THEN
               CD = COEFCD(5,8)  +
     &              COEFCD(5,7)/X
            ELSE
               CD = COEFCD(6,8) * (X ** 0.66146D0)
            ENDIF
C
C --- COEFFICIENT VISCACHE 2 CFD 90 %
C
         ELSE IF ( IRES .EQ. 1101 ) THEN
C
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE IF ( X .LT. BORNCD(2) ) THEN
               CD = COEFCD(1,8)  +
     &              COEFCD(1,9)*X
            ELSE IF ( X .LT. BORNCD(3) ) THEN
               CD = COEFCD(2,8)  +
     &              COEFCD(2,7)/X  +
     &              COEFCD(2,6)/(X*X) +
     &              COEFCD(2,5)/(X*X*X)
            ELSE
               CD = COEFCD(3,8)  +
     &              COEFCD(3,7)/X  +
     &              COEFCD(3,6)/(X*X) +
     &              COEFCD(3,5)/(X*X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 CFD 85 %
C
         ELSE IF ( IRES .EQ. 1102 ) THEN
C
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE IF ( X .LT. BORNCD(2) ) THEN
               CD = COEFCD(1,8)  +
     &              COEFCD(1,9)*X
            ELSE
               CD = COEFCD(2,8)  +
     &              COEFCD(2,7)/X  +
     &              COEFCD(2,6)/(X*X) +
     &              COEFCD(2,5)/(X*X*X) +
     &              COEFCD(2,4)/(X*X*X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 CFD 80 %
C
         ELSE IF ( IRES .EQ. 1103 ) THEN
C
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE IF ( X .LT. BORNCD(2) ) THEN
               CD = COEFCD(1,8)  +
     &              COEFCD(1,9)*X
            ELSE
               CD = COEFCD(2,8)  +
     &              COEFCD(2,7)/X  +
     &              COEFCD(2,6)/(X*X) +
     &              COEFCD(2,5)/(X*X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 CFD 50 %
C
         ELSE IF ( IRES .EQ. 1104 ) THEN
C
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE IF ( X .LT. BORNCD(2) ) THEN
               CD = COEFCD(1,8)  +
     &              COEFCD(1,9)*X
            ELSE
               CD = COEFCD(2,8)  +
     &              COEFCD(2,7)/X  +
     &              COEFCD(2,6)/(X*X) +
     &              COEFCD(2,5)/(X*X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 CFD 20 %
C
         ELSE IF ( IRES .EQ. 1105 ) THEN
C
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE IF ( X .LT. BORNCD(2) ) THEN
               CD = COEFCD(1,8)  +
     &              COEFCD(1,9)*X
            ELSE IF ( X .LT. BORNCD(3) ) THEN
               CD = COEFCD(2,8)  +
     &              COEFCD(2,7)/X  +
     &              COEFCD(2,6)/(X*X) +
     &              COEFCD(2,5)/(X*X*X)
            ELSE IF ( X .LT. BORNCD(4) ) THEN
               CD = COEFCD(3,8)  +
     &              COEFCD(3,7)/X  +
     &              COEFCD(3,6)/(X*X)
            ELSE
               CD = COEFCD(4,8)  +
     &              COEFCD(4,7)/X  +
     &              COEFCD(4,6)/(X*X) +
     &              COEFCD(4,5)/(X*X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 CFD 10 %
C
         ELSE IF ( IRES .EQ. 1106 ) THEN
C
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE IF ( X .LT. BORNCD(2) ) THEN
               CD = COEFCD(1,8)  +
     &              COEFCD(1,9)*X
            ELSE IF ( X .LT. BORNCD(3) ) THEN
               CD = COEFCD(2,8)  +
     &              COEFCD(2,7)/X  +
     &              COEFCD(2,6)/(X*X)
            ELSE IF ( X .LT. BORNCD(4) ) THEN
               CD = COEFCD(3,8)  +
     &              COEFCD(3,7)/X  +
     &              COEFCD(3,6)/(X*X)
            ELSE
               CD = COEFCD(4,8)  +
     &              COEFCD(4,7)/X  +
     &              COEFCD(4,6)/(X*X) +
     &              COEFCD(4,5)/(X*X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 TUM 90 %
C
         ELSE IF ( IRES .EQ. 1201 ) THEN
C
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE IF ( X .LT. BORNCD(2) ) THEN
               CD = COEFCD(1,8)  +
     &              COEFCD(1,9)*X
            ELSE
               CD = COEFCD(2,8)  +
     &              COEFCD(2,7)/X  +
     &              COEFCD(2,6)/(X*X) +
     &              COEFCD(2,5)/(X*X*X) +
     &              COEFCD(2,4)/(X*X*X*X) +
     &              COEFCD(2,3)/(X*X*X*X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 TUM 86 %
C
         ELSE IF ( IRES .EQ. 1202 ) THEN
C
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE IF ( X .LT. BORNCD(2) ) THEN
               CD = COEFCD(1,8)  +
     &              COEFCD(1,9)*X
            ELSE
               CD = COEFCD(2,8)  +
     &              COEFCD(2,7)/X  +
     &              COEFCD(2,6)/(X*X) +
     &              COEFCD(2,5)/(X*X*X) +
     &              COEFCD(2,4)/(X*X*X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 TUM 82 %
C
         ELSE IF ( IRES .EQ. 1203 ) THEN
C
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE IF ( X .LT. BORNCD(2) ) THEN
               CD = COEFCD(1,8)  +
     &              COEFCD(1,9)*X
            ELSE
               CD = COEFCD(2,8)  +
     &              COEFCD(2,7)/X  +
     &              COEFCD(2,6)/(X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 TUM 50 %
C
         ELSE IF ( IRES .EQ. 1204 ) THEN
C
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE IF ( X .LT. BORNCD(2) ) THEN
               CD = COEFCD(1,8)  +
     &              COEFCD(1,9)*X
            ELSE
               CD = COEFCD(2,8)  +
     &              COEFCD(2,7)/X  +
     &              COEFCD(2,6)/(X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 TUM 20 %
C
         ELSE IF ( IRES .EQ. 1205 ) THEN
C
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE IF ( X .LT. BORNCD(2) ) THEN
               CD = COEFCD(1,8)  +
     &              COEFCD(1,9)*X
            ELSE
               CD = COEFCD(2,8)  +
     &              COEFCD(2,7)/X  +
     &              COEFCD(2,6)/(X*X) +
     &              COEFCD(2,5)/(X*X*X)
            END IF
C
C
C --- COEFFICIENT VISCACHE 2 TUM 10 %
C
         ELSE IF ( IRES .EQ. 1206 ) THEN
C
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE IF ( X .LT. BORNCD(2) ) THEN
               CD = COEFCD(1,8)  +
     &              COEFCD(1,9)*X
            ELSE
               CD = COEFCD(2,8)  +
     &              COEFCD(2,7)/X  +
     &              COEFCD(2,6)/(X*X) +
     &              COEFCD(2,5)/(X*X*X) +
     &              COEFCD(2,4)/(X*X*X*X)
            END IF
C
         ELSE
C
            CALL U2MESS('F','MODELISA4_30')
C
         END IF
C
C
C **********************************************************************
C ***               FAISCEAU EN PAS TRIANGULAIRE LIGNE               ***
C **********************************************************************
C
      ELSE IF ( IPAS .EQ. 2 ) THEN
C
         IF ( IRES.GE.1.AND.IRES.LE.1000 ) THEN
C
            IF( X.LT.BORNCD(1) ) THEN
               CD = 0.D0
            ELSE
               IF( X.LT.BORNCD(NBORCD) ) THEN
                  DO 150 I = 2 , NBORCD
                    IF ( X.GE.BORNCD(I-1).AND.X.LT.BORNCD(I) ) THEN
                       CD = COEFCD(I-1,1)/(X*X*X*X*X*X*X) +
     &                      COEFCD(I-1,2)/(X*X*X*X*X*X) +
     &                      COEFCD(I-1,3)/(X*X*X*X*X) +
     &                      COEFCD(I-1,4)/(X*X*X*X) +
     &                      COEFCD(I-1,5)/(X*X*X) +
     &                      COEFCD(I-1,6)/(X*X) +
     &                      COEFCD(I-1,7)/(X) +
     &                      COEFCD(I-1,8)   +
     &                      COEFCD(I-1,9)*(X)  +
     &                      COEFCD(I-1,10)*(X*X) +
     &                      COEFCD(I-1,11)*(X*X*X)
                       GO TO 160
                    ENDIF
  150              CONTINUE
  160              CONTINUE
               ELSE
                  CD = COEFCD(NBORCD,1)/(X*X*X*X*X*X*X) +
     &                 COEFCD(NBORCD,2)/(X*X*X*X*X*X) +
     &                 COEFCD(NBORCD,3)/(X*X*X*X*X) +
     &                 COEFCD(NBORCD,4)/(X*X*X*X) +
     &                 COEFCD(NBORCD,5)/(X*X*X) +
     &                 COEFCD(NBORCD,6)/(X*X) +
     &                 COEFCD(NBORCD,7)/(X) +
     &                 COEFCD(NBORCD,8)   +
     &                 COEFCD(NBORCD,9)*(X) +
     &                 COEFCD(NBORCD,10)*(X*X) +
     &                 COEFCD(NBORCD,11)*(X*X*X)
               ENDIF
            ENDIF
C
C --- CELLULE DE TUBES VIBRANTS EN DEBUT DE FAISCEAU VISCACHE1.
C
         ELSE IF ( IRES .EQ. 1001 ) THEN
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE IF ( X .LT. BORNCD(2) ) THEN
               CD = COEFCD(1,8)  +
     &              COEFCD(1,9)*X
            ELSE IF ( X .LT. BORNCD(3) ) THEN
               CD = COEFCD(2,8)  +
     &              COEFCD(2,9)*X
            ELSE IF ( X .LT. BORNCD(4) ) THEN
               CD = COEFCD(3,8)  +
     &              COEFCD(3,7)/X  +
     &              COEFCD(3,6)/(X*X) +
     &              COEFCD(3,5)/(X*X*X)
            ELSE IF ( X .LT. BORNCD(5) ) THEN
               CD = COEFCD(4,8)  +
     &              COEFCD(4,7)/X  +
     &              COEFCD(4,6)/(X*X)
            ELSE
               CD = COEFCD(5,9)*X
            END IF
C
C --- CELLULE DE TUBES VIBRANTS EN MILIEU DE FAISCEAU VISCACHE1.
C
         ELSE IF ( IRES .EQ. 1002 ) THEN
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE IF ( X .LT. BORNCD(2) ) THEN
               CD = COEFCD(1,8)  +
     &              COEFCD(1,9)*X
            ELSE IF ( X .LT. BORNCD(3) ) THEN
               CD = COEFCD(2,8)  +
     &              COEFCD(2,9)*X
            ELSE IF ( X .LT. BORNCD(4) ) THEN
               CD = COEFCD(3,8)  +
     &              COEFCD(3,7)/X  +
     &              COEFCD(3,6)/(X*X)
            ELSE
               CD = COEFCD(4,8)  +
     &              COEFCD(4,9)*X
            END IF
C
C --- TUBE UNIQUE VIBRANT EN MILIEU DE FAISCEAU RIGIDE VISCACHE1.
C
         ELSE IF ( IRES .EQ. 1003 ) THEN
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE IF ( X .LT. BORNCD(2) ) THEN
               CD = COEFCD(1,8)  +
     &              COEFCD(1,9)*X
            ELSE IF ( X .LT. BORNCD(3) ) THEN
               CD = COEFCD(2,8)  +
     &              COEFCD(2,9)*X +
     &              COEFCD(2,10)*X*X +
     &              COEFCD(2,11)*X*X*X
            ELSE IF ( X .LT. BORNCD(4) ) THEN
               CD = COEFCD(3,8)  +
     &              COEFCD(3,7)/X
            ELSE
               CD = COEFCD(4,8) +
     &              (0.0214D0*(X ** 1.1146D0))
            END IF
C
C --- TUBE UNIQUE VIBRANT EN DEBUT DE FAISCEAU RIGIDE VISCACHE1.
C
         ELSE IF ( IRES .EQ. 1004 ) THEN
            IF ( X .LT. BORNCD(1) ) THEN
               CD = 0.D0
            ELSE IF ( X .LT. BORNCD(2) ) THEN
               CD = COEFCD(1,8)  +
     &              COEFCD(1,9)*X
            ELSE IF ( X .LT. BORNCD(3) ) THEN
               CD = COEFCD(2,8)  +
     &              COEFCD(2,9)*X +
     &              COEFCD(2,10)*X*X
            ELSE IF ( X .LT. BORNCD(4) ) THEN
               CD = COEFCD(3,8)  +
     &              COEFCD(3,9)*X
            ELSE IF ( X .LT. BORNCD(5) ) THEN
               CD = COEFCD(4,8)  +
     &              COEFCD(4,7)/X  +
     &              COEFCD(4,6)/(X*X) +
     &              COEFCD(4,5)/(X*X*X)
               CD = 0.95D0*CD
            ELSE IF ( X .LT. BORNCD(6) ) THEN
               CD = COEFCD(5,8)  +
     &              COEFCD(5,9)*X
            ELSE IF ( X .LT. BORNCD(7) ) THEN
               CD = COEFCD(6,8)  +
     &              COEFCD(6,7)/X  +
     &              COEFCD(6,6)/(X*X) +
     &              COEFCD(6,5)/(X*X*X) +
     &              COEFCD(6,4)/(X*X*X*X)
            ELSE IF ( X .LT. BORNCD(8) ) THEN
               CD = COEFCD(7,8)  +
     &              COEFCD(7,9)*X
            ELSE
               CD = COEFCD(8,8) +
     &              (0.00375D0*(X**2.76251D0))
            END IF
C
         END IF
C
      END IF
C
C --- INVERSION DE CD POUR CONVENTION DE SIGNE ET FIN DE COEFA.
C
      CD = -CD
C
 1000 CONTINUE
      CALL JEDEMA()
      END
