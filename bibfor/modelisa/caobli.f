      SUBROUTINE CAOBLI(FONREE,I,NB,K8JDDL,N1,RJCMU,BETA,KBETA,MOTFAC)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                  I,NB,       N1
      REAL*8                                  RJCMU(*),BETA
      CHARACTER*4       FONREE
      CHARACTER*8                   K8JDDL(*),           KBETA
      CHARACTER*(*)                                          MOTFAC
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 02/10/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
C
C     CALCUL LA LIAISON_DDL DONNEE DANS UN REPERE OBLIQUE
C
C IN  : FONREE : 'REEL' OU 'FONC'
C IN  : I      : NUMERO D'OCCURRENCE DU MOT CLE FACTEUR
C IN  : NB     : INDICE
C OUT : K8JDDL : VECTEUR DES DDL
C OUT : N1     : NOMBRE DE DDL
C OUT : RJCMU  : VECTEUR DES COEFFICIENTS MULTIPLICATEURS
C OUT : BETA   : COEFFICIENT IMPOSE SI FONREE = 'REEL'
C OUT : KBETA  : NOM DE LA FONCTION SI FONREE = 'FONC'
C IN  : MOTFAC : MOT CLE FACTEUR
C ----------------------------------------------------------------------
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32      JEXNOM, JEXNUM
C     ----------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------
      PARAMETER     ( NDDL = 6 )
      INTEGER       IMP
      REAL*8        DGRD, R8DGRD, ZERO
      REAL*8        ANGL(3), MAT(3,3), REEL
      CHARACTER*8   K8B, KDDL(NDDL), KFONC
C ----------------------------------------------------------------------
      DATA KDDL / 'DX' , 'DY' , 'DZ' , 'DRX' , 'DRY' , 'DRZ' /
C ----------------------------------------------------------------------
C
      DGRD = R8DGRD()
      ZERO = 0.D0
      ANGL(1) = ZERO
      ANGL(2) = ZERO
      ANGL(3) = ZERO
      CALL GETVR8(MOTFAC,'ANGL_NAUT',I,1,3,ANGL,NA)
      DO 10 I1 = 1,MIN(3,ABS(NA))
         ANGL(I1) = ANGL(I1) * DGRD
 10   CONTINUE
C
C     --- MATRICE DE PASSAGE AU REPER GLOBAL ---
      CALL MATROT ( ANGL , MAT )
C
C     --- ECRITURE DE LA RELATION DANS LE REPERE GLOBAL ---
      IF (FONREE.EQ.'REEL') THEN
         N1 = 0
         DO 20 I1 = 1,NDDL
            I2 = 0
            IF (I1.GT.3) I2 = I2 + 3
C
            CALL GETVR8(MOTFAC,KDDL(I1),I,1,1,REEL,IMP)
C
            IF (IMP.NE.0) THEN
               IF (MAT(I1,1).NE.ZERO) THEN
                  N1 = N1 + 1
                  IF (NB.EQ.1) THEN
                     I3 = I2 + 1
                     K8JDDL(N1) = KDDL(I3)
                     RJCMU(N1) = MAT(I1,1)
                  ENDIF
               ENDIF
               IF (MAT(I1,2).NE.ZERO) THEN
                  N1 = N1 + 1
                  IF (NB.EQ.1) THEN
                     I3 = I2 + 2
                     K8JDDL(N1) = KDDL(I3)
                     RJCMU(N1) = MAT(I1,2)
                  ENDIF
               ENDIF
               IF (MAT(I1,3).NE.ZERO) THEN
                  N1 = N1 + 1
                  IF (NB.EQ.1) THEN
                     I3 = I2 + 3
                     K8JDDL(N1) = KDDL(I3)
                     RJCMU(N1) = MAT(I1,3)
                  ENDIF
               ENDIF
               IF (NB.EQ.1) BETA = REEL
               GOTO 22
            ENDIF
 20      CONTINUE
 22      CONTINUE
C
      ELSE
C
         N1 = 0
         DO 30 I1 = 1,NDDL
            I2 = 0
            IF (I1.GT.3) I2 = I2 + 3
C
            CALL GETVID(MOTFAC,KDDL(I1),I,1,1,KFONC,IMP)
C
            IF (IMP.NE.0) THEN
               IF (MAT(I1,1).NE.ZERO) THEN
                  N1 = N1 + 1
                  IF (NB.EQ.1) THEN
                     I3 = I2 + 1
                     K8JDDL(N1) = KDDL(I3)
                     RJCMU(N1) = MAT(I1,1)
                  ENDIF
               ENDIF
               IF (MAT(I1,2).NE.ZERO) THEN
                  N1 = N1 + 1
                  IF (NB.EQ.1) THEN
                     I3 = I2 + 2
                     K8JDDL(N1) = KDDL(I3)
                     RJCMU(N1) = MAT(I1,2)
                  ENDIF
               ENDIF
               IF (MAT(I1,3).NE.ZERO) THEN
                  N1 = N1 + 1
                  IF (NB.EQ.1) THEN
                     I3 = I2 + 3
                     K8JDDL(N1) = KDDL(I3)
                     RJCMU(N1) = MAT(I1,3)
                  ENDIF
               ENDIF
               IF (NB.EQ.1) KBETA = KFONC
               GOTO 32
            ENDIF
 30      CONTINUE
 32      CONTINUE
      ENDIF
C
      END
