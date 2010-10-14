      SUBROUTINE AFFPOU(TMP,TMPF,FCX,NOM,
     &                  ISEC,IVAR,CAR,NCAR,VAL,TAB,EXP,
     &                  NBO,IOC,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8                                       VAL(*)
      CHARACTER*6                                              IOC
      CHARACTER*8       FCX, NOM,          CAR(*),      TAB(*),EXP(*)
      CHARACTER*24      TMP,TMPF
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 14/10/2010   AUTEUR BOYERE E.BOYERE 
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
C     ------------------------------------------------------------------
C     VERIFICATION DE LA BONNE AFFECTATION DES SECTIONS DE POUTRE :
C        - INTERDICTION D ECRASER UNE GEOMETRIE DE SECTION PAR UNE AUTRE
C        - INTERDICTION D ECRASER UNE VARIATION DE SECTION PAR UNE AUTRE
C          AFFECTATION DES CARACTERISTIQUES GENERALES ET GEOMETRIQUES
C          AUX MAILLES DE TYPE POUTRE DANS L OBJET TAMPON
C     ------------------------------------------------------------------
C     L'OBJET TAMPON CONTIENT (44*NBPOUTR) VALEURS
C     ------------------------------------------------------------------
C        TAB  1     2     3     4     5    6    7    8    9    10
C        0    A1    IY1   IZ1   AY1   AZ1  EY1  EZ1  JX1  RY1  RZ1
C        1    RT1   A2    IY2   IZ2   AY2  AZ2  EY2  EZ2  JX2  RY2
C        2    RZ2   RT2   TVAR  HY1   HZ1  EPY1 EPZ1 HY2  HZ2  EPY2
C        3    EPZ2  R1    E1    R2    E2   TSEC AI1  AI2  JG1  JG2
C        4    IYR21 IYR22 IZR21 IZR22
C     ------------------------------------------------------------------
C        TSEC = TYPE  GEOMETRIQUE DE SECTION : 0 = GENERALE
C                                              1 = RECTANGLE
C                                              2 = CERCLE
C        TVAR = TYPE DE VARIATION DE SECTION : 0 = CONSTANTE
C                                             (1 = AFFINITE)
C                                              2 = HOMOTHETIE
C     ------------------------------------------------------------------
C     ------ DEBUT COMMUNS NORMALISES  JEVEUX  ------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16               ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                 ZK32
      CHARACTER*80                                          ZK80
      COMMON  / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32       JEXNOM,  JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  ------------------------
      CHARACTER*24 VALK(2)
      CHARACTER*8  VALKM
      INTEGER      VALIM
      REAL*8       R8MAEM, TST, VALRM
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      TST = R8MAEM()
C
      CALL JENONU(JEXNOM(TMP,NOM),NUM)
C
C     --- TESTS D'ECRASEMENT DE SECTION ---
      IF (NUM.NE.0) THEN
         CALL JEVEUO(JEXNOM(TMP,NOM),'E',JDGE)
         IIVAR = NINT(ZR(JDGE+22))
         IISEC = NINT(ZR(JDGE+35))
         IF (IIVAR.NE.IVAR) THEN
             VALK(1) = IOC
             VALK(2) = NOM
             CALL U2MESK('A','MODELISA_92', 2 ,VALK)
            IER = IER + 1
         ENDIF
         IF (IISEC.NE.ISEC) THEN
             VALK(1) = IOC
             VALK(2) = NOM
             CALL U2MESK('A','MODELISA_93', 2 ,VALK)
            IER = IER + 1
         ENDIF
C
      ELSE
         CALL JECROC(JEXNOM(TMP,NOM))
         CALL JEVEUO(JEXNOM(TMP,NOM),'E',JDGE)
         DO 5 I = 1 , NBO
            ZR(JDGE+I-1) = TST
 5       CONTINUE
      ENDIF

C --- VERIFICATION QUE LES AY, AZ SONT >= 1
      DO 7 I = 1 , NCAR
         IF ( (CAR(I)(1:2).EQ.'AY').OR.(CAR(I)(1:2).EQ.'AZ')) THEN
            IF ( VAL(I) .LT. 1.0D0 ) THEN
               VALKM = CAR(I)
               VALRM = VAL(I)
               CALL U2MESG('F','MODELISA_23',1,VALKM,0,VALIM,1,VALRM)
            ENDIF
         ENDIF
7     CONTINUE

C     --- NOM DE LA FONCTION DU CX
      CALL JENONU(JEXNOM(TMPF,NOM),NUM)
      IF ( NUM .EQ. 0 ) THEN
        CALL JECROC(JEXNOM(TMPF,NOM))
      ENDIF
      CALL JEVEUO(JEXNOM(TMPF,NOM),'E',JDGEF)
      ZK8(JDGEF) = FCX
C
C     --- VALEURS AUX EXTREMITES POUR SECTION VARIABLE
      DO 10 I = 1 , NCAR
         DO 20 J = 1 , NBO
            IF (CAR(I).EQ.TAB(J)) ZR(JDGE+J-1) = VAL(I)
 20      CONTINUE
 10   CONTINUE
C
C     --- EXPANSION DES VALEURS AUX EXTREMITES POUR SECTION CONSTANTE
      DO 30 I = 1 , NCAR
         DO 40 J = 1 , NBO
            IF (CAR(I).EQ.EXP(J)) ZR(JDGE+J-1) = VAL(I)
 40      CONTINUE
 30   CONTINUE
C
C     --- EXPANSION DANS LE CAS PARTICULIER DE LA SECTION CARRE (H,EP)
      DO 50 I = 1 , NCAR
         IF (CAR(I).EQ.'H1      ' .OR. CAR(I).EQ.'H       ') THEN
            ZR(JDGE+23) = VAL(I)
            ZR(JDGE+24) = VAL(I)
         ENDIF
         IF (CAR(I).EQ.'H2      ' .OR. CAR(I).EQ.'H       ') THEN
            ZR(JDGE+27) = VAL(I)
            ZR(JDGE+28) = VAL(I)
         ENDIF
         IF (CAR(I).EQ.'EP1     ' .OR. CAR(I).EQ.'EP      ') THEN
            ZR(JDGE+25) = VAL(I)
            ZR(JDGE+26) = VAL(I)
         ENDIF
         IF (CAR(I).EQ.'EP2     ' .OR. CAR(I).EQ.'EP      ') THEN
            ZR(JDGE+29) = VAL(I)
            ZR(JDGE+30) = VAL(I)
         ENDIF
 50   CONTINUE
C
C     --- TYPE/GEOMETRIE  DE SECTION ---
      ZR(JDGE+22) = IVAR
      ZR(JDGE+35) = ISEC
C
      CALL JEDEMA()
      END
