      SUBROUTINE EXTRS2 ( RESU0, RESU1, TYPCON,
     >                    NBORDR, NUORDR, NBACC, NOMACC,
     >                    NBARCH, NUARCH, NBEXCL, CHEXCL, NBNOSY )
      IMPLICIT   NONE
      INTEGER NBORDR,NUORDR(*),NBARCH,NBACC,NUARCH(*),NBEXCL,NBNOSY
      CHARACTER*16 NOMACC(*),CHEXCL(*)
      CHARACTER*(*) RESU0, RESU1
      CHARACTER*16 TYPCON
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 29/04/2002   AUTEUR GNICOLAS G.NICOLAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C              SEE THE FILE "LICENSE.TERMS" FOR INFORMATION ON USAGE AND
C              REDISTRIBUTION OF THIS FILE.
C ======================================================================
C     OPERATEUR D'EXTRACTION
C     ------------------------------------------------------------------

C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C 0.3. ==> VARIABLES LOCALES
C
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'EXTRS2' )
C
      INTEGER LXLGUT
C
      INTEGER I,J,IRE1,IRE2,IADIN,IADOU,IRET
      CHARACTER*3 TYPE
      CHARACTER*16 NOMSYM
      CHARACTER*16 NOPARA
      CHARACTER*19 RESUIN,RESUOU
      CHARACTER*24 CHAMIN,CHAMOU
C     ------------------------------------------------------------------

      CALL JEMARQ( )
C
C               1234567890123456789
      RESUIN = '                   '
      RESUOU = '                   '
      I = LXLGUT(RESU0)
      RESUIN(1:I) = RESU0(1:I)
      I = LXLGUT(RESU1)
      RESUOU(1:I) = RESU1(1:I)
C

      CALL JEEXIN (RESUOU//'.DESC',IRET)
      IF ( IRET.EQ.0 ) THEN
        CALL RSCRSD (RESUOU,TYPCON,NBARCH)
      ENDIF


      DO 30 I = 1,NBNOSY

        CALL JENUNO(JEXNUM(RESUIN//'.DESC',I),NOMSYM)
        DO 10 J = 1,NBEXCL
          IF ( CHEXCL(J) .EQ. NOMSYM ) GO TO 30
   10   CONTINUE

        DO 20 J = 1,NBORDR
          IF (NUARCH(J).EQ.0) GO TO 20
          CALL RSEXCH(RESUIN,NOMSYM,NUORDR(J),CHAMIN,IRE1)
          IF (IRE1.GT.0) GO TO 20

          CALL RSEXCH(RESUOU,NOMSYM,NUORDR(J),CHAMOU,IRE2)
          IF (IRE2.EQ.0) THEN
          ELSE IF (IRE2.EQ.100) THEN
          ELSE
            CALL UTDEBM('F',NOMPRO,'APPEL ERRONE')
            CALL UTIMPI('L','NUMERO D''ORDRE',1,NUORDR(J))
            CALL UTIMPI('L','CODE RETOUR DE RSEXCH :',1,IRE2)
            CALL UTIMPK('L','PB CHAM_NO',1,CHAMOU)
            CALL UTFINM()
          END IF
          CALL COPISD('CHAMP_GD','G',CHAMIN,CHAMOU)
          CALL RSNOCH(RESUOU,NOMSYM,NUORDR(J),' ')
   20   CONTINUE
   30 CONTINUE


      DO 50 I = 1 , NBORDR
        IF ( NUARCH(I).EQ.0 ) GOTO 50
        DO 40 J = 1 , NBACC
          NOPARA = NOMACC(J)
          CALL RSADPA(RESUIN,'L',1,NOPARA,NUORDR(I),1,IADIN,TYPE)
          CALL RSADPA(RESUOU,'E',1,NOPARA,NUORDR(I),1,IADOU,TYPE)
          IF (TYPE(1:1).EQ.'I') THEN
            ZI(IADOU) = ZI(IADIN)
          ELSEIF (TYPE(1:1).EQ.'R') THEN
            ZR(IADOU) = ZR(IADIN)
          ELSEIF (TYPE(1:1).EQ.'C') THEN
            ZC(IADOU) = ZC(IADIN)
          ELSEIF (TYPE(1:3).EQ.'K80') THEN
            ZK80(IADOU) = ZK80(IADIN)
          ELSEIF (TYPE(1:3).EQ.'K32') THEN
            ZK32(IADOU) = ZK32(IADIN)
          ELSEIF (TYPE(1:3).EQ.'K24') THEN
            ZK24(IADOU) = ZK24(IADIN)
          ELSEIF (TYPE(1:3).EQ.'K16') THEN
            ZK16(IADOU) = ZK16(IADIN)
          ELSEIF (TYPE(1:2).EQ.'K8') THEN
            ZK8(IADOU) = ZK8(IADIN)
          ENDIF
   40   CONTINUE
   50 CONTINUE

      CALL JEDEMA( )

      END
