      SUBROUTINE EXTRS1 ( RESU0,
     >                    NBORDR, NUORDR, NBACC, NOMACC,
     >                    NBARCH, NUARCH, NBEXCL, CHEXCL, NBNOSY )
      IMPLICIT   NONE
      INTEGER NBORDR,NUORDR(*),NBARCH,NBACC,NUARCH(*),NBEXCL,NBNOSY
      CHARACTER*16 NOMACC(*),CHEXCL(*)
      CHARACTER*(*) RESU0
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 28/01/2003   AUTEUR DURAND C.DURAND 
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
C     OPERATEUR D'EXTRACTION
C     ------------------------------------------------------------------
C
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
      PARAMETER ( NOMPRO = 'EXTRS1' )
C
      INTEGER LXLGUT
C
      INTEGER IORDR, I, J, K, IATACH, IADIN, IADOU, IRE1
      INTEGER KORDR, IUNDF, ISNNEM
      REAL*8 RUNDF, R8VIDE
      CHARACTER*3 TYPE
      CHARACTER*16 NOMSYM
      CHARACTER*16 NOPARA
      CHARACTER*19 NOMSDR
      CHARACTER*24 CHAMIN
C     ------------------------------------------------------------------
C
      CALL JEMARQ( )
      RUNDF = R8VIDE()
      IUNDF = ISNNEM()
C
C               1234567890123456789
      NOMSDR = '                   '
      I = LXLGUT(RESU0)
      NOMSDR(1:I) = RESU0(1:I)
C
      DO 110 I = 1 , NBNOSY
         CALL JENUNO ( JEXNUM(NOMSDR//'.DESC',I), NOMSYM )
         CALL JEVEUO ( JEXNUM(NOMSDR//'.TACH',I), 'E', IATACH )
         DO 120 J = 1 , NBEXCL
            IF ( CHEXCL(J) .EQ. NOMSYM ) THEN
               DO 122 K = 1 , NBORDR
                  IF ( ZK24(IATACH+K-1)(1:1) .EQ. ' ' ) GOTO 122
                  CALL RSEXCH ( NOMSDR, NOMSYM, NUORDR(K), CHAMIN, IRE1)
                  IF ( IRE1 .NE. 0   ) THEN
                     CALL UTDEBM('F',NOMPRO,'APPEL ERRONE')
                     CALL UTIMPI('L','NUMERO D''ORDRE',1,NUORDR(K))
                     CALL UTIMPI('L','CODE RETOUR DE RSEXCH :',1,IRE1)
                     CALL UTIMPK('L','PB CHAM_NO',1,CHAMIN)
                     CALL UTFINM()
                  ENDIF
                  CALL DETRSD ( 'CHAMP_GD', CHAMIN )
 122           CONTINUE
               GOTO 110
            ENDIF
 120     CONTINUE
         IORDR = 0
         DO 130 J = 1 , NBORDR
            IF ( ZK24(IATACH+J-1)(1:1) .EQ. ' ' ) GOTO 130
            CALL RSEXCH ( NOMSDR, NOMSYM, NUORDR(J), CHAMIN, IRE1 )
            IF ( IRE1 .NE. 0   ) THEN
               CALL UTDEBM('F',NOMPRO,'APPEL ERRONE')
               CALL UTIMPI('L','NUMERO D''ORDRE',1,NUORDR(J))
               CALL UTIMPI('L','CODE RETOUR DE RSEXCH :',1,IRE1)
               CALL UTIMPK('L','PB CHAM_NO',1,CHAMIN)
               CALL UTFINM()
            ENDIF
            IF ( NUARCH(J) .EQ. 0 ) THEN
               CALL DETRSD ( 'CHAMP_GD', CHAMIN )
            ELSE
               IORDR = IORDR + 1
               ZK24(IATACH+IORDR-1)(1:19) = CHAMIN(1:19)
            ENDIF
 130     CONTINUE
         IORDR = IORDR + 1
         DO 132 J = IORDR , NBORDR
            IF ( ZK24(IATACH+J-1)(1:1) .NE. ' ' ) THEN
               ZK24(IATACH+J-1)(1:24) = ' '
            ENDIF
 132     CONTINUE
 110  CONTINUE
C
      IORDR = 0
      DO 50 I = 1 , NBORDR
        IF ( NUARCH(I).EQ.0 ) GOTO 50
        IORDR = IORDR + 1
        DO 40 J = 1 , NBACC
          NOPARA = NOMACC(J)
          CALL RSADPA ( NOMSDR, 'L', 1, NOPARA, NUORDR(I), 1,
     >                                                   IADIN, TYPE )
          CALL EXTRS3 ( NOMSDR, NOPARA, IORDR, 'E', 1, TYPE, IADOU )
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
C
      CALL JEECRA ( NOMSDR//'.ORDR', 'LONUTI', NBARCH, ' ')
      CALL JEVEUO ( NOMSDR//'.ORDR', 'E', KORDR )
      IORDR = 0
      DO 150 I = 1 , NBORDR
         IF ( NUARCH(I) .EQ. 0 ) GOTO 150
         IORDR = IORDR + 1
         ZI(KORDR+IORDR-1) = NUORDR(I)
 150  CONTINUE
C
      IORDR = IORDR + 1
      DO 152 I = IORDR , NBORDR
         ZI(KORDR+I-1) = IUNDF
         DO 154 J = 1 , NBACC
            NOPARA = NOMACC(J)
            CALL EXTRS3 ( NOMSDR, NOPARA, I, 'E', 1, TYPE, IADOU)
            IF (TYPE(1:1).EQ.'I') THEN
               ZI(IADOU) = IUNDF
            ELSEIF (TYPE(1:1).EQ.'R') THEN
               ZR(IADOU) = RUNDF
            ELSEIF (TYPE(1:1).EQ.'C') THEN
               ZC(IADOU) = DCMPLX(RUNDF,RUNDF)
            ELSEIF (TYPE(1:3).EQ.'K80') THEN
               ZK80(IADOU) = ' '
            ELSEIF (TYPE(1:3).EQ.'K32') THEN
               ZK32(IADOU) = ' '
            ELSEIF (TYPE(1:3).EQ.'K24') THEN
               ZK24(IADOU) = ' '
            ELSEIF (TYPE(1:3).EQ.'K16') THEN
               ZK16(IADOU) = ' '
            ELSEIF (TYPE(1:2).EQ.'K8') THEN
               ZK8(IADOU) = ' '
            ENDIF
 154     CONTINUE
 152  CONTINUE
C
      CALL JEDEMA( )
C
      END
