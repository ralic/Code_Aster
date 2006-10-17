      SUBROUTINE JXVERI ( CUNIT , CMESS )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 17/10/2006   AUTEUR MCOURTOI M.COURTOIS 
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
C TOLE CFT_720 CFT_726 CFT_889 CRP_18 CRS_508 CRS_505
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       CUNIT , CMESS
C ----------------------------------------------------------------------
C VERIFIE L'INTEGRITE DU CHAINAGE AVANT DES SEGMENTS DE VALEURS ET DE LA
C ZONE MEMOIRE UTILISEE
C
C IN  CUNIT  : NOM LOCAL DU FICHIER D'IMPRESSION
C IN  CMESS  : MESSAGE D'INFORMATION
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C ----------------------------------------------------------------------
      INTEGER          ISSTAT
      COMMON /ICONJE/  ISSTAT
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
      PARAMETER  ( N = 5 )
      CHARACTER*1      GENR    , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8) , TYPE(8) , DOCU(2) , ORIG(1) , RNOM(1)
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
      INTEGER          IDINIT   ,IDXAXD   ,ITRECH,ITIAD,ITCOL,LMOTS,IDFR
      COMMON /IXADJE/  IDINIT(2),IDXAXD(2),ITRECH,ITIAD,ITCOL,LMOTS,IDFR
C ----------------------------------------------------------------------
      CHARACTER*32     NOM32
      INTEGER          ICL
C DEB ------------------------------------------------------------------
      NOM32  = '??'
      DO 100 IZ=1,2
        ID = IDINIT(IZ)
        IF (ID .EQ. 0) GOTO 100
 10     CONTINUE
        IS = ISZON ( JISZON + ID )
        IF ( IS .NE. 0 ) THEN
          ISD  = ISZON(JISZON + ID + 3)
          IDOS = ISZON(JISZON + ID + 2)
          ISF  = ISZON(JISZON + IS - 4)
          ICL  = ISZON(JISZON + IS - 2)
          IDCO = ISZON(JISZON + IS - 3)
          NOM32 = ' '
          IF ( ISF .NE. ISTAT(1) .AND. IDOS .NE. 0 ) THEN
             IF ( IDCO .EQ. 0 ) THEN
                NOM32 = RNOM(JRNOM(ICL)+IDOS)
             ELSE
                NOM32(1:24) = RNOM(JRNOM(ICL)+IDCO)
                WRITE ( NOM32(25:32) , '(I8)') IDOS
             ENDIF
          ENDIF
          IF ( ISD .EQ. ISTAT(1) .AND. ISF .EQ. ISTAT(1) ) THEN
            NOM32 = '<<<<    ZONE LIBRE          >>>>'
          ENDIF
          IF ( (ISD.LT.ISTAT(1).OR.ISD.GT.ISTAT(2)) ) THEN
            CALL U2MESK('F','JEVEUX_15',1,NOM32)
          ELSE IF ( (ISF.NE.ISTAT(1)).AND.(ISF.LT.ISTAT(3)
     &        .OR. ISF.GT.ISTAT(4))) THEN
            CALL U2MESK('F','JEVEUX_16',1,NOM32)
          ENDIF
          ID  = IS
          GO TO 10
        ELSE
          ISD = ISZON(JISZON + ID + 3)
          IF (ISD.NE.ISSTAT) THEN
            CALL U2MESK('F','JEVEUX_17',1,NOM32)
          END IF
          IF ( IZ .EQ. 1 ) THEN
            IF ( IDINIT(2) .EQ. 0 ) THEN
              IF ( ID .NE. LISZON-3 ) THEN
                CALL U2MESK('F','JEVEUX_17',1,NOM32)
              ENDIF
            ELSE
              IF ( ID .NE. IDINIT(2)-8 ) THEN
                CALL U2MESK('F','JEVEUX_17',1,NOM32)
              ENDIF
            ENDIF
          ELSE
            IF ( ID .NE. LISZON-3 ) THEN
              CALL U2MESK('F','JEVEUX_17',1,NOM32)
            ENDIF
          ENDIF
        ENDIF
 100  CONTINUE
C FIN ------------------------------------------------------------------
      END
