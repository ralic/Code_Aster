      SUBROUTINE JXVERI ( CUNIT , CMESS )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 16/06/2000   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C ROUTINE AVEC ADHERENCE SYSTEME CRAY : HPCHECK
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
          ISD  = ISZON(JISZON + ID + 3) / ISSTAT
          IDOS = ISZON(JISZON + ID + 2)
          ISF  = ISZON(JISZON + IS - 4) / ISSTAT
          ICL  = ISZON(JISZON + IS - 2)
          IDCO = ISZON(JISZON + IS - 3)
          NOM32 = ' '
          IF ( ISF .NE. 1 .AND. IDOS .NE. 0 ) THEN
             IF ( IDCO .EQ. 0 ) THEN
                NOM32 = RNOM(JRNOM(ICL)+IDOS)
             ELSE
                NOM32(1:24) = RNOM(JRNOM(ICL)+IDCO)
                WRITE ( NOM32(25:32) , '(I8)') IDOS
             ENDIF
          ENDIF
          IF ( ISD .EQ. 1 .AND. ISF .EQ. 1 ) THEN
            NOM32 = '<<<<    ZONE LIBRE          >>>>'
          ENDIF
          IF ( (ISD.LT.1.OR.ISD.GT.2) ) THEN
            CALL JVMESS ('F','JXVERI01',' ECRASEMENT AMONT, L''OBJET :<'
     &                  //NOM32//'> EST PEUT ETRE ECRASE' )
          ELSE IF ( (ISF.NE.1).AND.(ISF.LT.3 .OR. ISF.GT.4)) THEN
            CALL JVMESS ('F','JXVERI02',' ECRASEMENT AVAL, L''OBJET :<'
     &                  //NOM32//'> EST PEUT ETRE ECRASE' )
          ENDIF
          ID  = IS
          GO TO 10
        ELSE
          ISD = ISZON(JISZON + ID + 3)
          IF (ISD.NE.ISSTAT) THEN
            CALL JVMESS ('F','JXVERI03',
     &                     ' CHAINAGE CASSE APRES L''OBJET : '//NOM32)
          END IF
        ENDIF
 100  CONTINUE
      IERR = 0
      CALL HPCHECK(IERR)
      IF ( IERR .NE. 0 ) THEN
        CALL JVMESS ('F','JXVERI04',' ERREUR DETECTEE CODE RETOUR'
     &              //' HPCHECK NON NUL ')
      ENDIF
C FIN ------------------------------------------------------------------
      END
