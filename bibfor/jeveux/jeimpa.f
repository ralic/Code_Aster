      SUBROUTINE JEIMPA ( CUNIT , NOMLU , COM )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 27/03/2002   AUTEUR VABHHTS J.PELLET 
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
C TOLE CFT_726 CFT_720 CRP_18 CRS_508 CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER *(*)      CUNIT , NOMLU , COM
C
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C
      INTEGER          IPGC, KDESMA, LGD, LGDUTI, KPOSMA, LGP, LGPUTI
      COMMON /IADMJE/  IPGC, KDESMA, LGD, LGDUTI, KPOSMA, LGP, LGPUTI
C
      INTEGER          NUMEC
      COMMON /INUMJE/  NUMEC
      CHARACTER*24     NOMEC
      COMMON /KNOMJE/  NOMEC
C     ------------------------------------------------------------------
      PARAMETER  ( N = 5 )
C
      CHARACTER*1      GENR    , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8) , TYPE(8) , DOCU(2) , ORIG(1) , RNOM(1)
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
C
      CHARACTER *8     NUME       , NOME
C
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
C     -----------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     ,IDIADD     , IDIADM     ,
     +               IDMARQ     , IDNOM      ,IDREEL     , IDLONG     ,
     +               IDLONO     , IDLUTI     ,IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 ,IDIADD = 2 , IDIADM = 3 ,
     +               IDMARQ = 4 , IDNOM  = 5 ,IDREEL = 6 , IDLONG = 7 ,
     +               IDLONO = 8 , IDLUTI = 9 ,IDNUM  = 10 )
C
      CHARACTER *75   CMESS
      CHARACTER *72   COML
      CHARACTER *32   NOML32
      CHARACTER *33   CVAL
      INTEGER         ICRE , IRET , IVAL
C
      PARAMETER     ( NNAO = 16 )
      PARAMETER     ( NNAC = 6  )
      CHARACTER *8    NAC(NNAC) , NAO(NNAO)
      CHARACTER *1    TAC(NNAC) , TAO(NNAO)  , GENRI
      INTEGER         LAC(NNAC) , LAO(NNAO)
      LOGICAL         TAB1(5,4) , TAB2(5,4) , TAB3(2,3)
      LOGICAL         LCONST , LCONTI , LCOL

      DATA             NUME       , NOME
     +               / '$$XNUM  ' , '$$XNOM  '  /
      DATA NAO     /
     1     'CLAS    ' , 'GENR    ' , 'TYPE    ' , 'LTYP    ' ,
     2     'DOCU    ' , 'DATE    ' , 'ORIG    ' , 'LONMAX  ' ,
     3     'NOMMAX  ' , 'LONUTI  ' , 'NOMUTI  ' , 'LONO    ' ,
     4     'IADM    ' , 'IADD    ' , 'LADD    ' , 'USAGE   ' /
      DATA NAC     /   'ACCES   ' , 'STOCKAGE' , 'MODELONG' ,
     1                 'NMAXOC  ' , 'NUTIOC  ' , 'LONT    ' /
      DATA TAO     /
     1     'K'      , 'K'      , 'K'      , 'I'      ,
     2     'K'      , 'I'      , 'K'      , 'I'      ,
     3     'I'      , 'I'      , 'I'      , 'I'      ,
     4     'I'      , 'I'      , 'I'      , 'K'      /
      DATA TAC     /
     1  'K'    , 'K'     , 'K'     , 'I'     ,'I' ,    'I' /
      DATA LAO     /
     1     1        , 1        , 1        , 0        ,
     2     4        , 0        , 8        , 0        ,
     3     0        , 0        , 0        , 0        ,
     4     0        , 0        , 0        , 3        /
      DATA LAC     /
     1    33        ,   8        ,   33       , 0          , 0  , 0   /
C 1 : CONT CSTE - 2 : DISP CSTE - 3 : CONT VARI - 4 : DISP VARI
C     - IRET = 3 - CONDITION D'ACCES A LONO / IADM / IADD / LADD / USAGE
      DATA (( TAB1(I,J),I=1,5),J=1,4)    /
     1     .FALSE.  , .FALSE.  , .FALSE.  , .FALSE.  , .TRUE.  ,
     2     .TRUE.   , .TRUE.   , .TRUE.   , .TRUE.   , .TRUE.  ,
     3     .FALSE.  , .FALSE.  , .FALSE.  , .FALSE.  , .TRUE.  ,
     4     .TRUE.   , .TRUE.   , .TRUE.   , .TRUE.   , .TRUE.  /
C     - IRET = 2 - CONDITION D'ACCES A LONO / IADM / IADD / LADD / USAGE
      DATA (( TAB2(I,J),I=1,5),J=1,4)    /
     1     .TRUE.  , .TRUE.   , .TRUE.   , .TRUE.   , .TRUE.  ,
     2     .FALSE. , .FALSE.  , .FALSE.  , .FALSE.  , .FALSE. ,
     3     .TRUE.  , .TRUE.   , .TRUE.   , .TRUE.   , .TRUE.  ,
     4     .FALSE. , .FALSE.  , .FALSE.  , .FALSE.  , .FALSE. /
C     ------------------- CONDITION D'ACCES A LON... / NOM...
      DATA (( TAB3(I,J),I=1,2),J=1,3)    /
     1     .TRUE.  , .FALSE. ,
     2     .TRUE.  , .FALSE. ,
     3     .FALSE. , .TRUE.  /
C DEB -----------------------------------------------------------------
      IPGCEX = IPGC
      IPGC = -2
      NOML32 = NOMLU
      COML   = COM
      ICRE = 0
      IRET = 0
      JCOL = 1
      ILON = 1
      CALL JJVERN ( NOML32 , ICRE , IRET )
C
      IF ( IRET .EQ. 0 ) THEN
        CMESS = 'NOM INEXISTANT DANS LES BASES OUVERTES'
        CALL JVMESS( 'S' , 'JEIMPA01' , CMESS)
      ELSE IF ( IRET .EQ. 1 ) THEN
        LCOL = .FALSE.
        IC = ICLAOS
        ID = IDATOS
      ELSE IF ( IRET .EQ. 2 ) THEN
        IC = ICLACO
        LCOL = .TRUE.
        CALL JJALLC ( ICLACO , IDATCO , 'L', IBACOL )
        ID     = ISZON(JISZON + IBACOL + IDDESO )
        IXLONG = ISZON(JISZON + IBACOL + IDLONG )
        LCONST = IXLONG .EQ. 0
        IXIADD = ISZON(JISZON + IBACOL + IDIADD )
        LCONTI = IXIADD .EQ. 0
        IF ( .NOT. LCONTI .AND.       LCONST ) JCOL = 2
        IF (       LCONTI .AND. .NOT. LCONST ) JCOL = 3
        IF ( .NOT. LCONTI .AND. .NOT. LCONST ) JCOL = 4
        IF ( NOML32(25:32) .NE. '        ') THEN
           CALL JJCROC ( NOML32(25:32) , ICRE )
           IRET = 3
        ENDIF
      ENDIF
      GENRI  = GENR ( JGENR(IC) + ID )
      JLON = 1
      IF ( GENRI .EQ. 'V' )  JLON = 2
      IF ( GENRI .EQ. 'N' )  JLON = 3
C
      CALL JVRINI ( CUNIT )
      CALL JVDEBM ( 'X' , 'JEIMPA' , 'IMPRESSION DES '//
     +              'ATTRIBUTS DE >'//NOML32(1:24)//'<'  )
      CALL JVIMPK ( 'L' , ' ' , 1 , COML )
      IF ( IRET .EQ. 3 ) THEN
         IF ( NOML32(25:32) .EQ. NOME ) THEN
            CALL JVIMPK ( 'S' , 'NOM OC' , 1 , NOMEC )
         ELSE IF ( NOML32(25:32) .EQ. NUME ) THEN
            CALL JVIMPI ( 'S' , 'NUM OC' , 1 , NUMEC )
         ENDIF
      ENDIF
      IF ( IRET . EQ. 2 ) THEN
        NNACI = NNAC
        IF ( .NOT. LCONTI ) NNACI = NNAC - 1
        DO 10 K = 1 , NNACI
           CALL JELIRA ( NOML32 , NAC(K) , IVAL , CVAL )
           IF ( TAC(K) .EQ. 'I' ) THEN
             CALL JVIMPI ( 'S' , NAC(K) , 1 , IVAL )
           ELSE
             CALL JVIMPK ( 'S' , NAC(K) , 1 , CVAL(1:LAC(K)) )
           ENDIF
   10   CONTINUE
      ENDIF
      DO 20 K = 1 , NNAO
          ICOL = K - 11
          IF ( NAO(K)(1:3) .EQ. 'LON' ) ILON = 1
          IF ( NAO(K)(1:3) .EQ. 'NOM' ) ILON = 2
          IF ( (K.LE.7)                                           .OR.
     +         (K.GT.7 .AND. K.LE.11
     +            .AND. LCONST .AND. TAB3(ILON,JLON) )            .OR.
     +         ( (IRET.EQ.1.OR.IRET.EQ.3) .AND. (K.GT.7 .AND. K.LE.11)
     +           .AND. TAB3(ILON,JLON) )                          .OR.
     +         ( IRET.EQ.2 .AND. (K.GT.11.AND.TAB2(ICOL,JCOL)))   .OR.
     +         ( IRET.EQ.3 .AND. (K.GT.11.AND.TAB1(ICOL,JCOL)))   .OR.
     +         ( IRET.EQ.1 .AND. (K.GT.11)                    ) ) THEN
            CALL JELIRA ( NOML32 , NAO(K) , IVAL , CVAL )
            IF ( TAO(K) .EQ. 'I' ) THEN
              CALL JVIMPI ( 'S' , NAO(K) , 1 , IVAL )
            ELSE
              CALL JVIMPK ( 'S' , NAO(K) , 1 , CVAL(1:LAO(K)) )
            ENDIF
          ENDIF
   20 CONTINUE
      CALL JVFINM
      CALL JVRINI ( ' ' )
      IF ( LCOL ) THEN
         CALL JJLIDE ( 'JEIMPA' , NOML32(1:24) , 2 )
      ENDIF
      IPGC = IPGCEX
      END
