      SUBROUTINE JJCROC ( KNAT , ICRE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 10/03/98   AUTEUR VABHHTS J.PELLET 
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
C TOLE CFT_720 CFT_726 CRP_18 CRS_508 CRS_512

      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                    ICRE
      CHARACTER*8         KNAT
C ----------------------------------------------------------------------
C INSERTION D'UN NOM DANS UN REPERTOIRE PRIVE
C ACTUALISE LE CONTENU DU COMMON  /IATCJE/
C
C IN  KNAT   : CHAINE VALANT '$$XNOM   ' OU '$$XNUM   '
C IN  ICRE   : CODE DE CREATION DU NOM PASSE A JJCODN
C              ICRE = 0 ON VERIFIE UNIQUEMENT L'EXISTENCE
C              ICRE = 1 ON CREE LE NOM
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C ----------------------------------------------------------------------
      PARAMETER      ( N = 5 )
      INTEGER          LTYP    , LONG    , DATE    , IADD    , IADM    ,
     +                 LONO    , HCOD    , CARA    , LUTI    , IMARQ
      COMMON /IATRJE/  LTYP(1) , LONG(1) , DATE(1) , IADD(1) , IADM(1) ,
     +                 LONO(1) , HCOD(1) , CARA(1) , LUTI(1) , IMARQ(1)
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     +                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
C
      CHARACTER*1      GENR    , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8) , TYPE(8) , DOCU(2) , ORIG(1) , RNOM(1)
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
C ----------------------------------------------------------------------
      INTEGER          NUMEC
      COMMON /INUMJE/  NUMEC
      CHARACTER*24     NOMEC
      COMMON /KNOMJE/  NOMEC
C
      CHARACTER *24                     NOMCO
      CHARACTER *32    NOMUTI , NOMOS ,         NOMOC , BL32
      COMMON /NOMCJE/  NOMUTI , NOMOS , NOMCO , NOMOC , BL32
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
C ----------------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     ,IDIADD     , IDIADM     ,
     +               IDMARQ     , IDNOM      ,IDREEL     , IDLONG     ,
     +               IDLONO     , IDLUTI     ,IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 ,IDIADD = 2 , IDIADM = 3 ,
     +               IDMARQ = 4 , IDNOM  = 5 ,IDREEL = 6 , IDLONG = 7 ,
     +               IDLONO = 8 , IDLUTI = 9 ,IDNUM  = 10 )
C ----------------------------------------------------------------------
      INTEGER          ILOREP , IDENO , ILNOM , ILMAX , ILUTI , IDEHC
      PARAMETER      ( ILOREP=1,IDENO=2,ILNOM=3,ILMAX=4,ILUTI=5,IDEHC=6)
C ----------------------------------------------------------------------
      CHARACTER *75    CMESS
      CHARACTER *24    NOM
      CHARACTER *8     CIOC
      INTEGER          JITAB
C
      CHARACTER *8     NUME       , NOME
      DATA             NUME       , NOME
     +               / '$$XNUM  ' , '$$XNOM  ' /
C DEB ------------------------------------------------------------------
      IF ( KNAT .EQ. '        ' ) THEN
C
C ------ REPERTOIRE DE NOM
C
        LTYPI   = LTYP (JLTYP(ICLAOS) + IDATOS )
        IADMI   = IADM( JIADM(ICLAOS) + IDATOS )
        IF ( IADMI .GT. 0 ) THEN
          KADM  = IADMI
          JITAB = JISZON + KADM - 1
          KITAB = JK1ZON + ( KADM - 1 ) * LOIS
          NMAX  = LONG (JLONG(ICLAOS)+IDATOS )
          NUTI  = LUTI (JLUTI(ICLAOS)+IDATOS )
          NUTIEX = NUTI
          NOM   = RNOM (JRNOM(ICLAOS)+IDATOS )(1:24)
          IF ( NUTI .EQ. 0 ) THEN
             NHCOD = JJPREM(NMAX)
             ISZON(JITAB + ILOREP) = NHCOD
             ISZON(JITAB + IDENO ) = ( IDEHC + NHCOD ) * LOIS
             ISZON(JITAB + ILNOM ) = LTYPI
             ISZON(JITAB + ILMAX ) = NMAX
             ISZON(JITAB + ILUTI ) = NUTI
             ISZON(JITAB + IDEHC ) = IDEHC
            DO 5 I = 1,NHCOD
              ISZON( JITAB + I + IDEHC ) = 0
   5      CONTINUE
            DO 6 I = 1,NMAX*LTYPI
              K1ZON( KITAB + ISZON(JITAB+IDENO) + I ) = '?'
   6      CONTINUE
          ENDIF
        ENDIF
        IDOC = JJCODN ( ICRE , NOM , NOMEC(1:LTYPI) ,
     +           ISZON(JITAB+1) , K1ZON(KITAB+1) , NMAX , NUTI )
        IF ( IDOC .GT. 0 ) THEN
           IDATOC = IDOC
           NOMOC  = NOMEC
        ELSE
           IDATOC = 0
           NOMOC  = BL32
        ENDIF
        NOMCO = '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
        IF ( NUTIEX .NE. NUTI ) LUTI (JLUTI(ICLAOS)+IDATOS) = NUTI
      ELSE
C
C ----- COLLECTION
C
        IBACOL = IADM(JIADM(ICLACO)+IDATCO)
        IC     = ICLACO
        IF ( KNAT .EQ. NUME ) THEN
C
C --------  ACCES PAR NUMERO
C
          IXNUM   = ISZON(JISZON+IBACOL+IDNUM )
          IXNOM   = ISZON(JISZON+IBACOL+IDNOM )
          IF ( IXNUM .NE. 0 ) THEN
            IBNUM = IADM(JIADM(IC)+IXNUM)
            NMAX   = ISZON(JISZON+IBNUM  )
            NUTI   = ISZON(JISZON+IBNUM+1)
          ELSE IF ( IXNOM .NE. 0 ) THEN
            IF ( ICRE .GT. 0 ) THEN
              CMESS = 'CREATION D''OBJET AUTORISEE UNIQUEMENT PAR NOM'
              CALL JVMESS ( 'S' , 'JJCROC01' , CMESS )
            ENDIF
            NMAX   = LONG ( JLONG(IC) + IXNOM )
            NUTI   = LUTI ( JLUTI(IC) + IXNOM )
          ENDIF
          IF ( NUMEC .LE. 0 .OR. NUMEC .GT. NMAX ) THEN
            CALL JVDEBM ( 'S', 'JJCROC03', 'NUMERO D''OBJET INVALIDE ')
            CALL JVIMPI ( 'S' , ':' , 1 , NUMEC )
            CALL JVFINM
          ENDIF
          NUTIEX = NUTI
          IDATOC = 0
          IF ( ICRE .GT. 0 ) THEN
            IF ( NUMEC .LE. NUTIEX ) THEN
              WRITE (CIOC , '(I8)' ) NUMEC
              CMESS = 'OBJET DE COLLECTION '//CIOC//' DEJA EXISTANT'
              CALL JVMESS ( 'S' , 'JJCROC02' , CMESS )
            ELSE
              IF ( NUTIEX .LT. NMAX ) THEN
                NUTI = NUTI + 1
                ISZON(JISZON+IBNUM+1) = NUTI
              ELSE
                CMESS = 'REPERTOIRE SATURE'
                CALL JVMESS ( 'F' , 'JJCROC03' , CMESS )
              ENDIF
            ENDIF
          ENDIF
          IDATOC = NUMEC
        ELSE IF ( KNAT .EQ. NOME ) THEN
C
C --------  ACCES PAR NOM
C
          IXNOM   = ISZON(JISZON+IBACOL+IDNOM )
          IF ( IXNOM .EQ. 0 ) THEN
            CMESS = 'ACCES PAR NOM A UNE COLLECTION NUMEROTEE '
            CALL JVMESS ( 'S' , 'JJCROC04' , CMESS )
          ELSE
            NMAX    = LONG (JLONG(IC)+IXNOM )
            NUTI    = LUTI (JLUTI(IC)+IXNOM )
            NUTIEX = NUTI
            IF ( NOMEC .NE. NOMOC .OR. ICRE .EQ. -3 ) THEN
              IBNOM   = IADM ( JIADM(IC) + IXNOM  )
              JITAB   = JISZON + IBNOM - 1
              KITAB   = JK1ZON + ( IBNOM - 1 ) * LOIS
              NOM     = RNOM (JRNOM(IC)+IXNOM )(1:24)
              LONGNO = LTYP (JLTYP(IC)+IXNOM )
              IF ( NUTI .EQ. 0 ) THEN
                NHCOD = JJPREM(NMAX)
                ISZON(JITAB + ILOREP) = NHCOD
                ISZON(JITAB + IDENO ) = ( IDEHC + NHCOD ) * LOIS
                ISZON(JITAB + ILNOM ) = LONGNO
                ISZON(JITAB + ILMAX ) = NMAX
                ISZON(JITAB + ILUTI ) = NUTI
                ISZON(JITAB + IDEHC ) = IDEHC
                DO 15 I = 1,NHCOD
                   ISZON( JITAB + I + IDEHC ) = 0
 15             CONTINUE
                DO 16 I = 1,NMAX * LONGNO
                   K1ZON( KITAB + ISZON(JITAB+IDENO) + I) = '?'
 16             CONTINUE
              ENDIF
              IDOC = JJCODN ( ICRE , NOM , NOMEC(1:LONGNO) ,
     +                   ISZON(JITAB+1) , K1ZON(KITAB+1) , NMAX , NUTI)
              IF ( IDOC .GT. 0 ) THEN
                 NOMOC   = NOMEC
                 IDATOC = IDOC
              ELSE
                 NOMOC   = BL32
                 IDATOC = 0
              ENDIF
              IF ( NUTIEX .NE. NUTI ) LUTI (JLUTI(IC)+IXNOM ) = NUTI
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C FIN ------------------------------------------------------------------
      END
