      SUBROUTINE JJIMPO (UNIT , IADMI , IDECI , IDATOC , GENRI , TYPEI,
     &                   LT    , LONOI , MESS , PARM )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 16/06/2004   AUTEUR DURAND C.DURAND 
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
C TOLE CFT_726 CFT_720 CFT_753 CRP_18 CRP_4 CRS_508 CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            UNIT
      INTEGER            IADMI , IDECI , IDATOC        , LT , LONOI
      CHARACTER*(*)      MESS  , PARM  , GENRI , TYPEI
C ----------------------------------------------------------------------
C ROUTINE UTILISATEUR : IMPRIME UN SEGMENT DE VALEURS
C
C IN  UNIT   : UNITE LOGIQUE D'IMPRESSION
C IN  IADMI  : ADRESSE DU PREMIER MOT DU SEGMENT DE VALEUR
C IN  IDECI  : DECALLAGE PAR RAPPORT A IADMI (EN OCTETS)
C IN  IDATOC : IDENTIFICATEUR DE L'OBJET
C IN  GENRI  : GENRE DE L'OBJET
C IN  TYPEI  : TYPE DE L'OBJET
C IN  LT     : LONGUEUR DU TYPE
C IN  LONOI  : LONGEUR EN ENTIER DU SEGMENT
C IN  MESS   : MESSAGE D'INFORMATION
C IN  PARM   : ?
C ----------------------------------------------------------------------
      PARAMETER      ( N = 5 )
      CHARACTER*1      GENR    , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8) , TYPE(8) , DOCU(2) , ORIG(1) , RNOM(1)
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      REAL*8           R8ZON(1)
      LOGICAL          LSZON(1)
      INTEGER*4        I4ZON(1)
      EQUIVALENCE    ( ISZON(1), K1ZON(1), R8ZON(1), LSZON(1), I4ZON(1))
C ----------------------------------------------------------------------
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     &                 DN2(N)
C
C ----------------------------------------------------------------------
      INTEGER          ILOREP , IDENO , ILNOM , ILMAX , ILUTI , IDEHC
      PARAMETER      ( ILOREP=1,IDENO=2,ILNOM=3,ILMAX=4,ILUTI=5,IDEHC=6)
C ----------------------------------------------------------------------
      CHARACTER*6      PGME
      PARAMETER      ( PGME = 'JJIMPO' )
      CHARACTER*75     CMESS
      CHARACTER*18     FMT
C DEB ------------------------------------------------------------------
C
      IF ( UNIT .EQ. 0 ) GOTO 999
      KADM = IADMI
      LADM = ISZON ( JISZON + KADM - 3 )
      IPS  = KADM - 4
      IEP  = KADM - 1
      IES  = ISZON ( JISZON + IPS ) - 4
      IDOS = ISZON(JISZON+IEP-1)
      ICLS = ISZON(JISZON+IES+2)
      IDCO = ISZON(JISZON+IES+1)
C
      JI4ZON = JISZON * LOIS*2/LOR8
C
      IF ( IDOS .EQ. 0 ) THEN
         CMESS = 'ECRASEMENT DE L''ADRESSE REPERTOIRE DU SEGMENT '
     &            //'DE VALEURS'
         CALL JVMESS ( 'S' , PGME//'01' , CMESS )
      ENDIF
      IF ( ICLS .LT. 0 .OR. ICLS .GT. LEN(CLASSE))  THEN
         CMESS = 'ECRASEMENT DE LA CLASSE DU SEGMENT DE VALEURS'
         CALL JVMESS ( 'S' , PGME//'02' , CMESS )
      ELSE
         IF ( CLASSE(ICLS:ICLS) .EQ. ' ' .OR.
     &        CLASSE(ICLS:ICLS) .EQ. '$' ) THEN
            CMESS = 'ECRASEMENT DE LA CLASSE DU SEGMENT DE VALEURS'
            CALL JVMESS ( 'S' , PGME//'03' , CMESS )
         ENDIF
      ENDIF
      IF ( IDATOC .EQ. 0 ) THEN
         WRITE (UNIT,'(/,'' IMPRESSION SEGMENT DE VALEURS >'',A,
     &            ''<'')') RNOM(JRNOM(ICLS)+IDOS)(1:32)
      ELSE IF ( IDATOC .EQ . -1 ) THEN
         WRITE (UNIT,'(/,'' IMPRESSION COLLECTION ENTIERE >''
     &            ,A,''<'')') RNOM(JRNOM(ICLS)+IDOS)(1:24)
      ELSE IF ( IDCO .GT. 0 ) THEN
         WRITE(UNIT,'(/,'' IMPRESSION OBJET DE COLLECTION >''
     &         ,A,''<  OC : '',I6)') RNOM(JRNOM(ICLS)+IDCO)(1:24),IDOS
      ELSE IF ( IDCO .EQ. 0 ) THEN
         WRITE(UNIT,'(/,'' IMPRESSION OBJET DE COLLECTION CONTIGUE>''
     &       ,A,''<  OC : '',I6)') RNOM(JRNOM(ICLS)+IDOS)(1:24),IDATOC
      ENDIF
      WRITE (UNIT,'(A,A)' ) ' >>>>> ',MESS(1:MIN(50,LEN(MESS)))
      IF ( GENRI .NE. 'N') THEN
         IF ( TYPEI .EQ. 'S' ) THEN
            JI = 1 + ((JISZON +KADM-1)*LOIS+IDECI)*2/LOR8 + LADM*2/LOR8
            NL = LONOI / (5*LOR8/2)
            ND = MOD( LONOI , (5*LOR8/2) ) / (LOR8/2)
            WRITE ( UNIT , '((I7,'' - '',5(I12,1X)))')
     &            (5*(L-1)+1,(I4ZON( JI + 5*(L-1)+K-1),K=1,5),L=1,NL)
            IF ( ND .NE. 0 ) THEN
               WRITE ( UNIT , '(I7,'' - '',5(I12,1X))')
     &                  5*NL+1,(I4ZON( JI + 5*NL+K-1),K=1,ND)
            ENDIF
         ELSE IF ( TYPEI .EQ. 'I' ) THEN
            JI = JISZON + KADM + IDECI/LOIS
            NL = LONOI / (5*LOIS)
            ND = MOD( LONOI , (5*LOIS) ) / LOIS
            WRITE ( UNIT , '((I7,'' - '',5(I12,1X)))')
     &            (5*(L-1)+1,(ISZON( JI + 5*(L-1)+K-1),K=1,5),L=1,NL)
            IF ( ND .NE. 0 ) THEN
               WRITE ( UNIT , '(I7,'' - '',5(I12,1X))')
     &                  5*NL+1,(ISZON( JI + 5*NL+K-1),K=1,ND)
            ENDIF
         ELSE IF ( TYPEI .EQ. 'R' ) THEN
            JI =   1 + ( (JISZON +KADM - 1)*LOIS +IDECI +LADM) / LOR8
            NL = LONOI / ( 5 * LOR8 )
            ND = MOD( LONOI , (5*LOR8) ) / LOR8
            WRITE ( UNIT , '((I7,'' - '',5(1PD12.5,1X)))')
     &              (5*(L-1)+1,(R8ZON( JI + 5*(L-1)+K-1),K=1,5),L=1,NL)
            IF ( ND .NE. 0 ) THEN
               WRITE ( UNIT , '(I7,'' - '',5(1PD12.5,1X))')
     &                  5*NL+1,(R8ZON( JI +5*NL+K-1),K=1,ND)
            ENDIF
         ELSE IF ( TYPEI .EQ. 'C' ) THEN
            JI =   1 + ( (JISZON +KADM - 1)*LOIS +IDECI +LADM) / LOR8
            NL = LONOI / ( 2 * LOC8)
            ND = MOD( LONOI , ( 2 * LOC8) ) / LOC8
            WRITE ( UNIT , '((I7,'' - '',1P,
     &                                   2(A1,D12.5,'','',D12.5,A1)))')
     &      (2*(L-1)+1,('(',R8ZON(JI+4*(L-1)+2*K),
     &                      R8ZON(JI+4*(L-1)+2*K+1),')',K=0,1),L=1,NL)
            IF ( ND .NE. 0 ) THEN
               WRITE ( UNIT , '((I7,'' - '',1P,
     &                                   2(A1,D12.5,'','',D12.5,A1)))')
     &         2*NL+1,'(',R8ZON(JI+4*(L-1)),
     &                     R8ZON(JI+4*(L-1)+1),')'
            ENDIF
         ELSE IF ( TYPEI .EQ. 'L' ) THEN
            JI = JISZON + KADM + IDECI/LOIS
            NL = LONOI / (20*LOLS)
            ND = MOD( LONOI , (20*LOLS) ) / LOLS
            WRITE ( UNIT , '((I7,'' - '',20(L1,1X)))')
     &            (20*(L-1)+1,(LSZON( JI +20*(L-1)+K-1),K=1,20),L=1,NL)
            IF ( ND .NE. 0 ) THEN
               WRITE ( UNIT , '(I7,'' - '',20(L1,1X))')
     &                  20*NL+1,(LSZON( JI + 20*NL+K-1),K=1,ND)
            ENDIF
         ELSE IF ( TYPEI .EQ. 'K' ) THEN
            JI = 1 + ( JISZON + KADM - 1) * LOIS + IDECI + LADM
            NB = MAX(65,MIN (81,LT+1)) / (LT+1)
            NL = LONOI / (NB* LT)
            ND = (MOD( LONOI , NB*LT )) / LT
            WRITE ( FMT,'(I2,''(A1,'',I2,''A1,A1)'')' ) NB,LT
            WRITE ( UNIT , '((I7,'' - '','//FMT//'))')
     &              ( NB*(L-1)+1,
     &         ('>',(K1ZON( JI+LT*((K-1)+(L-1)*NB)+J-1 ),J=1,LT),'<',
     &                                       K= 1,NB ) , L = 1,NL )
            IF ( ND .NE. 0 ) THEN
              WRITE ( UNIT , '(I7,'' - '','//FMT//')') NB*NL+1,
     &        ('>',(K1ZON (JI +LT*((K-1)+NL*NB)+J-1),J=1,LT),'<',K=1,ND)
            ENDIF
         ELSE
            CMESS = 'ERREUR DE TYPE SUR LE SEGMENT DE VALEURS'
            CALL JVMESS ( 'S' , PGME//'04' , CMESS )
         ENDIF
      ELSE
         NM     = ISZON(JISZON+KADM-1+ILMAX)
         NU     = ISZON(JISZON+KADM-1+ILUTI)
         IF ( NU .NE. 0 ) NM = NU
         IDENOM = ISZON(JISZON+KADM-1+IDENO)
         JI = 1 + IDENOM + ( JISZON + KADM - 1 ) * LOIS
         NB = MAX(65,MIN (81,LT+1)) / (LT+1)
         NL = NM / NB
         ND = MOD(NM,NB)
         WRITE ( FMT,'(I2,''(A1,'',I2,''A1,A1)'')' ) NB,LT
         WRITE ( UNIT , '((I7,'' - '','//FMT//'))')
     &        ( NB*(L-1)+1,
     &      ('>',(K1ZON( JI+LT*((K-1)+(L-1)*NB)+J-1 ),J=1,LT),'<',
     &                                      K= 1,NB ) , L = 1,NL )
         IF ( ND .NE. 0 ) THEN
            WRITE ( UNIT , '(I7,'' - '','//FMT//')') NB*NL+1,
     &        ('>',(K1ZON (JI +LT*((K-1)+NL*NB)+J-1),J=1,LT),'<',K=1,ND)
         ENDIF
      ENDIF
 999  CONTINUE
C FIN ------------------------------------------------------------------
      END
