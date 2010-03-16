      SUBROUTINE JECREC (NOMLU, LISTAT, ACCELU, STOCLU, LONGLU, NMAX)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 15/03/2010   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C TOLE CFT_720 CFT_726 CRP_18
C TOLE CRS_508
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER *(*)     NOMLU, LISTAT, ACCELU, STOCLU, LONGLU
      INTEGER                                                   NMAX
C     ------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
      PARAMETER  ( N = 5 )
      INTEGER          LTYP    , LONG    , DATE    , IADD    , IADM    ,
     &                 LONO    , HCOD    , CARA    , LUTI    , IMARQ   
      COMMON /IATRJE/  LTYP(1) , LONG(1) , DATE(1) , IADD(1) , IADM(1) ,
     &                 LONO(1) , HCOD(1) , CARA(1) , LUTI(1) , IMARQ(1)
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     &                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
      CHARACTER*1      GENR    , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8) , TYPE(8) , DOCU(2) , ORIG(1) , RNOM(1)
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
C     ------------------------------------------------------------------
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     &                 DN2(N)
      CHARACTER *24                     NOMCO
      CHARACTER *32    NOMUTI , NOMOS ,         NOMOC , BL32
      COMMON /NOMCJE/  NOMUTI , NOMOS , NOMCO , NOMOC , BL32
C     ------------------------------------------------------------------
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      INTEGER          IPGC,KDESMA(2),LGD,LGDUTI,KPOSMA(2),LGP,LGPUTI
      COMMON /IADMJE/  IPGC,KDESMA,   LGD,LGDUTI,KPOSMA,   LGP,LGPUTI
C     ------------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     ,IDIADD     , IDIADM     ,
     &               IDMARQ     , IDNOM      ,             IDLONG     ,
     &               IDLONO     , IDLUTI     ,IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 ,IDIADD = 2 , IDIADM = 3 ,
     &               IDMARQ = 4 , IDNOM  = 5 ,             IDLONG = 7 ,
     &               IDLONO = 8 , IDLUTI = 9 ,IDNUM  = 10 )
C     ------------------------------------------------------------------
      CHARACTER *8    STOCKA , CVAL(3)
      INTEGER         IBID,IADZON,KNOM(1),LVAL(3),ICRE,IRET,IRETC,ICREI
      CHARACTER *2    TA
      CHARACTER *32   NOML32 , NOM32
      CHARACTER *24   NOM24L
      CHARACTER *26   NOMPAR
C DEB ------------------------------------------------------------------
      IPGCEX = IPGC
      NOM24L = NOMLU
      IF ( NMAX .LE. 0 ) THEN
         CALL U2MESK('F','JEVEUX_67',1,NOM24L)
      ENDIF
C
      CALL JJANAL ( LISTAT , 3 , 3 , LVAL ,CVAL )
      ICLAS  = INDEX ( CLASSE , CVAL(1)(1:1) )
      IF ( ICLAS .EQ. 0 ) THEN
         CALL U2MESK('F','JEVEUX_68',1,CVAL(1)(1:1))
      ENDIF
C
      ICRE = 2
      CALL JJVERN( NOM24L//'        ' , ICRE , IRETC )
      ICRE = 1
C
      IF ( IRETC .EQ. 1 ) THEN
         CALL U2MESK('F','JEVEUX_69',1,NOM24L)
      ELSE
        STOCKA = STOCLU
        IF ( LVAL(3) .EQ. 2 ) READ ( CVAL(3)(2:LVAL(3)) , '(I1)' ) LENK
        IF ( LVAL(3) .EQ. 3 ) READ ( CVAL(3)(2:LVAL(3)) , '(I2)' ) LENK
        IF ( LVAL(3) .GT. 3 ) LENK = 512
        IF ( STOCKA .NE. 'CONTIG  ' .AND. STOCKA .NE. 'DISPERSE' ) THEN
           CALL U2MESK('F','JEVEUX_70',1,STOCKA)
        ELSE IF (LONGLU .NE. 'CONSTANT'.AND. CVAL(2)(1:1) .EQ. 'E') THEN
           CALL U2MESK('F','JEVEUX_71',1,NOM24L)
        ELSE IF ( STOCKA .EQ. 'CONTIG  ' .AND. LONGLU .NE. 'CONSTANT'
     &            .AND. CVAL(3)(1:1) .EQ. 'K' .AND. LENK .NE. 8
     &            .AND. LENK .NE. 16          .AND. LENK .NE.24 ) THEN
           CALL U2MESK('F','JEVEUX_72',1,NOM24L)
        ENDIF
C
        CALL JJCREC(ICLACO, IDATCO, 'X', 'I',IDNUM+1 , IADCOL )
        ISZON (JISZON + IADCOL + IVNMAX ) = NMAX
C     ------------------------------------------------------------------
        NOM32  = NOM24L//'$$DESO  '
        CALL JJCREN( NOM32 , ICRE , IRET )
        CALL JJCREC(ICLAOS,IDATOS,
     &              CVAL(2)(1:1),CVAL(3)(1:LVAL(3)),0,IADZON)
        IF ( CVAL(2)(1:1) .EQ. 'E' ) THEN
           IF (STOCKA .EQ. 'CONTIG  ') THEN
              LONO( JLONO(ICLAOS) + IDATOS) = NMAX
           ELSE IF( LONGLU .EQ. 'CONSTANT') THEN
              LONO( JLONO(ICLAOS) + IDATOS) = 1
           ENDIF
        ENDIF
        ISZON ( JISZON + IADCOL + IDDESO ) = IDATOS
C     ------------------------------------------------------------------
        NB = NMAX + 1
        IF(STOCKA.EQ.'DISPERSE') THEN
          NB = NMAX
          NOM32 = NOM24L//'$$IADD  '
          CALL JJCREN( NOM32 , ICRE , IRET )
          CALL JJCREC( ICLAOS , IDATOS , 'V' , 'I' , 2*NMAX , IBID )
          ISZON ( JISZON + IADCOL + IDIADD ) = IDATOS
C
          NOM32 = NOM24L//'$$IADM  '
          CALL JJCREN( NOM32 , ICRE , IRET )
          CALL JJCREC( ICLAOS , IDATOS , 'V' , 'I' , 2*NMAX , IBID )
          ISZON ( JISZON + IADCOL + IDIADM ) = IDATOS
C
          NOM32 = NOM24L//'$$MARQ  '
          CALL JJCREN( NOM32 , ICRE , IRET )
          CALL JJCREC( ICLAOS , IDATOS , 'V' , 'I' , 2*NMAX ,IBID )
          ISZON ( JISZON + IADCOL + IDMARQ ) = IDATOS
        ENDIF
C     ------------------------------------------------------------------
        NOMPAR = NOM24L//'$$'
        IF( (LONGLU .NE. 'CONSTANT' .AND. LONGLU .NE. 'VARIABLE')
     &      .OR. LEN(LONGLU) .NE. 8 ) THEN
          CALL U2MESS('F','JEVEUX_02')
          ICL = ICLACO
          IF ( LEN(LONGLU) .GT. 24 ) THEN
             CALL U2MESK('F','JEVEUX_73',1,LONGLU)
          ENDIF
          NOML32 = LONGLU
          NOMPAR = NOML32(1:24)//'&&'
          ICREI = 0
          CALL JJVERN ( NOML32 , ICREI , IRET )
          IF(IRET.EQ.0) THEN
             CALL JJVERN( NOML32 , ICRE , IRET )
             CALL JJCREC( ICLAOS , IDATOS , 'V', 'I', NMAX , IBID)
          ELSE IF(IRET.NE.1) THEN
             CALL U2MESK('F','JEVEUX_73',1,NOML32)
          ELSE
             IF ( ICL .NE. ICLAOS ) THEN
                CALL U2MESK('F','JEVEUX_74',1,NOML32)
             ENDIF
             NBL = LONG (JLONG(ICLAOS) + IDATOS)
             IF ( NBL .LT. NMAX ) THEN
                CALL U2MESK('F','JEVEUX_75',1,NOML32)
             ELSE IF ( TYPE(JTYPE(ICLAOS)+IDATOS) .NE. 'I' ) THEN
                CALL U2MESK('F','JEVEUX_76',1,NOML32)
             ENDIF
             IPGC = -1
             CALL JXVEUO( 'E' , ILONGU , 1 , JLONGU )
             IPGC = IPGCEX
          END IF
C     ------------------------------------------------------------------
        ELSE IF(LONGLU.EQ.'VARIABLE' .AND. LEN(LONGLU).EQ.8) THEN
          NOM32 = NOM24L//'$$LONG  '
          CALL JJCREN( NOM32 , ICRE , IRET )
          CALL JJCREC( ICLAOS , IDATOS , 'V', 'I', NMAX , IBID)
        END IF
        IF ( LONGLU .NE. 'CONSTANT' ) THEN
          ISZON ( JISZON + IADCOL + IDLONG ) = IDATOS
C     ------------------------------------------------------------------
          NOM32 = NOMPAR//'LONO  '
          IF ( NOM32(25:26) .EQ. '&&' ) THEN
             ICREI = 0
             CALL JJCREN ( NOM32 , ICREI , IRET )
             IF ( IRET .EQ. 0 ) THEN
                CALL JJCREN( NOM32 , ICRE , IRET )
                CALL JJCREC(ICLAOS ,IDATOS ,'V' ,'I' ,NB ,IBID)
             ENDIF
             IPGC = -1
             CALL JXVEUO( 'E' , ILONGU , 1 , JLONGU )
             IPGC = IPGCEX
          ELSE
             CALL JJCREN( NOM32 , ICRE , IRET )
             CALL JJCREC(ICLAOS ,IDATOS ,'V' ,'I' ,NB ,IBID)
          ENDIF
          ISZON ( JISZON + IADCOL + IDLONO ) = IDATOS
C     ------------------------------------------------------------------
          NOM32 = NOMPAR//'LUTI  '
          IF ( NOM32(25:26) .EQ. '&&' ) THEN
             ICREI = 0
             CALL JJCREN ( NOM32 , ICREI , IRET )
             IF ( IRET .EQ. 0 ) THEN
                CALL JJCREN( NOM32 , ICRE , IRET )
                CALL JJCREC( ICLAOS, IDATOS, 'V', 'I', NMAX, IBID)
             ENDIF
             IPGC = -1
             CALL JXVEUO( 'E' , ILONGU , 1 , JLONGU )
             IPGC = IPGCEX
          ELSE
             CALL JJCREN( NOM32 , ICRE , IRET )
             CALL JJCREC( ICLAOS, IDATOS, 'V', 'I', NMAX, IBID)
          ENDIF
          ISZON ( JISZON + IADCOL + IDLUTI ) = IDATOS
        ENDIF
C     ------------------------------------------------------------------
        TA = ACCELU(1:2)
        IF ( INDEX('NO $NU $',TA//' $') .EQ. 0 ) THEN
          CALL U2MESK('F','JEVEUX_81',1,TA)
        ELSE
          LA = LEN(ACCELU)
          IF ( LA .GT. 3 ) THEN
            IF ( ACCELU(3:3) .NE. ' ' ) THEN
              CALL U2MESK('F','JEVEUX_82',1,ACCELU)
            ENDIF
            IF ( LA .GT. 28 ) THEN
              CALL U2MESK('F','JEVEUX_83',1,ACCELU)
            ENDIF
            NOML32 = ACCELU(4:MIN(LA,LEN(NOML32)))
          ELSE
            NOML32 = ' '
          ENDIF
          IF  (NOML32.NE.' ') CALL U2MESS('F','JEVEUX_03')
        ENDIF
        IF ( TA .EQ. 'NO' .AND. NOML32 .NE. BL32 ) THEN
          ICL  = ICLACO
          ICREI = 0
          CALL JJVERN ( NOML32 , ICREI , IRET )
          IF ( IRET .EQ. 0 ) THEN
            CALL JJVERN( NOML32 , ICRE , IRET )
            CALL JJCREC( ICLAOS, IDATOS, 'N', 'K8', NMAX, IBID)
          ELSE IF(IRET.NE.1) THEN
             CALL U2MESK('F','JEVEUX_77',1,NOML32)
          ELSE
            IF ( ICL .NE. ICLAOS ) THEN
              CALL U2MESK('F','JEVEUX_78',1,NOML32)
            ENDIF
            NBL = LONG (JLONG ( ICLAOS) + IDATOS )
            IF ( NBL .LT. NMAX ) THEN
               CALL U2MESK('F','JEVEUX_79',1,NOML32)
            ELSE IF ( GENR(JGENR(ICLAOS)+IDATOS) .NE. 'N' ) THEN
               CALL U2MESK('F','JEVEUX_80',1,NOML32)
            END IF
            IPGC = -1
            CALL JXVEUO( 'E' , KNOM , 1 , JNOM )
            IF ( IADM(JIADM(ICLAOS)+2*IDATOS-1) .EQ. 0 ) THEN
              KNOM(JNOM + 4 - 1 ) = 0
              KNOM(JNOM + 5 - 1 ) = 0
            END IF
            IPGC = IPGCEX
          END IF
          ISZON ( JISZON + IADCOL + IDNOM ) = IDATOS
        ELSE IF( TA .EQ. 'NO' ) THEN
          NOM32 = NOM24L//'$$NOM   '
          CALL JJCREN( NOM32 , ICRE , IRET )
          CALL JJCREC( ICLAOS, IDATOS, 'N', 'K8', NMAX, IBID)
          ISZON ( JISZON + IADCOL + IDNOM ) = IDATOS
        ELSE IF ( TA .EQ. 'NU' ) THEN
          NOM32 = NOM24L//'$$NUM   '
          CALL JJCREN( NOM32 , ICRE , IRET )
          CALL JJCREC( ICLAOS, IDATOS, 'V', 'I', 2 , IADNUM)
          ISZON ( JISZON + IADNUM ) = NMAX
          ISZON ( JISZON + IADCOL + IDNUM ) = IDATOS
        END IF
      END IF
C FIN ------------------------------------------------------------------
      END
