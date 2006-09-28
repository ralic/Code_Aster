      SUBROUTINE JEIMPO ( UNIT , NOMLU , PARM , MESS )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C TOLE CFT_726 CFT_720 CRP_18 CRP_4 CRS_508 CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             UNIT
      CHARACTER *(*)      NOMLU , PARM , MESS
C     ------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C     -----------------------------------------------------------------
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
C     ------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      INTEGER          LTYP    , LONG    , DATE    , IADD    , IADM    ,
     &                 LONO    , HCOD    , CARA    , LUTI    , IMARQ
      COMMON /IATRJE/  LTYP(1) , LONG(1) , DATE(1) , IADD(1) , IADM(1) ,
     &                 LONO(1) , HCOD(1) , CARA(1) , LUTI(1) , IMARQ(1)
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     &                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
C
      CHARACTER*1      GENR    , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8) , TYPE(8) , DOCU(2) , ORIG(1) , RNOM(1)
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
C
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      INTEGER          IPGC, KDESMA, LGD, LGDUTI, KPOSMA, LGP, LGPUTI
      COMMON /IADMJE/  IPGC, KDESMA, LGD, LGDUTI, KPOSMA, LGP, LGPUTI
      INTEGER          NUMEC
      COMMON /INUMJE/  NUMEC
C     ------------------------------------------------------------------
      CHARACTER *75   CMESS
      CHARACTER *32   NOML32
      CHARACTER *1    GENRI , TYPEI
      INTEGER         ICRE , IRET , JCTAB , LTYPI , LONOI , IADDI(2)
      INTEGER         IBACOL , IXIADD , IXDESO
      LOGICAL         LCONST , LCOL
C     ------------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     ,IDIADD     , IDIADM     ,
     &               IDMARQ     , IDNOM      ,IDREEL     , IDLONG     ,
     &               IDLONO     , IDLUTI     ,IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 ,IDIADD = 2 , IDIADM = 3 ,
     &               IDMARQ = 4 , IDNOM  = 5 ,IDREEL = 6 , IDLONG = 7 ,
     &               IDLONO = 8 , IDLUTI = 9 ,IDNUM  = 10 )
C DEB ------------------------------------------------------------------
      IPGCEX = IPGC
      IPGC   = -2
      NOML32 = NOMLU
C
      LCOL = .FALSE.
      ICRE = 0
      CALL JJVERN ( NOML32 , ICRE , IRET )
C
      IF ( IRET .EQ. 0 ) THEN
        CMESS = 'OBJET INEXISTANT DANS LES BASES OUVERTES'
        CALL U2MESK('A','JEVEUX_01',1,CMESS)
        GOTO 9999
      ELSE IF ( IRET .EQ. 1 ) THEN
C
C ----  CAS D'UN OBJET SIMPLE
C
        INAT = 1
        IADMI  = IADM ( JIADM(ICLAOS) + IDATOS )
        IADMEX = IADMI
        GENRI  = GENR ( JGENR(ICLAOS) + IDATOS )
        TYPEI  = TYPE ( JTYPE(ICLAOS) + IDATOS )
        LTYPI  = LTYP ( JLTYP(ICLAOS) + IDATOS )
        LONOI  = LONO ( JLONO(ICLAOS) + IDATOS ) * LTYPI
        IF ( IADMEX .EQ. 0 ) THEN
          IADDI(1) = IADD ( JIADD(ICLAOS) + 2*IDATOS-1 )
          IADDI(2) = IADD ( JIADD(ICLAOS) + 2*IDATOS   )
          IF ( IADDI(1) .EQ. 0 ) THEN
            CMESS = 'OBJET INEXISTANT EN MEMOIRE ET SUR DISQUE'
            CALL U2MESK('A','JEVEUX_01',1,CMESS)
            GOTO 9999
          ENDIF
          CALL JJALTY (TYPEI , LTYPI , 'L' , 1 , JCTAB)
          IADMI = IADM ( JIADM(ICLAOS) + IDATOS )
        ENDIF
        IDECI = 0
        CALL JJIMPO ( UNIT,IADMI, IDECI, 0, GENRI, TYPEI, LTYPI, LONOI,
     &                MESS , PARM)
        IF ( IADMEX .EQ. 0 ) THEN
          CALL JJLIDE ( 'JEIMPO' , NOML32 , INAT )
        ENDIF
      ELSE IF ( IRET .EQ. 2 ) THEN
C
C ----- CAS D'UNE COLLECTION
C
        LCOL = .TRUE.
        CALL JJALLC ( ICLACO , IDATCO , 'L' , IBACOL )
        IF ( NOML32(25:32) .EQ. '        ') THEN
          INAT = 2
        ELSE
          CALL JJCROC ( NOML32(25:32) , ICRE )
          INAT = 3
        ENDIF
      ENDIF
      IF ( INAT .EQ. 2 ) THEN
C
C ----- CAS D'UNE COLLECTION ENTIERE
C
        IXIADD = ISZON ( JISZON + IBACOL + IDIADD )
        IXIADM = ISZON ( JISZON + IBACOL + IDIADM )
        IXDESO = ISZON ( JISZON + IBACOL + IDDESO )
        GENRI  = GENR( JGENR(ICLACO) + IXDESO )
        TYPEI  = TYPE( JTYPE(ICLACO) + IXDESO )
        LTYPI  = LTYP( JLTYP(ICLACO) + IXDESO )
        IF ( IXIADD .EQ. 0 ) THEN
C
C ------- COLLECTION CONTIGUE
C
          IADMI  = IADM ( JIADM(ICLACO) + IXDESO )
          IADDI(1) = IADD ( JIADD(ICLACO) + 2*IXDESO-1 )
          IADDI(2) = IADD ( JIADD(ICLACO) + 2*IXDESO   )
          IADMEX = IADMI
          IF ( IADMEX .EQ. 0 ) THEN
            IF ( IADDI(1) .EQ. 0 ) THEN
              CMESS = 'COLLECTION INEXISTANTE EN MEMOIRE ET SUR DISQUE'
              CALL U2MESK('A','JEVEUX_01',1,CMESS)
              GOTO 9999
            ENDIF
            CALL JJALTY (TYPEI , LTYPI , 'L' , 2 , JCTAB)
            IADMI = IADM ( JIADM(ICLACO) + IXDESO )
          ENDIF
          LONOI  = LONO( JLONO(ICLACO) + IXDESO ) * LTYPI
          IDECI  = 0
          CALL JJIMPO ( UNIT,IADMI, IDECI, -1, GENRI,TYPEI,LTYPI,LONOI,
     &                  MESS , PARM)
          IF ( IADMEX .EQ. 0 ) THEN
            CALL JJLIDE ( 'JEIMPO' , NOML32 , INAT )
          ENDIF
        ELSE
C
C ------- COLLECTION DISPERSEE
C
          NBMAX  = ISZON ( JISZON + IBACOL + IVNMAX )
          IBIADM = IADM ( JIADM(ICLACO) + IXIADM )
          IBIADD = IADM ( JIADM(ICLACO) + IXIADD )
          IDECI  = 0
          DO 10 K = 1,NBMAX
            IADMI = ISZON(JISZON + IBIADM - 1 + K )
            IF ( IADMI .EQ. 0 ) THEN
              IADDI(1) = ISZON(JISZON + IBIADD - 1 + 2*K-1 )
              IADDI(2) = ISZON(JISZON + IBIADD - 1 + 2*K   )
              IF ( IADDI(1) .EQ. 0 ) GOTO 10
              CALL JJALTY (TYPEI , LTYPI , 'L' , 3 , JCTAB)
              IADMI  = ISZON(JISZON + IBIADM - 1 + K )
            ENDIF
            IXLONO = ISZON ( JISZON + IBACOL + IDLONO )
            IF ( IXLONO .EQ. 0 ) THEN
              LONOI = LONO ( JLONO(ICLACO) + IXDESO ) * LTYPI
            ELSE
              IBLONO = IADM ( JIADM(ICLACO) + IXLONO )
              LONOI  = ISZON ( JISZON + IBLONO - 1 + K ) * LTYPI
            ENDIF
            CALL JJIMPO(UNIT,IADMI,IDECI,K,GENRI,TYPEI,LTYPI,
     &                  LONOI,MESS,PARM)
            NUMEC = K
            CALL JJLIDE ('JEIMPO' , NOML32//'$$XNUM  ' , 2)
 10       CONTINUE
        ENDIF
        CALL JJLIDE ( 'JEIMPO' , NOML32 , INAT )
      ELSE IF ( INAT .EQ. 3 ) THEN
C       ------ CAS D'UN OBJET DE COLLECTION  ------
         IXIADD = ISZON ( JISZON + IBACOL + IDIADD )
         IXIADM = ISZON ( JISZON + IBACOL + IDIADM )
         IXDESO = ISZON ( JISZON + IBACOL + IDDESO )
         IXLONO = ISZON ( JISZON + IBACOL + IDLONO )
         GENRI = GENR( JGENR(ICLACO) + IXDESO )
         TYPEI = TYPE( JTYPE(ICLACO) + IXDESO )
         LTYPI = LTYP( JLTYP(ICLACO) + IXDESO )
         IF ( IXIADD .EQ. 0 ) THEN
C           ----------- COLLECTION CONTIGUE
           LCONST = ( ISZON ( JISZON + IBACOL + IDLONG ) .EQ. 0 )
           IBDESO = IADM ( JIADM(ICLACO) + IXDESO )
           IADDI(1)  = IADD ( JIADD(ICLACO) + 2*IXDESO-1 )
           IADDI(2)  = IADD ( JIADD(ICLACO) + 2*IXDESO   )
           IADMEX = IBDESO
           IF ( IADMEX .EQ. 0 ) THEN
             IF ( IADDI(1) .EQ. 0 ) THEN
               CMESS = 'OBJET INEXISTANT EN MEMOIRE ET SUR DISQUE'
               CALL U2MESK('A','JEVEUX_01',1,CMESS)
               GOTO 9999
             ENDIF
             CALL JJALTY (TYPEI , LTYPI , 'L' , 2 , JCTAB)
             IBDESO = IADM ( JIADM(ICLACO) + IXDESO )
           ENDIF
           IF ( LCONST ) THEN
             LONOI = LONO ( JLONO(ICLACO) + IXDESO ) * LTYPI
             LONOI = LONOI / ISZON ( JISZON + IBACOL + IVNMAX )
             IADMI = IBDESO
             IDECI = ( IDATOC - 1 ) * LONOI
           ELSE
             IBLONO = IADM ( JIADM(ICLACO) + IXLONO )
             LONOI = LTYPI * ( ISZON(JISZON+IBLONO-1+IDATOC+1) -
     &                         ISZON(JISZON+IBLONO-1+IDATOC ) )
             IADMI = IBDESO
             IDECI = (LTYPI*(ISZON(JISZON+IBLONO-1+IDATOC)-1))
           ENDIF
           CALL JJIMPO(UNIT,IADMI,IDECI,IDATOC,GENRI,TYPEI,LTYPI,LONOI,
     &                   MESS , PARM)
           IF ( IADMEX .EQ. 0 ) THEN
              CALL JJLIDE ( 'JEIMPO' , NOML32 , INAT )
           ENDIF
         ELSE
C
C -------- COLLECTION DISPERSEE
C
           IBIADM = IADM ( JIADM(ICLACO) + IXIADM )
           IBIADD = IADM ( JIADM(ICLACO) + IXIADD )
           IADMI  = ISZON(JISZON + IBIADM - 1 + IDATOC )
           IADMEX = IADMI
           IDECI  = 0
           IF ( IADMEX .EQ. 0 ) THEN
             IADDI(1) = ISZON(JISZON + IBIADD - 1 + 2*IDATOC-1 )
             IADDI(2) = ISZON(JISZON + IBIADD - 1 + 2*IDATOC   )
             IF ( IADDI(1) .EQ. 0 ) THEN
               CMESS = 'OBJET INEXISTANT EN MEMOIRE ET SUR DISQUE'
               CALL U2MESK('A','JEVEUX_01',1,CMESS)
               GOTO 9999
             ENDIF
             CALL JJALTY (TYPEI , LTYPI , 'L' , INAT , JCTAB)
             IADMI  = ISZON(JISZON + IBIADM - 1 + IDATOC )
           ENDIF
           IXLONO = ISZON ( JISZON + IBACOL + IDLONO )
           IF ( IXLONO .EQ. 0 ) THEN
             LONOI = LONO( JLONO(ICLACO) + IXDESO ) * LTYPI
           ELSE
             IBLONO = IADM ( JIADM(ICLACO) + IXLONO )
             LONOI  = ISZON ( JISZON + IBLONO + IDATOC - 1 ) * LTYPI
           ENDIF
           CALL JJIMPO(UNIT,IADMI,IDECI,IDATOC,GENRI,TYPEI,LTYPI,LONOI,
     &                   MESS , PARM)
           IF ( IADMEX .EQ. 0 ) THEN
             CALL JJLIDE ( 'JEIMPO' , NOML32 , INAT )
           ENDIF
         ENDIF
      ENDIF
 9999 CONTINUE
      IF ( LCOL ) THEN
        CALL JJLIDE ( 'JEIMPO' , NOML32(1:24) , 2 )
      ENDIF
      IPGC = IPGCEX
C FIN ------------------------------------------------------------------
      END
