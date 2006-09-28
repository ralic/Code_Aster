      SUBROUTINE JENUNO ( NOMLU , NOMO )
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
C TOLE CFT_726 CFT_720 CRP_18 CRS_508  CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER *(*)      NOMLU , NOMO
C ----------------------------------------------------------------------
C RENVOIE LE NOM ASSOCIE A UN IDENTIFICATEUR
C
C IN  NOMLU  : NOM DE LA COLLECTION OU DU REPERTOIRE
C              L' APPEL DOIT S'EFFECTUER PAR L'INTERMEDIAIRE DE JEXNUM
C IN  NOMO   : NOM DANS LE REPERTOIRE
C
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C ----------------------------------------------------------------------
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
C ----------------------------------------------------------------------
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
C ----------------------------------------------------------------------
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      INTEGER          IPGC, KDESMA, LGD, LGDUTI, KPOSMA, LGP, LGPUTI
      COMMON /IADMJE/  IPGC, KDESMA, LGD, LGDUTI, KPOSMA, LGP, LGPUTI
      INTEGER          NUMEC
      COMMON /INUMJE/  NUMEC
C ----------------------------------------------------------------------
      INTEGER          ILOREP , IDENO , ILNOM , ILMAX , ILUTI , IDEHC
      PARAMETER      ( ILOREP=1,IDENO=2,ILNOM=3,ILMAX=4,ILUTI=5,IDEHC=6)
C ----------------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     ,IDIADD     , IDIADM     ,
     &               IDMARQ     , IDNOM      ,IDREEL     , IDLONG     ,
     &               IDLONO     , IDLUTI     ,IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 ,IDIADD = 2 , IDIADM = 3 ,
     &               IDMARQ = 4 , IDNOM  = 5 ,IDREEL = 6 , IDLONG = 7 ,
     &               IDLONO = 8 , IDLUTI = 9 ,IDNUM  = 10 )
C ----------------------------------------------------------------------
      CHARACTER *75    CMESS
      CHARACTER *32    NOML32
      CHARACTER *6     CNUMO , CLUTI
      CHARACTER *1     GENRI
      INTEGER          ICRE , IRET, ITAB
      CHARACTER *8     NUME
      DATA             NUME  / '$$XNUM  ' /
C DEB ------------------------------------------------------------------
      IPGCEX = IPGC
      IPGC = -2
C
      ICRE = 0
      NOML32 = NOMLU
      CALL JJVERN ( NOML32 , ICRE , IRET )
C
      IF ( IRET .EQ. 0 ) THEN
        CMESS = 'NOM DE COLLECTION OU DE REPERTOIRE INEXISTANT'
        CALL U2MESK('S','JEVEUX_01',1,CMESS)
      ELSE
        IF ( IRET .EQ. 1 ) THEN
C
C ------- OBJET DE TYPE REPERTOIRE
C
          GENRI = GENR ( JGENR(ICLAOS) + IDATOS )
          IF ( GENRI .NE. 'N' ) THEN
            CMESS = 'INTERROGATION SUR UN OBJET NON REPERTOIRE'
            CALL U2MESK('S','JEVEUX_01',1,CMESS)
          ENDIF
          LUTII = LUTI ( JLUTI(ICLAOS) + IDATOS )
          IF ( LUTII .LT. NUMEC .OR. NUMEC .LE. 0 ) THEN
            WRITE ( CLUTI , '(I6)' ) LUTII
            WRITE ( CNUMO , '(I6)' ) NUMEC
            CMESS = 'REPERTOIRE DE '//CLUTI//' NOMS NE CONTIENT PAS '
     &              //CNUMO
            CALL U2MESK('S','JEVEUX_01',1,CMESS)
          ENDIF
          IADMI  = IADM ( JIADM(ICLAOS) + IDATOS )
          IADMEX = IADMI
          IF ( IADMEX .EQ. 0 ) THEN
            CALL JXVEUO ( 'L' , ITAB , IRET , JCTAB )
            IADMI  = IADM ( JIADM(ICLAOS) + IDATOS )
          ENDIF
          KADM   = IADMI
          IDENOM = ISZON ( JISZON + KADM - 1 + IDENO )
          LNOM   = ISZON ( JISZON + KADM - 1 + ILNOM )
          IDECO = (KADM - 1) * LOIS + IDENOM + LNOM * (NUMEC - 1)
          NK = MIN ( LEN(NOMO) , LNOM )
          DO 10 K = 1 , NK
            NOMO(K:K) = K1ZON ( JK1ZON + IDECO + K )
   10     CONTINUE
          IF ( IADMEX .EQ. 0 ) THEN
            CALL JJLIDE ( 'JENUNO' , NOML32 , IRET )
          ENDIF
        ELSE IF ( IRET .EQ. 2 ) THEN
C
C ------- REPERTOIRE DE COLLECTION
C
          CALL JJALLC ( ICLACO , IDATCO , 'L' , IBACOL )
          IXNOM = ISZON ( JISZON + IBACOL + IDNOM )
          IF ( IXNOM .EQ. 0 ) THEN
            CMESS = 'INTERROGATION SUR UNE COLLECTION NON NOMMEE'
            CALL U2MESK('S','JEVEUX_01',1,CMESS)
          ENDIF
          LUTII = LUTI ( JLUTI(ICLACO) + IXNOM )
          IF ( LUTII .LT. NUMEC .OR. NUMEC .LE. 0) THEN
            WRITE ( CLUTI , '(I6)' ) LUTII
            WRITE ( CNUMO , '(I6)' ) NUMEC
            CMESS = 'REPERTOIRE DE '//CLUTI//' NOMS NE CONTIENT PAS '
     &              //CNUMO
            CALL U2MESK('S','JEVEUX_01',1,CMESS)
          ENDIF
          IADMI  = IADM ( JIADM(ICLACO) + IXNOM )
          KADM   = IADMI
          IDENOM = ISZON ( JISZON + KADM - 1 + IDENO )
          LNOM   = ISZON ( JISZON + KADM - 1 + ILNOM )
          IDECO = (KADM - 1) * LOIS + IDENOM + LNOM*(NUMEC - 1)
          NOMO = ' '
          DO 20 K = 1 , MIN ( LEN(NOMO) , LNOM )
            NOMO(K:K) = K1ZON ( JK1ZON + IDECO + K )
   20     CONTINUE
          CALL JJLIDE ( 'JENUNO' , NOMLU(1:24) , 2 )
        ELSE
          CMESS = 'ERREUR DE PROGRAMMATION'
          CALL U2MESK('S','JEVEUX_01',1,CMESS)
        ENDIF
      ENDIF
      IPGC = IPGCEX
C FIN ------------------------------------------------------------------
      END
