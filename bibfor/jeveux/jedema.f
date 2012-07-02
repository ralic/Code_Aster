      SUBROUTINE JEDEMA
C TOLE CRP_18 CRS_508
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
C ----------------------------------------------------------------------
C DECREMENTE LA MARQUE N ET LIBERE LES OBJETS MARQUES PAR N
C ON OPERE EN DEUX TEMPS : -1 LIBERATION DES COLLECTION PAR JJLIDE
C                          -2 LIBERATION DES OBJETS SIMPLES
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C-----------------------------------------------------------------------
      INTEGER IADYN ,JCARA ,JDATE ,JDOCU ,JGENR ,JHCOD ,JIADD 
      INTEGER JIADM ,JLONG ,JLONO ,JLTYP ,JLUTI ,JMARQ ,JORIG 
      INTEGER JRNOM ,JTYPE ,N 
C-----------------------------------------------------------------------
      PARAMETER      ( N = 5 )
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
C
      INTEGER          IPGC,KDESMA(2),LGD,LGDUTI,KPOSMA(2),LGP,LGPUTI
      COMMON /IADMJE/  IPGC,KDESMA,   LGD,LGDUTI,KPOSMA,   LGP,LGPUTI
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      CHARACTER *24                     NOMCO
      CHARACTER *32    NOMUTI , NOMOS ,         NOMOC , BL32
      COMMON /NOMCJE/  NOMUTI , NOMOS , NOMCO , NOMOC , BL32
      INTEGER          LUNDEF,IDEBUG
      COMMON /UNDFJE/  LUNDEF,IDEBUG
      REAL *8          SVUSE,SMXUSE
      COMMON /STATJE/  SVUSE,SMXUSE
C ----------------------------------------------------------------------
      INTEGER          K,IADMI,IDEB,IFIN,IDOS,IDCO,IC,IS
      CHARACTER *8     KSUF
      CHARACTER *24    D24
      DATA             D24 /'$$$$$$$$$$$$$$$$$$$$$$$$'/
C DEB ------------------------------------------------------------------
      IF (IPGC .EQ. 0 ) THEN
         CALL U2MESS('F','JEVEUX_06')
      ELSE
        IDEB = ISZON(JISZON+KPOSMA(1)+IPGC-1)
      ENDIF
      IFIN = LGDUTI-1
C
C --- ON TRAITE D'ABORD LES COLLECTIONS
C
      DO 100 K=IDEB,IFIN
        IADMI = ISZON(JISZON+KDESMA(1)+K)
        IF (IADMI .NE. 0 ) THEN
          IDOS  = ISZON(JISZON+IADMI-2)
          IS    = ISZON(JISZON+IADMI-4)
          IDCO  = ISZON(JISZON+IS-3)
          IC    = ISZON(JISZON+IS-2)
          IF (IDCO .GT.0 ) THEN
            ICLACO = IC
            IDATCO = IDCO
            NOMCO  = D24
            CALL JJLIDE ('JELIBE',RNOM(JRNOM(IC)+IDCO),2)
          ELSE IF (IDOS .GT. 0 ) THEN
            IF (GENR(JGENR(IC)+IDOS).EQ.'X' ) THEN
              ICLACO = IC
              IDATCO = IDOS
              NOMCO  = D24
              CALL JJLIDE ('JELIBE',RNOM(JRNOM(IC)+IDOS)(1:24),2)
            ENDIF
          ENDIF
        ENDIF
 100  CONTINUE
C
C --- ON TRAITE MAINTENANT LES OBJETS SIMPLES
C --- ET LE $$DESO DES COLLECTIONS CONTIGUES
C
      DO 200 K=IDEB,IFIN
        IADMI = ISZON(JISZON+KDESMA(1)+K)
        IF (IADMI .NE. 0 ) THEN
          IDOS  = ISZON(JISZON+IADMI-2)
          IS    = ISZON(JISZON+IADMI-4)
          IDCO  = ISZON(JISZON+IS-3)
          IC    = ISZON(JISZON+IS-2)
          IF ( IDCO .GT. 0 ) GOTO 200
          IF ( IDOS .GT. 0 ) THEN
          KSUF = RNOM(JRNOM(IC)+IDOS)(25:32)
            IF ( (KSUF(1:2).EQ.'$$' .AND. KSUF(3:6) .NE. 'DESO') .OR.
     &            KSUF(1:2).EQ.'&&' .OR.
     &            GENR(JGENR(IC)+IDOS).EQ.'X' ) GOTO 200
            IF (IDEBUG .EQ. 1) THEN
              ICLAOS = IC
              IDATOS = IDOS
              NOMOS  = D24
              CALL JJLIDE ('JELIBE',RNOM(JRNOM(IC)+IDOS)(1:24),1)
            ELSE
              IADYN = IADM(JIADM(IC)+2*IDOS)
              IMARQ ( JMARQ(IC)+2*IDOS-1 ) = 0
              IMARQ ( JMARQ(IC)+2*IDOS   ) = 0
              ISZON(JISZON+IADMI-1 ) = ISTAT(1)
              ISZON(JISZON+KDESMA(1)+K) = 0
              SVUSE = SVUSE - (ISZON(JISZON+IADMI-4) - IADMI+4)
              IF (IADYN .NE. 0) SVUSE = SVUSE - 1
              SMXUSE = MAX(SMXUSE,SVUSE)
            ENDIF
          ENDIF
        ENDIF
 200  CONTINUE
      LGPUTI = LGPUTI - 1
      LGDUTI = IDEB
      ISZON(JISZON+KPOSMA(1)+IPGC-1) = 0
      IPGC = IPGC - 1
C FIN ------------------------------------------------------------------
      END
