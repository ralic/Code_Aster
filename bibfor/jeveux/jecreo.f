      SUBROUTINE JECREO ( NOMLU , LISTAT )
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER *(*)      NOMLU , LISTAT
C     ==================================================================
      PARAMETER  ( N = 5 )
C
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     &                 DN2(N)
C     ------------------------------------------------------------------
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
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
C     ------------------------------------------------------------------
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
C     ------------------------------------------------------------------
      CHARACTER *1     TYPEI , GENRI
      INTEGER          NV , ICRE , IRET
      PARAMETER      ( NV = 3 )
      INTEGER          LVAL(NV)
      CHARACTER *8     CVAL(NV)
      CHARACTER *32    NOML32
      CHARACTER *4     IFMT
C     ------------------------------------------------------------------
      IF ( LEN(NOMLU) .GT. 24 ) THEN
         CALL U2MESK('F','JEVEUX_84',1,NOMLU)
      ENDIF
      NOML32 = NOMLU(1:MIN(24,LEN(NOMLU)))
C
      CALL JJANAL (  LISTAT  , NV , NV , LVAL , CVAL )
      ICLAS  = INDEX ( CLASSE , CVAL(1)(1:1) )
      IF ( ICLAS .EQ. 0 ) THEN
        CALL U2MESK('F','JEVEUX_68',1,CVAL(1))
      END IF
C
      ICRE = 1
      CALL JJVERN ( NOML32 , ICRE , IRET )
C
      IF ( IRET .EQ. 2 ) THEN
         CALL U2MESK('F','JEVEUX_85',1,NOML32)
      ELSE
         GENR(JGENR(ICLAOS)+IDATOS) = CVAL(2)(1:1)
         TYPE(JTYPE(ICLAOS)+IDATOS) = CVAL(3)(1:1)
         IF ( CVAL(3)(1:1) .EQ. 'K' .AND. LVAL(3) .EQ. 1 ) THEN
           CALL U2MESK('F','JEVEUX_86',1,NOML32)
         ELSE
           GENRI = GENR ( JGENR(ICLAOS) + IDATOS )
           TYPEI = TYPE ( JTYPE(ICLAOS) + IDATOS )
           IF ( GENRI .EQ. 'N' .AND. TYPEI .NE. 'K' ) THEN
             CALL U2MESK('F','JEVEUX_87',1,NOML32)
           ENDIF
           IF      ( TYPEI .EQ. 'K' ) THEN
             WRITE(IFMT,'(''(I'',I1,'')'')') LVAL(3) - 1
             READ ( CVAL(3)(2:LVAL(3)) , IFMT ) IV
             IF ( IV .LE. 0 .OR. IV .GT. 512 ) THEN
               CALL U2MESK('F','JEVEUX_88',1,CVAL(3))
             ENDIF
             IF ( GENRI .EQ. 'N' ) THEN
               IF ( MOD ( IV , LOIS ) .NE. 0 ) THEN
                 CALL U2MESK('F','JEVEUX_89',1,NOML32)
               ENDIF
               IF ( IV .GT. 24 ) THEN
                 CALL U2MESK('F','JEVEUX_90',1,NOML32)
               ENDIF
             ENDIF
           ELSE IF ( TYPEI .EQ. 'I' ) THEN
             IV = LOIS
           ELSE IF ( TYPEI .EQ. 'R' ) THEN
             IV = LOR8
           ELSE IF ( TYPEI .EQ. 'C' ) THEN
             IV = LOC8
           ELSE IF ( TYPEI .EQ. 'L' ) THEN
             IV = LOLS
           ELSE IF ( TYPEI .EQ. 'S' ) THEN
             IV = LOR8/2
           ELSE
             CALL U2MESK('F','JEVEUX_91',1,CVAL(3))
           ENDIF
           LTYP ( JLTYP(ICLAOS) + IDATOS ) = IV
         ENDIF
         IF ( CVAL(2)(1:1) .EQ. 'E' ) THEN
            LONG(JLONG(ICLAOS)+IDATOS) = 1
            LONO(JLONO(ICLAOS)+IDATOS) = 1
         ENDIF
      ENDIF
C
      END
