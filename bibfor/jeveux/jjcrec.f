      SUBROUTINE JJCREC ( ICL , IDA , GENRI , TYPEI , NB , IADMI)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 19/02/2008   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C TOLE CFT_726 CFT_720 CRP_18 CRS_508
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             ICL , IDA                 , NB , IADMI
      CHARACTER*(*)                   GENRI , TYPEI
C ----------------------------------------------------------------------
C CREATION D'UN OBJET SIMPLE ATTRIBUT COMPOSANT UNE COLLECTION
C
C IN  ICL   : CLASSE ASSOCIEE
C IN  IDA   : IDENTIFICATEUR
C IN  GENRI : GENRE DE L'OS
C IN  TYPEI : TYPE DE L'OS
C IN  NB    : LONGEUR EN MOTS DU SEGMENT DE VALEUR ASSOCIE
C OUT IADMI : ADRESSE DU SEGMENT DE VALEUR DANS ISZON
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C
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
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
C     ------------------------------------------------------------------
      CHARACTER*75     CMESS
      CHARACTER*4      IFMT
      INTEGER          ILOREP , IDENO , ILNOM , ILMAX , ILUTI , IDEHC
      PARAMETER      ( ILOREP=1,IDENO=2,ILNOM=3,ILMAX=4,ILUTI=5,IDEHC=6)
C DEB ------------------------------------------------------------------
      GENR(JGENR (ICL) + IDA ) = GENRI(1:1)
      TYPE(JTYPE (ICL) + IDA ) = TYPEI(1:1)
      IF ( GENRI .EQ. 'N' .AND. TYPEI(1:1) .NE. 'K' ) THEN
        CMESS = 'UN OBJET REPERTOIRE DOIT ETRE DE TYPE K'
        CALL U2MESK('F','JEVEUX_01',1,CMESS)
      ENDIF
      IF ( TYPEI(1:1) .EQ. 'K' ) THEN
        L = LEN(TYPEI)
        IF ( L .EQ. 1 ) THEN
          CMESS  = ' LTYP D''UN OBJET DE TYPE K NON DEFINI'
          CALL U2MESK('F','JEVEUX_01',1,CMESS)
        ENDIF
        WRITE(IFMT,'(''(I'',I1,'')'')') L - 1
        READ ( TYPEI(2:L) , IFMT ) IV
        IF ( IV .LE. 0 .OR. IV .GT. 512 ) THEN
          CMESS = 'LTYP D'' OBJET DE TYPE K INVALIDE >'//TYPEI(2:)
          CALL U2MESK('F','JEVEUX_01',1,CMESS)
        ENDIF
        IF ( GENRI .EQ. 'N' ) THEN
          IF ( MOD ( IV , LOIS ) .NE. 0 ) THEN
            CMESS = 'LTYP D'' OBJET REPERTOIRE NON MULTIPLE DE K8'
            CALL U2MESK('F','JEVEUX_01',1,CMESS)
          ENDIF
          IF ( IV .GT. 24 ) THEN
            CMESS = 'LTYP D''OBJET REPERTOIRE > 24'
            CALL U2MESK('F','JEVEUX_01',1,CMESS)
          ENDIF
        ENDIF
      ELSE IF ( TYPEI(1:1) .EQ. 'S' ) THEN
        IV = LOR8/2
      ELSE IF ( TYPEI(1:1) .EQ. 'I' ) THEN
        IV = LOIS
      ELSE IF ( TYPEI(1:1) .EQ. 'R' ) THEN
        IV = LOR8
      ELSE IF ( TYPEI(1:1) .EQ. 'C' ) THEN
        IV = LOC8
      ELSE IF ( TYPEI(1:1) .EQ. 'L' ) THEN
        IV = LOLS
      ELSE
        CMESS = 'TYPE D''OBJET DE REFERENCE INCORRECT'
        CALL U2MESK('F','JEVEUX_01',1,CMESS)
      ENDIF
      LTYP(JLTYP (ICL) + IDA ) = IV
      IADM(JIADM (ICL) + 2*IDA-1 ) = 0
      IADM(JIADM (ICL) + 2*IDA   ) = 0
      IF ( NB .GT. 0 ) THEN
        IF ( GENRI .EQ. 'N' ) THEN
          LONG ( JLONG(ICL) + IDA ) = NB
          LONOI = (IDEHC + JJPREM(NB)) * LOIS + (NB+1) * IV
          IF ( MOD(LONOI,IV) .GT. 0 ) THEN
            LONO ( JLONO(ICL) + IDA ) = (LONOI / IV) + 1
          ELSE
            LONO ( JLONO(ICL) + IDA ) = (LONOI / IV)
          ENDIF
        ELSE IF ( TYPEI(1:1) .EQ. 'C' ) THEN
          LONG(JLONG (ICL) + IDA ) = 2 * NB
          LONO(JLONO (ICL) + IDA ) = 2 * NB
        ELSE
          LONG(JLONG (ICL) + IDA ) = NB
          LONO(JLONO (ICL) + IDA ) = NB
        ENDIF
        NBL = LONO(JLONO (ICL) + IDA ) * IV
        CALL JJALLT(NBL,ICL,GENRI,TYPEI,IV,'INIT',IADMI,IADYN)
        IADM(JIADM (ICL) + 2*IDA-1 ) = IADMI
        IADM(JIADM (ICL) + 2*IDA   ) = IADYN
        CALL JJECRS (IADMI,ICL,IDA,0,'E',IMARQ(JMARQ(ICL)+2*IDA-1) )
        IF ( GENRI .EQ. 'N' ) THEN
          NHC = JJPREM(NB)
          ISZON(JISZON + IADMI - 1 + ILOREP ) = NHC
          ISZON(JISZON + IADMI - 1 + IDENO  ) = (IDEHC+NHC)*LOIS
          ISZON(JISZON + IADMI - 1 + ILNOM  ) = IV
          ISZON(JISZON + IADMI - 1 + ILMAX  ) = NB
          ISZON(JISZON + IADMI - 1 + ILUTI  ) = 0
          ISZON(JISZON + IADMI - 1 + IDEHC  ) = IDEHC
        ENDIF
      ELSE IF ( GENRI .EQ. 'E' ) THEN
        LONG(JLONG (ICL) + IDA ) = 1
      ENDIF
C FIN ------------------------------------------------------------------
      END
