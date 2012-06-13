      SUBROUTINE ENGTCN ( IFIC, CHAMNO, TYPTES, PRECI, FORMR )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER      IFIC
      CHARACTER*8  TYPTES
      CHARACTER*10 PRECI, FORMR
      CHARACTER*19 CHAMNO
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     COMMANDE:  ENGENDRE_TEST
C                TRAITEMENT DES SD CHAM_NO
C
C IN  : IFIC   : NUMERO D'UNITE IMPRESSION
C IN  : NOMSTR : NOM D'UNE SD RESULTAT
C IN  : TYPTES : TYPE DU TEST = SOMM_ABS, SOMM
C IN  : PRECI  : PRECISION POUR LE TEST_RESU
C IN  : FORMR  : FORMAT D'IMPRESSION DU CHAMP VALE REEL
C ----------------------------------------------------------------------
C
      INTEGER      IBID, VALI, I, JVALE, LONG, LG1, LG2, ISMAEM,
     +             LXLGUT
      REAL*8        VALR, R8PREM, R8MAEM
      CHARACTER*3  TYPE
      CHARACTER*8  K8B
      CHARACTER*80 FORM1, FORM2
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      LG1 = LXLGUT( FORMR  )
      LG2 = LXLGUT( TYPTES )
      FORM1 = '(''              TYPE_TEST= '''''//TYPTES(1:LG2)//
     +          ''''', VALE= '','//FORMR(1:LG1)//','' ),'')'
      FORM2 = '(''              TYPE_TEST= '''''//TYPTES(1:LG2)//
     +          ''''', VALE_I= '',I9,'' ),'')'
C
      WRITE(IFIC,1000)
C
      CALL JEVEUO ( CHAMNO//'.VALE', 'L', JVALE )
      CALL JELIRA ( CHAMNO//'.VALE', 'LONMAX', LONG, K8B )
      CALL JELIRA ( CHAMNO//'.VALE', 'TYPE', IBID, TYPE )
C
      WRITE(IFIC,1010) CHAMNO(1:8)
      WRITE(IFIC,1020) PRECI
C
      IF ( TYPE .EQ. 'I' ) THEN
         IF ( TYPTES .EQ. 'SOMM_ABS' ) THEN
            VALI = 0
            DO 110 I = 1 , LONG
               VALI = VALI + ABS(ZI(JVALE+I-1))
 110        CONTINUE
         ELSEIF ( TYPTES .EQ. 'SOMM' ) THEN
            VALI = 0
            DO 112 I = 1 , LONG
               VALI = VALI + ZI(JVALE+I-1)
 112        CONTINUE
         ELSEIF ( TYPTES .EQ. 'MAX' ) THEN
            VALI = -ISMAEM()
            DO 114 I = 1 , LONG
               VALI = MAX( VALI , ZI(JVALE+I-1) )
 114        CONTINUE
         ELSEIF ( TYPTES .EQ. 'MIN' ) THEN
            VALI = ISMAEM()
            DO 116 I = 1 , LONG
               VALI = MIN( VALI , ZI(JVALE+I-1) )
 116        CONTINUE
         ENDIF
         IF ( VALI .EQ. 0 ) WRITE(IFIC,1022)
         WRITE(IFIC,FORM2) VALI
C
      ELSEIF ( TYPE .EQ. 'R' ) THEN
         IF ( TYPTES .EQ. 'SOMM_ABS' ) THEN
            VALR = 0.D0
            DO 120 I = 1 , LONG
               VALR = VALR + ABS(ZR(JVALE+I-1))
 120        CONTINUE
         ELSEIF ( TYPTES .EQ. 'SOMM' ) THEN
            VALR = 0.D0
            DO 122 I = 1 , LONG
               VALR = VALR + ZR(JVALE+I-1)
 122        CONTINUE
         ELSEIF ( TYPTES .EQ. 'MAX' ) THEN
            VALR = -R8MAEM()
            DO 124 I = 1 , LONG
               VALR = MAX( VALR , ZR(JVALE+I-1) )
 124        CONTINUE
         ELSEIF ( TYPTES .EQ. 'MIN' ) THEN
            VALR = R8MAEM()
            DO 126 I = 1 , LONG
               VALR = MIN( VALR , ZR(JVALE+I-1) )
 126        CONTINUE
         ENDIF
         IF ( ABS(VALR) .LE. R8PREM() ) WRITE(IFIC,1022)
         WRITE(IFIC,FORM1) VALR
      ENDIF
C
      WRITE(IFIC,1030)
C
      CALL JEDEMA()
C
 1000 FORMAT ( 'TEST_RESU(CHAM_NO= ' )
C
 1010 FORMAT ('          _F( CHAM_GD= ',A8,', ' )
C
 1022 FORMAT ('              CRITERE= ''ABSOLU'', ')
 1020 FORMAT ('              REFERENCE= ''NON_REGRESSION'', ',
     +                      'PRECISION= ',A10,',')
C
 1030 FORMAT ( '          )' )
C
      END
