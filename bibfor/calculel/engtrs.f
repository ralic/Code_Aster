      SUBROUTINE ENGTRS ( IFIC, NOMSD, TYPTES, PRECI, FORMR )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER      IFIC
      CHARACTER*8  TYPTES
      CHARACTER*10 PRECI, FORMR
      CHARACTER*19 NOMSD
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
C                TRAITEMENT DES SD RESULTAT
C
C IN  : IFIC   : NUMERO D'UNITE IMPRESSION
C IN  : NOMSD : NOM D'UNE SD RESULTAT
C IN  : TYPTES : TYPE DU TEST = SOMM_ABS, SOMM
C IN  : PRECI  : PRECISION POUR LE TEST_RESU
C ----------------------------------------------------------------------
C
      INTEGER      IBID, NBORDT, VALI, JORDR, NBNOSY, ISY, IATACH, LG,
     +             LXLGUT, I, J, IORD, IRET, JVALE, LONG,
     +             LG1, LG2, ISMAEM
      REAL*8       R8B, VALR, R8PREM, R8MAEM
      COMPLEX*16   C16B
      CHARACTER*3  TYPE
      CHARACTER*8  K8B
      CHARACTER*16 NOMSYM
      CHARACTER*19 CHEXTR
      CHARACTER*90 FORM1, FORM2, FORM3
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
C --- NUMEROS D'ORDRE
C
      CALL RSORAC ( NOMSD, 'LONUTI', IBID, R8B, K8B, C16B, R8B, K8B,
     &              NBORDT, 1, IBID )
      CALL WKVECT ( '&&ENGTRS.NUME_ORDRE', 'V V I', NBORDT, JORDR )
      CALL RSORAC ( NOMSD, 'TOUT_ORDRE', IBID,R8B,K8B,C16B,R8B,K8B,
     &                                           ZI(JORDR),NBORDT,IBID)
C
C --- NOMS SYMBOLIQUES
C
      CALL JELIRA ( NOMSD//'.DESC', 'NOMMAX', NBNOSY, K8B )
      DO 100 ISY = 1,NBNOSY
         CALL JENUNO ( JEXNUM(NOMSD//'.DESC',ISY), NOMSYM )
         CALL JENONU ( JEXNOM(NOMSD//'.DESC',NOMSYM),IBID)
         CALL JEVEUO ( JEXNUM(NOMSD//'.TACH',IBID),'L',IATACH)
         LG = LXLGUT( NOMSYM )
C
         FORM3 = '(''          _F( RESULTAT= '',A8,'', NOM_CHAM= '''''//
     +           NOMSYM(1:LG)//''''', NUME_ORDRE= '',I6,'','')'
C
         DO 110 J = 1 , NBORDT
            IORD = ZI(JORDR+J-1)
            IF ( ZK24(IATACH-1+J)(1:1) .NE. ' ' ) THEN
               CALL RSEXCH ( NOMSD, NOMSYM, IORD, CHEXTR, IBID )

               CALL JEEXIN ( CHEXTR//'.VALE', IRET )
               IF ( IRET .NE. 0 ) THEN
                  CALL JEVEUO ( CHEXTR//'.VALE', 'L', JVALE )
                  CALL JELIRA ( CHEXTR//'.VALE', 'LONMAX', LONG, K8B )
                  IF ( LONG .EQ. 0 ) GOTO 110
                  CALL JELIRA ( CHEXTR//'.VALE', 'TYPE', IBID, TYPE )
                  GOTO 120
               ENDIF
               CALL JEEXIN ( CHEXTR//'.CELV', IRET )
               IF ( IRET .NE. 0 ) THEN
                  CALL JEVEUO ( CHEXTR//'.CELV', 'L', JVALE )
                  CALL JELIRA ( CHEXTR//'.CELV', 'LONMAX', LONG, K8B )
                  IF ( LONG .EQ. 0 ) GOTO 110
                  CALL JELIRA ( CHEXTR//'.CELV', 'TYPE', IBID, TYPE )
                  GOTO 120
               ENDIF
               GOTO 110
 120           CONTINUE
C
               WRITE(IFIC,FORM3) NOMSD(1:8), IORD
               WRITE(IFIC,1020) PRECI
C
               IF ( TYPE .EQ. 'I' ) THEN
                  IF ( TYPTES .EQ. 'SOMM_ABS' ) THEN
                     VALI = 0
                     DO 130 I = 1 , LONG
                        VALI = VALI + ABS(ZI(JVALE+I-1))
 130                 CONTINUE
                  ELSEIF ( TYPTES .EQ. 'SOMM' ) THEN
                     VALI = 0
                     DO 132 I = 1 , LONG
                        VALI = VALI + ZI(JVALE+I-1)
 132                 CONTINUE
                  ELSEIF ( TYPTES .EQ. 'MAX' ) THEN
                     VALI = -ISMAEM()
                     DO 134 I = 1 , LONG
                        VALI = MAX( VALI , ZI(JVALE+I-1) )
 134                 CONTINUE
                  ELSEIF ( TYPTES .EQ. 'MIN' ) THEN
                     VALI = ISMAEM()
                     DO 136 I = 1 , LONG
                        VALI = MIN( VALI , ZI(JVALE+I-1) )
 136                 CONTINUE
                   ENDIF
                   IF ( VALI .EQ. 0 ) WRITE(IFIC,1010)
                   WRITE(IFIC,FORM2) VALI
C
               ELSEIF ( TYPE .EQ. 'R' ) THEN
                  IF ( TYPTES .EQ. 'SOMM_ABS' ) THEN
                     VALR = 0.D0
                     DO 140 I = 1 , LONG
                        VALR = VALR + ABS(ZR(JVALE+I-1))
 140                 CONTINUE
                  ELSEIF ( TYPTES .EQ. 'SOMM' ) THEN
                     VALR = 0.D0
                     DO 142 I = 1 , LONG
                        VALR = VALR + ZR(JVALE+I-1)
 142                 CONTINUE
                  ELSEIF ( TYPTES .EQ. 'MAX' ) THEN
                     VALR = -R8MAEM()
                     DO 144 I = 1 , LONG
                        VALR = MAX( VALR , ZR(JVALE+I-1) )
 144                 CONTINUE
                  ELSEIF ( TYPTES .EQ. 'MIN' ) THEN
                     VALR = R8MAEM()
                     DO 146 I = 1 , LONG
                        VALR = MIN( VALR , ZR(JVALE+I-1) )
 146                 CONTINUE
                  ENDIF
                  IF ( ABS(VALR) .LE. R8PREM() ) WRITE(IFIC,1010)
                  WRITE(IFIC,FORM1) VALR
               ENDIF
C
            ENDIF
 110     CONTINUE
 100  CONTINUE
C
      WRITE(IFIC,1030)
C
      CALL JEDETR ( '&&ENGTRS.NUME_ORDRE' )
C
      CALL JEDEMA()
C
 1000 FORMAT ( 'TEST_RESU(RESU=( ' )
 1010 FORMAT ('              CRITERE= ''ABSOLU'', ')
 1020 FORMAT ('              REFERENCE= ''NON_REGRESSION'', ',
     +                      'PRECISION= ',A10,',')
 1030 FORMAT ( '          ),)' )
C
      END
