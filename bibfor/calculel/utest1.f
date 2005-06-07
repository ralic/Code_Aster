      SUBROUTINE UTEST1 ( CHAMGD, TYPTES, TYPRES, REFI, REFR, REFC,
     +                                            EPSI, CRIT, IFIC )
      IMPLICIT   NONE
      INTEGER              REFI, IFIC
      REAL*8               REFR, EPSI
      CHARACTER*8          TYPTES
      CHARACTER*(*)        CHAMGD, TYPRES, CRIT
      COMPLEX*16           REFC
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 01/02/2000   AUTEUR VABHHTS J.PELLET 
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
C ----------------------------------------------------------------------
C IN  : CHAMGD : NOM DU CHAM_GD
C IN  : TYPTES : TYPE DE TEST A EFFECTUER SUR LE CHAMP
C IN  : REFI   : VALEUR REELLE ENTIERE ATTENDUE
C IN  : REFR   : VALEUR REELLE ATTENDUE
C IN  : REFC   : VALEUR COMPLEXE ATTENDUE
C IN  : CRIT   : 'RELATIF' OU 'ABSOLU'(PRECISION RELATIVE OU ABSOLUE).
C IN  : EPSI   : PRECISION ESPEREE
C IN  : IFIC   : NUMERO LOGIQUE DU FICHIER DE SORTIE
C OUT : IMPRESSION SUR LISTING
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER       VALI, JVALE, IBID, NEQ, I,IRET1,IRET2
      REAL*8        VALR
      COMPLEX*16    VALC
      CHARACTER*1   TYPREZ
      CHARACTER*4   TYPE
      CHARACTER*5   SUFV
      CHARACTER*19  CHAM19
      CHARACTER*17  LABEL1, LABEL2
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      LABEL1 = ' '
      LABEL2 = ' '
      CHAM19 = CHAMGD
      TYPREZ = TYPRES(1:1)


C     -- LE CHAMP EXISTE-T-IL ?
C     =========================
      CALL JEEXIN ( CHAM19//'.VALE' , IRET1)
      IF (IRET1.GT.0) THEN
        SUFV='.VALE'
      ELSE
        CALL JEEXIN ( CHAM19//'.CELV' , IRET2)
        IF (IRET2.GT.0) THEN
          SUFV='.CELV'
        ELSE
          WRITE(IFIC,*) 'NOOK '
          GOTO 9999
        END IF
      END IF


      CALL JELIRA ( CHAM19//SUFV ,'TYPE', IBID, TYPE )
      IF ( TYPE(1:1) .NE. TYPREZ ) THEN
         WRITE(IFIC,*) 'NOOK '
         CALL UTMESS('A','UTEST1','LE CHAMP '//CHAM19//' EST A VALEURS '
     +                       //'DE TYPE  "'//TYPE//'"  ET LA VALEUR '
     +                       //'DE REFERENCE DE TYPE  "'//TYPREZ//'".')
         GOTO 9999
      ENDIF
C
      CALL JELIRA ( CHAM19//SUFV ,'LONMAX', NEQ, TYPE )
      CALL JEVEUO ( CHAM19//SUFV , 'L', JVALE )
  
  
      IF ( TYPE .EQ. 'I' ) THEN
         IF ( TYPTES .EQ. 'SOMM_ABS' ) THEN
            LABEL2 = ' SOMM_ABS '
            VALI = 0
            DO 100 I = 1 , NEQ
               VALI = VALI + ABS( ZI(JVALE+I-1) )
 100        CONTINUE
         ELSEIF ( TYPTES .EQ. 'SOMM' ) THEN
            LABEL2 = ' SOMM '
            VALI = 0
            DO 102 I = 1 , NEQ
               VALI = VALI + ZI(JVALE+I-1)
 102        CONTINUE
         ELSEIF ( TYPTES .EQ. 'MAX' ) THEN
            LABEL2 = ' MAX '
            VALI = ZI(JVALE-1+1)
            DO 104 I = 2 , NEQ
               VALI = MAX( VALI , ZI(JVALE+I-1) )
 104        CONTINUE
         ELSEIF ( TYPTES .EQ. 'MIN' ) THEN
            LABEL2 = ' MIN '
            VALI = ZI(JVALE-1+1)
            DO 106 I = 2 , NEQ
               VALI = MIN( VALI , ZI(JVALE+I-1) )
 106        CONTINUE
         ELSE
            WRITE(IFIC,*) 'NOOK '
            CALL UTMESS('A','UTEST1','"TYPE_TEST" INCONNU')
            GOTO 9999
         ENDIF


      ELSEIF ( TYPE .EQ. 'R' ) THEN
         IF ( TYPTES .EQ. 'SOMM_ABS' ) THEN
            LABEL2 = ' SOMM_ABS '
            VALR = 0.D0
            DO 200 I = 1 , NEQ
               VALR = VALR + ABS( ZR(JVALE+I-1) )
 200        CONTINUE
         ELSEIF ( TYPTES .EQ. 'SOMM' ) THEN
            LABEL2 = ' SOMM '
            VALR = 0.D0
            DO 202 I = 1 , NEQ
               VALR = VALR + ZR(JVALE+I-1)
 202        CONTINUE
         ELSEIF ( TYPTES .EQ. 'MAX' ) THEN
            VALR = ZR(JVALE)
            LABEL2 = ' MAX '
            DO 204 I = 2 , NEQ
               VALR = MAX( VALR , ZR(JVALE+I-1) )
 204        CONTINUE
         ELSEIF ( TYPTES .EQ. 'MIN' ) THEN
            VALR = ZR(JVALE)
            LABEL2 = ' MIN '
            DO 206 I = 2 , NEQ
               VALR = MIN( VALR , ZR(JVALE+I-1) )
 206        CONTINUE
         ELSE
            WRITE(IFIC,*) 'NOOK '
            CALL UTMESS('A','UTEST1','"TYPE_TEST" INCONNU')
            GOTO 9999
         ENDIF


      ELSEIF ( TYPE .EQ. 'C' ) THEN
         IF ( TYPTES .EQ. 'SOMM_ABS' ) THEN
            LABEL2 = ' SOMM_ABS '
            VALR = 0.D0
            DO 300 I = 1 , NEQ
               VALR = VALR + ABS( ZC(JVALE+I-1) )
 300        CONTINUE
         ELSEIF ( TYPTES .EQ. 'SOMM' ) THEN
            LABEL2 = ' SOMM '
            VALC = DCMPLX(0.D0,0.D0)
            DO 302 I = 1 , NEQ
               VALC = VALC + ZC(JVALE+I-1)
 302        CONTINUE
         ELSE
            WRITE(IFIC,*) 'NOOK '
            CALL UTMESS('A','UTEST1','"TYPE_TEST" INCONNU')
            GOTO 9999
         ENDIF
      ENDIF
C
      CALL UTITES ( LABEL1, LABEL2, TYPRES, REFI, REFR, REFC,
     +                         VALI, VALR, VALC, EPSI, CRIT, IFIC )
C
 9999 CONTINUE
C
      CALL JEDEMA()
      END
