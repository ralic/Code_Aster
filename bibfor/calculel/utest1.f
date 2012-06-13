      SUBROUTINE UTEST1 ( CHAMGD, TYPTES, TYPRES, NBREF, TBTXT,
     &                    REFI, REFR, REFC, EPSI, CRIT, IFIC, SSIGNE )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER              NBREF,REFI(NBREF), IFIC
      REAL*8               REFR(NBREF), EPSI
      CHARACTER*8          TYPTES
      CHARACTER*16         TBTXT(2)
      CHARACTER*(*)        CHAMGD, TYPRES, CRIT, SSIGNE
      COMPLEX*16           REFC(NBREF)
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C ----------------------------------------------------------------------
C IN  : CHAMGD : NOM DU CHAM_GD
C IN  : TYPTES : TYPE DE TEST A EFFECTUER SUR LE CHAMP
C IN  : NBREF  : NOMBRE DE VALEURS DE REFERENCE
C IN  : TBTXT  : (1)=REFERENCE, (2)=LEGENDE
C IN  : REFI   : VALEUR REELLE ENTIERE ATTENDUE
C IN  : REFR   : VALEUR REELLE ATTENDUE
C IN  : REFC   : VALEUR COMPLEXE ATTENDUE
C IN  : CRIT   : 'RELATIF' OU 'ABSOLU'(PRECISION RELATIVE OU ABSOLUE).
C IN  : EPSI   : PRECISION ESPEREE
C IN  : IFIC   : NUMERO LOGIQUE DU FICHIER DE SORTIE
C OUT : IMPRESSION SUR LISTING
C ----------------------------------------------------------------------
      INTEGER       VALI, JVALE, IBID, NEQ, I,IRET1,IRET2
      REAL*8        VALR
      COMPLEX*16    VALC
      CHARACTER*1   TYPREZ
      CHARACTER*24 VALK(3)
      CHARACTER*4   TYPE
      CHARACTER*5   SUFV
      CHARACTER*19  CHAM19
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
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
          VALK(1) = CHAM19
          VALK(2) = TYPE
          VALK(3) = TYPREZ
          CALL U2MESK('A','CALCULEL5_13', 3 ,VALK)
         GOTO 9999
      ENDIF
C
      CALL JELIRA ( CHAM19//SUFV ,'LONMAX', NEQ, TYPE )
      CALL JEVEUO ( CHAM19//SUFV , 'L', JVALE )
  
  
      IF ( TYPE .EQ. 'I' ) THEN
         IF ( TYPTES .EQ. 'SOMM_ABS' ) THEN
            VALI = 0
            DO 100 I = 1 , NEQ
               VALI = VALI + ABS( ZI(JVALE+I-1) )
 100        CONTINUE
         ELSEIF ( TYPTES .EQ. 'SOMM' ) THEN
            VALI = 0
            DO 102 I = 1 , NEQ
               VALI = VALI + ZI(JVALE+I-1)
 102        CONTINUE
         ELSEIF ( TYPTES .EQ. 'MAX' ) THEN
            VALI = ZI(JVALE-1+1)
            DO 104 I = 2 , NEQ
               VALI = MAX( VALI , ZI(JVALE+I-1) )
 104        CONTINUE
         ELSEIF ( TYPTES .EQ. 'MIN' ) THEN
            VALI = ZI(JVALE-1+1)
            DO 106 I = 2 , NEQ
               VALI = MIN( VALI , ZI(JVALE+I-1) )
 106        CONTINUE
         ELSE
            WRITE(IFIC,*) 'NOOK '
            CALL U2MESS('A','CALCULEL5_12')
            GOTO 9999
         ENDIF


      ELSEIF ( TYPE .EQ. 'R' ) THEN
         IF ( TYPTES .EQ. 'SOMM_ABS' ) THEN
            VALR = 0.D0
            DO 200 I = 1 , NEQ
               VALR = VALR + ABS( ZR(JVALE+I-1) )
 200        CONTINUE
         ELSEIF ( TYPTES .EQ. 'SOMM' ) THEN
            VALR = 0.D0
            DO 202 I = 1 , NEQ
               VALR = VALR + ZR(JVALE+I-1)
 202        CONTINUE
         ELSEIF ( TYPTES .EQ. 'MAX' ) THEN
            VALR = ZR(JVALE)
            DO 204 I = 2 , NEQ
               VALR = MAX( VALR , ZR(JVALE+I-1) )
 204        CONTINUE
         ELSEIF ( TYPTES .EQ. 'MIN' ) THEN
            VALR = ZR(JVALE)
            DO 206 I = 2 , NEQ
               VALR = MIN( VALR , ZR(JVALE+I-1) )
 206        CONTINUE
         ELSE
            WRITE(IFIC,*) 'NOOK '
            CALL U2MESS('A','CALCULEL5_12')
            GOTO 9999
         ENDIF


      ELSEIF ( TYPE .EQ. 'C' ) THEN
         IF ( TYPTES .EQ. 'SOMM_ABS' ) THEN
            VALR = 0.D0
            DO 300 I = 1 , NEQ
               VALR = VALR + ABS( ZC(JVALE+I-1) )
 300        CONTINUE
         ELSEIF ( TYPTES .EQ. 'SOMM' ) THEN
            VALC = DCMPLX(0.D0,0.D0)
            DO 302 I = 1 , NEQ
               VALC = VALC + ZC(JVALE+I-1)
 302        CONTINUE
         ELSE
            WRITE(IFIC,*) 'NOOK '
            CALL U2MESS('A','CALCULEL5_12')
            GOTO 9999
         ENDIF
      ENDIF
C
      CALL UTITES ( TBTXT(1),TBTXT(2), TYPRES, NBREF, REFI, REFR, REFC,
     +                      VALI, VALR, VALC, EPSI, CRIT, IFIC, SSIGNE )
C
 9999 CONTINUE
C
      CALL JEDEMA()
      END
