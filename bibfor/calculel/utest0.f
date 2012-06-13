      SUBROUTINE UTEST0 ( NOMTA, PARA, TYPTES, TYPRES, TBTXT, REFI,
     +                    REFR, REFC,  EPSI, CRIT, IFIC, SSIGNE )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER              REFI, IFIC
      REAL*8               REFR, EPSI
      CHARACTER*8          TYPTES
      CHARACTER*16         TBTXT(2)
      CHARACTER*(*)        NOMTA, PARA, TYPRES, CRIT, SSIGNE
      COMPLEX*16           REFC
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
C IN  : NOMTA  : NOM DE LA STRUCTURE "TABLE".
C IN  : PARA   : PARAMETRE A CHERCHER
C IN  : TYPTES : TYPE DE TEST A EFFECTUER SUR LE CHAMP
C IN  : TBTXT  : (1)=REFERENCE, (2)=LEGENDE
C IN  : REFI   : VALEUR REELLE ENTIERE ATTENDUE
C IN  : REFR   : VALEUR REELLE ATTENDUE
C IN  : REFC   : VALEUR COMPLEXE ATTENDUE
C IN  : CRIT   : 'RELATIF' OU 'ABSOLU'(PRECISION RELATIVE OU ABSOLUE).
C IN  : EPSI   : PRECISION ESPEREE
C IN  : IFIC   : NUMERO LOGIQUE DU FICHIER DE SORTIE
C OUT : IMPRESSION SUR LISTING
C ----------------------------------------------------------------------
      INTEGER       VALI, JVALE, JVALL, NBLIGN, NBPARA, I, ISMAEM, 
     +              JTBNP, JTBLP, IPAR
      REAL*8        VALR, R8MAEM
      COMPLEX*16    VALC
      LOGICAL       EXIST
      CHARACTER*1   TYPREZ
      CHARACTER*4   TYPE
      CHARACTER*19  NOMTAB
      CHARACTER*24  INPAR
      CHARACTER*24 VALK(2)
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMTAB = NOMTA
      INPAR  = PARA
      TYPREZ = TYPRES(1:1)
C
      CALL TBEXIP ( NOMTA, PARA, EXIST, TYPE )
C
      IF ( .NOT. EXIST ) THEN
         VALK(1) = PARA
         CALL U2MESK('F','CALCULEL6_85', 1 ,VALK)
      ENDIF
C
      IF ( TYPE(1:1) .NE. TYPREZ ) THEN
         WRITE(IFIC,*) 'NOOK '
          VALK(1) = TYPE
          VALK(2) = TYPREZ
          CALL U2MESK('A','CALCULEL5_11', 2 ,VALK)
         GOTO 9999
      ENDIF
C
      CALL JEVEUO ( NOMTAB//'.TBNP' , 'L', JTBNP )
      NBPARA = ZI(JTBNP  )
      NBLIGN = ZI(JTBNP+1)
C
      CALL JEVEUO ( NOMTAB//'.TBLP' , 'L', JTBLP )
      DO 10 IPAR = 1 , NBPARA
         IF ( INPAR .EQ. ZK24(JTBLP+4*(IPAR-1)) )  GOTO 12
 10   CONTINUE
 12   CONTINUE
      CALL JEVEUO ( ZK24(JTBLP+4*(IPAR-1)+2) , 'L', JVALE )
      CALL JEVEUO ( ZK24(JTBLP+4*(IPAR-1)+3) , 'L', JVALL )
C
      IF ( TYPE .EQ. 'I' ) THEN
         IF ( TYPTES .EQ. 'SOMM_ABS' ) THEN
            VALI = 0
            DO 100 I = 1 , NBLIGN
               IF (ZI(JVALL+I-1).EQ.1) VALI = VALI+ABS( ZI(JVALE+I-1) )
 100        CONTINUE
         ELSEIF ( TYPTES .EQ. 'SOMM' ) THEN
            VALI = 0
            DO 102 I = 1 , NBLIGN
               IF (ZI(JVALL+I-1).EQ.1) VALI = VALI + ZI(JVALE+I-1)
 102        CONTINUE
         ELSEIF ( TYPTES .EQ. 'MAX' ) THEN
            VALI = -ISMAEM()
            DO 104 I = 1 , NBLIGN
               IF (ZI(JVALL+I-1).EQ.1) VALI = MAX( VALI,ZI(JVALE+I-1) )
 104        CONTINUE
         ELSEIF ( TYPTES .EQ. 'MIN' ) THEN
            VALI = ISMAEM()
            DO 106 I = 1 , NBLIGN
               IF (ZI(JVALL+I-1).EQ.1) VALI = MIN( VALI,ZI(JVALE+I-1) )
 106        CONTINUE
         ELSE
            WRITE(IFIC,*) 'NOOK '
            CALL U2MESS('A','CALCULEL5_12')
            GOTO 9999
         ENDIF
      ELSEIF ( TYPE .EQ. 'R' ) THEN
         IF ( TYPTES .EQ. 'SOMM_ABS' ) THEN
            VALR = 0.D0
            DO 200 I = 1 , NBLIGN
               IF (ZI(JVALL+I-1).EQ.1) VALR = VALR+ABS( ZR(JVALE+I-1) )
 200        CONTINUE
         ELSEIF ( TYPTES .EQ. 'SOMM' ) THEN
            VALR = 0.D0
            DO 202 I = 1 , NBLIGN
               IF (ZI(JVALL+I-1).EQ.1) VALR = VALR + ZR(JVALE+I-1)
 202        CONTINUE
         ELSEIF ( TYPTES .EQ. 'MAX' ) THEN
            VALR = -R8MAEM()
            DO 204 I = 1 , NBLIGN
               IF (ZI(JVALL+I-1).EQ.1) VALR = MAX( VALR,ZR(JVALE+I-1) )
 204        CONTINUE
         ELSEIF ( TYPTES .EQ. 'MIN' ) THEN
            VALR = R8MAEM()
            DO 206 I = 1 , NBLIGN
               IF (ZI(JVALL+I-1).EQ.1) VALR = MIN( VALR,ZR(JVALE+I-1) )
 206        CONTINUE
         ELSE
            WRITE(IFIC,*) 'NOOK '
            CALL U2MESS('A','CALCULEL5_12')
            GOTO 9999
         ENDIF
      ELSEIF ( TYPE .EQ. 'C' ) THEN
         VALC = ( 0.D0 , 0.D0 )
         IF ( TYPTES .EQ. 'SOMM_ABS' ) THEN
            DO 300 I = 1 , NBLIGN
               IF (ZI(JVALL+I-1).EQ.1) VALC = VALC+ABS( ZC(JVALE+I-1) )
 300        CONTINUE
         ELSEIF ( TYPTES .EQ. 'SOMM' ) THEN
            DO 302 I = 1 , NBLIGN
               IF (ZI(JVALL+I-1).EQ.1) VALC = VALC + ZC(JVALE+I-1)
 302        CONTINUE
         ELSE
            WRITE(IFIC,*) 'NOOK '
            CALL U2MESS('A','CALCULEL5_12')
            GOTO 9999
         ENDIF
      ENDIF
C
      CALL UTITES ( TBTXT(1), TBTXT(2), TYPRES, 1, REFI, REFR, REFC,
     +                      VALI, VALR, VALC, EPSI, CRIT, IFIC, SSIGNE )
C
 9999 CONTINUE
C
      CALL JEDEMA()
      END
