      SUBROUTINE TBIM50 ( NOMTAB, PARA, NOMJV, NBTROU, TYPE )
      IMPLICIT   NONE
      INTEGER             NBTROU
      CHARACTER*(*)       NOMTAB, PARA, NOMJV, TYPE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 09/12/98   AUTEUR CIBHHLV L.VIVAN 
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
C      IMPRESSION DE LA TABLE AVEC PAGINATION
C ----------------------------------------------------------------------
C IN  : TABLE  : NOM DE LA STRUCTURE "TABLE"
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C ----------------------------------------------------------------------
      INTEGER       JVALE, I, J, KVALE, NBVAL
      CHARACTER*24  NOMJV1
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMJV1 = '&&TBIM50.VALE_1'
      NBTROU = 0
C
      CALL TBEXVE ( NOMTAB, PARA, NOMJV1, 'V', NBVAL , TYPE )
C
      IF ( NBVAL .EQ. 0 ) THEN
         CALL UTMESS ( 'F', 'TBIM50', 'Y A UN BUG 0' )
      ENDIF
C
      CALL JEVEUO ( NOMJV1, 'L', JVALE )
C
         IF     ( TYPE(1:1) .EQ. 'I'   ) THEN
            CALL WKVECT( NOMJV, 'V V I', NBVAL, KVALE )
            NBTROU = 1
            ZI(KVALE) = ZI(JVALE)
            DO 200 I = 2 , NBVAL
               DO 202 J = 1 , NBTROU
                  IF (ZI(KVALE+J-1) .EQ. ZI(JVALE+I-1) ) GOTO 200
 202           CONTINUE
               NBTROU = NBTROU + 1
               ZI(KVALE+NBTROU-1) = ZI(JVALE+I-1)
 200        CONTINUE
         ELSEIF ( TYPE(1:1) .EQ. 'R'   ) THEN
            CALL WKVECT( NOMJV, 'V V R', NBVAL, KVALE )
            NBTROU = 1
            ZR(KVALE) = ZR(JVALE)
            DO 204 I = 2 , NBVAL
               DO 206 J = 1 , NBTROU
                  IF (ZR(KVALE+J-1) .EQ. ZR(JVALE+I-1) ) GOTO 204
 206           CONTINUE
               NBTROU = NBTROU + 1
               ZR(KVALE+NBTROU-1) = ZR(JVALE+I-1)
 204        CONTINUE
         ELSEIF ( TYPE(1:1) .EQ. 'C'   ) THEN
            CALL WKVECT( NOMJV, 'V V C', NBVAL, KVALE )
            NBTROU = 1
            ZC(KVALE) = ZC(JVALE)
            DO 208 I = 2 , NBVAL
               DO 210  J = 1 , NBTROU
                  IF (ZC(KVALE+J-1) .EQ. ZC(JVALE+I-1) ) GOTO 208
 210           CONTINUE
               NBTROU = NBTROU + 1
               ZC(KVALE+NBTROU-1) = ZC(JVALE+I-1)
 208        CONTINUE
         ELSEIF ( TYPE(1:3) .EQ. 'K80' ) THEN
            CALL WKVECT( NOMJV, 'V V K80', NBVAL, KVALE )
            NBTROU = 1
            ZK80(KVALE) = ZK80(JVALE)
            DO 212 I = 2 , NBVAL
               DO 214  J = 1 , NBTROU
                  IF (ZK80(KVALE+J-1) .EQ. ZK80(JVALE+I-1) ) GOTO 212
 214           CONTINUE
               NBTROU = NBTROU + 1
               ZK80(KVALE+NBTROU-1) = ZK80(JVALE+I-1)
 212        CONTINUE
         ELSEIF ( TYPE(1:3) .EQ. 'K32' ) THEN
            CALL WKVECT( NOMJV, 'V V K32', NBVAL, KVALE )
            NBTROU = 1
            ZK32(KVALE) = ZK32(JVALE)
            DO 216 I = 2 , NBVAL
               DO 218  J = 1 , NBTROU
                  IF (ZK32(KVALE+J-1) .EQ. ZK32(JVALE+I-1) ) GOTO 216
 218           CONTINUE
               NBTROU = NBTROU + 1
               ZK32(KVALE+NBTROU-1) = ZK32(JVALE+I-1)
 216        CONTINUE
         ELSEIF ( TYPE(1:3) .EQ. 'K24' ) THEN
            CALL WKVECT( NOMJV, 'V V K24', NBVAL, KVALE )
            NBTROU = 1
            ZK24(KVALE) = ZK24(JVALE)
            DO 220 I = 2 , NBVAL
               DO 222  J = 1 , NBTROU
                  IF (ZK24(KVALE+J-1) .EQ. ZK24(JVALE+I-1) ) GOTO 220
 222           CONTINUE
               NBTROU = NBTROU + 1
               ZK24(KVALE+NBTROU-1) = ZK24(JVALE+I-1)
 220        CONTINUE
         ELSEIF ( TYPE(1:3) .EQ. 'K16' ) THEN
            CALL WKVECT( NOMJV, 'V V K16', NBVAL, KVALE )
            NBTROU = 1
            ZK16(KVALE) = ZK16(JVALE)
            DO 224 I = 2 , NBVAL
               DO 226  J = 1 , NBTROU
                  IF (ZK16(KVALE+J-1) .EQ. ZK16(JVALE+I-1) ) GOTO 224
 226           CONTINUE
               NBTROU = NBTROU + 1
               ZK16(KVALE+NBTROU-1) = ZK16(JVALE+I-1)
 224        CONTINUE
         ELSEIF ( TYPE(1:2) .EQ. 'K8'  ) THEN
            CALL WKVECT( NOMJV, 'V V K8', NBVAL, KVALE )
            NBTROU = 1
            ZK8(KVALE) = ZK8(JVALE)
            DO 228 I = 2 , NBVAL
               DO 230  J = 1 , NBTROU
                  IF (ZK8(KVALE+J-1) .EQ. ZK8(JVALE+I-1) ) GOTO 228
 230           CONTINUE
               NBTROU = NBTROU + 1
               ZK8(KVALE+NBTROU-1) = ZK8(JVALE+I-1)
 228        CONTINUE
         ENDIF
C
         CALL JEDETR ( NOMJV1 )
         CALL JEECRA ( NOMJV , 'LONUTI' , NBTROU , ' ' )
C
      CALL JEDEMA()
C
      END
