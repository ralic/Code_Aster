      SUBROUTINE TBTR01 ( TABIN, NBPARA, NOPARA, NBLIGN, NUME )
      IMPLICIT   NONE
      INTEGER             NBPARA, NBLIGN, NUME(*)
      CHARACTER*(*)       TABIN, NOPARA
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 27/09/2004   AUTEUR CIBHHLV L.VIVAN 
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
C     TRI D'UNE TABLE.
C ----------------------------------------------------------------------
C IN  : TABIN  : NOM DE LA TABLE DONT ON VEUT TRIER DES LIGNES
C IN  : NBPARA : NOMBRE DE PARAMETRE DE LA TABLE "TABIN"
C IN  : NOPARA : LA PARAMETRE A TRIER
C IN  : NBLIGN : NOMBRE DE LIGNES A TRIER
C VAR : NUME   : IN  : NUMERO DES LIGNES A TRIER DANS "TABIN"
C                OUT : LES NUMEROS DANS UN ORDRE CROISSANT
C                      LES "VIDE" EN TETE
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
      INTEGER      JTBLP, JNUVI, JNUNV, I, J, JVALE, JVALL, NBVID, 
     +             NBNVD, JTRI, LVALE, JNUME
      CHARACTER*4  TYPE
      CHARACTER*19 NOMTAB
      CHARACTER*24 NOMJV, NOMJVL, INPAR, JNPAR
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMTAB = TABIN
      CALL JEVEUO ( NOMTAB//'.TBLP' , 'L', JTBLP )
C
      CALL WKVECT ( '&&TBTR01.NUME  ', 'V V I', NBLIGN, JNUME )
      CALL WKVECT ( '&&TBTR01.VIDE  ', 'V V I', NBLIGN, JNUVI )
      CALL WKVECT ( '&&TBTR01.N_VIDE', 'V V I', NBLIGN, JNUNV )
      DO 10 I = 1 , NBLIGN
         ZI(JNUME+I-1) = NUME(I)
 10   CONTINUE
C
      INPAR = NOPARA
      DO 100 J = 1 , NBPARA
         JNPAR = ZK24(JTBLP+4*(J-1))
         IF ( INPAR .EQ. JNPAR ) THEN
            TYPE   = ZK24(JTBLP+4*(J-1)+1)
            NOMJV  = ZK24(JTBLP+4*(J-1)+2)
            NOMJVL = ZK24(JTBLP+4*(J-1)+3)
            CALL JEVEUO ( NOMJV , 'L', JVALE )
            CALL JEVEUO ( NOMJVL, 'L', JVALL )
            NBVID = 0
            NBNVD = 0
            DO 20 I = 1 , NBLIGN
               IF ( ZI(JVALL+NUME(I)-1).EQ.0 ) THEN
                  NBVID = NBVID + 1
                  ZI(JNUVI+NBVID-1) = NUME(I)
               ELSE
                  NBNVD = NBNVD + 1
                  ZI(JNUNV+NBNVD-1) = NUME(I)
               ENDIF
 20         CONTINUE
            GOTO 102
         ENDIF
 100  CONTINUE
 102  CONTINUE
C
      IF ( NBNVD .EQ. 0 ) GOTO 9999
C
      CALL WKVECT ( '&&TBTR01.TRI', 'V V I', NBNVD, JTRI )
C
      IF ( TYPE(1:1) .EQ. 'I' ) THEN
         CALL WKVECT ( '&&TBTR01.VALEUR', 'V V I', NBNVD, LVALE )
         DO 210 I = 1 , NBNVD
            ZI(LVALE+I-1) = ZI(JVALE+ZI(JNUNV+I-1)-1)
 210     CONTINUE
         CALL TBTRII ( NBNVD, ZI(LVALE), ZI(JTRI) )
         CALL JEDETR ( '&&TBTR01.VALEUR' )
      ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
         CALL WKVECT ( '&&TBTR01.VALEUR', 'V V R', NBNVD, LVALE )
         DO 220 I = 1 , NBNVD
            ZR(LVALE+I-1) = ZR(JVALE+ZI(JNUNV+I-1)-1)
 220     CONTINUE
         CALL TBTRIR ( NBNVD, ZR(LVALE), ZI(JTRI) )
         CALL JEDETR ( '&&TBTR01.VALEUR' )
      ELSEIF ( TYPE(1:3) .EQ. 'K80' ) THEN
         CALL WKVECT ( '&&TBTR01.VALEUR', 'V V K80', NBNVD, LVALE )
         DO 230 I = 1 , NBNVD
            ZK80(LVALE+I-1) = ZK80(JVALE+ZI(JNUNV+I-1)-1)
 230     CONTINUE
         CALL TBTRIK ( NBNVD, ZK80(LVALE), ZI(JTRI) )
         CALL JEDETR ( '&&TBTR01.VALEUR' )
      ELSEIF ( TYPE(1:3) .EQ. 'K32' ) THEN
         CALL WKVECT ( '&&TBTR01.VALEUR', 'V V K32', NBNVD, LVALE )
         DO 240 I = 1 , NBNVD
            ZK32(LVALE+I-1) = ZK32(JVALE+ZI(JNUNV+I-1)-1)
 240     CONTINUE
         CALL TBTRIK ( NBNVD, ZK32(LVALE), ZI(JTRI) )
         CALL JEDETR ( '&&TBTR01.VALEUR' )
      ELSEIF ( TYPE(1:3) .EQ. 'K24' ) THEN
         CALL WKVECT ( '&&TBTR01.VALEUR', 'V V K24', NBNVD, LVALE )
         DO 250 I = 1 , NBNVD
            ZK24(LVALE+I-1) = ZK24(JVALE+ZI(JNUNV+I-1)-1)
 250     CONTINUE
         CALL TBTRIK ( NBNVD, ZK24(LVALE), ZI(JTRI) )
         CALL JEDETR ( '&&TBTR01.VALEUR' )
      ELSEIF ( TYPE(1:3) .EQ. 'K16' ) THEN
         CALL WKVECT ( '&&TBTR01.VALEUR', 'V V K16', NBNVD, LVALE )
         DO 260 I = 1 , NBNVD
            ZK16(LVALE+I-1) = ZK16(JVALE+ZI(JNUNV+I-1)-1)
 260     CONTINUE
         CALL TBTRIK ( NBNVD, ZK16(LVALE), ZI(JTRI) )
         CALL JEDETR ( '&&TBTR01.VALEUR' )
      ELSEIF ( TYPE(1:2) .EQ. 'K8' ) THEN
         CALL WKVECT ( '&&TBTR01.VALEUR', 'V V K8', NBNVD, LVALE )
         DO 270 I = 1 , NBNVD
            ZK8(LVALE+I-1) = ZK8(JVALE+ZI(JNUNV+I-1)-1)
 270     CONTINUE
         CALL TBTRIK ( NBNVD, ZK8(LVALE), ZI(JTRI) )
         CALL JEDETR ( '&&TBTR01.VALEUR' )
      ENDIF
C
      DO 302 I = 1 , NBVID
         NUME(I) = 0
 302  CONTINUE
C
      DO 304 I = 1 , NBNVD
         NUME(NBVID+I) = ZI(JNUME+ZI(JTRI+I-1)-1)
 304  CONTINUE
C
      CALL JEDETR ( '&&TBTR01.TRI' )
C
 9999  CONTINUE
C
      CALL JEDETR ( '&&TBTR01.NUME  ' )
      CALL JEDETR ( '&&TBTR01.VIDE  ' )
      CALL JEDETR ( '&&TBTR01.N_VIDE' )
C
      CALL JEDEMA()
      END
