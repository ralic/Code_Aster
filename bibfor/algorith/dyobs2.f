      SUBROUTINE DYOBS2 ( MAILLA, NBOCC, NTOBS )
      IMPLICIT   NONE
      INTEGER             NBOCC, NTOBS
      CHARACTER*8         MAILLA
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/05/2000   AUTEUR VABHHTS J.PELLET 
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
C     SAISIE DU MOT CLE FACTEUR "OBSERVATION"
C            VERIFICATION DES DONNEES
C            COMPTAGE DES FONCTIONS
C ----------------------------------------------------------------------
C     --- DEBUT DECLARATIONS NORMALISEES JEVEUX ------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
      CHARACTER*32       JEXNUM , JEXNOM
C     --- FIN DECLARATIONS NORMALISEES JEVEUX --------------------------
      INTEGER      N1, N2, N3, N4, N5, N6, N7, I, J, K, L, INO,
     +             IGNO, IOCC, NBTNO, NBN, IOBS, KNBNC, IBID, NTCMP,
     +             NCHP, NCMP, NBNC, NBNO, NBMA, NBGN, NBPO,
     +             JNOE, JGRN, JMAI, JPOI, JNOG, KNCMP, KNCHP,
     +             KKKMA, KCHAM, KCOMP, KNUCM, KNOEU, KMAIL, KPOIN
      CHARACTER*8  K8B, NOMGD
      CHARACTER*24 NOMNOE, GRPNO
C DEB------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMNOE = MAILLA//'.NOMNOE'
      GRPNO  = MAILLA//'.GROUPENO'
C
      CALL WKVECT ( '&&DYOBSE.MAILLA'   , 'V V K8' , 1     , KKKMA )
      CALL WKVECT ( '&&DYOBSE.NOM_CHAM' , 'V V K16', NTOBS , KCHAM )
      CALL WKVECT ( '&&DYOBSE.NOM_CMP ' , 'V V K8' , NTOBS , KCOMP )
      CALL WKVECT ( '&&DYOBSE.NUME_CMP' , 'V V I'  , NTOBS , KNUCM )
      CALL WKVECT ( '&&DYOBSE.NOEUD'    , 'V V K8' , NTOBS , KNOEU )
      CALL WKVECT ( '&&DYOBSE.MAILLE'   , 'V V K8' , NTOBS , KMAIL )
      CALL WKVECT ( '&&DYOBSE.POINT'    , 'V V I'  , NTOBS , KPOIN )
C
      ZK8 ( KKKMA ) = MAILLA
C
      IOBS = 0
C
      DO 10 IOCC = 1 , NBOCC
C
C ------ LES CHAMPS ----------------------------------------------------
C
         CALL GETVTX ( 'OBSERVATION', 'NOM_CHAM', IOCC,1,0, K8B, N1 )
         NCHP = -N1
         CALL WKVECT ( '&&DYOBS2.NOM_CHAM', 'V V K16', NCHP, KNCHP )
         CALL GETVTX ( 'OBSERVATION', 'NOM_CHAM', IOCC,1,NCHP,
     +                                            ZK16(KNCHP), N1 )
         DO 12 I = 1 , NCHP
            IF ( ZK16(KNCHP+I-1)(1:4) .EQ. 'DEPL' ) THEN
               NOMGD = 'DEPL_R'
            ELSEIF ( ZK16(KNCHP+I-1)(1:4) .EQ. 'VITE' ) THEN
               NOMGD = 'DEPL_R'
            ELSEIF ( ZK16(KNCHP+I-1)(1:4) .EQ. 'ACCE' ) THEN
               NOMGD = 'DEPL_R'
            ELSEIF ( ZK16(KNCHP+I-1)(1:9) .EQ. 'SIEF_ELGA' ) THEN
               NOMGD = 'SIEF_R'
            ELSEIF ( ZK16(KNCHP+I-1)(1:9) .EQ. 'VARI_ELGA' ) THEN
               NOMGD = 'VARI_R'
            ENDIF
 12      CONTINUE
C
C ------ LES COMPOSANTES -----------------------------------------------
C
         NBNC = 0
         CALL GETVTX ( 'OBSERVATION', 'NOM_CMP' , IOCC,1,0, K8B, N2 )
C        CALL GETVIS ( 'OBSERVATION', 'NUME_CMP', IOCC,1,0, IBID,N3 )
         N3=0
         NCMP = -N2
         IF ( N3 .NE. 0 ) NBNC = -N3
         NTCMP = NCMP * MAX(1,NBNC)
         CALL WKVECT ( '&&DYOBS2.NOM_CMP' , 'V V K8', NTCMP, KNCMP )
         CALL WKVECT ( '&&DYOBS2.NUME_CMP', 'V V I' , NTCMP, KNBNC )
         CALL UTCMP2 ( NOMGD, 'OBSERVATION', IOCC, ZK8(KNCMP), NCMP,
     +                                             ZI(KNBNC),  NBNC )
C
C ------ LES NOEUDS ET MAILLES -----------------------------------------
C
         CALL GETVID ( 'OBSERVATION','NOEUD'   , IOCC,1,0, K8B ,N4 )
         CALL GETVID ( 'OBSERVATION','GROUP_NO', IOCC,1,0, K8B ,N5 )
         CALL GETVID ( 'OBSERVATION','MAILLE'  , IOCC,1,0, K8B ,N6 )
         CALL GETVIS ( 'OBSERVATION','POINT'   , IOCC,1,0, IBID,N7 )
         IF ( N4 .NE. 0 ) THEN
            NBNO = -N4
            CALL WKVECT ('&&DYOBS2.LIST_NOEU','V V K8',NBNO,JNOE)
            CALL GETVID ( 'OBSERVATION','NOEUD', IOCC,1,NBNO,
     +                                                    ZK8(JNOE),N4)
         ENDIF
         IF ( N5 .NE. 0 ) THEN
            NBGN = -N5
            CALL WKVECT ('&&DYOBS2.LIST_GRNO','V V K8',NBGN,JGRN)
            CALL GETVID ( 'OBSERVATION','GROUP_NO', IOCC,1,NBGN,
     +                                                    ZK8(JGRN),N5)
            NBTNO = 0
            DO 22 IGNO = 0 , NBGN-1
               CALL JELIRA ( JEXNOM(GRPNO,ZK8(JGRN+IGNO)),
     +                                              'LONMAX', NBN, K8B )
               NBTNO = NBTNO + NBN
 22         CONTINUE
            CALL WKVECT ('&&DYOBS2.LIST_NOEU','V V K8',NBTNO,JNOE)
            NBNO = 0
            DO 24 IGNO = 0 , NBGN-1
               CALL JELIRA ( JEXNOM(GRPNO,ZK8(JGRN+IGNO)),
     +                                              'LONMAX', NBN, K8B )
               CALL JEVEUO ( JEXNOM(GRPNO,ZK8(JGRN+IGNO)), 'L', JNOG )
               DO 26 INO = 0 , NBN-1
                  CALL JENUNO ( JEXNUM(NOMNOE,ZI(JNOG+INO)),
     +                                                ZK8(JNOE+NBNO) )
                  NBNO = NBNO + 1
 26            CONTINUE
 24         CONTINUE
            CALL JEDETR ( '&&DYOBS2.LIST_GRNO' )
         ENDIF
         IF ( N6 .NE. 0 ) THEN
            NBMA = -N6
            CALL WKVECT ('&&DYOBS2.LIST_MAIL','V V K8',NBMA,JMAI)
            CALL GETVID ( 'OBSERVATION','MAILLE', IOCC,1,NBMA,
     +                                                    ZK8(JMAI),N6)
         ENDIF
         IF ( N7 .NE. 0 ) THEN
            NBPO = -N7
            CALL WKVECT ('&&DYOBS2.LIST_POIN','V V I',NBPO,JPOI)
            CALL GETVIS ( 'OBSERVATION','POINT', IOCC,1,NBPO,
     +                                                    ZI(JPOI),N7)
         ENDIF
C
C ------ ON STOCKE -----------------------------------------------------
C
         DO 100 I = 1 , NCHP
C
            DO 110 J = 1 , NCMP
C
               IF (     ZK16(KNCHP+I-1)(1:4) .EQ. 'DEPL' .OR.
     +                  ZK16(KNCHP+I-1)(1:4) .EQ. 'VITE' .OR.
     +                  ZK16(KNCHP+I-1)(1:4) .EQ. 'ACCE' ) THEN
C
                  DO 120 K = 1 , NBNO
                     ZK16(KCHAM+IOBS) = ZK16(KNCHP+I-1)
                     ZK8 (KCOMP+IOBS) = ZK8(KNCMP+J-1)
                     ZK8 (KNOEU+IOBS) = ZK8(JNOE+K-1)
                     IOBS = IOBS + 1
 120              CONTINUE
C
               ELSEIF ( ZK16(KNCHP+I-1)(1:9) .EQ. 'SIEF_ELGA' ) THEN
C
                  DO 130 K = 1 , NBMA
                     DO 132 L = 1 , NBPO
                        ZK16(KCHAM+IOBS) = ZK16(KNCHP+I-1)
                        ZK8 (KCOMP+IOBS) = ZK8(KNCMP+J-1)
                        ZK8 (KMAIL+IOBS) = ZK8(JMAI+K-1)
                        ZI  (KPOIN+IOBS) = ZI(JPOI+L-1)
                        IOBS = IOBS + 1
 132                 CONTINUE
 130              CONTINUE
C
               ELSEIF ( ZK16(KNCHP+I-1)(1:9) .EQ. 'VARI_ELGA' ) THEN
C
                  DO 142 K = 1 , NBMA
                     DO 144 L = 1 , NBPO
                        ZK16(KCHAM+IOBS) = ZK16(KNCHP+I-1)
                        ZK8 (KCOMP+IOBS) = ZK8(KNCMP+J-1)
                        ZI  (KNUCM+IOBS) = ZI(KNBNC+J-1)
                        ZK8 (KMAIL+IOBS) = ZK8(JMAI+K-1)
                        ZI  (KPOIN+IOBS) = ZI(JPOI+L-1)
                        IOBS = IOBS + 1
 144                 CONTINUE
 142              CONTINUE
C
               ENDIF
C
 110        CONTINUE
C
 100     CONTINUE
C
         CALL JEDETR ( '&&DYOBS2.NOM_CHAM' )
         CALL JEDETR ( '&&DYOBS2.NOM_CMP'  )
         CALL JEDETR ( '&&DYOBS2.NUME_CMP' )
         IF ( N4+N5 .NE. 0 ) CALL JEDETR ( '&&DYOBS2.LIST_NOEU' )
         IF ( N6 .NE. 0 )    CALL JEDETR ( '&&DYOBS2.LIST_MAIL' )
         IF ( N7 .NE. 0 )    CALL JEDETR ( '&&DYOBS2.LIST_POIN' )
C
 10   CONTINUE
C
      IF (IOBS.GT.NTOBS) CALL UTMESS('F','DYOBS2','DEBORDEMENT TABLEAU')
      CALL JEECRA ( '&&DYOBSE.NOM_CHAM', 'LONUTI', IOBS, ' ' )
C
      CALL JEDEMA()
C
      END
