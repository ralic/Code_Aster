      SUBROUTINE DYOBS2(MOTFAC,MAILLA,NBOCC ,LSUIVI,NTOBS,
     &                  NBSUIV,SDOBSE,SDSUIV)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/10/2010   AUTEUR PELLET J.PELLET 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      CHARACTER*16 MOTFAC
      CHARACTER*8  MAILLA
      CHARACTER*24 SDSUIV
      CHARACTER*14 SDOBSE
      INTEGER      NBOCC,NBSUIV
      INTEGER      NTOBS
      LOGICAL      LSUIVI(NBOCC)
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (OBSERVATION - CREATION SD)
C
C SAISIE DES DONNEES
C
C ----------------------------------------------------------------------
C
C
C IN  MOTFAC : MOT-CLEF FACTEUR POUR OBSERVATION
C IN  MAILLA : NOM DU MAILLAGE
C IN  NBOCC  : NOMBRE D'OCCURENCES DU MOT-CLEF FACTEUR OBSERVATION
C IN  LSUIVI : POUR CHAQUE OBSERVATION DIT SI C'EST UN SUIVI_DDL OU PAS
C IN  NBSUIV : NOMBRE D'OBSERVATION EN MODE SUIVI_DDL
C IN  NBOCC  : NOMBRE D'OCCURENCES DU MOT-CLEF FACTEUR OBSERVATION
C IN  NTOBS  : NOMBRE D'OBSERVATIONS PAR INSTANT D'OBSERVATION
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
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
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER      N1, N2, N4, N5, N6, N7, I, J, K, L, N8, N9, N10
      INTEGER      IOCC, IOBS, KNBNC, IBID
      INTEGER      NCHP, NCMP, NBNO, NBMA,NBPO
      INTEGER      JNOE,JMAI, JPOI, KNCMP, KNCHP
      INTEGER      KKKMA, KCHAM, KCOMP, KNUCM, KNOEU, KMAIL, KPOIN
      INTEGER      JCHAM, JCOMP, JNUCM, JNOEU, JMAIL, JPOIN, JEXTR
      INTEGER      IICHAM, IICOMP, IINUCM, IINOEU, IIMAIL, IIPOIN
      INTEGER      JSUINB, JSUIMA, IOBS1, IOBS2
      CHARACTER*8  K8B, NOMGD
      LOGICAL      CHAMES,CHAMEV
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL WKVECT(SDOBSE(1:14)//'.MAILLA'   ,'V V K8' ,1    ,KKKMA)
      CALL WKVECT(SDSUIV(1:14)//'.NBSUIV'   ,'V V I'  ,1    ,JSUINB)
      CALL WKVECT(SDSUIV(1:14)//'.MAILLA'   ,'V V K8' ,1    ,JSUIMA)
      ZK8(JSUIMA)= MAILLA
      ZI(JSUINB) = NBSUIV
C
      IF(NTOBS.GT.0)THEN
        CALL WKVECT(SDOBSE(1:14)//'.NOM_CHAM' ,'V V K16',NTOBS ,KCHAM)
        CALL WKVECT(SDOBSE(1:14)//'.NOM_CMP ' ,'V V K8' ,NTOBS ,KCOMP)
        CALL WKVECT(SDOBSE(1:14)//'.NUME_CMP' ,'V V I'  ,NTOBS ,KNUCM)
        CALL WKVECT(SDOBSE(1:14)//'.NOEUD'    ,'V V K8' ,NTOBS ,KNOEU)
        CALL WKVECT(SDOBSE(1:14)//'.MAILLE'   ,'V V K8' ,NTOBS ,KMAIL)
        CALL WKVECT(SDOBSE(1:14)//'.POINT'    ,'V V I'  ,NTOBS ,KPOIN)
      ENDIF
      IF(NBSUIV.GT.0)THEN
        CALL WKVECT(SDSUIV(1:14)//'.NOM_CHAM' ,'V V K16',NBSUIV,JCHAM)
        CALL WKVECT(SDSUIV(1:14)//'.NOM_CMP ' ,'V V K8' ,NBSUIV,JCOMP)
        CALL WKVECT(SDSUIV(1:14)//'.NUME_CMP' ,'V V I'  ,NBSUIV,JNUCM)
        CALL WKVECT(SDSUIV(1:14)//'.NOEUD'    ,'V V K8' ,NBSUIV,JNOEU)
        CALL WKVECT(SDSUIV(1:14)//'.MAILLE'   ,'V V K8' ,NBSUIV,JMAIL)
        CALL WKVECT(SDSUIV(1:14)//'.POINT'    ,'V V I'  ,NBSUIV,JPOIN)
        CALL WKVECT(SDSUIV(1:14)//'.EXTREMA'  ,'V V I'  ,NBSUIV,JEXTR)
      ENDIF
C
      ZK8 ( KKKMA ) = MAILLA
C
      IOBS1 = 0
      IOBS2 = 0
C
      DO 10 IOCC = 1 , NBOCC

         IF(LSUIVI(IOCC))THEN
           IICHAM=JCHAM
           IICOMP=JCOMP
           IINUCM=JNUCM
           IINOEU=JNOEU
           IIMAIL=JMAIL
           IIPOIN=JPOIN
           IOBS=IOBS2
         ELSE
           IICHAM=KCHAM
           IICOMP=KCOMP
           IINUCM=KNUCM
           IINOEU=KNOEU
           IIMAIL=KMAIL
           IIPOIN=KPOIN
           IOBS=IOBS1
         ENDIF
C
C --- LES CHAMPS
C
         CALL GETVTX (MOTFAC,'NOM_CHAM',IOCC,1,0,K8B,N1)
         NCHP = -N1
         CALL WKVECT ('&&DYOBS2.NOM_CHAM', 'V V K16', NCHP, KNCHP )
         CALL GETVTX (MOTFAC,'NOM_CHAM',IOCC,1,NCHP,
     &                ZK16(KNCHP),N1)
         CHAMES = .FALSE.
         CHAMEV = .FALSE.
         DO 12 I = 1 , NCHP
            IF ( ZK16(KNCHP+I-1)(1:4) .EQ. 'DEPL' ) THEN
               NOMGD  = 'DEPL_R'
            ELSEIF ( ZK16(KNCHP+I-1)(1:4) .EQ. 'VITE' ) THEN
               NOMGD  = 'DEPL_R'
            ELSEIF ( ZK16(KNCHP+I-1)(1:4) .EQ. 'ACCE' ) THEN
               NOMGD  = 'DEPL_R'
            ELSEIF ( ZK16(KNCHP+I-1)(1:9) .EQ. 'VALE_CONT' ) THEN
               NOMGD  = 'INFC_R'
            ELSEIF ( ZK16(KNCHP+I-1)(1:9) .EQ. 'SIEF_ELGA' ) THEN
               NOMGD  = 'SIEF_R'
               CHAMES = .TRUE.
            ELSEIF ( ZK16(KNCHP+I-1)(1:9) .EQ. 'VARI_ELGA' ) THEN
               NOMGD  = 'VARI_R'
               CHAMES = .TRUE.
            ELSEIF ( ZK16(KNCHP+I-1)(1:4) .EQ. 'TEMP' ) THEN
               NOMGD = 'TEMP_R'
            ELSEIF (LSUIVI(IOCC) .AND.
     &              ZK16(KNCHP+I-1)(1:9) .EQ. 'FORC_NODA' ) THEN
               NOMGD = 'FORC_R'
            ENDIF
 12      CONTINUE
C
C --- LES COMPOSANTES
C
         CALL GETVTX (MOTFAC,'NOM_CMP',IOCC,1,0,K8B,N2)
         NCMP = -N2

         CALL WKVECT ('&&DYOBS2.NOM_CMP' ,'V V K8',NCMP,KNCMP)
         CALL WKVECT ('&&DYOBS2.NUME_CMP','V V I' ,NCMP,KNBNC)
         CALL UTCMP2 (NOMGD,'OBSERVATION',IOCC,NCMP,ZK8(KNCMP),
     &                ZI(KNBNC),IBID)
C
C --- LES NOEUDS ET MAILLES
C
         CALL GETVTX (MOTFAC,'NOEUD'   , IOCC,1,0, K8B ,N4 )
         CALL GETVTX (MOTFAC,'GROUP_NO', IOCC,1,0, K8B ,N5 )
         IF (CHAMEV.OR.CHAMES) THEN
           CALL GETVTX (MOTFAC,'MAILLE'  , IOCC,1,0, K8B ,N6 )
           CALL GETVIS (MOTFAC,'POINT'   , IOCC,1,0, IBID,N7 )
         ELSE
           N6     = 0
           N7     = 0
         ENDIF
C
         IF (LSUIVI(IOCC))THEN
           CALL GETVTX(MOTFAC,'GROUP_MA', IOCC,1,0, K8B ,N8 )
           CALL GETVTX(MOTFAC,'VALE_MAX' ,IOCC,1,0,K8B ,N9 )
           CALL GETVTX(MOTFAC,'VALE_MIN' ,IOCC,1,0,K8B ,N10 )
         ENDIF
C
         IF ( N4 .NE. 0 ) THEN
            NBNO = -N4
            CALL WKVECT ('&&DYOBS2.LIST_NOEU','V V K8',NBNO,JNOE)
            CALL GETVTX (MOTFAC,'NOEUD', IOCC,1,NBNO,ZK8(JNOE),N4)
         ENDIF
         IF ( N5 .NE. 0 ) THEN
            CALL RELIEM (' ',MAILLA,'NO_NOEUD',MOTFAC,IOCC,1,
     &                'GROUP_NO','GROUP_NO','&&DYOBS2.LIST_NOEU',NBNO)
            CALL JEVEUO ('&&DYOBS2.LIST_NOEU','L',JNOE)
         ENDIF
         IF ( N6 .NE. 0 ) THEN
            NBMA = -N6
            CALL WKVECT ('&&DYOBS2.LIST_MAIL','V V K8',NBMA,JMAI)
            CALL GETVTX (MOTFAC,'MAILLE', IOCC,1,NBMA,ZK8(JMAI),N6)
         ENDIF
         IF ( N7 .NE. 0 ) THEN
            NBPO = -N7
            CALL WKVECT ('&&DYOBS2.LIST_POIN','V V I',NBPO,JPOI)
            CALL GETVIS (MOTFAC,'POINT', IOCC,1,NBPO,ZI(JPOI),N7)
         ENDIF
         IF(LSUIVI(IOCC))THEN
           IF( N8 .NE. 0 ) THEN
             CALL RELIEM (' ',MAILLA,'NO_MAILLE',MOTFAC,IOCC,1,
     &                'GROUP_MA','GROUP_MA','&&DYOBS2.LIST_MAIL',NBMA)
             CALL JEVEUO ('&&DYOBS2.LIST_MAIL','L',JMAI)
           ENDIF
           IF ( (N9.NE.0) .OR. (N10.NE.0) ) THEN
             NBPO=1
             NBMA=1
             NBNO=1
           ENDIF
         ENDIF
C
C --- ON STOCKE
C
         DO 100 I = 1 , NCHP
            DO 110 J = 1 , NCMP
               IF (     ZK16(KNCHP+I-1)(1:4) .EQ. 'DEPL'      .OR.
     &                  ZK16(KNCHP+I-1)(1:4) .EQ. 'VITE'      .OR.
     &                  ZK16(KNCHP+I-1)(1:4) .EQ. 'ACCE'      .OR.
     &                  ZK16(KNCHP+I-1)(1:9) .EQ. 'VALE_CONT' .OR.
     &                  ZK16(KNCHP+I-1)(1:4) .EQ. 'TEMP'      .OR.
     &                 (ZK16(KNCHP+I-1)(1:9) .EQ. 'FORC_NODA'
     &                        .AND. LSUIVI(IOCC))          ) THEN
C
                  DO 120 K = 1 , NBNO
                     ZK16(IICHAM+IOBS) = ZK16(KNCHP+I-1)
                     ZK8 (IICOMP+IOBS) = ZK8(KNCMP+J-1)
                     IF(LSUIVI(IOCC))THEN
C  ON STOCKE DANS LE VECTEUR EXTREMA LE TYPE DE VALEUR SOUHAITE
C  =0 AU NOEUD SPECIFIE
C  =1 LE MINIMUM DU CHAMP
C  =2 LE MAXIMUM DU CHAMP
                       IF (N9.NE.0) THEN
                          ZI(JEXTR+IOBS)=2
                       ELSE IF (N10.NE.0) THEN
                          ZI(JEXTR+IOBS)=1
                       ELSE
                          ZI(JEXTR+IOBS)=0
                          ZK8(IINOEU+IOBS) = ZK8(JNOE+K-1)
                       ENDIF
                     ELSE
                       ZK8(IINOEU+IOBS) = ZK8(JNOE+K-1)
                     ENDIF
                     IOBS = IOBS + 1
 120              CONTINUE
C
               ELSEIF ( ZK16(KNCHP+I-1)(1:9) .EQ. 'SIEF_ELGA' ) THEN
C
                  DO 130 K = 1 , NBMA
                     DO 132 L = 1 , NBPO
                        ZK16(IICHAM+IOBS) = ZK16(KNCHP+I-1)
                        ZK8 (IICOMP+IOBS) = ZK8(KNCMP+J-1)
                        IF(LSUIVI(IOCC))THEN
C  ON STOCKE DANS LE VECTEUR EXTREMA LE TYPE DE VALEUR SOUHAITE
C  =0 AU POINT SPECIFIE
C  =1 LE MINIMUM DU CHAMP
C  =2 LE MAXIMUM DU CHAMP
                           IF (N9.NE.0) THEN
                             ZI(JEXTR+IOBS)=2
                           ELSE IF (N10.NE.0) THEN
                             ZI(JEXTR+IOBS)=1
                           ELSE
                             ZI(JEXTR+IOBS)=0
                             ZK8(IIMAIL+IOBS) = ZK8(JMAI+K-1)
                             ZI(IIPOIN+IOBS) = ZI(JPOI+L-1)
                           ENDIF
                        ELSE
                           ZK8(IIMAIL+IOBS) = ZK8(JMAI+K-1)
                           ZI(IIPOIN+IOBS) = ZI(JPOI+L-1)
                        ENDIF
                        IOBS = IOBS + 1
 132                 CONTINUE
 130              CONTINUE
C
               ELSEIF ( ZK16(KNCHP+I-1)(1:9) .EQ. 'VARI_ELGA' ) THEN
C
                  DO 142 K = 1 , NBMA
                     DO 144 L = 1 , NBPO
                        ZK16(IICHAM+IOBS) = ZK16(KNCHP+I-1)
                        ZK8 (IICOMP+IOBS) = ZK8(KNCMP+J-1)
                        ZI  (IINUCM+IOBS) = ZI(KNBNC+J-1)
                        IF(LSUIVI(IOCC))THEN
C  ON STOCKE DANS LE VECTEUR EXTREMA LE TYPE DE VALEUR SOUHAITE
C  =0 AU POINT SPECIFIE
C  =1 LE MINIMUM DU CHAMP
C  =2 LE MAXIMUM DU CHAMP
                          IF (N9.NE.0) THEN
                            ZI(JEXTR+IOBS)=2
                          ELSE IF (N10.NE.0) THEN
                            ZI(JEXTR+IOBS)=1
                          ELSE
                            ZI(JEXTR+IOBS)=0
                            ZK8(IIMAIL+IOBS) = ZK8(JMAI+K-1)
                            ZI(IIPOIN+IOBS) = ZI(JPOI+L-1)
                          ENDIF
                        ELSE
                          ZK8(IIMAIL+IOBS) = ZK8(JMAI+K-1)
                          ZI(IIPOIN+IOBS) = ZI(JPOI+L-1)
                        ENDIF
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
         CALL JEDETR ('&&DYOBS2.NOM_CHAM' )
         CALL JEDETR ('&&DYOBS2.NOM_CMP'  )
         CALL JEDETR ('&&DYOBS2.NUME_CMP' )
         CALL JEDETR ('&&DYOBS2.LIST_NOEU')
         CALL JEDETR ('&&DYOBS2.LIST_MAIL')
         CALL JEDETR ('&&DYOBS2.LIST_POIN')
C
         IF(LSUIVI(IOCC))THEN
           IOBS2=IOBS
         ELSE
           IOBS1=IOBS
         ENDIF

 10   CONTINUE
C
      IF (IOBS1.GT.NTOBS .OR.IOBS2.NE.NBSUIV) THEN
        CALL ASSERT(.FALSE.)
      ENDIF

      IF(IOBS1.GT.0)THEN
        CALL JEECRA (SDOBSE(1:14)//'.NOM_CHAM', 'LONUTI', IOBS1, ' ' )
      ENDIF
C
      CALL JEDEMA()
C
      END
