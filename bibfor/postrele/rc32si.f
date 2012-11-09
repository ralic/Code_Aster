      SUBROUTINE RC32SI()
      IMPLICIT   NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C     ------------------------------------------------------------------
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C     RECUPERATION DES DONNEES DE "SITUATION"
C
C     ------------------------------------------------------------------
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
C
      INTEGER      N1, NBSITU, IOCC, IBID, NUME, II, NOCC, ING,
     &             JNBOCC, JNUMGR, JPRESA, JPRESB, NBCHAR, JCHAR,
     &             JNSITU, JCOMBI, JNBGR, IG, NUMPAS(2), NBVG, JNBVG,
     &             NBGR, NUMGR, NBSIGR, JNSG, NBTH, JSEIGR, NSCY,
     &             NBM, VALI(3), NBGRT, NUMG1, NUMG2, NBSEIS, NUMGS,
     &             NBSG1, NBSG2, NBSG3, NBP12, NBP23, NBP13, NDIM,
     &             JSPAS, JSIGR, JSP12, JSP23, JSP13
      LOGICAL      YAPASS
      CHARACTER*8  K8B, KNUME, OUINON
      CHARACTER*16 MOTCL1, MOTCL2
      INTEGER      IARG
C DEB ------------------------------------------------------------------
C
      MOTCL1 = 'SITUATION'
      MOTCL2 = 'SEISME'
C
      CALL GETFAC ( MOTCL1, NBSITU )
      CALL GETFAC ( MOTCL2, NBSEIS )
C
      NDIM = NBSITU + NBSEIS
      CALL WKVECT('&&RC32SI.NUME_GROUP'     , 'V V I',   NDIM, JNBGR )
      CALL WKVECT('&&RC32SI.SITU_GROUP'     , 'V V I', 2*NDIM, JSIGR )
C
      CALL WKVECT('&&RC3200.SITU_NUMERO'    , 'V V I',   NDIM, JNSITU)
      CALL WKVECT('&&RC3200.SITU_NB_OCCUR'  , 'V V I', 2*NDIM, JNBOCC)
      CALL WKVECT('&&RC3200.SITU_PRES_A'    , 'V V R', NBSITU, JPRESA)
      CALL WKVECT('&&RC3200.SITU_PRES_B'    , 'V V R', NBSITU, JPRESB)
      CALL WKVECT('&&RC3200.SITU_COMBINABLE', 'V V L',   NDIM, JCOMBI)
      CALL JECREC('&&RC3200.SITU_ETAT_A'    , 'V V I', 'NO',
     &                               'DISPERSE', 'VARIABLE', NDIM   )
      CALL JECREC('&&RC3200.SITU_ETAT_B'    , 'V V I', 'NO',
     &                               'DISPERSE', 'VARIABLE', NBSITU )
      CALL JECREC('&&RC3200.SITU_THERMIQUE' , 'V V I', 'NO',
     &                               'DISPERSE', 'VARIABLE', NDIM   )
C
      CALL WKVECT ('&&RC32SI.PASSAGE_1_2', 'V V I', NDIM, JSP12 )
      CALL WKVECT ('&&RC32SI.PASSAGE_2_3', 'V V I', NDIM, JSP23 )
      CALL WKVECT ('&&RC32SI.PASSAGE_1_3', 'V V I', NDIM, JSP13 )
      CALL JEECRA ('&&RC32SI.PASSAGE_1_2','LONUTI', 0, ' ')
      CALL JEECRA ('&&RC32SI.PASSAGE_2_3','LONUTI', 0, ' ')
      CALL JEECRA ('&&RC32SI.PASSAGE_1_3','LONUTI', 0, ' ')
C
      NBGR = 0
      YAPASS = .FALSE.
C
      DO 10, IOCC = 1, NBSITU, 1
C
         CALL CODENT ( IOCC , 'D0' , K8B  )
C
C ------ LE NUMERO DE SITUATION:
C        -----------------------
C
         CALL GETVIS ( MOTCL1, 'NUME_SITU', IOCC,IARG,1, NUME, N1 )
         ZI(JNSITU+IOCC-1) = NUME
         KNUME = 'S       '
         CALL CODENT ( NUME , 'D0' , KNUME(2:8)  )
C
C ------ LE NOMBRE D'OCCURRENCE:
C        -----------------------
C
         CALL GETVIS ( MOTCL1, 'NB_OCCUR', IOCC,IARG,1, NOCC, N1 )
         ZI(JNBOCC+2*IOCC-2) = NOCC
C
C ------ LES PRESSIONS:
C        --------------
C
         CALL GETVR8 (MOTCL1,'PRES_A',IOCC,IARG,1,
     &                ZR(JPRESA+IOCC-1),N1)
         CALL GETVR8 (MOTCL1,'PRES_B',IOCC,IARG,1,
     &                ZR(JPRESB+IOCC-1),N1)
C
C ------ LES NUMEROS DE GROUPE:
C        ----------------------
C
        CALL GETVIS ( MOTCL1, 'NUME_GROUPE', IOCC,IARG,0, NUMPAS, N1 )
        IF ( N1 .NE. 0 ) THEN
           NBVG = -N1
           CALL WKVECT('&&RC32SI.VALE_GR', 'V V I', NBVG, JNBVG )
           CALL GETVIS (MOTCL1,'NUME_GROUPE',IOCC,IARG,NBVG,
     &                  ZI(JNBVG),N1)
           DO 26 ING = 1,NBVG
              NUMGR = ZI(JNBVG+ING-1)
              IF ( NUMGR .LE. 0 ) CALL U2MESS('F','POSTRCCM_12')
              DO 20 IG = 1 , NBGR
                 IF ( ZI(JNBGR+IG-1) .EQ. NUMGR ) GOTO 21
 20           CONTINUE
              NBGR = NBGR + 1
              ZI(JNBGR+NBGR-1) = NUMGR
 21           CONTINUE
 26        CONTINUE
           IF ( NBVG .EQ. 1 ) THEN
              ZI(JSIGR+2*IOCC-2) = ZI(JNBVG)
              ZI(JSIGR+2*IOCC-1) = ZI(JNBVG)
           ELSE
              ZI(JSIGR+2*IOCC-2) = ZI(JNBVG)
              ZI(JSIGR+2*IOCC-1) = ZI(JNBVG+1)
           ENDIF
           CALL JEDETR ( '&&RC32SI.VALE_GR' )
        ENDIF
C
C ------ LES NUMEROS DE PASSAGE:
C        -----------------------
C
        CALL GETVIS ( MOTCL1, 'NUME_PASSAGE', IOCC,IARG,0, NUMPAS, N1 )
        IF ( N1 .NE. 0 ) THEN
           CALL GETVIS (MOTCL1,'NUME_PASSAGE',IOCC,IARG,2,
     &                  NUMPAS, N1 )
           IF ( NUMPAS(1) .LE. 0 ) CALL U2MESS('F','POSTRCCM_12')
           IF ( NUMPAS(2) .LE. 0 ) CALL U2MESS('F','POSTRCCM_12')
           IF ( NUMPAS(1) .GT. 3 ) CALL U2MESS('F','POSTRCCM_12')
           IF ( NUMPAS(2) .GT. 3 ) CALL U2MESS('F','POSTRCCM_12')
           YAPASS = .TRUE.
           ZI(JSIGR+2*IOCC-2) = MIN ( NUMPAS(1), NUMPAS(2) )
           ZI(JSIGR+2*IOCC-1) = MAX ( NUMPAS(1), NUMPAS(2) )
           NUMGR = NUMPAS(1)
           DO 22 IG = 1 , NBGR
              IF ( ZI(JNBGR+IG-1) .EQ. NUMGR ) GOTO 23
 22        CONTINUE
           NBGR = NBGR + 1
           ZI(JNBGR+NBGR-1) = NUMGR
 23        CONTINUE
           NUMGR = NUMPAS(2)
           DO 24 IG = 1 , NBGR
              IF ( ZI(JNBGR+IG-1) .EQ. NUMGR ) GOTO 25
 24        CONTINUE
           NBGR = NBGR + 1
           ZI(JNBGR+NBGR-1) = NUMGR
 25        CONTINUE
        ENDIF
C
C ------ COMBINABLE DANS SON GROUPE:
C        ---------------------------
         CALL GETVTX ( MOTCL1, 'COMBINABLE', IOCC,IARG,1, OUINON, N1 )
         IF ( OUINON(1:3) .EQ. 'OUI' ) THEN
            ZL(JCOMBI+IOCC-1) = .TRUE.
         ELSE
            ZL(JCOMBI+IOCC-1) = .FALSE.
         ENDIF
C
C ------ ETAT DE CHARGEMENT POUR "A":
C        ----------------------------
         CALL GETVIS ( MOTCL1, 'CHAR_ETAT_A', IOCC,IARG,0, IBID, N1 )
         NBCHAR = -N1
         CALL JECROC (JEXNOM('&&RC3200.SITU_ETAT_A',KNUME))
         CALL JEECRA (JEXNOM('&&RC3200.SITU_ETAT_A',KNUME),
     &                                          'LONMAX', NBCHAR, ' ' )
         CALL JEECRA (JEXNOM('&&RC3200.SITU_ETAT_A',KNUME),
     &                                          'LONUTI', NBCHAR, ' ' )
         CALL JEVEUO (JEXNOM('&&RC3200.SITU_ETAT_A',KNUME),'E', JCHAR )
         CALL GETVIS (MOTCL1,'CHAR_ETAT_A',IOCC,IARG,NBCHAR,
     &                ZI(JCHAR),N1)
C
C ------ ETAT DE CHARGEMENT POUR "B":
C        ----------------------------
         CALL GETVIS ( MOTCL1, 'CHAR_ETAT_B', IOCC,IARG,0, IBID, N1 )
         NBCHAR = -N1
         CALL JECROC (JEXNOM('&&RC3200.SITU_ETAT_B',KNUME))
         CALL JEECRA (JEXNOM('&&RC3200.SITU_ETAT_B',KNUME),
     &                                          'LONMAX', NBCHAR, ' ' )
         CALL JEECRA (JEXNOM('&&RC3200.SITU_ETAT_B',KNUME),
     &                                          'LONUTI', NBCHAR, ' ' )
         CALL JEVEUO (JEXNOM('&&RC3200.SITU_ETAT_B',KNUME),'E', JCHAR )
         CALL GETVIS (MOTCL1,'CHAR_ETAT_B',IOCC,IARG,NBCHAR,
     &                ZI(JCHAR),N1)
C
C ------ TRANSITOIRE THERMIQUE ASSOCIE A LA SITUATION:
C        ---------------------------------------------
         CALL GETVIS ( MOTCL1, 'NUME_RESU_THER', IOCC,IARG,0, IBID, N1 )
         NBTH = -N1
         CALL JECROC (JEXNOM('&&RC3200.SITU_THERMIQUE',KNUME))
         NBM = MAX(1,NBTH)
         CALL JEECRA (JEXNOM('&&RC3200.SITU_THERMIQUE',KNUME),
     &                                      'LONMAX',NBM ,' ')
C
         IF ( NBTH .EQ. 0 ) THEN
            CALL JEECRA (JEXNOM('&&RC3200.SITU_THERMIQUE',KNUME),
     &                                      'LONUTI', 0,' ')
         ELSE
            CALL JEECRA (JEXNOM('&&RC3200.SITU_THERMIQUE',KNUME),
     &                                              'LONUTI', NBTH,' ')
            CALL JEVEUO (JEXNOM('&&RC3200.SITU_THERMIQUE',KNUME),'E',
     &                                                          JCHAR )
            CALL GETVIS ( MOTCL1, 'NUME_RESU_THER', IOCC,IARG,
     &                                            NBTH,ZI(JCHAR), N1 )
         ENDIF
C
 10   CONTINUE
C
      DO 110, IOCC = 1, NBSEIS, 1
C
         CALL GETVIS ( MOTCL2, 'NUME_GROUPE', IOCC,IARG,1, NUME, N1 )
         ZI(JSIGR+2*(NBSITU+IOCC)-2) = NUME
         ZI(JSIGR+2*(NBSITU+IOCC)-1) = NUME
C
         ZL(JCOMBI+NBSITU+IOCC-1) = .TRUE.
C
C ------ LE NUMERO DE SITUATION:
C        -----------------------
         CALL GETVIS ( MOTCL2, 'NUME_SITU', IOCC,IARG,1, NUME, N1 )
         ZI(JNSITU+NBSITU+IOCC-1) = NUME
         KNUME = 'S       '
         CALL CODENT ( NUME , 'D0' , KNUME(2:8)  )
C
C ------ LE NOMBRE D'OCCURRENCE DE SEISME:
C        ---------------------------------
         CALL GETVIS ( MOTCL2, 'NB_OCCUR', IOCC,IARG,1, NOCC, N1 )
         ZI(JNBOCC+2*(NBSITU+IOCC)-2) = NOCC
         CALL GETVIS ( MOTCL2, 'NB_CYCL_SEISME', IOCC,IARG,1, NSCY, N1 )
         ZI(JNBOCC+2*(NBSITU+IOCC)-1) = NSCY
C
C ------ ETAT DE CHARGEMENT:
C        -------------------
         CALL GETVIS ( MOTCL2, 'CHAR_ETAT', IOCC,IARG,0, IBID, N1 )
         NBCHAR = -N1
         CALL JECROC (JEXNOM('&&RC3200.SITU_ETAT_A',KNUME))
         CALL JEECRA (JEXNOM('&&RC3200.SITU_ETAT_A',KNUME),
     &                                          'LONMAX', NBCHAR, ' ' )
         CALL JEECRA (JEXNOM('&&RC3200.SITU_ETAT_A',KNUME),
     &                                          'LONUTI', NBCHAR, ' ' )
         CALL JEVEUO (JEXNOM('&&RC3200.SITU_ETAT_A',KNUME),'E', JCHAR )
         CALL GETVIS ( MOTCL2,'CHAR_ETAT',IOCC,IARG,NBCHAR,ZI(JCHAR),N1)
C
C ------ SEISME : PAS DE TRANSITOIRE THERMIQUE
C        ---------------------------------------------
         NBTH = 0
         CALL JECROC (JEXNOM('&&RC3200.SITU_THERMIQUE',KNUME))
         NBM = MAX(1,NBTH)
         CALL JEECRA (JEXNOM('&&RC3200.SITU_THERMIQUE',KNUME),
     &                                      'LONMAX',NBM ,' ')
         CALL JEECRA (JEXNOM('&&RC3200.SITU_THERMIQUE',KNUME),
     &                                      'LONUTI', 0,' ')
C
 110  CONTINUE
C
      CALL ORDIS ( ZI(JNBGR) , NBGR )
C
      IF ( NBGR.GT.3 .AND. YAPASS ) CALL U2MESS('F','POSTRCCM_34')
C
C     ------------------------------------------------------------------
C --- ON AJOUTE 1 GROUPE POUR LES SITUATIONS DE PASSAGE
      IF ( YAPASS ) NBGR = NBGR + 1
C
C     ------------------------------------------------------------------
C --- DEFINITION DES GROUPES
      CALL WKVECT('&&RC3200.SITU_NUME_GROUP', 'V V I', NBGR, JNUMGR )
      CALL WKVECT('&&RC3200.SITU_SEISME'    , 'V V I', NBGR, JSEIGR )
      CALL JECREC('&&RC3200.LES_GROUPES', 'V V I', 'NU',
     &                               'DISPERSE', 'VARIABLE', NBGR )
C
      IF ( YAPASS ) THEN
         NBGRT = NBGR - 1
      ELSE
         NBGRT = NBGR
      ENDIF
      DO 30 IG = 1, NBGRT, 1
C
         NUMGR = ZI(JNBGR+IG-1)
C
         ZI(JNUMGR+IG-1) = NUMGR
C
C ------ ON COMPTE LES SITUATIONS DU GROUPE
         NBSIGR = 0
         DO 32, IOCC = 1, NBSITU, 1
           CALL GETVIS ( MOTCL1, 'NUME_GROUPE',IOCC,IARG,0,NBVG,N1)
           IF ( N1 .NE. 0 ) THEN
             NBVG = -N1
             CALL WKVECT('&&RC32SI.VALE_GR', 'V V I', NBVG, JNBVG )
             CALL GETVIS(MOTCL1,'NUME_GROUPE',IOCC,IARG,NBVG,
     &                   ZI(JNBVG),N1)
             DO 321 ING = 1,NBVG
               IF ( ZI(JNBVG+ING-1) .EQ. NUMGR )  NBSIGR = NBSIGR + 1
 321         CONTINUE
             CALL JEDETR ( '&&RC32SI.VALE_GR' )
           ENDIF
           CALL GETVIS ( MOTCL1, 'NUME_PASSAGE', IOCC,IARG,0, NBVG, N1 )
           IF ( N1 .NE. 0 ) THEN
              CALL GETVIS (MOTCL1,'NUME_PASSAGE',IOCC,IARG,2,
     &                     NUMPAS,N1)
              IF ( NUMPAS(1) .EQ. NUMGR )  NBSIGR = NBSIGR + 1
              IF ( NUMPAS(2) .EQ. NUMGR )  NBSIGR = NBSIGR + 1
            ENDIF
 32      CONTINUE
C
C ------ ON COMPTE LES SITUATIONS DE SEISME
C
         DO 36, IOCC = 1, NBSEIS, 1
           CALL GETVIS ( MOTCL2, 'NUME_GROUPE',IOCC,IARG,1,NUMGS,N1)
           IF ( NUMGS .EQ. NUMGR )  NBSIGR = NBSIGR + 1
 36      CONTINUE
C
C ------ ON STOCKE LE NUMERO DE L'OCCURRENCE
         CALL JECROC (JEXNUM('&&RC3200.LES_GROUPES',NUMGR))
         CALL JEECRA (JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'LONMAX',
     &                                                   NBSIGR,' ')
         CALL JEECRA (JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'LONUTI',
     &                                                   NBSIGR,' ')
         CALL JEVEUO (JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'E',JNSG)
         II = 0
         DO 34, IOCC = 1, NBSITU, 1
           CALL GETVIS ( MOTCL1, 'NUME_GROUPE',IOCC,IARG,0,NBVG,N1)
           IF ( N1 .NE. 0 ) THEN
             NBVG = -N1
             CALL WKVECT('&&RC32SI.VALE_GR', 'V V I', NBVG, JNBVG )
             CALL GETVIS(MOTCL1,'NUME_GROUPE',IOCC,IARG,NBVG,
     &                   ZI(JNBVG),N1)
             DO 341 ING = 1,NBVG
               IF ( ZI(JNBVG+ING-1) .EQ. NUMGR ) THEN
                 II = II + 1
                 ZI(JNSG+II-1) = IOCC
               ENDIF
 341         CONTINUE
             CALL JEDETR ( '&&RC32SI.VALE_GR' )
           ENDIF
           CALL GETVIS ( MOTCL1, 'NUME_PASSAGE', IOCC,IARG,0, NBVG, N1 )
           IF ( N1 .NE. 0 ) THEN
              CALL GETVIS (MOTCL1,'NUME_PASSAGE',IOCC,IARG,2,
     &                     NUMPAS,N1)
              IF ( NUMPAS(1) .EQ. NUMGR )  THEN
                 II = II + 1
                 ZI(JNSG+II-1) = IOCC
              ENDIF
              IF ( NUMPAS(2) .EQ. NUMGR )  THEN
                 II = II + 1
                 ZI(JNSG+II-1) = IOCC
              ENDIF
           ENDIF
 34      CONTINUE
         DO 38, IOCC = 1, NBSEIS, 1
           CALL GETVIS ( MOTCL2, 'NUME_GROUPE',IOCC,IARG,1,NUMGS,N1)
           IF ( NUMGS .EQ. NUMGR ) THEN
             II = II + 1
             ZI(JNSG+II-1) = NBSITU+IOCC
             IF ( ZI(JSEIGR+IG-1) .NE. 0 ) THEN
                VALI(1) = NUMGR
                VALI(2) = IOCC
                VALI(3) = ZI(JSEIGR+IG-1)
                CALL U2MESI('F','POSTRCCM_26',3,VALI)
             ENDIF
             ZI(JSEIGR+IG-1) = IOCC
          ENDIF
 38      CONTINUE
C
 30   CONTINUE
C     ------------------------------------------------------------------
C --- TRAITEMENT DES SITUATIONS DE PASSAGE
      IF ( YAPASS ) THEN
C
         CALL WKVECT ('&&RC32SI.PASSAGE_SIT', 'V V I', 3, JSPAS )
C
         NBSG1 = 0
         NBSG2 = 0
         NBSG3 = 0
         NBP12 = 0
         NBP23 = 0
         NBP13 = 0
         DO 40, IOCC = 1, NDIM, 1
            NUMG1 = ZI(JSIGR+2*IOCC-2)
            NUMG2 = ZI(JSIGR+2*IOCC-1)
            IF ( NUMG1.EQ.1 .AND. NUMG2.EQ.1 ) THEN
               NBSG1 = NBSG1 + 1
            ELSEIF ( NUMG1.EQ.1 .AND. NUMG2.EQ.2 ) THEN
               NBSG1 = NBSG1 + 1
               NBP12 = NBP12 + 1
               ZI(JSP12+NBP12-1) = IOCC
            ELSEIF ( NUMG1.EQ.2 .AND. NUMG2.EQ.2 ) THEN
               NBSG2 = NBSG2 + 1
            ELSEIF ( NUMG1.EQ.2 .AND. NUMG2.EQ.3 ) THEN
               NBSG2 = NBSG2 + 1
               NBP23 = NBP23 + 1
               ZI(JSP23+NBP23-1) = IOCC
            ELSEIF ( NUMG1.EQ.3 .AND. NUMG2.EQ.3 ) THEN
               NBSG3 = NBSG3 + 1
            ELSEIF ( NUMG1.EQ.1 .AND. NUMG2.EQ.3 ) THEN
               NBSG3 = NBSG3 + 1
               NBP13 = NBP13 + 1
               ZI(JSP13+NBP13-1) = IOCC
            ENDIF
 40      CONTINUE
         CALL JEECRA ('&&RC32SI.PASSAGE_1_2','LONUTI', NBP12, ' ')
         CALL JEECRA ('&&RC32SI.PASSAGE_2_3','LONUTI', NBP23, ' ')
         CALL JEECRA ('&&RC32SI.PASSAGE_1_3','LONUTI', NBP13, ' ')
         ZI(JSPAS  ) = NBSG1
         ZI(JSPAS+1) = NBSG2
         ZI(JSPAS+2) = NBSG3
C
         ZI(JNUMGR+NBGR-1) = -NBGR
         CALL JECROC (JEXNUM('&&RC3200.LES_GROUPES',NBGR))
         CALL JEECRA (JEXNUM('&&RC3200.LES_GROUPES',NBGR),'LONMAX',
     &                                                   NDIM,' ')
         CALL JEVEUO (JEXNUM('&&RC3200.LES_GROUPES',NBGR),'E',JNSG)
C
         II = 0
         DO 42, IOCC = 1, NDIM, 1
            NUMG1 = ZI(JSIGR+2*IOCC-2)
            NUMG2 = ZI(JSIGR+2*IOCC-1)
            IF ( NUMG1.EQ.1 .AND. NUMG2.EQ.1 ) THEN
               II = II + 1
               ZI(JNSG+II-1) = IOCC
            ENDIF
 42      CONTINUE
         DO 44, IOCC = 1, NDIM, 1
            NUMG1 = ZI(JSIGR+2*IOCC-2)
            NUMG2 = ZI(JSIGR+2*IOCC-1)
            IF ( NUMG1.EQ.1 .AND. NUMG2.EQ.2 ) THEN
               II = II + 1
               ZI(JNSG+II-1) = IOCC
            ENDIF
 44      CONTINUE
         DO 46, IOCC = 1, NDIM, 1
            NUMG1 = ZI(JSIGR+2*IOCC-2)
            NUMG2 = ZI(JSIGR+2*IOCC-1)
            IF ( NUMG1.EQ.2 .AND. NUMG2.EQ.2 ) THEN
               II = II + 1
               ZI(JNSG+II-1) = IOCC
            ENDIF
 46      CONTINUE
         DO 48, IOCC = 1, NDIM, 1
            NUMG1 = ZI(JSIGR+2*IOCC-2)
            NUMG2 = ZI(JSIGR+2*IOCC-1)
            IF ( NUMG1.EQ.2 .AND. NUMG2.EQ.3 ) THEN
               II = II + 1
               ZI(JNSG+II-1) = IOCC
            ENDIF
 48      CONTINUE
         DO 50, IOCC = 1, NDIM, 1
            NUMG1 = ZI(JSIGR+2*IOCC-2)
            NUMG2 = ZI(JSIGR+2*IOCC-1)
            IF ( NUMG1.EQ.3 .AND. NUMG2.EQ.3 ) THEN
               II = II + 1
               ZI(JNSG+II-1) = IOCC
            ENDIF
 50      CONTINUE
         DO 52, IOCC = 1, NDIM, 1
            NUMG1 = ZI(JSIGR+2*IOCC-2)
            NUMG2 = ZI(JSIGR+2*IOCC-1)
            IF ( NUMG1.EQ.1 .AND. NUMG2.EQ.3 ) THEN
               II = II + 1
               ZI(JNSG+II-1) = IOCC
            ENDIF
 52      CONTINUE
         CALL JEECRA (JEXNUM('&&RC3200.LES_GROUPES',NBGR),'LONUTI',
     &                                                   II,' ')
      ENDIF
C
      CALL JEDETR ( '&&RC32SI.NUME_GROUP' )
C
      END
