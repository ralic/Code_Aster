      SUBROUTINE RC32SI
      IMPLICIT   NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 27/11/2007   AUTEUR VIVAN L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*32     JEXNOM, JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      N1, NBSITU, IOCC, IBID, NUME, I, II, NOCC, NSCY,
     &             JNBOCC, JNUMGR, JPRESA, JPRESB, NBCHAR, JCHAR, IP,
     &             JNSITU, JCOMBI, JNBGR, IG, NUMPAS(2),
     &             NBGR, NUMGR, NBSIGR, JNSG, INSG, NBTH, JSEIGR,
     &             NBM, VALI(3), NBGRT, NUMG1, NUMG2, 
     &             NBSG1, NBSG2, NBSG3, NBP12, NBP23, NBP13,
     &             JSPAS, JSIGR, JSP12, JSP23, JSP13
      LOGICAL      YAPASS, YASEIS
      CHARACTER*8  K8B, KNUME, OUINON
      CHARACTER*16 MOTCLF
C DEB ------------------------------------------------------------------
C
      MOTCLF = 'SITUATION'
C
      CALL GETFAC ( MOTCLF, NBSITU )
C
      CALL WKVECT('&&RC32SI.NUME_GROUP'     , 'V V I', NBSITU,  JNBGR )
      CALL WKVECT('&&RC32SI.SITU_GROUP'     , 'V V I',2*NBSITU, JSIGR )
C
      CALL WKVECT('&&RC3200.SITU_NUMERO'    , 'V V I'  ,NBSITU, JNSITU)
      CALL WKVECT('&&RC3200.SITU_NB_OCCUR'  , 'V V I',2*NBSITU, JNBOCC)
      CALL WKVECT('&&RC3200.SITU_PRES_A'    , 'V V R'  ,NBSITU, JPRESA)
      CALL WKVECT('&&RC3200.SITU_PRES_B'    , 'V V R'  ,NBSITU, JPRESB)
      CALL WKVECT('&&RC3200.SITU_COMBINABLE', 'V V L'  ,NBSITU, JCOMBI)
      CALL JECREC('&&RC3200.SITU_ETAT_A'    , 'V V I', 'NO',
     &                               'DISPERSE', 'VARIABLE', NBSITU )
      CALL JECREC('&&RC3200.SITU_ETAT_B'    , 'V V I', 'NO',
     &                               'DISPERSE', 'VARIABLE', NBSITU )
      CALL JECREC('&&RC3200.SITU_THERMIQUE' , 'V V I', 'NO',
     &                               'DISPERSE', 'VARIABLE', NBSITU )
C
      CALL WKVECT ('&&RC32SI.PASSAGE_1_2', 'V V I', NBSITU, JSP12 )
      CALL WKVECT ('&&RC32SI.PASSAGE_2_3', 'V V I', NBSITU, JSP23 )
      CALL WKVECT ('&&RC32SI.PASSAGE_1_3', 'V V I', NBSITU, JSP13 )
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
         CALL GETVIS ( MOTCLF, 'NUME_SITU', IOCC,1,1, NUME, N1 )
         ZI(JNSITU+IOCC-1) = NUME
         KNUME = 'S       '
         CALL CODENT ( NUME , 'D0' , KNUME(2:8)  )
C
C ------ LE NOMBRE D'OCCURRENCE ET LE NOMBRE DE SOUS-CYCLE:
C        --------------------------------------------------
C
         CALL GETVIS ( MOTCLF, 'NB_OCCUR', IOCC,1,1, NOCC, N1 )
         ZI(JNBOCC+2*IOCC-2) = NOCC
         NSCY = 0
         CALL GETVIS ( MOTCLF, 'NB_CYCL_SEISME', IOCC,1,1, NSCY, N1 )
         ZI(JNBOCC+2*IOCC-1) = NSCY
C
C ------ LES PRESSIONS:
C        --------------
C
         CALL GETVR8 ( MOTCLF,'PRES_A', IOCC,1,1, ZR(JPRESA+IOCC-1),N1)
         CALL GETVR8 ( MOTCLF,'PRES_B', IOCC,1,1, ZR(JPRESB+IOCC-1),N1)
C
C ------ LE NUMERO DE GROUPE:
C        --------------------
C
         CALL GETVIS ( MOTCLF, 'NUME_GROUPE', IOCC,1,1, NUMGR, N1)
C
         IF ( NUMGR .LE. 0 ) CALL U2MESS('F','POSTRCCM_12')
         DO 20 IG = 1 , NBGR
            IF ( ZI(JNBGR+IG-1) .EQ. NUMGR ) GOTO 22
 20      CONTINUE
         NBGR = NBGR + 1
         ZI(JNBGR+NBGR-1) = NUMGR
 22      CONTINUE
C
C ------ LES NUMEROS DE PASSAGE:
C        -----------------------
C
        CALL GETVIS ( MOTCLF, 'NUME_PASSAGE', IOCC,1,2, NUMPAS, N1 )
        IF ( N1 .NE. 0 ) THEN
           YAPASS = .TRUE.
           ZI(JSIGR+2*IOCC-2)  = MIN ( NUMPAS(1), NUMPAS(2) )
           ZI(JSIGR+2*IOCC-1)  = MAX ( NUMPAS(1), NUMPAS(2) )
        ELSE
           ZI(JSIGR+2*IOCC-2) = NUMGR
           ZI(JSIGR+2*IOCC-1) = NUMGR
        ENDIF
C
C ------ COMBINABLE DANS SON GROUPE:
C        ---------------------------
C
         CALL GETVTX ( MOTCLF, 'COMBINABLE', IOCC,1,1, OUINON, N1 )
         IF ( OUINON(1:3) .EQ. 'OUI' ) THEN
            ZL(JCOMBI+IOCC-1) = .TRUE.
         ELSE
            ZL(JCOMBI+IOCC-1) = .FALSE.
         ENDIF
C
C ------ ETAT DE CHARGEMENT POUR "A":
C        ----------------------------
C
         CALL GETVIS ( MOTCLF, 'CHAR_ETAT_A', IOCC,1,0, IBID, N1 )
         NBCHAR = -N1
         CALL JECROC (JEXNOM('&&RC3200.SITU_ETAT_A',KNUME))
         CALL JEECRA (JEXNOM('&&RC3200.SITU_ETAT_A',KNUME),
     &                                          'LONMAX', NBCHAR, ' ' )
         CALL JEECRA (JEXNOM('&&RC3200.SITU_ETAT_A',KNUME),
     &                                          'LONUTI', NBCHAR, ' ' )
         CALL JEVEUO (JEXNOM('&&RC3200.SITU_ETAT_A',KNUME),'E', JCHAR )
         CALL GETVIS ( MOTCLF,'CHAR_ETAT_A',IOCC,1,NBCHAR,ZI(JCHAR),N1)
C
C ------ ETAT DE CHARGEMENT POUR "B":
C        ----------------------------
C
         CALL GETVIS ( MOTCLF, 'CHAR_ETAT_B', IOCC,1,0, IBID, N1 )
         NBCHAR = -N1
         CALL JECROC (JEXNOM('&&RC3200.SITU_ETAT_B',KNUME))
         CALL JEECRA (JEXNOM('&&RC3200.SITU_ETAT_B',KNUME),
     &                                          'LONMAX', NBCHAR, ' ' )
         CALL JEECRA (JEXNOM('&&RC3200.SITU_ETAT_B',KNUME),
     &                                          'LONUTI', NBCHAR, ' ' )
         CALL JEVEUO (JEXNOM('&&RC3200.SITU_ETAT_B',KNUME),'E', JCHAR )
         CALL GETVIS ( MOTCLF,'CHAR_ETAT_B',IOCC,1,NBCHAR,ZI(JCHAR),N1)
C
C ------ TRANSITOIRE THERMIQUE ASSOCIE A LA SITUATION:
C        ---------------------------------------------
C
         CALL GETVIS ( MOTCLF, 'NUME_RESU_THER', IOCC,1,0, IBID, N1 )
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
            CALL GETVIS ( MOTCLF, 'NUME_RESU_THER', IOCC,1,
     &                                            NBTH,ZI(JCHAR), N1 )
         ENDIF
C
 10   CONTINUE
C
      CALL ORDIS ( ZI(JNBGR) , NBGR )
C
      IF ( NBGR.GT.2 .AND. YAPASS ) CALL U2MESS('F','POSTRCCM_34')
C
C     ------------------------------------------------------------------
C --- ON AJOUTE 1 GROUPE POUR LES SITUATIONS DE PASSAGE
C
      IF ( YAPASS ) NBGR = NBGR + 1
C
C     ------------------------------------------------------------------
C --- DEFINITION DES GROUPES
C
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
C
         NBSIGR = 0
         DO 32, IOCC = 1, NBSITU, 1
            CALL GETVIS ( MOTCLF, 'NUME_GROUPE', IOCC,1,1, INSG, N1 )
            IF ( INSG .EQ. NUMGR )  THEN
               NBSIGR = NBSIGR + 1
            ELSE
               CALL GETVIS ( MOTCLF, 'NUME_PASSAGE',IOCC,1,2,NUMPAS,N1)
               IF ( N1 .NE. 0 ) THEN
                  IF ( NUMPAS(1) .EQ. NUMGR )  NBSIGR = NBSIGR + 1
                  IF ( NUMPAS(2) .EQ. NUMGR )  NBSIGR = NBSIGR + 1
              ENDIF
           ENDIF
 32      CONTINUE
C
C ------ ON STOCKE LE NUMERO DE L'OCCURRENCE
C
         CALL JECROC (JEXNUM('&&RC3200.LES_GROUPES',NUMGR))
         CALL JEECRA (JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'LONMAX',
     &                                                   NBSIGR,' ')
         CALL JEECRA (JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'LONUTI',
     &                                                   NBSIGR,' ')
         CALL JEVEUO (JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'E',JNSG)
         II = 0
         DO 34, IOCC = 1, NBSITU, 1
            CALL GETVIS ( MOTCLF, 'NUME_GROUPE', IOCC,1,1, INSG, N1 )
            IF ( INSG .EQ. NUMGR ) THEN
               II = II + 1
               ZI(JNSG+II-1) = IOCC
C
C ------------ A-T-ON UN SEISME DANS CE GROUPE ?
               CALL GETVIS ( MOTCLF, 'NB_CYCL_SEISME',IOCC,1,1,NSCY,N1)
               IF ( N1 .NE. 0 ) THEN
                  IF ( ZI(JSEIGR+IG-1) .NE. 0 ) THEN
                     VALI(1) = NUMGR
                     VALI(2) = IOCC
                     VALI(3) = ZI(JSEIGR+IG-1)
                     CALL U2MESI('F','POSTRCCM_26',3,VALI)
                  ENDIF
                  ZI(JSEIGR+IG-1) = IOCC
               ENDIF
            ELSE
               CALL GETVIS ( MOTCLF, 'NUME_PASSAGE',IOCC,1,2,NUMPAS,N1)
               IF ( N1 .NE. 0 ) THEN
                  IF ( NUMPAS(1) .EQ. NUMGR )  THEN
                     II = II + 1
                     ZI(JNSG+II-1) = IOCC
                  ENDIF
                  IF ( NUMPAS(2) .EQ. NUMGR )  THEN
                     II = II + 1
                     ZI(JNSG+II-1) = IOCC
                  ENDIF
              ENDIF
            ENDIF
 34      CONTINUE
C
 30   CONTINUE
C     ------------------------------------------------------------------
C --- TRAITEMENT DES SITUATIONS DE PASSAGE
C
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
         YASEIS = .FALSE.
         DO 40, IOCC = 1, NBSITU, 1
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
            CALL GETVIS ( MOTCLF, 'NB_CYCL_SEISME',IOCC,1,1,NSCY,N1)
            IF ( N1 .NE. 0 ) THEN
               ZI(JSEIGR+NBGR-1) = IOCC
               IF ( YASEIS ) CALL U2MESS('F','POSTRCCM_35')
               YASEIS = .TRUE.
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
     &                                                   NBSITU,' ')
         CALL JEVEUO (JEXNUM('&&RC3200.LES_GROUPES',NBGR),'E',JNSG)
C
         II = 0
         DO 42, IOCC = 1, NBSITU, 1
            NUMG1 = ZI(JSIGR+2*IOCC-2)
            NUMG2 = ZI(JSIGR+2*IOCC-1)
            IF ( NUMG1.EQ.1 .AND. NUMG2.EQ.1 ) THEN
               II = II + 1
               ZI(JNSG+II-1) = IOCC
            ENDIF
 42      CONTINUE
         DO 44, IOCC = 1, NBSITU, 1
            NUMG1 = ZI(JSIGR+2*IOCC-2)
            NUMG2 = ZI(JSIGR+2*IOCC-1)
            IF ( NUMG1.EQ.1 .AND. NUMG2.EQ.2 ) THEN
               II = II + 1
               ZI(JNSG+II-1) = IOCC
            ENDIF
 44      CONTINUE
         DO 46, IOCC = 1, NBSITU, 1
            NUMG1 = ZI(JSIGR+2*IOCC-2)
            NUMG2 = ZI(JSIGR+2*IOCC-1)
            IF ( NUMG1.EQ.2 .AND. NUMG2.EQ.2 ) THEN
               II = II + 1
               ZI(JNSG+II-1) = IOCC
            ENDIF
 46      CONTINUE
         DO 48, IOCC = 1, NBSITU, 1
            NUMG1 = ZI(JSIGR+2*IOCC-2)
            NUMG2 = ZI(JSIGR+2*IOCC-1)
            IF ( NUMG1.EQ.2 .AND. NUMG2.EQ.3 ) THEN
               II = II + 1
               ZI(JNSG+II-1) = IOCC
            ENDIF
 48      CONTINUE
         DO 50, IOCC = 1, NBSITU, 1
            NUMG1 = ZI(JSIGR+2*IOCC-2)
            NUMG2 = ZI(JSIGR+2*IOCC-1)
            IF ( NUMG1.EQ.3 .AND. NUMG2.EQ.3 ) THEN
               II = II + 1
               ZI(JNSG+II-1) = IOCC
            ENDIF
 50      CONTINUE
         DO 52, IOCC = 1, NBSITU, 1
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
