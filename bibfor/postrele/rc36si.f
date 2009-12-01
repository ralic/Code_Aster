      SUBROUTINE RC36SI ( NOMA, NBMA, LISTMA )
      IMPLICIT   NONE
      INTEGER             NBMA, LISTMA(*)
      CHARACTER*8         NOMA
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 24/03/2009   AUTEUR GALENNE E.GALENNE 
C TOLE CRP_20 CRP_21
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
C
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
C     RECUPERATION DES DONNEES DE "SITUATION"
C
C IN  : NOMA   : MAILLAGE
C IN  : NBMA   : NOMBRE DE MAILLES D'ANALYSE
C IN  : LISTMA : LISTE DES MAILLES D'ANALYSE
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
      INTEGER      N1, NBSITU, IOCC, IBID, JMOMEA,JMOMEB,II,NOCC,JRETH,
     &             JNBOCC, JNUMGR, JPRESA, JPRESB,NBCHAR,JCHAR1,JCHAR2,
     &             JNSITU, JCOMBI, JPASSA, JNBGR, IG, NUMPAS(2), NSCY,
     &             NBGR, NUMGR, NBSIGR, JNSG, NBTH, JSEIGR,JCHTH,NUME,
     &             NBM, NBP12, NBP23, NBP13, JSP12, JSP23, JSP13,
     &             NBSG1, NBSG2, NBSG3, JSIGR, VALI(3), NBGRT, NUMG1,
     &             NUMG2,JSPAS, ING, JNBVG, NBVG, NDIM, NUMGS,NBSEIS 
      LOGICAL      YAPASS
      CHARACTER*8  K8B, OUINON
      CHARACTER*16 MOTCL1, MOTCL2
      CHARACTER*24 CHMOME
C DEB ------------------------------------------------------------------

      MOTCL1 = 'SITUATION'
      MOTCL2 = 'SEISME'
C
      CALL GETFAC ( MOTCL1, NBSITU )
      CALL GETFAC ( MOTCL2, NBSEIS )
C
      NDIM = NBSITU + NBSEIS
      CALL WKVECT('&&RC36SI.NUME_GROUP', 'V V I',   NDIM, JNBGR )
      CALL WKVECT('&&RC32SI.SITU_GROUP', 'V V I', 2*NDIM, JSIGR )
C
      CALL WKVECT('&&RC3600.SITU_NUMERO'    , 'V V I',  NDIM, JNSITU)
      CALL WKVECT('&&RC3600.SITU_NB_OCCUR'  , 'V V I',2*NDIM, JNBOCC)
      CALL WKVECT('&&RC3600.SITU_PRES_A'    , 'V V R',NBSITU, JPRESA)
      CALL WKVECT('&&RC3600.SITU_PRES_B'    , 'V V R',NBSITU, JPRESB)
      CALL WKVECT('&&RC3600.SITU_COMBINABLE', 'V V L',  NDIM, JCOMBI)
      CALL WKVECT('&&RC3600.SITU_PASSAGE'   , 'V V I',2*NBSITU, JPASSA)
      CALL WKVECT('&&RC3600.SITU_MOMENT_A'  , 'V V K24',  NDIM, JMOMEA)
      CALL WKVECT('&&RC3600.SITU_MOMENT_B'  , 'V V K24',NBSITU, JMOMEB)
      CALL JECREC('&&RC3600.SITU_THERMIQUE' , 'V V I'  , 'NU',
     &                               'DISPERSE', 'VARIABLE', NDIM )
      CALL WKVECT('&&RC3600.CHAM_THER', 'V V K24', NDIM, JCHTH )
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
C
C ------ LE NUMERO DE SITUATION:
C        -----------------------
C
         CALL GETVIS (MOTCL1,'NUME_SITU',IOCC,1,1,ZI(JNSITU+IOCC-1),N1)
C
C
C ------ LE NOMBRE D'OCCURRENCE:
C        -----------------------
C
         CALL GETVIS ( MOTCL1, 'NB_OCCUR', IOCC,1,1, NOCC, N1 )
         ZI(JNBOCC+2*IOCC-2) = NOCC
C
C
C ------ LES PRESSIONS:
C        --------------
C
         CALL GETVR8 ( MOTCL1,'PRES_A', IOCC,1,1, ZR(JPRESA+IOCC-1),N1)
         CALL GETVR8 ( MOTCL1,'PRES_B', IOCC,1,1, ZR(JPRESB+IOCC-1),N1)
C
C
C ------ LES NUMEROS DE GROUPE:
C        ----------------------
C
        CALL GETVIS ( MOTCL1, 'NUME_GROUPE', IOCC,1,0, NUMPAS, N1 )
        IF ( N1 .NE. 0 ) THEN
           NBVG = -N1
           CALL WKVECT('&&RC36SI.VALE_GR', 'V V I', NBVG, JNBVG )
           CALL GETVIS (MOTCL1,'NUME_GROUPE', IOCC,1,NBVG,ZI(JNBVG),N1)
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
           CALL JEDETR ( '&&RC36SI.VALE_GR' )
        ENDIF
C
C ------ LES NUMEROS DE PASSAGE:
C        -----------------------
C
        CALL GETVIS ( MOTCL1, 'NUME_PASSAGE', IOCC,1,0, NUMPAS, N1 )
        IF ( N1 .NE. 0 ) THEN
           CALL GETVIS ( MOTCL1, 'NUME_PASSAGE', IOCC,1,2, NUMPAS, N1 )
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
C
C ------ COMBINABLE DANS SON GROUPE:
C        ---------------------------
C
         CALL GETVTX ( MOTCL1, 'COMBINABLE', IOCC,1,1, OUINON, N1 )
         IF ( OUINON(1:3) .EQ. 'OUI' ) THEN
            ZL(JCOMBI+IOCC-1) = .TRUE.
         ELSE
            ZL(JCOMBI+IOCC-1) = .FALSE.
         ENDIF
C
C
C ------ ETAT DE CHARGEMENT POUR "A":
C        ----------------------------
C
         CALL GETVIS ( MOTCL1, 'CHAR_ETAT_A', IOCC,1,0, IBID, N1 )
         NBCHAR = -N1
         CALL WKVECT ( '&&RC36SI.CHAR_ETAT', 'V V I', NBCHAR, JCHAR1 )
         CALL GETVIS ( MOTCL1,'CHAR_ETAT_A',IOCC,1,NBCHAR,ZI(JCHAR1),N1)
C
         CHMOME = '&&RC36SI_A'//K8B
         CALL RC36CM ( IOCC, 'A', NBMA, LISTMA,
     &                                     NBCHAR, ZI(JCHAR1), CHMOME )
         ZK24(JMOMEA+IOCC-1) = CHMOME
         CALL JEDETR ( '&&RC36SI.CHAR_ETAT' )
C
C
C ------ ETAT DE CHARGEMENT POUR "B":
C        ----------------------------
C
         CALL GETVIS ( MOTCL1, 'CHAR_ETAT_B', IOCC,1,0, IBID, N1 )
         NBCHAR = -N1
         CALL WKVECT ( '&&RC36SI.CHAR_MECA', 'V V I', NBCHAR, JCHAR2 )
         CALL GETVIS ( MOTCL1,'CHAR_ETAT_B',IOCC,1,NBCHAR,ZI(JCHAR2),N1)
C
         CHMOME = '&&RC36SI_B'//K8B
         CALL RC36CM ( IOCC, 'B', NBMA, LISTMA,
     &                                     NBCHAR, ZI(JCHAR2), CHMOME )
         ZK24(JMOMEB+IOCC-1) = CHMOME
         CALL JEDETR ( '&&RC36SI.CHAR_MECA' )
C
C
C ------ TRANSITOIRE THERMIQUE ASSOCIE A LA SITUATION:
C        ---------------------------------------------
C
         CALL GETVIS ( MOTCL1, 'NUME_RESU_THER', IOCC,1,0, IBID, N1 )
         NBTH = -N1
         CALL JECROC (JEXNUM('&&RC3600.SITU_THERMIQUE',IOCC))
         NBM = MAX(1,NBTH)
         CALL JEECRA (JEXNUM('&&RC3600.SITU_THERMIQUE',IOCC),
     &                                      'LONMAX',NBM ,' ')
C
         IF ( NBTH .EQ. 0 ) THEN
            CALL JEECRA (JEXNUM('&&RC3600.SITU_THERMIQUE',IOCC),
     &                                      'LONUTI', 0,' ')
         ELSE
            CALL JEECRA (JEXNUM('&&RC3600.SITU_THERMIQUE',IOCC),
     &                                              'LONUTI', NBTH,' ')
            CALL JEVEUO (JEXNUM('&&RC3600.SITU_THERMIQUE',IOCC),'E',
     &                                                          JRETH )
            CALL GETVIS ( MOTCL1, 'NUME_RESU_THER', IOCC,1,
     &                                            NBTH,ZI(JRETH), N1 )
C     ------------------------------------------------------------------
C                   RESULTATS DES CALCULS THERMIQUES
C     ------------------------------------------------------------------
C
          CALL RC36TH (NOMA,NBMA,LISTMA,ZK24(JCHTH),IOCC,NBTH,ZI(JRETH))
         ENDIF
C
 10   CONTINUE
C
      DO 110, IOCC = 1, NBSEIS, 1
C
         CALL CODENT ( NBSITU+IOCC , 'D0' , K8B  )
C
         CALL GETVIS ( MOTCL2, 'NUME_GROUPE', IOCC,1,1, NUME, N1 )
         ZI(JSIGR+2*(NBSITU+IOCC)-2) = NUME
         ZI(JSIGR+2*(NBSITU+IOCC)-1) = NUME
C
         ZL(JCOMBI+NBSITU+IOCC-1) = .TRUE.
C
C
C ------ LE NUMERO DE SITUATION:
C        -----------------------
C
         CALL GETVIS (MOTCL2,'NUME_SITU',IOCC,1,1,
     +                                   ZI(JNSITU+NBSITU+IOCC-1),N1)
C
C
C ------ LE NOMBRE D'OCCURRENCE:
C        -----------------------
C
         CALL GETVIS ( MOTCL2, 'NB_OCCUR', IOCC,1,1, NOCC, N1 )
         ZI(JNBOCC+2*(NBSITU+IOCC)-2) = NOCC
         CALL GETVIS ( MOTCL2, 'NB_CYCL_SEISME', IOCC,1,1, NSCY, N1 )
         ZI(JNBOCC+2*(NBSITU+IOCC)-1) = NSCY
C
C
C ------ ETAT DE CHARGEMENT:
C        -------------------
C
         CALL GETVIS ( MOTCL2, 'CHAR_ETAT', IOCC,1,0, IBID, N1 )
         NBCHAR = -N1
         CALL WKVECT ( '&&RC36SI.CHAR_ETAT', 'V V I', NBCHAR, JCHAR1 )
         CALL GETVIS ( MOTCL2,'CHAR_ETAT',IOCC,1,NBCHAR,ZI(JCHAR1),N1)
C
         CHMOME = '&&RC36SI_A'//K8B
         CALL RC36CM ( IOCC, 'S', NBMA, LISTMA,
     &                                     NBCHAR, ZI(JCHAR1), CHMOME )
         ZK24(JMOMEA+NBSITU+IOCC-1) = CHMOME
         CALL JEDETR ( '&&RC36SI.CHAR_ETAT' )
C
C
C ------ TRANSITOIRE THERMIQUE ASSOCIE A LA SITUATION:
C        ---------------------------------------------
C
         NBTH = 0
         CALL JECROC (JEXNUM('&&RC3600.SITU_THERMIQUE',NBSITU+IOCC))
         NBM = MAX(1,NBTH)
         CALL JEECRA (JEXNUM('&&RC3600.SITU_THERMIQUE',NBSITU+IOCC),
     &                                      'LONMAX',NBM ,' ')
C
         CALL JEECRA (JEXNUM('&&RC3600.SITU_THERMIQUE',NBSITU+IOCC),
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
C
      IF ( YAPASS ) NBGR = NBGR + 1
C
C     ------------------------------------------------------------------
C --- DEFINITION DES GROUPES
C
      CALL WKVECT('&&RC3600.SITU_NUME_GROUP', 'V V I', NBGR, JNUMGR )
      CALL WKVECT('&&RC3600.SITU_SEISME'    , 'V V I', NBGR, JSEIGR )
      CALL JECREC('&&RC3600.LES_GROUPES', 'V V I', 'NU',
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
           CALL GETVIS ( MOTCL1, 'NUME_GROUPE',IOCC,1,0,NBVG,N1)
           IF ( N1 .NE. 0 ) THEN
             NBVG = -N1
             CALL WKVECT('&&RC36SI.VALE_GR', 'V V I', NBVG, JNBVG )
             CALL GETVIS(MOTCL1,'NUME_GROUPE',IOCC,1,NBVG,ZI(JNBVG),N1)
             DO 321 ING = 1,NBVG
               IF ( ZI(JNBVG+ING-1) .EQ. NUMGR )  NBSIGR = NBSIGR + 1
 321         CONTINUE
             CALL JEDETR ( '&&RC36SI.VALE_GR' )
           ENDIF
           CALL GETVIS ( MOTCL1, 'NUME_PASSAGE', IOCC,1,0, NBVG, N1 )
           IF ( N1 .NE. 0 ) THEN
              CALL GETVIS ( MOTCL1, 'NUME_PASSAGE',IOCC,1,2,NUMPAS,N1)
              IF ( NUMPAS(1) .EQ. NUMGR )  NBSIGR = NBSIGR + 1
              IF ( NUMPAS(2) .EQ. NUMGR )  NBSIGR = NBSIGR + 1
            ENDIF
 32      CONTINUE
C
C ------ ON COMPTE LES SITUATIONS DE SEISME
C
         DO 36, IOCC = 1, NBSEIS, 1
           CALL GETVIS ( MOTCL2, 'NUME_GROUPE',IOCC,1,1,NUMGS,N1)
           IF ( NUMGS .EQ. NUMGR )  NBSIGR = NBSIGR + 1
 36      CONTINUE
C
C ------ ON STOCKE LE NUMERO DE L'OCCURRENCE
C
         CALL JECROC (JEXNUM('&&RC3600.LES_GROUPES',NUMGR))
         CALL JEECRA (JEXNUM('&&RC3600.LES_GROUPES',NUMGR),'LONMAX',
     &                                                   NBSIGR,' ')
         CALL JEVEUO (JEXNUM('&&RC3600.LES_GROUPES',NUMGR),'E',JNSG)
         II = 0
         DO 34, IOCC = 1, NBSITU, 1
           CALL GETVIS ( MOTCL1, 'NUME_GROUPE',IOCC,1,0,NBVG,N1)
           IF ( N1 .NE. 0 ) THEN
             NBVG = -N1
             CALL WKVECT('&&RC36SI.VALE_GR', 'V V I', NBVG, JNBVG )
             CALL GETVIS(MOTCL1,'NUME_GROUPE',IOCC,1,NBVG,ZI(JNBVG),N1)
             DO 341 ING = 1,NBVG
               IF ( ZI(JNBVG+ING-1) .EQ. NUMGR ) THEN
                 II = II + 1
                 ZI(JNSG+II-1) = IOCC
               ENDIF
 341         CONTINUE
             CALL JEDETR ( '&&RC36SI.VALE_GR' )
           ENDIF
           CALL GETVIS ( MOTCL1, 'NUME_PASSAGE', IOCC,1,0, NBVG, N1 )
           IF ( N1 .NE. 0 ) THEN
              CALL GETVIS ( MOTCL1, 'NUME_PASSAGE',IOCC,1,2,NUMPAS,N1)
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
           CALL GETVIS ( MOTCL2, 'NUME_GROUPE',IOCC,1,1,NUMGS,N1)
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
         CALL JECROC (JEXNUM('&&RC3600.LES_GROUPES',NBGR))
         CALL JEECRA (JEXNUM('&&RC3600.LES_GROUPES',NBGR),'LONMAX',
     &                                                    NDIM,' ')
         CALL JEVEUO (JEXNUM('&&RC3600.LES_GROUPES',NBGR),'E',JNSG)
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
         CALL JEECRA (JEXNUM('&&RC3600.LES_GROUPES',NBGR),'LONUTI',
     &                                                   II,' ')
      ENDIF
C
      CALL JEDETR ( '&&RC36SI.NUME_GROUP' )
C
      END
