      SUBROUTINE UTDIDT(GETSET,SDDISC,TYPQUE,IOCC  ,QUEST,
     &                  VALR  ,VALI  ,VALK  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/10/2012   AUTEUR DESOZA T.DESOZA 
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
C TOLE CRP_20
C RESPONSABLE GENIAUT S.GENIAUT
C
      IMPLICIT      NONE
      INCLUDE 'jeveux.h'
      INTEGER       IOCC,VALI
      REAL*8        VALR
      CHARACTER*1   GETSET
      CHARACTER*4   TYPQUE
      CHARACTER*(*) QUEST,VALK
      CHARACTER*19  SDDISC
C
C ----------------------------------------------------------------------
C
C
C ROUTINE UTILITAIRE SUR LA DISCRETISATION TEMPORELLE
C   ACCES AUX SD LOCALES !! ET NON A LA SD DE L'OPERATEUR DEFI_LIST_INST
C   MAIS L'ARCHITECTURE ETANT LA MEME, LES OBJETS DOIVENT ETRE EN
C   CONFORMITE AVEC LA ROUTINE OP0028 (DEFI_LIST_INST)
C
C ----------------------------------------------------------------------
C
C IN  GETSET : 'L' -> LECTURE
C              'E' -> ECRITURE
C IN  SDDISC : SDDISC LOCALE A OP00700
C IN  TYPQUE : TYPE DE DEMANDE (LIST, ECHE OU ADAP)
C IN  IOCC   : NUMERO OCCURRENCE (POUR ECHEC/ADAPT)
C IN  QUEST  : QUESTION
C I/O VALI   : VALEUR ENTIERE
C I/O VALR   : VALEUR REELLE
C I/O VALK   : VALEUR CHAINE
C
C
C
C
C
      INTEGER      DFLLVD,LEEVR ,LEEVK ,LESUR ,LAEVR ,LATPR ,LATPK
      INTEGER      IECHEC,IADAPT
      CHARACTER*24 TPSINF,TPSRPC,TPSPIL
      INTEGER      JLINR ,JREAPC,JPIL
      CHARACTER*24 TPSEVR,TPSEVK,TPSESU
      INTEGER      JEEVR ,JEEVK ,JESUR
      CHARACTER*24 TPSAVR,TPSAVK,TPSTPR,TPSTPK
      INTEGER      JAEVR ,JAEVK ,JATPR ,JATPK
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      IF (GETSET.EQ.'L') THEN
        VALK = ' '
        VALI = 0
        VALR = 0.D0
      ENDIF
C
C --- TAILLE DES VECTEURS
C
      LEEVR  = DFLLVD('LEEVR')
      LEEVK  = DFLLVD('LEEVK')
      LESUR  = DFLLVD('LESUR')
      LAEVR  = DFLLVD('LAEVR')
      LATPR  = DFLLVD('LATPR')
      LATPK  = DFLLVD('LATPK')
C
      CALL ASSERT(TYPQUE.EQ.'LIST'.OR.
     &            TYPQUE.EQ.'ECHE'.OR.
     &            TYPQUE.EQ.'ADAP')

      CALL ASSERT(GETSET.EQ.'L'.OR.GETSET.EQ.'E')

C     ------------------------------------------------------------------
C                     QUESTION SUR LA LISTE
C     ------------------------------------------------------------------

      IF (TYPQUE.EQ.'LIST') THEN
        TPSINF = SDDISC(1:19)//'.LINF'
        CALL JEVEUO(TPSINF,GETSET,JLINR )

        IF (QUEST.EQ.'METHODE') THEN
          IF (GETSET.EQ.'L') THEN
            VALI = NINT(ZR(JLINR-1+1))
            IF (VALI.EQ.1) VALK = 'MANUEL'
            IF (VALI.EQ.2) VALK = 'AUTO'
          ELSEIF (GETSET.EQ.'E') THEN
            IF (VALK.EQ.'MANUEL') THEN
              ZR(JLINR-1+1) = 1
            ELSEIF (VALK.EQ.'AUTO') THEN
              ZR(JLINR-1+1) = 2
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
          ENDIF

        ELSEIF (QUEST.EQ.'PAS_MINI') THEN
          IF (GETSET.EQ.'L') THEN
            VALR = ZR(JLINR-1+2)
          ELSEIF (GETSET.EQ.'E') THEN
            ZR(JLINR-1+2) = VALR
          ENDIF

        ELSEIF (QUEST.EQ.'PAS_MAXI') THEN
          IF (GETSET.EQ.'L') THEN
            VALR = ZR(JLINR-1+3)
          ELSEIF (GETSET.EQ.'E') THEN
            ZR(JLINR-1+3) = VALR
          ENDIF

        ELSEIF (QUEST.EQ.'NB_PAS_MAXI') THEN
          IF (GETSET.EQ.'L') THEN
            VALI = NINT(ZR(JLINR-1+4))
          ELSEIF (GETSET.EQ.'E') THEN
            ZR(JLINR-1+4) = VALI
          ENDIF

        ELSEIF (QUEST.EQ.'DTMIN') THEN
          IF (GETSET.EQ.'L') THEN
            VALR = ZR(JLINR-1+5)
          ELSEIF (GETSET.EQ.'E') THEN
            ZR(JLINR-1+5) = VALR
          ENDIF

        ELSEIF (QUEST.EQ.'DT-') THEN
          IF (GETSET.EQ.'L') THEN
            VALR = ZR(JLINR-1+6)
          ELSEIF (GETSET.EQ.'E') THEN
            ZR(JLINR-1+6) = VALR
          ENDIF

        ELSEIF (QUEST.EQ.'EXIS_DECOUPE') THEN
          IF (GETSET.EQ.'L') THEN
            VALI = NINT(ZR(JLINR-1+7))
            IF (VALI.EQ.0) VALK = 'NON'
            IF (VALI.EQ.1) VALK = 'OUI'
          ELSEIF (GETSET.EQ.'E') THEN
            IF (VALK.EQ.'NON') THEN
              ZR(JLINR-1+7) = 0
            ELSEIF (VALK.EQ.'OUI') THEN
              ZR(JLINR-1+7) = 1
            ELSE
              WRITE(6,*) 'VALK: ',VALK
              CALL ASSERT(.FALSE.)
            ENDIF
          ENDIF

        ELSEIF (QUEST.EQ.'EXIS_REAC_PRECOND') THEN
          IF (GETSET.EQ.'L') THEN
            VALI = NINT(ZR(JLINR-1+11))
            IF (VALI.EQ.0) VALK = 'NON'
            IF (VALI.EQ.1) VALK = 'OUI'
          ELSEIF (GETSET.EQ.'E') THEN
            IF (VALK.EQ.'NON') THEN
              ZR(JLINR-1+11) = 0
            ELSEIF (VALK.EQ.'OUI') THEN
              ZR(JLINR-1+11) = 1
            ELSE
              WRITE(6,*) 'VALK: ',VALK
              CALL ASSERT(.FALSE.)
            ENDIF
          ENDIF

        ELSEIF (QUEST.EQ.'NBINST') THEN
          IF (GETSET.EQ.'L') THEN
            VALI = NINT(ZR(JLINR-1+8))
          ELSEIF (GETSET.EQ.'E') THEN
            ZR(JLINR-1+8) = VALI
          ENDIF

        ELSEIF (QUEST.EQ.'NECHEC') THEN
          IF (GETSET.EQ.'L') THEN
            VALI = NINT(ZR(JLINR-1+9))
          ELSEIF (GETSET.EQ.'E') THEN
            ZR(JLINR-1+9) = VALI
          ENDIF

        ELSEIF (QUEST.EQ.'NADAPT') THEN
          IF (GETSET.EQ.'L') THEN
            VALI = NINT(ZR(JLINR-1+10))
          ELSEIF (GETSET.EQ.'E') THEN
            ZR(JLINR-1+10) = VALI
          ENDIF

        ELSE
          CALL ASSERT(.FALSE.)

        ENDIF

C     ------------------------------------------------------------------
C                     QUESTION SUR L'ECHEC
C     ------------------------------------------------------------------

      ELSEIF (TYPQUE.EQ.'ECHE') THEN
        TPSEVR = SDDISC(1:19)//'.EEVR'
        TPSEVK = SDDISC(1:19)//'.EEVK'
        TPSESU = SDDISC(1:19)//'.ESUR'
        TPSRPC = SDDISC(1:19)//'.REPC'
        TPSPIL = SDDISC(1:19)//'.EPIL'
        CALL JEVEUO(TPSEVR,GETSET,JEEVR )
        CALL JEVEUO(TPSEVK,GETSET,JEEVK )
        CALL JEVEUO(TPSESU,GETSET,JESUR )
        CALL JEVEUO(TPSRPC,GETSET,JREAPC)
        CALL JEVEUO(TPSPIL,GETSET,JPIL  )
        IECHEC = IOCC

        IF (QUEST.EQ.'NOM_EVEN') THEN
          IF (GETSET.EQ.'L') THEN

            VALI = NINT(ZR(JEEVR-1+LEEVR*(IECHEC-1)+1))

            IF (VALI.EQ.0) VALK = 'ERRE'
            IF (VALI.EQ.1) VALK = 'DELTA_GRANDEUR'
            IF (VALI.EQ.2) VALK = 'COLLISION'
            IF (VALI.EQ.3) VALK = 'INTERPENETRATION'
            IF (VALI.EQ.4) VALK = 'DIVE_RESI'
            IF (VALI.EQ.5) VALK = 'INSTABILITE'
          ELSEIF (GETSET.EQ.'E') THEN
            IF (VALK.EQ.'ERRE') THEN
              ZR(JEEVR-1+LEEVR*(IECHEC-1)+1) = 0.D0
            ELSEIF (VALK.EQ.'DELTA_GRANDEUR') THEN
              ZR(JEEVR-1+LEEVR*(IECHEC-1)+1) = 1.D0
            ELSEIF (VALK.EQ.'COLLISION') THEN
              ZR(JEEVR-1+LEEVR*(IECHEC-1)+1) = 2.D0
            ELSEIF (VALK.EQ.'INTERPENETRATION') THEN
              ZR(JEEVR-1+LEEVR*(IECHEC-1)+1) = 3.D0
            ELSEIF (VALK.EQ.'DIVE_RESI') THEN
              ZR(JEEVR-1+LEEVR*(IECHEC-1)+1) = 4.D0
            ELSEIF (VALK.EQ.'INSTABILITE') THEN
              ZR(JEEVR-1+LEEVR*(IECHEC-1)+1) = 5.D0
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
          ENDIF

        ELSEIF (QUEST.EQ.'ACTION') THEN
          IF (GETSET.EQ.'L') THEN
            VALI = NINT(ZR(JEEVR-1+LEEVR*(IECHEC-1)+2))
            IF (VALI.EQ.0) VALK = 'ARRET'
            IF (VALI.EQ.1) VALK = 'REAC_PRECOND'
            IF (VALI.EQ.2) VALK = 'DECOUPE'
            IF (VALI.EQ.3) VALK = 'ITER_SUPPL'
            IF (VALI.EQ.4) VALK = 'AUTRE_PILOTAGE'
            IF (VALI.EQ.5) VALK = 'ADAPT_COEF_PENA'
            IF (VALI.EQ.6) VALK = 'CONTINUE'
          ELSEIF (GETSET.EQ.'E') THEN
            IF (VALK.EQ.'ARRET') THEN
              ZR(JEEVR-1+LEEVR*(IECHEC-1)+2) = 0.D0
            ELSEIF (VALK.EQ.'REAC_PRECOND') THEN
              ZR(JEEVR-1+LEEVR*(IECHEC-1)+2) = 1.D0
            ELSEIF (VALK.EQ.'DECOUPE') THEN
              ZR(JEEVR-1+LEEVR*(IECHEC-1)+2) = 2.D0
            ELSEIF (VALK.EQ.'ITER_SUPPL') THEN
              ZR(JEEVR-1+LEEVR*(IECHEC-1)+2) = 3.D0
            ELSEIF (VALK.EQ.'AUTRE_PILOTAGE') THEN
              ZR(JEEVR-1+LEEVR*(IECHEC-1)+2) = 4.D0
            ELSEIF (VALK.EQ.'ADAPT_COEF_PENA') THEN
              ZR(JEEVR-1+LEEVR*(IECHEC-1)+2) = 5.D0
            ELSEIF (VALK.EQ.'CONTINUE') THEN
              ZR(JEEVR-1+LEEVR*(IECHEC-1)+2) = 6.D0
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
          ENDIF

        ELSEIF (QUEST.EQ.'VERIF_EVEN') THEN
          IF (GETSET.EQ.'L') THEN
            VALI = NINT(ZR(JEEVR-1+LEEVR*(IECHEC-1)+3))
            IF (VALI.EQ.0) VALK = 'OUI'
            IF (VALI.EQ.1) VALK = 'NON'
          ELSEIF (GETSET.EQ.'E') THEN
            IF (VALK.EQ.'OUI') THEN
              ZR(JEEVR-1+LEEVR*(IECHEC-1)+3) = 0
            ELSEIF (VALK.EQ.'NON') THEN
              ZR(JEEVR-1+LEEVR*(IECHEC-1)+3) = 1
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
          ENDIF
C
C ----- PARAMETRES EVENEMENT 'DELTA_GRANDEUR'
C
        ELSEIF (QUEST.EQ.'NOM_CHAM') THEN
          IF (GETSET.EQ.'L') THEN
            VALK = ZK16(JEEVK-1+LEEVK*(IECHEC-1)+1)
          ELSEIF (GETSET.EQ.'E') THEN
            ZK16(JEEVK-1+LEEVK*(IECHEC-1)+1) = VALK
          ENDIF

        ELSEIF (QUEST.EQ.'NOM_CMP') THEN
          IF (GETSET.EQ.'L') THEN
            VALK = ZK16(JEEVK-1+LEEVK*(IECHEC-1)+2)
          ELSEIF (GETSET.EQ.'E') THEN
            ZK16(JEEVK-1+LEEVK*(IECHEC-1)+2) = VALK
          ENDIF

        ELSEIF (QUEST.EQ.'CRIT_COMP') THEN
          IF (GETSET.EQ.'L') THEN
            VALK = ZK16(JEEVK-1+LEEVK*(IECHEC-1)+3)
          ELSEIF (GETSET.EQ.'E') THEN
            ZK16(JEEVK-1+LEEVK*(IECHEC-1)+3) = VALK
          ENDIF

        ELSEIF (QUEST.EQ.'VALE_REF') THEN
          IF (GETSET.EQ.'L') THEN
            VALR = ZR(JEEVR-1+LEEVR*(IECHEC-1)+5)
          ELSEIF (GETSET.EQ.'E') THEN
            ZR(JEEVR-1+LEEVR*(IECHEC-1)+5) = VALR
          ENDIF
C
C ----- PARAMETRES EVENEMENT 'INTERPENETRATION'
C
        ELSEIF (QUEST.EQ.'PENE_MAXI') THEN
          IF (GETSET.EQ.'L') THEN
            VALR = ZR(JEEVR-1+LEEVR*(IECHEC-1)+6)
          ELSEIF (GETSET.EQ.'E') THEN
            ZR(JEEVR-1+LEEVR*(IECHEC-1)+6) = VALR
          ENDIF
C
C ----- PARAMETRES ACTION 'DECOUPE'
C
        ELSEIF (QUEST.EQ.'SUBD_METHODE') THEN
          IF (GETSET.EQ.'L') THEN
            VALI = NINT(ZR(JESUR-1+LESUR*(IECHEC-1)+1))
            IF (VALI.EQ.0) VALK = 'AUCUNE'
            IF (VALI.EQ.1) VALK = 'MANUEL'
            IF (VALI.EQ.2) VALK = 'AUTO'
          ELSEIF (GETSET.EQ.'E') THEN
            IF (VALK .EQ. 'AUCUNE')     THEN
              ZR(JESUR-1+LESUR*(IECHEC-1)+1) = 0
            ELSEIF (VALK .EQ. 'MANUEL') THEN
              ZR(JESUR-1+LESUR*(IECHEC-1)+1) = 1
            ELSEIF (VALK .EQ. 'AUTO')   THEN
              ZR(JESUR-1+LESUR*(IECHEC-1)+1) = 2
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
          ENDIF

        ELSEIF (QUEST.EQ.'SUBD_METHODE_AUTO') THEN
          IF (GETSET.EQ.'L') THEN
            VALI = NINT(ZR(JESUR-1+LESUR*(IECHEC-1)+10))
            IF (VALI.EQ.1) VALK = 'COLLISION'
            IF (VALI.EQ.2) VALK = 'EXTRAPOLE'
          ELSEIF (GETSET.EQ.'E') THEN
            IF (VALK .EQ. 'COLLISION')     THEN
              ZR(JESUR-1+LESUR*(IECHEC-1)+10) = 1
            ELSEIF (VALK .EQ. 'EXTRAPOLE') THEN
              ZR(JESUR-1+LESUR*(IECHEC-1)+10) =21
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
          ENDIF

        ELSEIF (QUEST.EQ.'SUBD_PAS') THEN
          IF (GETSET.EQ.'L') THEN
            VALI = NINT(ZR(JESUR-1+LESUR*(IECHEC-1)+2))
          ELSEIF (GETSET.EQ.'E') THEN
            ZR(JESUR-1+LESUR*(IECHEC-1)+2) = VALI
          ENDIF

        ELSEIF (QUEST.EQ.'SUBD_PAS_MINI') THEN
          IF (GETSET.EQ.'L') THEN
            VALR = ZR(JESUR-1+LESUR*(IECHEC-1)+3)
          ELSEIF (GETSET.EQ.'E') THEN
            ZR(JESUR-1+LESUR*(IECHEC-1)+3) = VALR
          ENDIF

        ELSEIF (QUEST.EQ.'SUBD_NIVEAU') THEN
          IF (GETSET.EQ.'L') THEN
            VALI = NINT(ZR(JESUR-1+LESUR*(IECHEC-1)+4))
          ELSEIF (GETSET.EQ.'E') THEN
            ZR(JESUR-1+LESUR*(IECHEC-1)+4) = VALI
          ENDIF

        ELSEIF (QUEST.EQ.'SUBD_INST') THEN
          IF (GETSET.EQ.'L') THEN
            VALR = ZR(JESUR-1+LESUR*(IECHEC-1)+5)
          ELSEIF (GETSET.EQ.'E') THEN
            ZR(JESUR-1+LESUR*(IECHEC-1)+5) = VALR
          ENDIF

        ELSEIF (QUEST.EQ.'SUBD_DUREE') THEN
          IF (GETSET.EQ.'L') THEN
            VALR = ZR(JESUR-1+LESUR*(IECHEC-1)+6)
          ELSEIF (GETSET.EQ.'E') THEN
            ZR(JESUR-1+LESUR*(IECHEC-1)+6) = VALR
          ENDIF

        ELSEIF (QUEST.EQ.'SUBD_RATIO') THEN
          IF (GETSET.EQ.'L') THEN
            VALI = NINT(ZR(JESUR-1+LESUR*(IECHEC-1)+9))
          ELSEIF (GETSET.EQ.'E') THEN
            ZR(JESUR-1+LESUR*(IECHEC-1)+9) = VALI
          ENDIF
C
C ---- PARAMETRES ACTION 'REAC_PRECOND'
C
        ELSEIF (QUEST.EQ.'ESSAI_REAC_PRECOND') THEN
          IF(GETSET.EQ.'L') THEN
             VALI = ZI(JREAPC)
          ELSE IF(GETSET.EQ.'E') THEN
            ZI(JREAPC) = VALI
          ENDIF
C
C ----- PARAMETRES ACTION 'ITER_SUPPL'
C
        ELSEIF (QUEST.EQ.'PCENT_ITER_PLUS') THEN
          IF (GETSET.EQ.'L') THEN
            VALR = ZR(JESUR-1+LESUR*(IECHEC-1)+7)
          ELSEIF (GETSET.EQ.'E') THEN
            ZR(JESUR-1+LESUR*(IECHEC-1)+7) = VALR
          ENDIF
C
C ----- PARAMETRES ACTION 'ADAPT_COEF_PENA'
C
        ELSEIF (QUEST.EQ.'COEF_MAXI') THEN
          IF (GETSET.EQ.'L') THEN
            VALR = ZR(JESUR-1+LESUR*(IECHEC-1)+8)
          ELSEIF (GETSET.EQ.'E') THEN
            ZR(JESUR-1+LESUR*(IECHEC-1)+8) = VALR
          ENDIF
C
C ----- PARAMETRES ACTION 'AUTRE_PILOTAGE'
C
        ELSEIF (QUEST.EQ.'CHOIX_SOLU_PILO') THEN
          IF (GETSET.EQ.'L') THEN
            VALI = ZI(JPIL)
            IF (VALI.EQ.1) VALK = 'NATUREL'
            IF (VALI.EQ.2) VALK = 'AUTRE'
          ELSE IF(GETSET.EQ.'E') THEN
            IF(VALK.EQ.'NATUREL') THEN
              ZI(JPIL)=1
            ELSEIF(VALK.EQ.'AUTRE') THEN
              ZI(JPIL)=2
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
          ENDIF

        ELSEIF (QUEST.EQ.'ESSAI_ITER_PILO') THEN
          IF(GETSET.EQ.'L') THEN
             VALI = ZI(JPIL+1)
          ELSE IF(GETSET.EQ.'E') THEN
            ZI(JPIL+1) = VALI
          ENDIF

        ELSE
          CALL ASSERT(.FALSE.)

        ENDIF

C     ------------------------------------------------------------------
C                     QUESTION SUR L'ADAPTATION
C     ------------------------------------------------------------------

      ELSEIF (TYPQUE.EQ.'ADAP') THEN
        TPSAVR = SDDISC(1:19)//'.AEVR'
        TPSAVK = SDDISC(1:19)//'.AEVK'
        TPSTPR = SDDISC(1:19)//'.ATPR'
        TPSTPK = SDDISC(1:19)//'.ATPK'
        TPSPIL = SDDISC(1:19)//'.EPIL'
        CALL JEVEUO(TPSAVR,GETSET,JAEVR )
        CALL JEVEUO(TPSAVK,GETSET,JAEVK )
        CALL JEVEUO(TPSTPR,GETSET,JATPR )
        CALL JEVEUO(TPSTPK,GETSET,JATPK )
        CALL JEVEUO(TPSPIL,GETSET,JPIL  )
        IADAPT = IOCC

        IF (QUEST.EQ.'NOM_EVEN') THEN
          IF (GETSET.EQ.'L') THEN
            VALI = NINT(ZR(JAEVR-1+LAEVR*(IADAPT-1)+1))
            IF (VALI.EQ.0) VALK = 'AUCUN'
            IF (VALI.EQ.1) VALK = 'TOUT_INST'
            IF (VALI.EQ.2) VALK = 'SEUIL_SANS_FORMULE'
            IF (VALI.EQ.3) VALK = 'SEUIL_AVEC_FORMULE'
          ELSEIF(GETSET.EQ.'E') THEN
            IF (VALK.EQ.'AUCUN') THEN
              ZR(JAEVR-1+LAEVR*(IADAPT-1)+1) = 0
            ELSEIF (VALK.EQ.'TOUT_INST') THEN
              ZR(JAEVR-1+LAEVR*(IADAPT-1)+1) = 1
            ELSEIF (VALK.EQ.'SEUIL_SANS_FORMULE') THEN
              ZR(JAEVR-1+LAEVR*(IADAPT-1)+1) = 2
            ELSEIF (VALK.EQ.'SEUIL_AVEC_FORMULE') THEN
              ZR(JAEVR-1+LAEVR*(IADAPT-1)+1) = 3
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
          ENDIF

        ELSEIF (QUEST.EQ.'NB_INCR_SEUIL') THEN
          IF (GETSET.EQ.'L') THEN
            VALI = NINT(ZR(JAEVR-1+LAEVR*(IADAPT-1)+2))
          ELSEIF(GETSET.EQ.'E') THEN
            ZR(JAEVR-1+LAEVR*(IADAPT-1)+2) = 1
          ENDIF

        ELSEIF (QUEST.EQ.'NOM_PARA') THEN
          IF (GETSET.EQ.'L') THEN
            VALI = NINT(ZR(JAEVR-1+LAEVR*(IADAPT-1)+3))
            IF (VALI.EQ.1) VALK = 'NB_ITER_NEWT'
            IF (VALI.EQ.2) VALK = 'DP'
          ELSEIF(GETSET.EQ.'E') THEN
            IF (VALK.EQ.'NB_ITER_NEWT') THEN
              ZR(JAEVR-1+LAEVR*(IADAPT-1)+3) = 1
            ELSEIF (VALK.EQ.'SEUIL_AVEC_FORMULE') THEN
              ZR(JAEVR-1+LAEVR*(IADAPT-1)+3) = 2
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
          ENDIF

        ELSEIF (QUEST.EQ.'CRIT_COMP') THEN
          IF (GETSET.EQ.'L') THEN
            VALI = NINT(ZR(JAEVR-1+LAEVR*(IADAPT-1)+4))
            IF (VALI.EQ.1) VALK = 'LT'
            IF (VALI.EQ.2) VALK = 'GT'
            IF (VALI.EQ.3) VALK = 'LE'
            IF (VALI.EQ.4) VALK = 'GE'
          ELSEIF(GETSET.EQ.'E') THEN
            IF (VALK.EQ.'LT') THEN
              ZR(JAEVR-1+LAEVR*(IADAPT-1)+4) = 1
            ELSEIF (VALK.EQ.'GT') THEN
              ZR(JAEVR-1+LAEVR*(IADAPT-1)+4) = 2
            ELSEIF (VALK.EQ.'LE') THEN
              ZR(JAEVR-1+LAEVR*(IADAPT-1)+4) = 3
            ELSEIF (VALK.EQ.'GE') THEN
              ZR(JAEVR-1+LAEVR*(IADAPT-1)+4) = 4
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
          ENDIF

        ELSEIF (QUEST.EQ.'VALE') THEN
          IF (GETSET.EQ.'L') THEN
            VALR = ZR(JAEVR-1+LAEVR*(IADAPT-1)+5)
            VALI = NINT(ZR(JAEVR-1+LAEVR*(IADAPT-1)+5))
          ELSEIF (GETSET.EQ.'E') THEN
            ZR(JAEVR-1+LAEVR*(IADAPT-1)+5) = VALR
          ENDIF

        ELSEIF (QUEST.EQ.'NB_EVEN_OK') THEN
          IF (GETSET.EQ.'L') THEN
            VALR = ZR(JAEVR-1+LAEVR*(IADAPT-1)+6)
            VALI = NINT(VALR)
          ELSEIF (GETSET.EQ.'E') THEN
            ZR(JAEVR-1+LAEVR*(IADAPT-1)+6) = VALI
          ENDIF

        ELSEIF (QUEST.EQ.'METHODE') THEN
          IF (GETSET.EQ.'L') THEN
            VALI = NINT(ZR(JATPR-1+LATPR*(IADAPT-1)+1))
            IF (VALI.EQ.1) VALK = 'FIXE'
            IF (VALI.EQ.2) VALK = 'DELTA_GRANDEUR'
            IF (VALI.EQ.3) VALK = 'ITER_NEWTON'
            IF (VALI.EQ.4) VALK = 'FORMULE'
            IF (VALI.EQ.5) VALK = 'IMPLEX'
          ELSEIF(GETSET.EQ.'E') THEN
            IF (VALK.EQ.'FIXE') THEN
              ZR(JATPR-1+LATPR*(IADAPT-1)+1) = 1
            ELSEIF (VALK.EQ.'DELTA_GRANDEUR') THEN
              ZR(JATPR-1+LATPR*(IADAPT-1)+1) = 2
            ELSEIF (VALK.EQ.'ITER_NEWTON') THEN
              ZR(JATPR-1+LATPR*(IADAPT-1)+1) = 3
            ELSEIF (VALK.EQ.'FORMULE') THEN
              ZR(JATPR-1+LATPR*(IADAPT-1)+1) = 4
            ELSEIF (VALK.EQ.'IMPLEX') THEN
              ZR(JATPR-1+LATPR*(IADAPT-1)+1) = 5
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
          ENDIF

        ELSEIF (QUEST.EQ.'PCENT_AUGM') THEN
          IF (GETSET.EQ.'L') THEN
            VALR = ZR(JATPR-1+LATPR*(IADAPT-1)+2)
          ELSEIF(GETSET.EQ.'E') THEN
            ZR(JATPR-1+LATPR*(IADAPT-1)+2) = VALR
          ENDIF

        ELSEIF (QUEST.EQ.'VALE_REF') THEN
          IF (GETSET.EQ.'L') THEN
            VALR = ZR(JATPR-1+LATPR*(IADAPT-1)+3)
          ELSEIF(GETSET.EQ.'E') THEN
            ZR(JATPR-1+LATPR*(IADAPT-1)+3) = VALR
          ENDIF

        ELSEIF (QUEST.EQ.'NU_CMP') THEN
          IF (GETSET.EQ.'L') THEN
            VALR = ZR(JATPR-1+LATPR*(IADAPT-1)+4)
            VALI = NINT(VALR)
          ELSEIF(GETSET.EQ.'E') THEN
            ZR(JATPR-1+LATPR*(IADAPT-1)+4) = VALI
          ENDIF

        ELSEIF (QUEST.EQ.'NB_ITER_NEWTON_REF') THEN
          IF (GETSET.EQ.'L') THEN
            VALR = ZR(JATPR-1+LATPR*(IADAPT-1)+5)
            VALI = NINT(VALR)
          ELSEIF(GETSET.EQ.'E') THEN
            ZR(JATPR-1+LATPR*(IADAPT-1)+5) = VALI
          ENDIF

        ELSEIF (QUEST.EQ.'NOM_CHAM') THEN
          IF (GETSET.EQ.'L') THEN
            VALK = ZK16(JATPK-1+LATPK*(IADAPT-1)+2)
          ELSEIF(GETSET.EQ.'E') THEN
            ZK16(JATPK-1+LATPK*(IADAPT-1)+2) = VALK
          ENDIF

        ELSEIF (QUEST.EQ.'NOM_CMP') THEN
          IF (GETSET.EQ.'L') THEN
            VALK = ZK16(JATPK-1+LATPK*(IADAPT-1)+3)
          ELSEIF(GETSET.EQ.'E') THEN
            ZK16(JATPK-1+LATPK*(IADAPT-1)+3) = VALK
          ENDIF

        ELSE
          CALL ASSERT(.FALSE.)

        ENDIF

      ENDIF
C
      CALL JEDEMA()
      END
