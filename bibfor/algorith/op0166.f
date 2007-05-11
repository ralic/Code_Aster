      SUBROUTINE OP0166 ( IER )
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/05/2007   AUTEUR PELLET J.PELLET 
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
C
C     COMMANDE:  PROJ_CHAMP
C
      IMPLICIT   NONE
C
C 0.1. ==> ARGUMENTS
C
      INTEGER            IER
C
C 0.2. ==> COMMUNS
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      REAL*8             ZR,R8B
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC,C16B
      COMMON  / CVARJE / ZC(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
C 0.3. ==> VARIABLES LOCALES
C
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'OP0166' )
C
      INTEGER IAUX, JAUX, IRET
      INTEGER IE, N1, N2, N3, NX, I
      INTEGER IOC, NBOCC, NB, JMODL
      INTEGER IEQ, NBEQUA
      INTEGER IBID, KVALE,NBORDR
      INTEGER NRPASS, NBPASS
      INTEGER ADRECG
C
      CHARACTER*4   TYPE
      CHARACTER*8   K8,K8BID,NOMA1,NOMA2,RESUIN,K8B
      CHARACTER*8   LERES0, NOPASE, MODEL2
      CHARACTER*16  TYPRES, NOMCMD
      CHARACTER*19  RESUOU, CHAM1, CHAM2, NUAGE1, NUAGE2,METHOD
      CHARACTER*19  LMA, LNO, LMA1, LNO1, LMA2, LNO2
      CHARACTER*19  LERES1
      CHARACTER*24 NORECG
      CHARACTER*24 VALK(2)
C
      LOGICAL ELTF
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
      CALL INFMAJ()
      IER = 0
C
C               12   345678   9012345678901234
      NORECG = '&&'//NOMPRO//'_PARA_SENSI     '
C
      CALL GETRES( RESUOU , TYPRES , NOMCMD )
      CALL TITRE
      CALL GETVTX(' ','METHODE' ,1,1,1, METHOD, N3 )
C
C     --- SENSIBILITE : NOMBRE DE PASSAGES ---
      IAUX = 1
      JAUX = 1
      CALL PSRESE ( ' ', IBID, IAUX, RESUOU, JAUX,
     &              NBPASS, NORECG, IRET )
      CALL JEVEUO ( NORECG, 'L', ADRECG )
C
C     -- QUELQUES VERIFS. :
C     ----------------------
      IF (METHOD.EQ.'ELEM') THEN
        ELTF=.TRUE.
C       -- POUR L'INSTANT 'ELTFXX' N'EST PERMIS QUE POUR LES EVOL_XXX
        CALL GETVID(' ','RESULTAT'       ,1,1,1, RESUIN, N1 )
        IF (N1.EQ.0) CALL U2MESS('F','ALGORITH9_65')

      ELSE IF (METHOD(1:6).EQ.'NUAGE_') THEN
        ELTF=.FALSE.
C       -- POUR L'INSTANT 'NUAGE_DEG_*' N'EST PERMIS QUE POUR LES CHAMPS
        CALL GETVID(' ','CHAM_NO'       ,1,1,1, CHAM1, N1 )
        IF (N1.EQ.0) CALL U2MESS('F','ALGORITH9_66')
      ELSE
        CALL U2MESS('F','CALCULEL_2')
      END IF
C
C============ DEBUT DE LA BOUCLE SUR LE NOMBRE DE PASSAGES =============
      DO 30 , NRPASS = 1 , NBPASS
C
C        POUR LE PASSAGE NUMERO NRPASS :
C        . NOPASE : NOM DU PARAMETRE DE SENSIBILITE EVENTUELLEMENT
C        . LERES1 : NOM DU CHAMP DE RESULTAT A COMPLETER
C                   C'EST RESUOU POUR UN CALCUL STANDARD, UN NOM
C                   COMPOSE A PARTIR DE RESUOU ET NOPASE POUR UN CALCUL
C                   DE SENSIBILITE
C        . LERES0 : IDEM POUR RESUIN
C
        NOPASE = ZK24(ADRECG+2*NRPASS-1)(1:8)
        LERES1 = ZK24(ADRECG+2*NRPASS-2)(1:19)
C
C DANS LE CAS D'UN CALCUL STANDARD :
C
        IF ( NOPASE.EQ.' ' ) THEN
C
          LERES0 = RESUIN
C
C DANS LE CAS D'UN CALCUL DE DERIVE :
C
        ELSE
C
          CALL PSRENC ( RESUIN, NOPASE, LERES0, IRET )
          IF ( IRET.NE.0 ) THEN
             VALK(1) = RESUIN
             VALK(2) = NOPASE
             CALL U2MESK('A','SENSIBILITE_3', 2 ,VALK)
            GOTO 30
          ENDIF
C
        ENDIF
C
C     1.-- CAS METHODE: 'ELEM' :
C     ----------------------------
      IF (ELTF) THEN
        CALL PJEFTE (LERES0, LERES1 )
C
C     2. -- CAS METHODE: 'NUAGE_DEG_0/1' :
C     ---------------------------------
      ELSE
         CALL GETVID(' ','CHAM_NO'     ,1,1,1, CHAM1, N1 )
         CALL CHPVER('F',CHAM1,'NOEU','*',IE)
         CALL DISMOI('F', 'NOM_MAILLA', CHAM1, 'CHAMP', IBID, NOMA1, IE)

C        C'EST LE MAILLAGE MAILLA1 QUI IMPOSE LA DIMENSION D'ESPACE DES
C        DEUX NUAGES :
         NX=3
         CALL DISMOI('F','Z_CST' ,NOMA1,'MAILLAGE',IBID,K8,IE)
         IF (K8.EQ.'OUI') NX=2

         CALL GETVID(' ','CHAM_NO_REFE',1,1,1, CHAM2, N2 )
         CALL CHPVER('F',CHAM2,'NOEU','*',IE)
         CALL DISMOI('F', 'NOM_MAILLA', CHAM2, 'CHAMP', IBID, NOMA2, IE)
C
C
         CALL GETFAC ( 'VIS_A_VIS' , NBOCC )
         IF ( NBOCC .NE. 0 ) THEN
C
            CALL COPISD('CHAMP_GD','G',CHAM2,LERES1)
            CALL JELIRA(LERES1//'.VALE','LONMAX',NBEQUA,K8BID)
            CALL JELIRA(LERES1//'.VALE', 'TYPE' ,IBID  ,TYPE )
            CALL JEVEUO(LERES1//'.VALE','E',KVALE)
            IF ( TYPE(1:1) .EQ. 'R' ) THEN
               DO 10 IEQ = 0 , NBEQUA-1
                  ZR(KVALE+IEQ) = 0.D0
 10            CONTINUE
            ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
               DO 12 IEQ = 0 , NBEQUA-1
                  ZC(KVALE+IEQ) = 0.D0
 12            CONTINUE
            ENDIF
C
            DO 100 IOC = 1 , NBOCC
C
              LMA1 = '&&LISTE_MA_1'
              LNO1 = '&&LISTE_NO_1'
              CALL UTNUAV ( NOMA1, 1, IOC, LMA1, LNO1 )
C
              LMA2 = '&&LISTE_MA_2'
              LNO2 = '&&LISTE_NO_2'
              CALL UTNUAV ( NOMA2, 2, IOC, LMA2, LNO2 )
C
              NUAGE1 = '&&NUAGE1'
              NUAGE2 = '&&NUAGE2'
              CALL CHPNUA ( NX,CHAM1, LNO1 , NUAGE1 )
              CALL CHPNUA ( NX,LERES1, LNO2 , NUAGE2 )
C
              CALL PRONUA ( METHOD , NUAGE1 , NUAGE2 )
C
              CALL NUACHP ( NUAGE2 , LMA2 , LNO2 , LERES1 )
C
              CALL DETRSD ( 'NUAGE', NUAGE1 )
              CALL DETRSD ( 'NUAGE', NUAGE2 )
              CALL JEDETC ( 'V' , '&&LISTE_' , 1 )
C
 100       CONTINUE
C
         ELSE
C
            CALL COPISD('CHAMP_GD','G',CHAM2,LERES1)
            CALL JELIRA(LERES1//'.VALE','LONMAX',NBEQUA,K8BID)
            CALL JELIRA(LERES1//'.VALE', 'TYPE' ,IBID  ,TYPE )
            CALL JEVEUO(LERES1//'.VALE','E',KVALE)
            IF ( TYPE(1:1) .EQ. 'R' ) THEN
               DO 20 IEQ = 0 , NBEQUA-1
                  ZR(KVALE+IEQ) = 0.D0
 20            CONTINUE
            ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
               DO 22 IEQ = 0 , NBEQUA-1
                  ZC(KVALE+IEQ) = 0.D0
 22            CONTINUE
            ENDIF
C
            LMA = ' '
            LNO = ' '
            NUAGE1 = '&&NUAGE1'
            NUAGE2 = '&&NUAGE2'
            CALL CHPNUA ( NX,CHAM1, LNO , NUAGE1 )
            CALL CHPNUA ( NX,LERES1, LNO , NUAGE2 )
C
            CALL PRONUA ( METHOD , NUAGE1 , NUAGE2 )
C
            CALL NUACHP ( NUAGE2 , LMA , LNO , LERES1 )
C
            CALL DETRSD ( 'NUAGE', NUAGE1 )
            CALL DETRSD ( 'NUAGE', NUAGE2 )
C
         ENDIF
      ENDIF
C
   30 CONTINUE
C
C --- STOCKAGE
C
      IF (ELTF .AND. TYPRES(1:9).NE.'EVOL_CHAR' ) THEN
        CALL RSORAC(LERES1,'LONUTI',IBID,R8B,K8B,C16B,R8B,K8B,NBORDR,1,
     &              IBID)

        CALL GETVID(' ','MODELE_2',0,1,1,MODEL2,N1)
        CALL JEVEUO(LERES1//'.MODL','E',JMODL)
        DO 70 I=1,NBORDR
          ZK8(JMODL+I-1)=MODEL2
 70     CONTINUE
      ENDIF

C============= FIN DE LA BOUCLE SUR LE NOMBRE DE PASSAGES ==============
C
C
      CALL JEDEMA()
      END
