      SUBROUTINE OP0051 ( IER )
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 07/10/2004   AUTEUR GNICOLAS G.NICOLAS 
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
C     ------------------------------------------------------------------
C     OPERATEUR   POST_RELEVE_T
C     ------------------------------------------------------------------
C
      IMPLICIT   NONE
C
C 0.1. ==> ARGUMENTS
C
      INTEGER          IER
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
      CHARACTER*32     JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
C 0.3. ==> VARIABLES LOCALES
C
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'OP0051' )
C
      INTEGER IFM, NIV
      INTEGER IAUX, JAUX
      INTEGER      ICHEF, IE, IOCC, IRET, IVCHF, JACC, JACCIS, JACCR8,
     >             JCHEF, JTAC, JVAC, N1, NBACCE, NBCHEF, NBPOST, 
     >             NBRESU, NBVCHF, IBID
      INTEGER NRPASS, NBPASS, ADRECG
C
      REAL*8       EPSI
C
      CHARACTER*1  CA
      CHARACTER*2  CODACC, DIM
      CHARACTER*6  MCF
      CHARACTER*8  K8B, RESUCO, CRITER
      CHARACTER*8 NOPASE, LERES0
      CHARACTER*16 NOMCMD, CONCEP, NCHEFF, NCHSYM, OPTION
      CHARACTER*19 LATAB1, TABLE1
      CHARACTER*19 NCH19
      CHARACTER*24 XNUMCP, XNOMCP, VNOMCH, VCODOP, XNOVAR
      CHARACTER*24 NACCIS, NACCR8, NCH24, NLSMAC, NLSNAC
      CHARACTER*24 NORECG
      LOGICAL      TROUVE
C     ------------------------------------------------------------------
C
C====
C 1. PREALABLES
C====
C
      CALL JEMARQ()
C               12   345678   9012345678901234
      NCHEFF = '&&'//NOMPRO//'.CHAMP19'
      XNOMCP = '&&'//NOMPRO//'.NOM.COMPOSANTES'
      XNOVAR = '&&'//NOMPRO//'.NOM.VARI       '
      XNUMCP = '&&'//NOMPRO//'.NUM.COMPOSANTES'
      VNOMCH = '&&'//NOMPRO//'.NOM.CHAMPEFFECT'
      VCODOP = '&&'//NOMPRO//'.CODE.OPERATION '
      NACCIS = '&&'//NOMPRO//'.ACCES.ENTIER   '
      NACCR8 = '&&'//NOMPRO//'.ACCES.REEL     '
      NLSMAC = '&&'//NOMPRO//'.MAILLES.ACTIVES'
      NLSNAC = '&&'//NOMPRO//'.NOEUDS .ACTIFS '
      NORECG = '&&'//NOMPRO//'_RESULTA_GD     '
C
C====
C  2. RECUPERATION DES OPERANDES
C====
C 2.1. ==>  RECUPERATION DU NIVEAU D'IMPRESSION
C
      CALL INFMAJ
      CALL INFNIV(IFM,NIV)
C
C 2.2. ==> LE CONCEPT DE SORTIE, SON TYPE, LA COMMANDE
C
      CALL GETRES ( TABLE1, CONCEP, NOMCMD )
      IF ( NIV.GE.2 ) THEN
       CALL UTMESS('I',NOMPRO,'CREATION/EXTENSION DE LA TABLE '//TABLE1)
      ENDIF
C
C 2.3. ==> PHASE DE VERIFICATIONS SUPPLEMENTAIRES
C
      CALL RVVSUP()
C
C 2.4. ==> PHASE DE VERIFICATION D'EXISTENCE DES ARGUMENTS
C
      CALL RVGARG ( XNOMCP, XNUMCP, VNOMCH, VCODOP, XNOVAR )
C
C 2.5. ==> PHASE D'INITIALISATION DE LA TABLE
C
      MCF = 'ACTION'
      CALL GETFAC ( MCF, NBPOST )
C
      CALL RVPAR0 ( TABLE1, MCF, NBPOST )
      DIM = '  '
C
C====
C 3. TRAITEMENT EFFECTIF
C====
C============ DEBUT DE LA BOUCLE SUR LES POST-TRAITEMENTS ==============
C
      DO 3, IOCC = 1, NBPOST, 1
C
C 3.1. ==> VERIFICATION DE COHERENCE DES ARGUMENTS DE LA COMMANDE
C
        CALL RVCOHE ( XNUMCP, XNOMCP, VNOMCH, IOCC, IRET )
C
        IF ( IRET.NE.0 ) THEN
C
        CALL GETVTX ( MCF, 'MOYE_NOEUD', IOCC,1,1, K8B, N1 )
        IF ( K8B(1:1) .EQ. 'O' ) THEN
          CA = 'N'
        ELSE
          CA = 'E'
        ENDIF
C
C        --- EST-CE UN RESULTAT ? ---
C
        RESUCO = '        '
        CALL GETVID ( MCF, 'RESULTAT', IOCC,1,1, RESUCO, NBRESU )
C
C        --- NOMBRE DE PASSAGES POUR LA SENSIBILITE ---
C
        IAUX = IOCC
        IF ( NBRESU .NE. 0 ) THEN
          IBID = 1
        ELSE
          IBID = 2
        ENDIF
        JAUX = 1
        CALL PSRESE ( MCF, IAUX, IBID, TABLE1, JAUX,
     >                NBPASS, NORECG, IRET )
C
        IF ( IRET.EQ.0 ) THEN
C
        CALL JEVEUO ( NORECG, 'L', ADRECG )
C
C============ DEBUT DE LA BOUCLE SUR LE NOMBRE DE PASSAGES =============
        DO 30 , NRPASS = 1 , NBPASS
C
C        POUR LE PASSAGE NUMERO NRPASS :
C        POUR LE PASSAGE NUMERO NRPASS :
C        . NOPASE : NOM DU PARAMETRE DE SENSIBILITE EVENTUELLEMENT
C        . LATAB1 : NOM DE LA TABLE A COMPLETER
C                   C'EST TABLE1 POUR UN CALCUL STANDARD, UN NOM
C                   COMPOSE A PARTIR DE TABLE1 ET NOPASE POUR UN CALCUL
C                   DE SENSIBILITE
C        . LERES0 : IDEM POUR RESUCO
C
C                  1234567890123456789
         LATAB1 = '                   '
         LATAB1(1:8) = ZK24(ADRECG+2*NRPASS-2)(1:8)
         NOPASE = ZK24(ADRECG+2*NRPASS-1)(1:8)
CGN         PRINT *,'. LATAB1 = ', LATAB1
CGN         PRINT *,'. NOPASE = ', NOPASE
C
C        --- SAISIE DES CHAMPS EFFECTIFS A POST-TAITER ---
C
         IF ( NBRESU .NE. 0 ) THEN
C
            IF (NOPASE.EQ.' ') THEN
              LERES0 = RESUCO
            ELSE
              CALL PSRENC ( RESUCO, NOPASE, LERES0, IRET )
              IF ( IRET.NE.0 ) THEN
                CALL UTMESS ('F', NOMPRO,
     >  'IMPOSSIBLE DE TROUVER LE RESULTAT DERIVE ASSOCIE AU RESULTAT '
     >  //RESUCO//' ET AU PARAMETRE SENSIBLE '//NOPASE)
              ENDIF
            ENDIF
C
C           /* CAS D' UN RESULTAT */
C
            CALL GETVTX ( MCF, 'NOM_CHAM' , IOCC,1,1, NCHSYM, N1 )
            CALL GETVTX ( MCF, 'CRITERE'  , IOCC,1,1, CRITER, N1 )
            CALL GETVR8 ( MCF, 'PRECISION', IOCC,1,1, EPSI  , N1 )
C
            CALL RVGACC ( IOCC, CODACC, NACCIS, NACCR8, NBACCE )
C
            CALL JEVEUO ( NACCIS, 'L', JACCIS )
            CALL JEVEUO ( NACCR8, 'L', JACCR8 )
C
            CALL RVGCHF ( EPSI, CRITER,
     >                    LERES0, RESUCO, NOPASE, NCHSYM, CODACC,
     >                    ZI(JACCIS), ZR(JACCR8), NBACCE, NCHEFF, CA )
C
            CALL JEDETR ( NACCIS )
            CALL JEDETR ( NACCR8 )
C
         ELSE
C
C           /* CAS D' UN CHAMP DE GRANDEUR */
C
            CALL JECREO ( NCHEFF//'.TYPACCE', 'V E K8' )
            CALL JEVEUO ( NCHEFF//'.TYPACCE', 'E',JACC )
C
            ZK8(JACC) = 'DIRECT  '
C
            CALL WKVECT(NCHEFF//'.VALACCE','V V I',1,JACC)
C
            ZI(JACC) = 1
C
            CALL JECREC(NCHEFF//'.LSCHEFF','V V K24','NU',
     >                                        'DISPERSE','VARIABLE',1)
C
            CALL JECROC(JEXNUM(NCHEFF//'.LSCHEFF',1))
            CALL JEECRA(JEXNUM(NCHEFF//'.LSCHEFF',1),'LONMAX',1,' ')
            CALL JEVEUO(JEXNUM(NCHEFF//'.LSCHEFF',1),'E',JACC)
C
            CALL GETVID ( MCF, 'CHAM_GD', IOCC,1,1, ZK24(JACC), N1 )
C
            CALL DISMOI('F','TYPE_CHAMP',ZK24(JACC),'CHAMP',IBID,K8B,IE)
            IF ( K8B(1:4) .EQ. 'ELNO' ) THEN
               CALL DISMOI('F','NOM_OPTION',ZK24(JACC),'CHAMP',
     >                                                 IBID,OPTION,IE)
               IF ( CA .EQ. 'N' ) THEN
                  IF ( OPTION(6:14) .EQ. 'ELNO_DEPL' ) THEN
                     CALL RVGCH1 ( ZK24(JACC))
                  ENDIF
               ENDIF
            ENDIF
C
         ENDIF
C
C        =====================================================
C        I     ON DISPOSE MAINTENANT DE :                    I
C        I                                                   I
C        I     LA XD NCHEFF DES CHAMPS EFFECTIFS MIS EN JEU  I
C        I     POUR L' OCCURENCE COURANTE (DE TYPE IDENTIQUE)I
C        I     DOCUMENTATION : CF RVGCHF                     I
C        I                                                   I
C        I     LES XD DE NOMS XNOMCP ET XNUMCP DES NOMS ET   I
C        I     NUMEROS DES CMPS MISES EN JEU                 I
C        I     DOCUMENTATION : CF RVGARG                     I
C        I                                                   I
C        I     DU VECTEUR VCODOP CONTENANT LES CODES DES     I
C        I     DES POST-TRAITEMENT PAR OCCURENCES            I
C        I     DOCUMENTATION : CF RVGARG                     I
C        I                                                   I
C        I  ON EST SUR QUE :                                 I
C        I                                                   I
C        I     TOUTES LES CMP MISES EN JEU SONT LEGALES      I
C        I                                                   I
C        I     LES CHAM_ELEM SONT BIEN "AUX NOEUDS"          I
C        I                                                   I
C        I     LES MAILLAGES, COURBES ET NOEUDS SONT         I
C        I     COHERANTS AVEC LES CHAMPS                     I
C        I                                                   I
C        I  ON DOIT SAISIR LE LIEU DU POST-TRAITEMENT :      I
C        I                                                   I
C        I     CE LIEU EST LE MEME POUR TOUS LES CHAMPS      I
C        I     EFFECTIFS                                     I
C        =====================================================
C
         CALL JELIRA ( NCHEFF//'.LSCHEFF', 'NMAXOC', NBVCHF, K8B )
C
         IVCHF  = 0
         TROUVE = .FALSE.
C
 300     CONTINUE
         IF ( (.NOT. TROUVE) .AND. (IVCHF .LT. NBVCHF) ) THEN
C
            IVCHF = IVCHF + 1
            ICHEF = 0
C
            CALL JELIRA ( JEXNUM(NCHEFF//'.LSCHEFF', IVCHF),
     >                                    'LONMAX', NBCHEF, K8B )
            CALL JEVEUO ( JEXNUM(NCHEFF//'.LSCHEFF',IVCHF), 'L', JCHEF )
C
 310        CONTINUE
            IF ( (.NOT. TROUVE) .AND. (ICHEF .LT. NBCHEF) ) THEN
C
               ICHEF = ICHEF + 1
C
               NCH24 = ZK24(JCHEF + ICHEF-1)
C
               IF ( NCH24(1:1) .NE. '&' ) TROUVE = .TRUE.
C
               GOTO 310
C
            ENDIF
C
            GOTO 300
C
         ENDIF
C
         IF ( .NOT. TROUVE ) THEN
C
            CALL UTDEBM('F',NOMCMD,'****************************')
            CALL UTIMPI('L','* POST_TRAITEMENT NUMERO : ',1,IOCC)
            CALL UTIMPK('L','* INEXISTENCE DES CHAMP-GD',0,' ')
            CALL UTIMPK('L','* PAS DE POST-TRAITEMENT',0,' ')
            CALL UTIMPK('L','****************************',0,' ')
            CALL UTFINM()
         ELSE
C
C           --- SAISIE DU LIEU DU POST-TRAITEMENT DE L' OCCURENCE ---
C
            CALL RVOUEX ( MCF, IOCC, NCH24, XNOMCP,NLSMAC,NLSNAC,IRET)
C
            IF ( IRET .EQ. 0 ) THEN
C
               CALL UTDEBM('F',NOMCMD,
     +                  '****************************************')
               CALL UTIMPI('L','* POST_TRAITEMENT NUMERO : ',1,IOCC)
               CALL UTIMPK('L','* AUCUNES MAILLES NE CORRESPONDENT'//
     +                         ' AUX CRITERES DEMANDES',0,DIM)
               CALL UTIMPK('L','* PAS DE POST-TRAITEMENT',0,' ')
               CALL UTIMPK('L',
     + '********************************************************',0,' ')
               CALL UTFINM()
C
            ELSE
C
               CALL JEVEUO(NCHEFF//'.TYPACCE','L',JTAC)
               CALL JEVEUO(NCHEFF//'.VALACCE','L',JVAC)
C
               DO 400, IVCHF = 1, NBVCHF, 1
C
                  CALL JELIRA(JEXNUM(NCHEFF//'.LSCHEFF',IVCHF),
     >                                          'LONMAX',NBCHEF,K8B)
                  CALL JEVEUO(JEXNUM(NCHEFF//'.LSCHEFF',IVCHF),
     >                                                    'L',JCHEF)
C
                  DO 410, ICHEF = 1, NBCHEF
C
                     NCH19 = ZK24(JCHEF + ICHEF-1)(1:19)
C
                     CALL RVPOST ( MCF, IOCC, DIM, IVCHF, ICHEF, NCHEFF,
     >                             XNOMCP, LERES0, NCH19, NLSMAC,
     >                             NLSNAC, LATAB1, XNOVAR )
C
 410              CONTINUE
C
 400           CONTINUE
C
            ENDIF
C
            CALL JEEXIN ( NLSMAC, N1 )
            IF ( N1 .NE. 0 ) CALL JEDETR ( NLSMAC )
C
            CALL JEEXIN ( NLSNAC, N1 )
            IF ( N1 .NE. 0 ) CALL JEDETR ( NLSNAC )
C
         ENDIF
C
         CALL JEDETR ( NCHEFF//'.NOMRESU' )
         CALL JEDETR ( NCHEFF//'.TYPACCE' )
         CALL JEDETR ( NCHEFF//'.VALACCE' )
         CALL JEDETR ( NCHEFF//'.LSCHEFF' )
C
   30   CONTINUE
C============= FIN DE LA BOUCLE SUR LE NOMBRE DE PASSAGES ==============
        CALL JEDETR ( NORECG )
C
        ENDIF
C
        ENDIF
C
    3 CONTINUE
C
C============= FIN DE LA BOUCLE SUR LES POST-TRAITEMENTS ===============
C
      CALL JEDEMA()
C
      END
