      SUBROUTINE ORDLRL (LIGRCZ, LISREZ)
      IMPLICIT REAL*8 (A-H,O-Z)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 25/11/98   AUTEUR CIBHHGB G.BERTRAND 
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
      CHARACTER*(*)      LIGRCZ,LISREZ
C -----------------------------------------------------------------
C     MISE A JOUR DE L'OBJET DE TYPE  LISTE_RELA ET DE NOM
C     LISREL  :
C               LES RELATIONS SONT REORDONNEES  PAR ORDRE DE NOEUD
C               CROISSANT ET POUR UN NOEUD DONNE PAR DDL CROISSANT
C
C               LES RELATIONS STRICTEMENT EGALES SONT ELIMINEES
C               (I.E. ON NE GARDE QUE LA RELATION DE PLUS GRAND
C                     INDICE DANS LISREL)
C -----------------------------------------------------------------
C     L'OBJET LISREL DOIT OBLIGATOIREMENT EXISTER
C -----------------------------------------------------------------
C  LIGRCZ        - IN    - K19  - : NOM DU LIGREL DE CHARGE
C                - JXIN  -      -
C -----------------------------------------------------------------
C  LISREZ        - IN    - K19  - : NOM DE LA LISTE_RELA
C                - JXVAR -      -
C -------------------------------------------------------
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
      COMMON  / KVARJE /ZK8(1),ZK16(1),ZK24(1),ZK32(1), ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ------
C
C --------- VARIABLES LOCALES ---------------------------
      CHARACTER*19       LIGRCH,LISREL
      PARAMETER    (NMOCL = 300)
      COMPLEX*16    COPROC, RAPCOC
      CHARACTER*4   TYPCOE
      CHARACTER*8   NOMNOE, KBID, NOMG
      CHARACTER*8   NOMA, MOD, CMP, NOMCMP(NMOCL)
      CHARACTER*9   NOMTE
      CHARACTER*19  LIGRMO
      INTEGER       DG
      LOGICAL       EXISDG
      CHARACTER*1 K1BID
C --------- FIN  DECLARATIONS  VARIABLES LOCALES --------
C
      CALL JEMARQ()
      LISREL=LISREZ
      LIGRCH=LIGRCZ

C --- INITIALISATION ---
      EPS1 = 1.D2*R8PREM()
      EPS2 = 1.D0/R8GAEM()
C --- MODELE ASSOCIE AU LIGREL DE CHARGE ---
C
      CALL DISMOI('F','NOM_MODELE',LIGRCH(1:8),'CHARGE',IBID,MOD,IER)
C
C ---  LIGREL DU MODELE ---
C
      LIGRMO = MOD(1:8)//'.MODELE'
C
C --- MAILLAGE ASSOCIE AU MODELE ---
C
      CALL JEVEUO(LIGRMO//'.NOMA','L',JNOMA)
      NOMA = ZK8(JNOMA)
C
C --- RECUPERATION DES NOMS DES DDLS   ---
C
      IF (LIGRCH(12:13).EQ.'TH')  THEN
        NOMG = 'TEMP_R'
      ELSE IF (LIGRCH(12:13).EQ.'ME')  THEN
        NOMG = 'DEPL_R'
      ELSE IF (LIGRCH(12:13).EQ.'AC')  THEN
        NOMG = 'PRES_C'
      ENDIF
      CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',NOMG),'L',INOM)
      CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP',NOMG),'LONMAX',NBCMP,K1BID)
      NDDLA = NBCMP-1
      IF (NDDLA.GT.NMOCL) THEN
        CALL UTDEBM('F','ORDLRL','NOMBRE DE CMPS SUPERIEUR AU MAX')
        CALL UTIMPI('L','NMAXCMP= ',1,NMOCL)
        CALL UTIMPI('L','NCMP   = ',1,NDDLA)
        CALL UTFINM()
      ENDIF
C
      DO 1 I=1,NBCMP
        NOMCMP(I)=ZK8(INOM-1+I)
 1    CONTINUE
      CALL DISMOI('F','NB_EC',NOMG,'GRANDEUR',NBEC,KBID,IERD)
C
C --- ACCES A L'OBJET .PRNM ---
C
      IF (NBEC.GT.10) THEN
         CALL UTMESS('F','ORDLRL',
     &                   'LE DESCRIPTEUR_GRANDEUR DES DEPLACEMENTS'//
     &                    ' NE TIENT PAS SUR DIX ENTIERS CODES')
      ELSE
         CALL JEVEUO (LIGRMO//'.PRNM','L',JPRNM)
      END IF
C
C --- ACCES AUX COMPOSANTES DE LA LISTE_RELA
C
      CALL JEVEUO(LISREL//'.RLCO','L',IDCOE)
      CALL JEVEUO(LISREL//'.RLDD','L',IDL)
      CALL JEVEUO(LISREL//'.RLNO','L',IDNOE)
      CALL JEVEUO(LISREL//'.RLBE','L',IDBETA)
      CALL JEVEUO(LISREL//'.RLNT','L',IDTERM)
      CALL JEVEUO(LISREL//'.RLPO','L',IDPOIN)
      CALL JEVEUO(LISREL//'.RLSU','E',IDSURC)
      CALL JEVEUO(LISREL//'.RLTC','L',IDTYCO)
C
C --- TYPE DE VALEUR DES COEFFICIENTS DES RELATIONS ---
C
      TYPCOE = ZK8(IDTYCO)(1:4)
C
C --- NOMBRE DE RELATIONS DE LA LISTE_RELA
C
      CALL JEVEUO(LISREL//'.RLNR','L',IDNBRE)
      NBRELA = ZI(IDNBRE)
C
C --- NOMBRE DE TERMES  MAX IMPLIQUES DANS UNE RELATION
C
      NBTEMA = 0
      DO 10 IRELA = 1,NBRELA
C ---          NOMBRE DE TERMES DE LA RELATION       ---
         IF (NBTEMA.LT.ZI(IDTERM+IRELA-1)) NBTEMA = ZI(IDTERM+IRELA-1)
  10  CONTINUE
C
C --- CREATION D'UN VECTEUR DE TRAVAIL DESTINE A CONTENIR
C --- L'INDICE DU PLUS GRAND COEFFICIENT EN VALEUR ABSOLUE
C --- (MODULE) D'UNE RELATION
C
      CALL WKVECT ('&&ORDLRL.COEFMAX','V V I',NBRELA,IDCOMA)
C
C --- CREATION D'UN VECTEUR DE TRAVAIL DESTINE A CONTENIR
C --- LES NUMEROS DES NOEUDS D'UNE RELATION
C
      CALL WKVECT ('&&ORDLRL.NOEUD_RELA','V V I',NBTEMA,INOREL)
C
C --- CREATION D'UN VECTEUR DE TRAVAIL DESTINE A CONTENIR
C --- LE NOMBRE D'OCCURENCES DE CHAQUE NOEUD APPRAISSANT
C --- DANS UNE RELATION
C
      CALL WKVECT ('&&ORDLRL.NOEUD_OCC','V V I',NBTEMA,INOCC)
C
C --- CREATION D'UN VECTEUR DE TRAVAIL DESTINE A CONTENIR
C --- LES COEFFICIENTS REELS D'UNE RELATION
C
      CALL WKVECT ('&&ORDLRL.COEF_R','V V R',NBTEMA,IDCOER)
C
C --- CREATION D'UN VECTEUR DE TRAVAIL DESTINE A CONTENIR
C --- LES COEFFICIENTS COMPLEXES D'UNE RELATION
C
      CALL WKVECT ('&&ORDLRL.COEF_C','V V C',NBTEMA,IDCOEC)
C
C --- REARRANGEMENT DES TABLEAUX DES NOEUDS, DES DDLS ET DES
C --- COEFFICIENTS DE LA RELATION SELON L'ORDRE CROISSANT
C --- DES NOEUDS ET DES DDLS POUR UN NOEUD DONNE
C
C --- BOUCLE SUR LES RELATIONS DE LA LISTE_RELA
C
      DO 20 IRELA = 1,NBRELA
         IPNTRL = ZI(IDPOIN+IRELA-1)
         NBTERM = ZI(IDTERM+IRELA-1)
         IDECAL = IPNTRL - NBTERM
         IDCOEF = IDCOE + IDECAL
         IDNOEU = IDNOE + IDECAL
         IDDL   = IDL   + IDECAL
C
         IF (TYPCOE.EQ.'COMP') THEN
             DO 30 INO = 1,NBTERM
               ZC(IDCOEC+INO-1) = ZC(IDCOEF+INO-1)
 30          CONTINUE
         ELSE
             DO 40 INO = 1,NBTERM
               ZR(IDCOER+INO-1) = ZR(IDCOEF+INO-1)
 40          CONTINUE
         ENDIF
C
C ---          BOUCLE SUR LE NOMBRE DE NOEUDS LIES PAR ---
C ---          LA RELATION                             ---
C
         DO 50 INO = 1,NBTERM
C ---                 NOM DU NOEUD              ---
               NOMNOE = ZK8(IDNOEU+INO-1)
C ---                 NUMERO DU NOEUD              ---
               CALL JENONU(JEXNOM(NOMA//'.NOMNOE',NOMNOE),IN)
C
               ZI(INOREL+INO-1) = IN
C
               CMP = ZK8(IDDL+INO-1)
               ICMP = INDIK8(NOMCMP,CMP,1,NBCMP)
               IF (.NOT.EXISDG(ZI(JPRNM-1+(IN-1)*NBEC+1),ICMP)) THEN
                 CALL UTDEBM('F','ORDLRL','LE DDL')
                 CALL UTIMPK('S',CMP,1,'EST INTERDIT POUR LE NOEUD')
                 CALL UTIMPK('S',NOMNOE,1,' ' )
                 CALL UTFINM()
               ENDIF
 50      CONTINUE
C
C ---          REARRANGEMENT DES TABLEAUX DE LA RELATION SELON
C ---           L'ORDRE CROISSANT DES NOEUDS ET L'ORDRE
C ---           CROISSANT DES DDLS POUR UN NOEUD DONNE
C
         CALL ORDREL(ZI(INOREL), ZK8(IDNOEU), ZK8(IDDL), ZR(IDCOER),
     +               ZC(IDCOEC), ZI(INOCC), NBTERM, ZK8(INOM), NDDLA)
C
C ---         REAFFECTATION DU TABLEAU DES COEFFICIENTS
C
         IF (TYPCOE.EQ.'COMP') THEN
             DO 60 INO = 1,NBTERM
                ZC(IDCOEF+INO-1) = ZC(IDCOEC+INO-1)
 60          CONTINUE
         ELSE
             DO 70 INO = 1,NBTERM
               ZR(IDCOEF+INO-1) = ZR(IDCOER+INO-1)
 70          CONTINUE
         ENDIF
C
         COEMAX = 0.0D0
         IF (TYPCOE.EQ.'COMP') THEN
             DO 80 INO = 1,NBTERM
                IF (ABS(ZC(IDCOEC+INO-1)).GT.COEMAX) THEN
                    COEMAX = ABS(ZC(IDCOEC+INO-1))
                    INDMAX = INO
                ENDIF
 80          CONTINUE
         ELSE
             DO 90 INO = 1,NBTERM
                IF (ABS(ZR(IDCOER+INO-1)).GT.COEMAX) THEN
                    COEMAX = ABS(ZR(IDCOER+INO-1))
                    INDMAX = INO
                ENDIF
 90          CONTINUE
         ENDIF
         ZI(IDCOMA+IRELA-1) = INDMAX
C --- FIN DE LA BOUCLE SUR LES RELATIONS  ---
 20   CONTINUE
C
C --- IDENTIFICATION DES RELATIONS REDONDANTES ---
C
      DO 100 IRELA1 = NBRELA,2,-1
         IPNTR1 = ZI(IDPOIN+IRELA1-1)
         NBTER1 = ZI(IDTERM+IRELA1-1)
         IDECA1 = IPNTR1 - NBTER1
         IDCOE1 = IDCOE + IDECA1
         IDNOE1 = IDNOE + IDECA1
         IDDL1  = IDL   + IDECA1
C
         INDMAX = ZI(IDCOMA+IRELA1-1)
C
         IF (TYPCOE.EQ.'COMP') THEN
            IF (ABS(ZC(IDCOE1+INDMAX-1)).LT.EPS2) THEN
                CALL UTMESS('F','ORDLRL','PROBLEME SUR'
     &    //' UNE RELATION : LES COEFFICIENTS SONT TROP PETITS')
            ENDIF
         ELSE
            IF (ABS(ZR(IDCOE1+INDMAX-1)).LT.EPS2) THEN
                CALL UTMESS('F','ORDLRL','PROBLEME SUR'
     &    //' UNE RELATION : LES COEFFICIENTS SONT TROP PETITS')
            ENDIF
         ENDIF
C
C ---          COMPARAISON DE LA RELATION COURANTE      ---
C ---          AVEC LES RELATIONS RESTANTES             ---
C ---          CAS COMPLEXE                             ---
C
         IF (TYPCOE.EQ.'COMP') THEN
           DO 110 IRELA2 = IRELA1-1,1,-1
            IPNTR2 = ZI(IDPOIN+IRELA2-1)
            NBTER2 = ZI(IDTERM+IRELA2-1)
            IDECA2 = IPNTR2 - NBTER2
            IDCOE2 = IDCOE + IDECA2
            IDNOE2 = IDNOE + IDECA2
            IDDL2  = IDL   + IDECA2
C
            COPROC = ZC(IDCOE2+INDMAX-1)/ZC(IDCOE1+INDMAX-1)
C
            IF (NBTER1.EQ.NBTER2) THEN
               ICOMP = 0
               DO 120 INO = 1, NBTER1
                  IF (ZK8(IDNOE1+INO-1).EQ.ZK8(IDNOE2+INO-1)) THEN
                       IF (ZK8(IDDL1+INO-1).EQ.ZK8(IDDL2+INO-1)) THEN
                           RAPCOC = COPROC*ZC(IDCOE1+INO-1)
                           EPSREL = EPS1*ABS(ZC(IDCOE1+INO-1))
                           DIFREL = ABS(ZC(IDCOE2+INO-1)-RAPCOC)
                           IF (DIFREL.LE.EPSREL) GOTO 120
                           ICOMP =1
                           GOTO 121
                       ELSE
                           ICOMP = 1
                           GOTO 121
                       ENDIF
                  ELSE
                    ICOMP = 1
                    GOTO 121
                  ENDIF
 120              CONTINUE
 121              CONTINUE
C
                  IF (ICOMP.EQ.0) THEN
                      ZI(IDSURC+IRELA2-1) = 1
                  ENDIF
               ENDIF
C ---        FIN DE LA BOUCLE SUR LES RELATIONS  IRELA2 ---
 110      CONTINUE
C
C ---          CAS REEL                             ---
C
         ELSE
           DO 130 IRELA2 = IRELA1-1,1,-1
            IPNTR2 = ZI(IDPOIN+IRELA2-1)
            NBTER2 = ZI(IDTERM+IRELA2-1)
            IDECA2 = IPNTR2 - NBTER2
            IDCOE2 = IDCOE + IDECA2
            IDNOE2 = IDNOE + IDECA2
            IDDL2  = IDL   + IDECA2
C
            COPROR = ZR(IDCOE2+INDMAX-1)/ZR(IDCOE1+INDMAX-1)
C
            IF (NBTER1.EQ.NBTER2) THEN
               ICOMP = 0
               DO 140 INO = 1, NBTER1
                   IF (ZK8(IDNOE1+INO-1).EQ.ZK8(IDNOE2+INO-1)) THEN
                     IF (ZK8(IDDL1+INO-1).EQ.ZK8(IDDL2+INO-1)) THEN
                           RAPCOE = COPROR*ZR(IDCOE1+INO-1)
                           EPSREL = EPS1*ABS(ZR(IDCOE1+INO-1))
                           DIFREL = ABS(ZR(IDCOE2+INO-1)-RAPCOE)
                           IF (DIFREL.LE.EPSREL) GOTO 140
                           ICOMP = 1
                           GOTO 141
                     ELSE
                           ICOMP = 1
                           GOTO 141
                     ENDIF
                   ELSE
                     ICOMP = 1
                     GOTO 141
                   ENDIF
 140               CONTINUE
 141               CONTINUE
C
                  IF (ICOMP.EQ.0) THEN
                      ZI(IDSURC+IRELA2-1) = 1
                  ENDIF
               ENDIF
C ---        FIN DE LA BOUCLE SUR LES RELATIONS  IRELA2 ---
 130      CONTINUE
         ENDIF
C ---  FIN DE LA BOUCLE SUR LES RELATIONS  IRELA1 ---
 100   CONTINUE
C
C ---  MENAGE  ---
C
      CALL JEDETR ('&&ORDLRL.NOEUD_RELA')
      CALL JEDETR ('&&ORDLRL.NOEUD_OCC')
      CALL JEDETR ('&&ORDLRL.COEFMAX')
      CALL JEDETR ('&&ORDLRL.COEF_R')
      CALL JEDETR ('&&ORDLRL.COEF_C')
C
      CALL JEDEMA()
      END
