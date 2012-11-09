      SUBROUTINE TOPOCA ( TABLCA, MAILLA, ICABL, NBF0, NBNOCA, NUMACA )
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C-----------------------------------------------------------------------
C  DESCRIPTION : CARACTERISATION DE LA TOPOLOGIE D'UN CABLE
C  -----------   APPELANT : OP0180 , OPERATEUR DEFI_CABLE_BP
C
C                EN SORTIE ON AJOUTE DES LIGNES DANS LA TABLE RESULTAT
C                LES CASES RENSEIGNEES CORRESPONDENT AUX PARAMETRES
C                <NUME_CABLE>, <NOEUD_CABLE>, <MAILLE_CABLE>,
C                <NOM_CABLE>, <NOM_ANCRAGE1> ET <NOM_ANCRAGE2>
C
C  IN     : TABLCA : CHARACTER*19
C                    NOM DE LA TABLE DECRIVANT LES CABLES
C  IN     : MAILLA : CHARACTER*8 , SCALAIRE
C                    NOM DU CONCEPT MAILLAGE ASSOCIE A L'ETUDE
C  IN     : ICABL  : INTEGER , SCALAIRE
C                    NUMERO DU CABLE
C  OUT    : NBF0   : INTEGER , SCALAIRE
C                    NOMBRE D'ANCRAGES ACTIFS DU CABLE (0, 1 OU 2)
C  IN/OUT : NBNOCA : INTEGER , VECTEUR DE DIMENSION NBCABL
C                    CONTIENT LES NOMBRES DE NOEUDS DE CHAQUE CABLE
C  IN     : NUMACA : CHARACTER*19 , SCALAIRE
C                    NOM D'UN VECTEUR D'ENTIERS POUR STOCKAGE DES
C                    NUMEROS DES MAILLES APPARTENANT AUX CABLES
C                    CE VECTEUR EST COMPLETE A CHAQUE PASSAGE DANS LA
C                    ROUTINE TOPOCA : REAJUSTEMENT DE LA DIMENSION PUIS
C                    REMPLISSAGE DU DERNIER SOUS-BLOC ALLOUE
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C  TOLE CRP_20
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
C
C ARGUMENTS
C ---------
      CHARACTER*8   MAILLA
      INTEGER       ICABL, NBF0, NBNOCA(*)
      CHARACTER*19  NUMACA, TABLCA
C
C VARIABLES LOCALES
C -----------------
      INTEGER       IBID, IMAIL, INO, IRET, ISUIV, ISUIV0(2), IVOIS,
     &              JCXMA, JNOMAD, JNONO1, JNONO2, JNONOD, JNUMA1,
     &              JNUMA2, JNUMAC, JNUMAD, JTYMA, LONUTI, NBCHEM,
     &              NBMAIL, NBNO1, NBNO2, NBSUIV, NO1, NO2, NTSEG,
     &              NUMAIL, N1
      REAL*8        RBID
      COMPLEX*16    CBID
      LOGICAL       OK1, OK2
      CHARACTER*1   K1B
      CHARACTER*3   K3B
      CHARACTER*8   K8B, NOANCR(2), NOGRMA, NOGRNA(2), NOCOUR, NOPREC,
     &              NOSUI1, NOSUI2, NOSUIV, NOVOIS, TYANCR(2)
      CHARACTER*8   VK(4), NOGRNO(2), PRESEN(2)
      CHARACTER*24  CONXMA, GRMAMA, NOMAMA, NONOMA, TYMAMA
      CHARACTER*24  VALK(3)
      CHARACTER*24  PARAM(5)
      INTEGER      IARG
      DATA          PARAM /'NUME_CABLE              ',
     &                     'NOEUD_CABLE             ',
     &                     'NOM_CABLE               ',
     &                     'NOM_ANCRAGE1            ',
     &                     'NOM_ANCRAGE2            '/

C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      CALL JEMARQ()
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 1   SAISIE DES ENTITES TOPOLOGIQUES ASSOCIEES AU CABLE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      CONXMA = MAILLA//'.CONNEX'
      GRMAMA = MAILLA//'.GROUPEMA'
      NOMAMA = MAILLA//'.NOMMAI'
      NONOMA = MAILLA//'.NOMNOE'
      TYMAMA = MAILLA//'.TYPMAIL'
      CALL JEVEUO(TYMAMA,'L',JTYMA)
C
C 1.1 SAISIE DES MAILLES ASSOCIEES
C ---
      CALL GETVEM(MAILLA,'MAILLE','DEFI_CABLE','MAILLE',
     &           ICABL,IARG,0,K8B,NBMAIL)
C
C.... SAISIE DIRECTE
C
      IF ( NBMAIL.NE.0 ) THEN
C
         NBMAIL = ABS(NBMAIL)
         CALL WKVECT('&&TOPOCA.NOMAIL_DEF','V V K8',NBMAIL,JNOMAD)
         CALL WKVECT('&&TOPOCA.NUMAIL_DEF','V V I' ,NBMAIL,JNUMAD)
         CALL GETVEM(MAILLA,'MAILLE','DEFI_CABLE','MAILLE',
     &              ICABL,IARG,NBMAIL,ZK8(JNOMAD),IBID)
         DO 10 IMAIL = 1, NBMAIL
            CALL JENONU(JEXNOM(NOMAMA,ZK8(JNOMAD+IMAIL-1)),
     &                  ZI(JNUMAD+IMAIL-1))
  10     CONTINUE
C
C.... SAISIE INDIRECTE PAR UN GROUPE DE MAILLES
C
      ELSE
C
         CALL GETVEM(MAILLA,'GROUP_MA','DEFI_CABLE','GROUP_MA',
     &                ICABL,IARG,1,NOGRMA,IBID)
         CALL JELIRA(JEXNOM(GRMAMA,NOGRMA),'LONUTI',NBMAIL,K1B)
         CALL JEVEUO(JEXNOM(GRMAMA,NOGRMA),'L',JNUMAD)
C
      ENDIF
C
C 1.2 VERIFICATION DU TYPE DES MAILLES ET DETERMINATION SIMULTANEE
C --- DE LEURS NOEUDS EXTREMITES
C
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG2'),NTSEG)
C
      CALL WKVECT('&&TOPOCA.NOMNOE_DEF','V V K8',2*NBMAIL,JNONOD)
C
      DO 20 IMAIL = 1, NBMAIL
         NUMAIL = ZI(JNUMAD+IMAIL-1)
         IF ( ZI(JTYMA+NUMAIL-1).NE.NTSEG ) THEN
            WRITE(K3B,'(I3)') ICABL
            CALL U2MESK('F','MODELISA7_54',1,K3B)
         ENDIF
         CALL JEVEUO(JEXNUM(CONXMA,NUMAIL),'L',JCXMA)
         NO1 = ZI(JCXMA)
         NO2 = ZI(JCXMA+1)
         CALL JENUNO(JEXNUM(NONOMA,NO1),ZK8(JNONOD+2*(IMAIL-1)))
         CALL JENUNO(JEXNUM(NONOMA,NO2),ZK8(JNONOD+2*(IMAIL-1)+1))
  20  CONTINUE
C
C 1.3 SAISIE DU GROUP_NO D'ANCRAGE DU CABLE EVENTUELLEMENT
C ---
      NOGRNO(1) = '        '
      NOGRNO(2) = '        '
      CALL GETVTX ('DEFI_CABLE','GROUP_NO_FUT',ICABL,IARG,2,NOGRNO,N1)
      IF ( N1.EQ.1 ) THEN
        CALL GETVTX ('CONE','PRESENT',1,IARG,2,PRESEN,N1)
        IF (PRESEN(2)(1:3).EQ.'OUI') THEN
          NOGRNO(2) = NOGRNO(1)
          NOGRNO(1) = '        '
        ENDIF
      ENDIF

C
C 1.4 SAISIE DES NOEUDS D'ANCRAGE DU CABLE
C ---
      CALL GETVEM(MAILLA,'NOEUD','DEFI_CABLE','NOEUD_ANCRAGE',
     &                  ICABL,IARG,0,K8B,IBID)
C
      IF ( IBID.EQ.0 ) THEN
C
         CALL GETVEM(MAILLA,'GROUP_NO','DEFI_CABLE','GROUP_NO_ANCRAGE',
     &                        ICABL,IARG,2,NOGRNA(1),IBID)
C
         CALL UTNONO(' ',MAILLA,'NOEUD',NOGRNA(1),K8B,IRET)
         IF ( IRET.EQ.10 ) THEN
            CALL U2MESK('F','ELEMENTS_67',1,NOGRNA(1))
         ELSE IF ( IRET.EQ.1 ) THEN
            VALK(1) = K8B
            CALL U2MESG('A', 'SOUSTRUC_87',1,VALK,0,0,0,0.D0)
         ENDIF
         NOANCR(1) = K8B
C
         CALL UTNONO(' ',MAILLA,'NOEUD',NOGRNA(2),K8B,IRET)
         IF ( IRET.EQ.10 ) THEN
            CALL U2MESK('F','ELEMENTS_67',1,NOGRNA(1))
         ELSE IF ( IRET.EQ.1 ) THEN
            VALK(1) = K8B
            CALL U2MESG('A', 'SOUSTRUC_87',1,VALK,0,0,0,0.D0)
         ENDIF
         NOANCR(2) = K8B
C
      ELSE
C
         CALL GETVEM(MAILLA,'NOEUD','DEFI_CABLE','NOEUD_ANCRAGE',
     &                     ICABL,IARG,2,NOANCR(1),IBID)
C
      ENDIF
C
      CALL GETVTX(' ','TYPE_ANCRAGE' ,ICABL,IARG,2,TYANCR(1),IBID)
      NBF0 = 0
      IF ( TYANCR(1)(1:5).EQ.'ACTIF' ) NBF0 = NBF0 + 1
      IF ( TYANCR(2)(1:5).EQ.'ACTIF' ) NBF0 = NBF0 + 1
      IF ( (NBF0.EQ.1).AND.(TYANCR(1)(1:6).EQ.'PASSIF') ) THEN
         K8B = NOANCR(1)
         NOANCR(1) = NOANCR(2)
         NOANCR(2) = K8B
         K8B = TYANCR(1)
         TYANCR(1) = TYANCR(2)
         TYANCR(2) = K8B
      ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 2   DETERMINATION D'UN CHEMIN CONTINU DEFINISSANT LE CABLE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C 2.1 DETERMINATION DU NOMBRE DE CHEMINS POSSIBLES AU DEPART DU PREMIER
C --- NOEUD D'ANCRAGE
C
      NBCHEM = 0
      DO 30 INO = 1, 2*NBMAIL
         IF ( ZK8(JNONOD+INO-1).EQ.NOANCR(1) ) THEN
            IF ( MOD(INO,2).EQ.0 ) THEN
               ISUIV = INO - 1
            ELSE
               ISUIV = INO + 1
            ENDIF
            NOSUIV = ZK8(JNONOD+ISUIV-1)
            IF ( NOSUIV.NE.NOANCR(1) ) THEN
               NBCHEM = NBCHEM + 1
               IF ( NBCHEM.GT.2 ) THEN
                  WRITE(K3B,'(I3)') ICABL
                   VALK(1) = K3B
                   VALK(2) = NOANCR(1)
                   CALL U2MESK('F','MODELISA7_55', 2 ,VALK)
               ENDIF
               ISUIV0(NBCHEM) = ISUIV
            ENDIF
         ENDIF
  30  CONTINUE
C
      IF ( NBCHEM.EQ.0 ) THEN
         WRITE(K3B,'(I3)') ICABL
          VALK(1) = K3B
          VALK(2) = NOANCR(1)
          CALL U2MESK('F','MODELISA7_56', 2 ,VALK)
      ENDIF
C
      NOSUI1 = ZK8(JNONOD+ISUIV0(1)-1)
      IF ( NBCHEM.EQ.2 ) THEN
         NOSUI2 = ZK8(JNONOD+ISUIV0(2)-1)
         IF ( NOSUI1.EQ.NOSUI2 ) NBCHEM = 1
      ENDIF
C
C 2.2 TENTATIVE DE PARCOURS DU PREMIER CHEMIN POSSIBLE
C ---
      CALL WKVECT('&&TOPOCA.NUMAIL_CH1','V V I' ,NBMAIL  ,JNUMA1)
      CALL WKVECT('&&TOPOCA.NOMNOE_CH1','V V K8',NBMAIL+1,JNONO1)
C
      OK1 = .FALSE.
C
      NBNO1 = 1
      ZK8(JNONO1) = NOANCR(1)
      IF ( MOD(ISUIV0(1),2).EQ.0 ) THEN
         IMAIL = ISUIV0(1)/2
      ELSE
         IMAIL = (ISUIV0(1)+1)/2
      ENDIF
      ZI(JNUMA1) = ZI(JNUMAD+IMAIL-1)
      NOPREC = NOANCR(1)
      NOCOUR = NOSUI1
C
C.... REPETER (DEBUT)
  40  CONTINUE
         IF ( NOCOUR.EQ.NOANCR(2) ) THEN
            NBNO1 = NBNO1 + 1
            ZK8(JNONO1+NBNO1-1) = NOANCR(2)
            OK1 = .TRUE.
            GO TO 60
         ENDIF
         NBSUIV = 0
         DO 50 INO = 1, 2*NBMAIL
            IF ( ZK8(JNONOD+INO-1).EQ.NOCOUR ) THEN
               IF ( MOD(INO,2).EQ.0 ) THEN
                  IVOIS = INO - 1
               ELSE
                  IVOIS = INO + 1
               ENDIF
               NOVOIS = ZK8(JNONOD+IVOIS-1)
               IF ( (NOVOIS.NE.NOCOUR).AND.(NOVOIS.NE.NOPREC) ) THEN
                  NBSUIV = NBSUIV + 1
                  IF ( NBSUIV.GT.1 ) GO TO 60
                  NOSUIV = NOVOIS
                  ISUIV = IVOIS
               ENDIF
            ENDIF
  50     CONTINUE
         IF ( NBSUIV.EQ.0 ) GO TO 60
         NBNO1 = NBNO1 + 1
         ZK8(JNONO1+NBNO1-1) = NOCOUR
         IF ( MOD(ISUIV,2).EQ.0 ) THEN
            IMAIL = ISUIV/2
         ELSE
            IMAIL = (ISUIV+1)/2
         ENDIF
         ZI(JNUMA1+NBNO1-1) = ZI(JNUMAD+IMAIL-1)
         NOPREC = NOCOUR
         NOCOUR = NOSUIV
         IF ( NBNO1.LT.NBMAIL+1 ) GO TO 40
C
C.... REPETER (FIN)
  60  CONTINUE
C
C 2.3 TENTATIVE DE PARCOURS DU SECOND CHEMIN POSSIBLE LE CAS ECHEANT
C ---
      OK2 = .FALSE.
C
      IF ( NBCHEM.EQ.2 ) THEN
C
         CALL WKVECT('&&TOPOCA.NUMAIL_CH2','V V I' ,NBMAIL  ,JNUMA2)
         CALL WKVECT('&&TOPOCA.NOMNOE_CH2','V V K8',NBMAIL+1,JNONO2)
C
         NBNO2 = 1
         ZK8(JNONO2) = NOANCR(1)
         IF ( MOD(ISUIV0(2),2).EQ.0 ) THEN
            IMAIL = ISUIV0(2)/2
         ELSE
            IMAIL = (ISUIV0(2)+1)/2
         ENDIF
         ZI(JNUMA2) = ZI(JNUMAD+IMAIL-1)
         NOPREC = NOANCR(1)
         NOCOUR = NOSUI2
C
C....... REPETER (DEBUT)
  70     CONTINUE
            IF ( NOCOUR.EQ.NOANCR(2) ) THEN
               NBNO2 = NBNO2 + 1
               ZK8(JNONO2+NBNO2-1) = NOANCR(2)
               OK2 = .TRUE.
               GO TO 90
            ENDIF
            NBSUIV = 0
            DO 80 INO = 1, 2*NBMAIL
               IF ( ZK8(JNONOD+INO-1).EQ.NOCOUR ) THEN
                  IF ( MOD(INO,2).EQ.0 ) THEN
                     IVOIS = INO - 1
                  ELSE
                     IVOIS = INO + 1
                  ENDIF
                  NOVOIS = ZK8(JNONOD+IVOIS-1)
                  IF ( (NOVOIS.NE.NOCOUR).AND.(NOVOIS.NE.NOPREC) ) THEN
                     NBSUIV = NBSUIV + 1
                     IF ( NBSUIV.GT.1 ) GO TO 90
                     NOSUIV = NOVOIS
                     ISUIV = IVOIS
                  ENDIF
               ENDIF
  80        CONTINUE
            IF ( NBSUIV.EQ.0 ) GO TO 90
            NBNO2 = NBNO2 + 1
            ZK8(JNONO2+NBNO2-1) = NOCOUR
            IF ( MOD(ISUIV,2).EQ.0 ) THEN
               IMAIL = ISUIV/2
            ELSE
               IMAIL = (ISUIV+1)/2
            ENDIF
            ZI(JNUMA2+NBNO2-1) = ZI(JNUMAD+IMAIL-1)
            NOPREC = NOCOUR
            NOCOUR = NOSUIV
            IF ( NBNO2.LT.NBMAIL+1 ) GO TO 70
C
C....... REPETER (FIN)
  90     CONTINUE
C
      ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 3   MISE A JOUR DES OBJETS DE SORTIE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C 3.1 AMBIGUITE SI DEUX CHEMINS CONTINUS POSSIBLES
C ---
      IF ( OK1 .AND. OK2 ) THEN
         WRITE(K3B,'(I3)') ICABL
          VALK(1) = K3B
          VALK(2) = NOANCR(1)
          VALK(3) = NOANCR(2)
          CALL U2MESK('F','MODELISA7_57', 3 ,VALK)
C
C 3.2 MISE A JOUR DES OBJETS DE SORTIE
C ---
      ELSE
C
C 3.2.1  CAS OU LE PREMIER CHEMIN POSSIBLE EST VALIDE
C .....
         IF ( OK1 ) THEN
C
            NBNOCA(ICABL) = NBNO1
C
            IF ( ICABL.EQ.1 ) THEN
               CALL JEECRA(NUMACA,'LONUTI',NBNO1-1,' ')
               CALL JEVEUO(NUMACA,'E',JNUMAC)
               DO 100 IMAIL = 1, NBNO1-1
                  ZI(JNUMAC+IMAIL-1) = ZI(JNUMA1+IMAIL-1)
 100           CONTINUE
            ELSE
               CALL JELIRA(NUMACA,'LONUTI',LONUTI,K1B)
               CALL JEECRA(NUMACA,'LONUTI',LONUTI+NBNO1-1,' ')
               CALL JEVEUO(NUMACA,'E',JNUMAC)
               DO 110 IMAIL = 1, NBNO1-1
                  ZI(JNUMAC+LONUTI+IMAIL-1) = ZI(JNUMA1+IMAIL-1)
 110           CONTINUE
            ENDIF
C
            DO 120 INO = 1, NBNO1
C                CALL TBAJLI(TABLCA,2,PARAM,
C      +                     ICABL,RBID,CBID,ZK8(JNONO1+INO-1),0)

               VK(1) = ZK8(JNONO1+INO-1)
               VK(2) = NOGRMA
               VK(3) = NOGRNO(1)
               VK(4) = NOGRNO(2)
               CALL TBAJLI(TABLCA,5,PARAM,ICABL,RBID,CBID,VK,0)

 120        CONTINUE
C
C 3.2.2  CAS OU LE SECOND CHEMIN POSSIBLE EST VALIDE
C .....
         ELSE IF ( OK2 ) THEN
C
            NBNOCA(ICABL) = NBNO2
C
            IF ( ICABL.EQ.1 ) THEN
               CALL JEECRA(NUMACA,'LONUTI',NBNO2-1,' ')
               CALL JEVEUO(NUMACA,'E',JNUMAC)
               DO 130 IMAIL = 1, NBNO2-1
                  ZI(JNUMAC+IMAIL-1) = ZI(JNUMA2+IMAIL-1)
 130           CONTINUE
            ELSE
               CALL JELIRA(NUMACA,'LONUTI',LONUTI,K1B)
               CALL JEECRA(NUMACA,'LONUTI',LONUTI+NBNO2-1,' ')
               CALL JEVEUO(NUMACA,'E',JNUMAC)
               DO 140 IMAIL = 1, NBNO2-1
                  ZI(JNUMAC+LONUTI+IMAIL-1) = ZI(JNUMA2+IMAIL-1)
 140           CONTINUE
            ENDIF
C
            DO 150 INO = 1, NBNO2
C                CALL TBAJLI(TABLCA,2,PARAM,
C      +                     ICABL,RBID,CBID,ZK8(JNONO2+INO-1),0)

               VK(1) = ZK8(JNONO2+INO-1)
               VK(2) = NOGRMA
               VK(3) = NOGRNO(1)
               VK(4) = NOGRNO(2)
               CALL TBAJLI(TABLCA,5,PARAM,ICABL,RBID,CBID,VK,0)

 150        CONTINUE
C
C 3.2.3  AUCUN CHEMIN CONTINU VALIDE
C .....
         ELSE
C
            WRITE(K3B,'(I3)') ICABL
            CALL U2MESK('F','MODELISA7_58',1,K3B)
         ENDIF
C
      ENDIF
C
C --- MENAGE
      CALL JEDETR('&&TOPOCA.NOMAIL_DEF')
      CALL JEDETR('&&TOPOCA.NUMAIL_DEF')
      CALL JEDETR('&&TOPOCA.NOMNOE_DEF')
      CALL JEDETR('&&TOPOCA.NUMAIL_CH1')
      CALL JEDETR('&&TOPOCA.NOMNOE_CH1')
      CALL JEDETR('&&TOPOCA.NUMAIL_CH2')
      CALL JEDETR('&&TOPOCA.NOMNOE_CH2')
C
      CALL JEDEMA()
C
C --- FIN DE TOPOCA.
      END
