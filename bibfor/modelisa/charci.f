      SUBROUTINE CHARCI(CHCINE,MFACT,NOC,NDDL,NOMDDL,TYPE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) CHCINE,MFACT,NOMDDL(*)
      CHARACTER*1 TYPE
      INTEGER NOC,NDDL
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 17/12/2002   AUTEUR CIBHHGB G.BERTRAND 
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
C-----------------------------------------------------------------------
C OBJET :
C        TRAITEMENT DES MOTS CLES FACTEURS DE L'OPERATEUR
C
C-----------------------------------------------------------------------
C VAR  CHCINE  K*19    : NOM DE LA CHARGE CINEMATIQUE
C IN   MFACT   K*16    : MOT CLE FACTEUR A TRAITER
C                        MOTS CLES ADMIS : MECA_IMPO
C                                          THER_IMPO
C                                          ACOU_IMPO
C IN   NOC     I       : NOMBRE D'OCCURRENCE DU MOT_CLE FACTEUR
C IN   NDDL    I       : DIMENSION DE NOMDDL
C IN   NOMDDL  K*16(*) : TABLEAU DES NOMS DES DEGRES DE LIBERTE QUI
C                        PEUVENT ETRE IMPOSES
C IN   TYPE    K*1     : 'R','C' OU 'F' TYPE DES MOTS CLES
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      CHARACTER*2 TYP
      CHARACTER*8 MA,MO,MCLE
      CHARACTER*16 OPER,MFAC,KBID
      CHARACTER*19 CHCI,LIGRMO
      CHARACTER*24 CINO,CGNO,CNUDDL,CVLDDL
      CHARACTER*24 GRNO,NOMNOE,NPROL
      DATA NPROL/'                   .PROL'/
C --- DEBUT -----------------------------------------------------------
C
      CALL JEMARQ()
      IF (NOC.EQ.0.D0) GOTO 9999
      CHCI = CHCINE
      MFAC = MFACT
      CALL DISMOI('F','NOM_MODELE',CHCI,'CHARGE',IBID,MO,IER)
      CALL DISMOI('F','NOM_MAILLA',MO,'MODELE',IBID,MA,IER)
      CALL DISMOI('F','PHENOMENE',MO,'MODELE',IBID,KBID,IER)
      CALL DISMOI('F','NUM_GD',KBID,'PHENOMENE',NGD,KBID,IER)
      CALL DISMOI('F','NB_NO_MAILLA',MA,'MAILLAGE',NBNOMA,KBID,IER)
      GRNO=MA//'.GROUPENO'
      NOMNOE=MA//'.NOMNOE'
C --- NOM DE TABLEAUX DE TRAVAIL GERES PAR JEVEUX
C
      CINO = '&&CHARCI.INO'
      CGNO = '&&CHARCI.GNO'
      CNUDDL = '&&CHARCI.NUMDDL'
      CVLDDL = '&&CHARCI.VALDDL'
      DO 1 IOC = 1,NOC
C
C --- LECTURE DES MOTS CLES 'TOUT', 'GROUP_NO' OU 'NOEUD'
C
        CALL GETVTX(MFAC,'TOUT',IOC,1,0,KBID,ILA)
        IF (ILA.NE.0) THEN
C
C       -----------
C       CAS DE TOUT
C       -----------
C
          IIMA = NBNOMA
          NINO = 0
          IDINO = 0
        ELSE
          IIMA = 0
          CALL GETVEM(MA,'GROUP_NO',MFAC,'GROUP_NO',
     +         IOC,1,0,KBID,ILA)
          IF (ILA.NE.0) THEN
            ILA = -ILA
C
C       ---------------
C       CAS DE GROUP_NO
C       ---------------
C
            CALL WKVECT(CGNO,'V V K8',ILA,IDGNO)
            CALL GETVEM(MA,'GROUP_NO',MFAC,'GROUP_NO',
     +           IOC,1,ILA,ZK8(IDGNO),IBID)
            NINO = 0
            DO 10 IGR = 1, ILA
C
C --- VERIFICATION DE L'EXISTANCE DES GROUP_NO
              CALL JENONU(JEXNOM(GRNO,ZK8(IDGNO-1+IGR)),INUM)
              IF (INUM.EQ.0) THEN
                CALL UTDEBM('F','CHARCI_1','ERREUR DANS LE FICHIER DE '
     +                      //'COMMANDE')
                CALL UTIMPK('L','LE GROUP_NO : ',1,ZK8(IDGNO-1+IGR))
                CALL UTIMPK('S','NE FAIT PAS PARTIE DU MAILLAGE : ',
     +                      1,MA)
                CALL UTFINM
              ENDIF
              CALL JELIRA(JEXNOM(GRNO,ZK8(IDGNO-1+IGR)),'LONMAX'
     +                   ,LNO,KBID)
              NINO = NINO + LNO
10          CONTINUE
            CALL WKVECT(CINO,'V V I',NINO,IDINO)
            INO = 0
            DO 11 IGR = 1, ILA
              CALL JEVEUO(JEXNOM(GRNO,ZK8(IDGNO-1+IGR)),'L',IDGRNO)
              CALL JELIRA(JEXNOM(GRNO,ZK8(IDGNO-1+IGR)),'LONMAX'
     +                   ,LNO,KBID)
              DO 12 I = 1, LNO
                INO = INO + 1
                ZI(IDINO-1+INO) = ZI(IDGRNO-1+I)
12            CONTINUE
11          CONTINUE
            CALL JEDETR(CGNO)
          ELSE
C
C       ------------
C       CAS DE NOEUD
C       ------------
C
            CALL GETVEM(MA,'NOEUD',MFAC,'NOEUD',
     +        IOC,1,0,KBID,ILA)
C --- D'APRES LA COMMANDE ON NE PEUT PAS AVOIR ILA=0
            NINO = -ILA
            CALL WKVECT(CGNO,'V V K8',NINO,IDGNO)
            CALL GETVEM(MA,'NOEUD',MFAC,'NOEUD',
     +        IOC,1,NINO,ZK8(IDGNO),IBID)
            CALL WKVECT(CINO,'V V I',NINO,IDINO)
            DO 20 I = 0,NINO-1
              CALL JENONU(JEXNOM(NOMNOE,ZK8(IDGNO+I)),INO)
              ZI(IDINO+I) = INO
20          CONTINUE
            CALL JEDETR(CGNO)
          ENDIF
        ENDIF
C
C --- LECTURE DES MOTS CLES RELATIFS AUX VALEURS IMPOSEES
C
        CALL WKVECT(CNUDDL,' V V I',NDDL,IDNDDL)
        IF (TYPE.EQ.'F') THEN
          TYP = 'K8'
        ELSE IF (TYPE.EQ.'R') THEN
          TYP = TYPE
        ELSE IF (TYPE.EQ.'C') THEN
          TYP = TYPE
        ELSE
          CALL UTMESS('F','CHARCI_1','TYPE INCONNU VERIFIER LE CALL'
     +               //' A CHARCI')
        ENDIF
        CALL WKVECT(CVLDDL,' V V '//TYP,NDDL,IDVDDL)
        NBDDL = 0
        DO 30 IDDL = 1,NDDL
          IF (TYPE.EQ.'R')
     +      CALL GETVR8(MFAC,NOMDDL(IDDL),IOC,1,1,ZR(IDVDDL+NBDDL),ILA)
          IF (TYPE.EQ.'C')
     +      CALL GETVC8(MFAC,NOMDDL(IDDL),IOC,1,1,ZC(IDVDDL+NBDDL),ILA)
          IF (TYPE.EQ.'F')CALL GETVID(MFAC,NOMDDL(IDDL),IOC,1,1,
     +  ZK8(IDVDDL+NBDDL),ILA)
          IF (ILA.NE.0) THEN
            ZI(IDNDDL+NBDDL)= IDDL
            NBDDL = NBDDL+1
          ENDIF
30      CONTINUE
C --- ON RECHERCHE SI UNE QUAND ON A DES FONCT. IL Y EN A UNE = F(TPS)
C
        IF (TYPE.EQ.'F') THEN
          DO 130 I = 1,NBDDL
            NPROL(1:8) = ZK8(IDVDDL-1+I)
            CALL JEVEUO(NPROL,'L',IDPROL)
            IF ( ZK16(IDPROL+2).EQ.'INST') THEN
              CALL JEVEUO(CHCI(1:8)//'.TYPE','E',IDTYPE)
              ZK8(IDTYPE)(5:7) = '_FT'
              GOTO 131
            ELSE IF (( ZK16(IDPROL).EQ.'NAPPE').AND.
     +               ( ZK16(IDPROL+5).EQ.'INST')) THEN
              CALL JEVEUO(CHCI(1:8)//'.TYPE','E',IDTYPE)
              ZK8(IDTYPE)(5:7) = '_FT'
              GOTO 131
            ENDIF
130       CONTINUE
        ENDIF
131     CONTINUE
        CALL CHCSUR(CHCI,MO,NGD,IIMA,NINO,ZI(IDINO),NBDDL,ZI(IDNDDL),
     +              CVLDDL,TYPE)
        IF (IDINO.NE.0) CALL JEDETR(CINO)
        CALL JEDETR(CNUDDL)
        CALL JEDETR(CVLDDL)
1     CONTINUE
 9999 CONTINUE
      CALL JEDEMA()
      END
