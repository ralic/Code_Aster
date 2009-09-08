      SUBROUTINE OP0152(IERR)
      IMPLICIT NONE
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/09/2009   AUTEUR PELLET J.PELLET 
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
C---------------------------------------------------------------------
C AUTEUR : G.ROUSSEAU
C OPERATEUR CALCULANT LA MASSE AJOUTEE, L'AMORTISSEMENT
C  ET LA RIGIDITE AJOUTEE EN THEORIE POTENTIELLE : CALC_MATR_AJOU
C     SUR BASE MODALE DE LA STRUCTURE DANS LE VIDE
C
C ATTENTION : LA MATR_ASSE GENERALISEE PRODUITE PAS CETTE COMMANDE
C             EST CREEE AU FORMAT LIGNE DE CIEL ET TRANSFORMEE AU
C             AU FORMAT MORSE A LA FIN DE OP0152 : UALFVA
C
C---------------------------------------------------------------------
C--------- DEBUT DES COMMUNS JEVEUX ----------------------------------
      CHARACTER*32     JEXNUM, JEXNOM, JEXR8, JEXATR
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16           ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     --- FIN DES COMMUNS JEVEUX ------------------------------------
      LOGICAL       VRAI,EXIGEO
      INTEGER       LDBLO,IBID,NPREC,MATER,NBVALE,NBREFE,NBDESC
      INTEGER       NBMO,NBMODE,NDBLE,INDICE,TABAD(5)
      INTEGER       IAVALE,IADESC,IAREFE,I,J,IMODE,IACONL
      INTEGER       IADIRG,IALIME,IBLO,IBLODI,IERD,IERR
      INTEGER       ILIRES,IMADE,IMDG,IND,INEQU
      INTEGER       IPHI1,IPHI2,IPRSTO,IRANG,IREFE,IRET,ITXSTO
      INTEGER       ITYSTO,ITZSTO,IVALK,JMDG,JND,JRANG,KCOMPT,JSCBL
      INTEGER       JSCDI,JSCDE,JSCHC,JSCIB,LLDESC,N1BLOC,N2BLOC
      INTEGER       NBID,NBLOC,NSTOC,NTBLOC,NTERM,NUEQ
      INTEGER       N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14
      INTEGER       IFM,NIV
      REAL*8        TPS(6),EPS,R8BID,MIJ,CIJ,KIJ,KIJ1,CIJ1,CIJ2
      REAL*8        BID,EBID
      CHARACTER*1   DIR
      CHARACTER*2   MODEL
      CHARACTER*3   INCR,ND
      CHARACTER*8   NOMRES, K8BID,MODMEC,REP,PHIBAR
      CHARACTER*8   MOFLUI,MOINT,MA,MATERI,NOMCMP(6)
      CHARACTER*8   CHAR,NUMGEN,MODGEN,MATGEN,LPAIN(2),LPAOUT(1)
      CHARACTER*9   OPTION
      CHARACTER*14  NU,NUM,NUDDL,NUGENE
      CHARACTER*16  RK16
      CHARACTER*16  TYPRES,NOMCOM
      CHARACTER*19  CH19,VECSO1,VECSO2
      CHARACTER*19  MAX,MAY,MAZ,VESTO1,VESTO2,CHAMNO,PHIB19
      CHARACTER*19  CHAINE,STOLCI,TPZSTO,SOLVEU
      CHARACTER*24  CHGEOM,LCHIN(2)
      CHARACTER*24  NOMCHA,CH24,CARELE,TIME,NOCHAM
      CHARACTER*24  MATE,LIGRMO,PHIB24,MADE
      CHARACTER*72  K72B
      COMPLEX*16    C16B,CBID
C -----------------------------------------------------------------
       DATA NOMCMP /'INST    ','DELTAT  ','THETA   ',
     &             'KHI     ','R       ','RHO     '/
       DATA TPS    /0.0D0,2*1.0D0,3*0.0D0/
       DATA SOLVEU   /'&&OP0152.SOLVEUR'/

C-----------------------------------------------------------------

      CALL JEMARQ()
C
      CALL INFMAJ
      CALL INFNIV(IFM,NIV)
C
      TIME = '&TIME'
      VRAI = .TRUE.
C
      CALL GETRES(NOMRES,TYPRES,NOMCOM)
C
C----------RECUPERATION DES ARGUMENTS DE LA COMMANDE--------------
C
C
      MATERI = ' '
      CALL GETVID(' ','MODELE_FLUIDE',0,1,1,MOFLUI,N1)
      CALL GETVID(' ','CHARGE',0,1,1,CHAR,N2)
      CALL GETVID(' ','MODELE_INTERFACE',0,1,1,MOINT,N3)
      CALL GETVID(' ','CHAM_MATER',0,1,1,MATERI,N4)
      MODMEC=' '
      CALL GETVID(' ','MODE_MECA',0,1,1,MODMEC,N5)
      CALL GETVID(' ','CHAM_NO',0,1,0,CHAMNO,N6)
      CALL GETVID(' ','NUME_DDL_GENE',0,1,1,NUMGEN,N9)
      CALL GETVID(' ','MODELE_GENE',0,1,1,MODGEN,N10)
      CALL GETVID(' ','POTENTIEL',0,1,1,PHIBAR,N12)
      CALL GETVTX(' ','OPTION',0,1,1,OPTION,N13)
      CALL GETVTX(' ','NOEUD_DOUBLE',0,1,1,ND,N14)
C
C LECTURE DES PARAMETRES DONNES APRES LE MOT CLE FACTEUR SOLVEUR
C
      CALL CRESOL (SOLVEU,' ')

C VERIFICATIONS SUPPLEMENTAIRES

      CALL VER152(OPTION,MOFLUI,MOINT,N12,MODEL)

C EXTRACTION DU POTENTIEL PERMANENT DES VITESSES

      IF(OPTION.EQ.'AMOR_AJOU'.OR.OPTION.EQ.'RIGI_AJOU')THEN
        CALL RSEXCH(PHIBAR,'TEMP',0,PHIB24,IRET)
      ENDIF


C     CAS NUME_DDL_GENE PRESENT
C     ------------------------------------
      IF (N9.NE.0) THEN
        NUGENE = NUMGEN
        STOLCI =  NUGENE//'.SLCS'

      ELSE
C       CREATION D UN NUME_DDL_GENE
C       ------------------------------------
        NUGENE =  NOMRES
        STOLCI =  NUGENE//'.SLCS'
        CALL CRNSLV(NUGENE,'LDLT','SANS','G')

        NBMODE = -N6
        CALL WKVECT(STOLCI//'.SCHC','G V I',NBMODE,JSCHC)
        DO 200 I=1,NBMODE
          ZI(JSCHC+I-1)=I
200     CONTINUE

        NBLOC=1
        CALL WKVECT(STOLCI//'.SCIB','G V I',NBMODE,JSCIB)
        DO 110 I=1,NBMODE
          ZI(JSCIB+I-1)=NBLOC
110     CONTINUE

        CALL WKVECT(STOLCI//'.SCBL','G V I',NBLOC+1,JSCBL)
        ZI(JSCBL)=0
        ZI(JSCBL+1)=NBMODE

        CALL WKVECT(STOLCI//'.SCDI','G V I',NBMODE,JSCDI)
        NTERM=0
        DO 120 I=1,NBMODE
          NTERM=NTERM+ZI(JSCHC+I-1)
          ZI(JSCDI+I-1)=NTERM
120     CONTINUE

        CALL WKVECT(STOLCI//'.SCDE','G V I',6,JSCDE)
        ZI(JSCDE-1+1)=NBMODE
        ZI(JSCDE-1+2)=NTERM
        ZI(JSCDE-1+3)=NBLOC
        ZI(JSCDE-1+4)=NBMODE
      ENDIF


      IF (N6 .NE. 0) THEN
             N7 = -N6
             VRAI=.FALSE.
      ELSE
             N7=0
      ENDIF

C--------- RECUPERATION DU MATERIAU FLUIDE----------------------------
        IF ( N4 .NE. 0 ) THEN
           CALL RCMFMC ( MATERI , MATE )
        ELSE
           MATE = ' '
        ENDIF

C--------CALCUL DE LA MATRICE ASSEMBLEE DE RIGIDITE DU FLUIDE---------

        CALL RIGFLU(MOFLUI,TIME,NOMCMP,TPS,N2,CHAR,MATE,SOLVEU,
     &              MA,NU)

C=====================================================================
C---------------- ALTERNATIVE CHAMNO OU MODE_MECA OU---------
C-----------------------------MODELE-GENE--------------------
C=====================================================================

C----------------------------------------------------------------
         IF (N5.GT.0) THEN
             CALL RSORAC(MODMEC,'LONUTI',IBID,BID,K8BID,CBID,
     +             EBID,'ABSOLU',NBMODE,1,NBID)
             NBMO = NBMODE
             CALL RSEXCH(MODMEC,'DEPL',1,NOMCHA,IRET)
             NOCHAM = NOMCHA
         ELSE
            IF(N7.GT.0) THEN
             NBMO = N7
C 1ERE CREATION DE VECTEUR DE NOMS DES CHAMPS DE DEPL_R
C REPRESENTANT LES MODES
C EN CAS D'UTILISATION DU MOT CLE CHAM-NO, CECI POUR
C MAT152
             CALL JECREO('&&OP0152.VEC','V V K8')
             CALL JEECRA('&&OP0152.VEC','LONMAX',N7, ' ')
             CALL JEVEUO('&&OP0152.VEC','E',IVALK)
             CALL GETVID(' ','CHAM_NO',0,1,N7,ZK8(IVALK),N6)
             NOCHAM = ZK8(IVALK)
             CALL CHPVER('F',NOCHAM,'NOEU','DEPL_R',IERD)
            ENDIF
         ENDIF
C--------------------------------------------------------------
C CALCUL DES MATR_ELEM AX ET AY DANS L'OPTION FLUX_FLUI_X ET _Y
C---------------SUR LE MODELE INTERFACE(THERMIQUE)-------------
C CALCUL DES MATRICES MODALES BI POUR L OPTION AMOR_AJOU
C--------------------------------------------------------------
         CALL MAT152(OPTION,MODEL,MOINT,NOCHAM,IVALK,NBMO,
     +               MAX,MAY,MAZ,NUM)
         CALL JEEXIN('&&MAT152.MADE',IRET)
         IF (IRET.GT.0) CALL JEVEUO('&&MAT152.MADE','E',IMADE)
C DESTRUCTION DU VECTEUR DE NOMS DES DEPL-R POUR RECREATION DS
C PHI152
         CALL JEEXIN('&&OP0152.VEC',IRET)
         IF (IRET.GT.0) CALL JEDETR('&&OP0152.VEC')

C================================================================
C CALCUL ET STOCKAGE DES POTENTIELS INSTATIONNAIRES PHI1 ET PHI2
C CORRESPONDANT RESPECTIVEMENT AUX EFFETS INERTIELS
C ET AUX EFFETS D'AMORTISSEMENT ET DE RAIDEUR DU FLUIDE
C SUR LA STRUCTURE
C================================================================
         CALL PHI152(MODEL,OPTION,MATE,PHIB24,MA,NU,NUM,NBMODE,
     &               SOLVEU,INDICE,TABAD)

C VERIFICATION D EXISTENCE DE VECTEUR DE CHAMPS AUX NOEUDS CREES
C DS PHI152 ILS SERONT ENSUITE EXPLOITES DS CAL152 ENTRE AUTRES
C VECTEUR DE NOMS DU POTENTIEL INSTATIONNAIRE PHI1 : MASSE AJOU
C ON Y STOCKE LES NOMS DES POTENTIELS INSTATIONNAIRES POUR
C CHAQUE MODE DE STRUCTURE
         CALL JEEXIN('&&OP0152.PHI1',IRET)
         IF (IRET.GT.0) CALL JEVEUO('&&OP0152.PHI1','E',IPHI1)

C VECTEUR DE NOMS DU POTENTIEL INSTATIONNAIRE PHI2 : AMOR AJOU
C RIGI AJOU
         CALL JEEXIN('&&OP0152.PHI2',IRET)
         IF (IRET.GT.0) CALL JEVEUO('&&OP0152.PHI2','E',IPHI2)

C VECTEUR DE NOMS DES CHAMPS DE DEPL_R REPRESENTANT LES MODES
C EN CAS D'UTILISATION DU MOT CLE CHAM-NO
         CALL JEEXIN('&&OP0152.VEC',IRET)
         IF (IRET.GT.0) CALL JEVEUO('&&OP0152.VEC','E',IVALK)


C
C================================================================
C----------- CREATION DE LA MATR_ASSE_GENE    -------------------
C----------- CONTENANT LA MASSE AJOUTEE RESULTAT   --------------
C================================================================
C
         CALL MAG152(N9,N10,NOMRES,NUGENE,MODMEC,MODGEN,
     &               NBLOC,INDICE)

C=====================================================================
C---------------------------------------------------------------------
C              CALCUL SUR MODELE GENERALISE
C---------------------------------------------------------------------
C=====================================================================

      IF(N10.GT.0) THEN
         IF (ND.EQ.'OUI') THEN
            NDBLE=1
         ELSE
            NDBLE=0
         ENDIF
         CALL CALMDG(MODEL,MODGEN,NUGENE,NUM,NU,MA,MATE,MOINT,
     &             MOFLUI,NDBLE,
     &             ITXSTO,ITYSTO,ITZSTO,IPRSTO,NBMO,IADIRG)

      ENDIF

C
C=============================================================
C--------REMPLISSAGE DU  .VALE : CALCUL DE LA MASSE AJOUTEE
C=============================================================
C
C---------------------IMPRESSION DES RESULTATS------------------

      IF (NIV .GT. 1) THEN
           IF (OPTION.EQ.'MASS_AJOU')THEN
      WRITE(IFM,*) '        '
      WRITE(IFM,*) '          =======MATRICE DE MASSE AJOUTEE======='
             IF (N10.GT.0) THEN
               WRITE(IFM,*) '           ========HORS DDL DE LAGRANGE==='
             ENDIF
           ENDIF
           IF (OPTION.EQ.'AMOR_AJOU')THEN
      WRITE(IFM,*) '        '
      WRITE(IFM,*) '         =====MATRICE D AMORTISSEMENT AJOUTE====='
           ENDIF
           IF (OPTION.EQ.'RIGI_AJOU')THEN
      WRITE(IFM,*) '        '
      WRITE(IFM,*) '        =======MATRICE DE RIGIDITE AJOUTEE======='
           ENDIF
      ENDIF
C---------------------------------------------------------------
      IF ((N10.GT.0).OR.(INDICE.EQ.1)) THEN

C CALCUL DES MASSES AJOUTEES - PRODUITS SCALAIRES SUR MODELE
C GENERALISE - CAS DE LA SOUS-STRUCTURATION DYNAMIQUE
C OU BIEN CAS DE MODES RESTITUES SUR MAILLAGE SQUELETTE

         IF (INDICE.EQ.1) THEN
             ITXSTO = TABAD(1)
             ITYSTO = TABAD(2)
             ITZSTO = TABAD(3)
             IPRSTO = TABAD(4)
             IADIRG = TABAD(5)
             NBMO=NBMODE
         ENDIF

        CALL MAMODG(MODEL,STOLCI,NOMRES,ITXSTO,ITYSTO,ITZSTO,
     &              IPRSTO,IADIRG,NBMO,MAX,MAY,MAZ,NBLOC)
       ELSE

C CAS CLASSIQUE

        CALL JEVEUO(STOLCI//'.SCDI','L',JSCDI)
        CALL JEVEUO(STOLCI//'.SCBL','L',JSCBL)
        CALL JEVEUO(STOLCI//'.SCHC','L',JSCHC)
        CALL JEVEUO(STOLCI//'.SCIB','L',JSCIB)
C
C     BOUCLE SUR LES BLOCS DE LA MATRICE ASSEMBLEE GENE
C
      CALL ASSERT(NBLOC.EQ.1)
      DO 40 IBLO=1,NBLOC
         CALL JECROC(JEXNUM(NOMRES//'           .UALF',IBLO))
         CALL JEVEUO(JEXNUM(NOMRES//'           .UALF',IBLO),'E',
     &              LDBLO)
C----------------------------------------------------------------
C
C         BOUCLE SUR LES COLONNES DE LA MATRICE ASSEMBLEE
C
          N1BLOC=ZI(JSCBL+IBLO-1)+1
          N2BLOC=ZI(JSCBL+IBLO)
C
C
        DO 10 I = N1BLOC,N2BLOC
          DO 30 J = (I-ZI(JSCHC+I-1)+1),I

C----------------------------------------------------------------
C ICI ON CALCULE LA MASSE AJOUTEE SUR UN MODELE GENERALISE
C----------------------------------------------------------------

C------------------------------------------------------------------
C ICI ON CALCULE LA MATRICE DE MASSE AJOUTEE SOIT SUR UN MODE_MECA
C SOIT SUR UN CHAM_NO
C------------------------------------------------------------------

           IF (N7.GT.0) THEN
             CHAMNO=ZK8(IVALK+I-1)
           ENDIF

             CALL CAL152(OPTION,MAX,MAY,MAZ,MODEL,PHIB24,IPHI1,IPHI2,
     &              IMADE, MODMEC,CHAMNO,NUM,VRAI,I,J,MIJ,CIJ,KIJ)



C-----------STOCKAGE DANS LA MATR_ASSE_GENE  ------
C
C        CAS DE LA PROJECTION MODALE OU CHAM_NO
C

                 IF (OPTION.EQ.'MASS_AJOU')THEN
                  ZR(LDBLO+ZI(JSCDI+I-1)+J-I-1) = MIJ
                 ENDIF
                 IF (OPTION.EQ.'AMOR_AJOU')THEN
                  ZR(LDBLO+ZI(JSCDI+I-1)+J-I-1) = CIJ
                 ENDIF
                 IF (OPTION.EQ.'RIGI_AJOU')THEN
                  ZR(LDBLO+ZI(JSCDI+I-1)+J-I-1) = KIJ
                 ENDIF
C
C===============================================================
C---------------IMPRESSION DES RESULTATS------------------------
C===============================================================
C
      IF (NIV .GT. 1) THEN
        IF (((N9.GT.0).AND.(N5.NE.0)).OR.(N6.NE.0)) THEN
               IF(OPTION.EQ.'MASS_AJOU') THEN
                  WRITE(IFM,350) I,J,ZR(LDBLO+J+(I-1)*I/2 -1)
               ENDIF
               IF(OPTION.EQ.'AMOR_AJOU') THEN
                  WRITE(IFM,450) I,J,ZR(LDBLO+J+(I-1)*I/2 -1)
               ENDIF
               IF(OPTION.EQ.'RIGI_AJOU') THEN
                  WRITE(IFM,550) I,J,ZR(LDBLO+J+(I-1)*I/2 -1)
               ENDIF
350   FORMAT(18X,'M',2 I 4,1X,'=',1X, D 12.5)
450   FORMAT(18X,'C',2 I 4,1X,'=',1X, D 12.5)
550   FORMAT(18X,'K',2 I 4,1X,'=',1X, D 12.5)
        ENDIF
      ENDIF
30        CONTINUE
10      CONTINUE
40     CONTINUE
      ENDIF
C
      IF (NIV .GT. 1) THEN

       WRITE(IFM,*) '              ============================'
       WRITE(IFM,*) '              =======FIN IMPRESSION======='
       WRITE(IFM,*) '              ============================'

      ENDIF


C     -- ON REPASSE AU STOCKAGE MORSE QUI EST LE STOCKAGE NORMAL :
C     ------------------------------------------------------------
      CALL UALFVA(NOMRES,'G')


      CALL JEDETC('G','&&RIGFLU',1)
      CALL JEDETC('G','&&CALMAA',1)

      CALL JEDEMA()
      END
