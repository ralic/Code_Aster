      SUBROUTINE IMCSMV(FICHIE,NOMSDZ,OPTIOZ,NBNO,LISNOZ,NBCMP,LISCMZ,
     &                  NBCHIF,EPS)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 01/02/2000   AUTEUR VABHHTS J.PELLET 
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
C TOLE CRP_20
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)

C     IMCSMV  -- IMPRESSION DE LA MATR_ASSE DE NOM NOMSD, COMPLEXE,
C                SYMETRIQUE ET STOCKEE MORSE
C                DANS LE FICHIER DE NOM FICHIER.
C                LE GRAIN DE L'IMPRESSION EST 'VALEUR'
C                (I.E. ON IMPRIME UNE VALEUR PAR LIGNE)


C   ARGUMENT        E/S  TYPE         ROLE
C    FICHIE          IN    K*     NOM DU FICHIER OU L'ON IMPRIME
C                                 LA MATR_ASSE,
C    NOMSDZ          IN    K*     NOM DE LA MATR_ASSE
C    OPTIOZ          IN    K*     OPTION D'IMPRESSION
C                                     = 'SOUS_MATRICE'
C                                  OU = 'LIGNE'
C                                  OU = 'COLONNE'
C                                  SI = 'SOUS_MATRICE' ON IMPRIME
C                                       UNIQUEMENT LES COMPOSANTES
C                                       LICITES DES NOEUDS DE LA LISTE
C                                       LISNOZ ET SI CELLE-CI EST VIDE
C                                       (NBNO = 0)
C                                       ON IMPRIME CELLES DE TOUS LES
C                                       NOEUDS (PHYSIQUES) DU MAILLAGE
C                                  SI = 'LIGNE' ON IMPRIME TOUTES LES
C                                       LES LIGNES RELATIVES AUX
C                                       COMPOSANTES LICITES DES NOEUDS
C                                       DE LA LISTE LISNOZ ET DE TOUS
C                                       LES NOEUDS SI CETTE LISTE EST
C                                       VIDE (NBNO =0 )
C                                  SI = 'COLONNE' ON IMPRIME TOUTES LES
C                                       LES COLONNES RELATIVES AUX
C                                       COMPOSANTES LICITES DES NOEUDS
C                                       DE LA LISTE LISNOZ ET DE TOUS
C                                       LES NOEUDS SI CETTE LISTE EST
C                                       VIDE (NBNO =0)
C    NBNO            IN    I     NOMBRE DE NOEUDS DE LA LISTE LISNOZ
C                                SI = 0 LA LISTE LISNOZ EST VIDE ET
C                                L'ON PREND EN COMPTE TOUS LES NOEUDS
C                                (PHYSIQUES) DU MAILLAGE
C    LISNOZ          IN    K*    LISTE DES NOEUDS POUR-LESQUELS ON
C                                DESIRE L'IMPRESSION DES VALEURS
C                                DE LA MATRICE
C    NBCMP           IN    I     NOMBRE DE COMPOSANTES DE LA LISTE
C                                LISCMZ DES COMPOSANTES
C    LISCMZ          IN    K*    LISTE DES COMPOSANTES POUR-LESQUELLES
C                                ON DESIRE L'IMPRESSION DES VALEURS
C                                DE LA MATRICE
C    NBCHIF          IN    I     NOMBRE DE CHIFFRES A METTRE APRES
C                                LA VIRGULE DANS LE FORMAT D'IMPRESSION
C    EPS             IN    R     PRECISION SERVANT A TESTER LA NULLITE
C                                D'UN TERME DE LA MATRICE
C.========================= DEBUT DES DECLARATIONS ====================
C ----- COMMUNS NORMALISES  JEVEUX
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
      CHARACTER*32 JEXNUM,JEXNOM
C -----  ARGUMENTS
      CHARACTER*(*) FICHIE,NOMSDZ,OPTIOZ,LISNOZ,LISCMZ
C -----  VARIABLES LOCALES
      CHARACTER*1 K1BID
      CHARACTER*8 K8BID,NOMA,NOMGD,NOMNO1,NOMCM1,NOMNO2,NOMCM2
      CHARACTER*8 M8BLAN,NOMNOU
      CHARACTER*12 OPTION
      CHARACTER*14 NUMDDL
      CHARACTER*19 NOMSD,PRCHNO
      CHARACTER*24 LISNOT,LISCMT,LISVAT,LISBI1,LISBI2
      CHARACTER*24 LISNOE,LISCMP,LISICM
      CHARACTER*24 NOLILI,VALE,NOEUMA
      INTEGER GD,TYPSYM
      COMPLEX*16 ZEROC
      LOGICAL IMPLIG
C.========================= DEBUT DU CODE EXECUTABLE ==================

      CALL JEMARQ()

C --- INITIALISATIONS :
C     ---------------
      NOMSD = NOMSDZ
      LISNOE = LISNOZ
      LISCMP = LISCMZ

      ZEROC = (0.0D0,0.0D0)
      M8BLAN = '        '

C --- RECUPERATION DES NOEUDS POUR-LESQUELS ON VEUT L'IMPRESSION
C --- DE LA MATRICE :
C     -------------
      IF (NBNO.EQ.0) THEN

C ---    ON FABRIQUE UN VECTEUR BIDON DE K8 D'1 SEUL TERME SI LA
C ---    EST VIDE :
C        --------
        LISBI1 = '&&IMCSMV.BID1'
        CALL WKVECT(LISBI1,'V V K8',1,IDLINO)
      ELSE
        CALL JEVEUO(LISNOE,'L',IDLINO)
      END IF

C --- RECUPERATION DES COMPOSANTES POUR-LESQUELLES ON VEUT L'IMPRESSION
C --- DE LA MATRICE :
C     -------------
      IF (NBCMP.EQ.0) THEN

C ---    ON FABRIQUE UN VECTEUR BIDON DE K8 D'1 SEUL TERME SI LA
C ---    EST VIDE :
C        --------
        LISBI2 = '&&IMCSMV.BID2'
        CALL WKVECT(LISBI2,'V V K8',1,IDLICM)
      ELSE
        CALL JEVEUO(LISCMP,'L',IDLICM)
      END IF

C --- CONSTRUCTION DU DESCRIPTEUR DE LA MATRICE :
C     -----------------------------------------
      CALL MTDSCR(NOMSD)

C --- RECUPERATION DU DESCRIPTEUR DE LA MATRICE :
C     -----------------------------------------
      CALL JEVEUO(NOMSD//'.&INT','L',LMAT)

C --- RECUPERATION DU .REFA DE LA MATRICE :
C     -----------------------------------
      CALL JEVEUO(NOMSD//'.REFA','L',IDREFE)

C --- NOMBRE D'EQUATIONS :
C     ------------------
      NEQ = ZI(LMAT+2)

C --- INDICATEUR DE LA SYMETRIE DE LA MATRICE :
C     ---------------------------------------

C --- ADRESSE DU TABLEAU DES ADRESSES DES TERMES DIAGONAUX DANS
C --- LES BLOCS :
C     ---------
      CALL MTDSC2(ZK24(ZI(LMAT+1)),'ADIA','L',IDADIA)

C --- ADRESSE DU TABLEAU DES ADRESSES DU DEBUT DES BLOCS :
C     --------------------------------------------------
      CALL MTDSC2(ZK24(ZI(LMAT+1)),'ABLO','L',IDBLOC)

C --- NOMBRE DE BLOCS DE LA DEMI-MATRICE :
C     ----------------------------------
      NBBLOC = ZI(LMAT+13)

C --- LONGUEUR D'UN BLOC :
C     ------------------

C --- RECUPERATION DU NUME_DDL ASSOCIE A LA MATRICE :
C     ---------------------------------------------
      CALL DISMOI('F','NOM_NUME_DDL',NOMSD,'MATR_ASSE',IBID,NUMDDL,IER)

C --- RECUPERATION DU MAILLAGE ASSOCIE A LA MATRICE :
C     ---------------------------------------------
      CALL DISMOI('F','NOM_MAILLA',NOMSD,'MATR_ASSE',IBID,NOMA,IER)

C --- RECUPERATION DU TABLEAU DES NUMEROS DE COLONNES DES TERMES
C --- DE LA MATRICE :
C     -------------
      CALL JEVEUO(NUMDDL//'.SMOS.HCOL','L',IDHCOL)

C --- RECUPERATION DU PROF_CHNO :
C     -------------------------
      PRCHNO = NUMDDL//'.NUME'

C --- .VALE DE LA MATRICE :
C     -------------------
      VALE = NOMSD//'.VALE'

C --- RECUPERATION DU .DEEQ DU PROF_CHNO :
C     ----------------------------------
      CALL JEVEUO(PRCHNO//'.DEEQ','L',IDEEQ)

C --- RECUPERATION DU .DELG DU PROF_CHNO :
C     ----------------------------------
      CALL JEVEUO(PRCHNO//'.DELG','L',IDELG)

C --- TABLEAU DES NUMEROS D'EQUATIONS :
C     -------------------------------
      CALL JEVEUO(PRCHNO//'.NUEQ','L',IANUEQ)

C --- RECUPERATION DU .PRNO ASSOCIE AU MAILLAGE :
C     -----------------------------------------
      CALL JELIRA(PRCHNO//'.PRNO','NMAXOC',NLILI,K8BID)
      K = 0
      DO 10 I = 1,NLILI
        CALL JENUNO(JEXNUM(PRCHNO//'.LILI',I),NOLILI)
        IF (NOLILI(1:8).NE.'&MAILLA ') GO TO 10
        K = I
   10 CONTINUE
      IF (K.EQ.0) THEN
        CALL UTMESS('F','IMCSMV',
     &              'ERREUR DANS LA RECUPERATION DU NUME.PRNO')
      END IF
      CALL JEVEUO(JEXNUM(PRCHNO//'.PRNO',K),'L',IDPRNO)

C --- RECUPERATION DE LA GRANDEUR ASSOCIEE A LA MATRICE  :
C     -------------------------------------------------
      CALL DISMOI('F','NOM_GD',NUMDDL,'NUME_DDL',IBID,NOMGD,IER)

C --- NUMERO DE LA GRANDEUR  :
C     ---------------------
      CALL JENONU(JEXNOM('&CATA.GD.NOMCMP',NOMGD),GD)

C --- NOMBRE D'ENTIERS CODES ASSOCIE A LA GRANDEUR  :
C     --------------------------------------------

C --- NOMBRE DE COMPOSANTES ASSOCIEES A LA GRANDEUR :
C     ---------------------------------------------
      CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP',NOMGD),'LONMAX',NCMPMX,K1BID)
      CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',NOMGD),'L',INOCMP)

C --- COLLECTION DES NOMS DES NOEUDS DU MAILLAGE  :
C     ------------------------------------------
      NOEUMA = NOMA//'.NOMNOE'

C --- CREATION DU TABLEAU DE TRAVAIL DES NOMS DES NOEUDS DE
C --- LA LIGNE COURANTE DE LA MATRICE :
C     -------------------------------
      LISNOT = '&&IMCSMV.NOMNOE'
      CALL WKVECT(LISNOT,'V V K8',NEQ,IDNOMN)

C --- CREATION DU TABLEAU DE TRAVAIL DES NOMS DES COMPOSANTES DE
C --- LA LIGNE COURANTE DE LA MATRICE :
C     -------------------------------
      LISCMT = '&&IMCSMV.NOMCMP'
      CALL WKVECT(LISCMT,'V V K8',NEQ,IDNOMC)

C --- CREATION DU TABLEAU DE TRAVAIL DES VALEURS DE
C --- LA LIGNE COURANTE DE LA MATRICE :
C     -------------------------------
      LISVAT = '&&IMCSMV.LIGVAL'
      CALL WKVECT(LISVAT,'V V C',NEQ,IDVALL)

C --- CREATION DU TABLEAU D'INDICATEURS DISANT SI UNE COMPOSANTE EST
C ---  A PRENDRE EN COMPTE OU NON :
C     ---------------------------
      LISICM = '&&IMCSMV.INDCMP'
      CALL WKVECT(LISICM,'V V I',NEQ,IDINCM)

C --- INITIALISATION DE L'INDICE DE DEBUT DE LIGNE IDEBLI
C --- ET DE L'INDICE DU TERME COURANT KIN DE LA MATRICE :
C     -------------------------------------------------
      IDEBLI = 1
      KIN = 0

C --- BOUCLE SUR LES BLOCS DE LA MATRICE :
C     ----------------------------------
      DO 100 IBLOC = 1,NBBLOC

C ---    RECUPERATION DU BLOC COURANT :
C        ----------------------------
        CALL JEVEUO(JEXNUM(VALE,IBLOC),'L',IABLOC)

C ---    IL1 : NUMERO DE LA PREMIERE LIGNE (OU COLONNE) DU BLOC
C ---    IL2 : NUMERO DE LA DERNIERE LIGNE (OU COLONNE) DU BLOC :
C        ------------------------------------------------------
        IL1 = ZI(IDBLOC+IBLOC-1) + 1
        IL2 = ZI(IDBLOC+IBLOC)

C ---    BOUCLE SUR LES LIGNES DU BLOC COURANT :
C        -------------------------------------
        DO 90 IEQUA = IL1,IL2

          NOMNO1 = M8BLAN
          NOMCM1 = M8BLAN

C ---       RECUPERATION DU NOEUD CORRESPONDANT AU DDL IEQUA :
C           ------------------------------------------------
          INO = ZI(IDEEQ+2* (IEQUA-1)+1-1)

C ---       NUMCM1 NUMERO DE COMPOSANTE CORRESPONDANTE AU DDL IEQUA :
C           -------------------------------------------------------
          NUMCM1 = ZI(IDEEQ+2* (IEQUA-1)+2-1)

C ---       RECUPERATION DU NOM DU NOEUD INO :
C           --------------------------------
          IF (INO.GT.0) THEN
            CALL JENUNO(JEXNUM(NOEUMA,INO),NOMNO1)

C ---       RECUPERATION DU NOM DE LA COMPOSANTE NUMCM1 DANS LE CAS
C ---       OU IL S'AGIT D'UN DDL PHYSIQUE (I.E. NUMCM1 > 0) :
C           ------------------------------------------------
            IF (NUMCM1.GT.0) THEN
              NOMCM1 = ZK8(INOCMP+NUMCM1-1)

C ---       ON NE FAIT L'IMPRESSION DE LA LIGNE DE LA MATRICE
C ---       CORRESPONDANTE AU DDL NOMCM1 DU NOEUD NOMNO1 QUE
C ---       SI CE NOEUD ET CETTE COMPOSANTE SONT LICITES :
C           --------------------------------------------
              IF (IMPLIG(NOMNO1,NBNO,ZK8(IDLINO)) .AND.
     &            IMPLIG(NOMCM1,NBCMP,ZK8(IDLICM))) THEN

C ---       INDICE DU TERME DIAGONAL :
C           ------------------------
                IFINLI = ZI(IDADIA+IEQUA-1)

C ---       LONGUEUR DE LA LIGNE COURANTE :
C           -----------------------------
                ILONG = IFINLI - IDEBLI + 1

                IF (ILONG.LE.0) GO TO 80

C ---       POSITION DU TERME DIAGONAL DE LA LIGNE IEQUA DANS LE BLOC :
C           --------------------------------------------------------

C ---       INITIALISATION DES TABLEAUX DE TRAVAIL  :
C           --------------------------------------
                DO 20 IEQ = 1,NEQ
                  ZK8(IDNOMN+IEQ-1) = M8BLAN
                  ZK8(IDNOMC+IEQ-1) = M8BLAN
                  ZC(IDVALL+IEQ-1) = ZEROC
   20           CONTINUE


                K = 0

C ---       BOUCLE SUR LES TERMES DE LA LIGNE IEQUA
C ---       (JUSQU'AU TERME DIAGONAL) :
C           -------------------------
                DO 30 ITERM = IDEBLI,IFINLI

                  KIN = KIN + 1

C ---          POSITION DU TERME COURANT DANS LE BLOC :
C              --------------------------------------
                  INDB = IABLOC + KIN - 1

C ---          INDICE DE COLONNE DU TERME COURANT  :
C              ----------------------------------
                  INDCOL = ZI(IDHCOL+ITERM-1)

C ---          STOCKAGE DU TERME POUR L'IMPRESSION S'IL EST NON-NUL :
C              ----------------------------------------------------
                  IF (ABS(ZC(INDB)).GT.EPS) THEN

C ---            RECUPERATION DU NOEUD CORRESPONDANT AU DDL COURANT :
C                --------------------------------------------------
                    JNO = ZI(IDEEQ+2* (INDCOL-1)+1-1)

                    IF (JNO.GT.0) THEN

C ---            NUMCM2 NUMERO DE COMPOSANTE CORRESPONDANT
C ---            AU DDL ITERM :
C                ------------
                      NUMCM2 = ZI(IDEEQ+2* (INDCOL-1)+2-1)

C ---            RECUPERATION DU NOM DU NOEUD JNO :
C                --------------------------------
                      CALL JENUNO(JEXNUM(NOEUMA,JNO),NOMNO2)

C ---            RECUPERATION DU NOM DE LA COMPOSANTE NUMCM2 DANS LE CAS
C ---            OU IL S'AGIT D'UN DDL PHYSIQUE (I.E. NUMCM2 > 0) :
C                ------------------------------------------------
                      IF (NUMCM2.GT.0) THEN
                        NOMCM2 = ZK8(INOCMP+NUMCM2-1)

C ---            RECUPERATION DU NOMBRE DE COMPOSANTES DU NOEUD JNO :
C                --------------------------------------------------

C ---            AFFECTATION DES TABLEAUX DE TRAVAIL :
C                -----------------------------------
                        K = K + 1

                        ZK8(IDNOMN+K-1) = NOMNO2
                        ZK8(IDNOMC+K-1) = NOMCM2
                        ZC(IDVALL+K-1) = ZC(INDB)

                      END IF
                    END IF
                  END IF
   30           CONTINUE
C           -----------------------------------------
C ---       - RECONSTITUTION DE LA LIGNE IEQUA      -
C ---       -  (POUR LES TERMES APRES LA DIAGONALE) -
C           -----------------------------------------

C ---       INITIALISATION DE L'INDICE DE DEBUT DE LA LIGNE JEQUA :
C           -----------------------------------------------------
                JDEBLI = ZI(IDADIA+IEQUA-1) + 1

C ---       BOUCLE SUR LES LIGNES SUIVANT LA LIGNE IEQUA DANS LE
C ---       BLOC COURANT  :
C           ------------
                DO 70 JEQUA = IEQUA + 1,IL2

C ---          INDICE DE COLONNE DU PREMIER TERME DE LA LIGNE JEQUA :
C              ----------------------------------------------------
                  JCOL1 = ZI(IDHCOL+JDEBLI-1)

C ---          SI CET INDICE EST PLUS GRAND QUE IEQUA
C ---          PAS DE TRAITEMENT A FAIRE :
C              -------------------------
                  IF (JCOL1.GT.IEQUA) GO TO 60

C ---          INDICE DU TERME DIAGONAL DE LA LIGNE JEQUA :
C              ------------------------------------------
                  JFINLI = ZI(IDADIA+JEQUA-1)

C ---          LONGUEUR DE LA LIGNE COURANTE :
C              -----------------------------
                  JLONG = JFINLI - JDEBLI + 1

                  IF (JLONG.LE.0) GO TO 60
                  JTERM1 = 0

C ---          BOUCLE SUR LES TERMES DE LA LIGNE JEQUA POUR
C ---          RECUPERER LE TERME SUR LA COLONNE IEQUA
C ---          S'IL Y EN A UN ET AUQUEL CAS JTERM1 N'EST PAS NUL
C ---          ET EST L'INDICE DE CE TERME DANS LA MATRICE :
C              -------------------------------------------
                  DO 40 JTERM = JDEBLI,JFINLI
                    JNDCOL = ZI(IDHCOL+JTERM-1)
                    IF (JNDCOL.EQ.IEQUA) THEN
                      JTERM1 = JTERM
                      GO TO 50
                    END IF
   40             CONTINUE
   50             CONTINUE
                  IF (JTERM1.EQ.0) GO TO 60

C ---          POSITION DE CE TERME DANS LE BLOC :
C              ---------------------------------
                  JNDB = IABLOC + JTERM1 - 1

C ---          STOCKAGE DU TERME POUR L'IMPRESSION S'IL EST NON-NUL :
C              ----------------------------------------------------
                  IF (ABS(ZC(JNDB)).GT.EPS) THEN

C ---            RECUPERATION DU NOEUD CORRESPONDANT AU DDL JEQUA :
C                --------------------------------------------------
                    JNO = ZI(IDEEQ+2* (JEQUA-1)+1-1)

C ---            NUMCM2 NUMERO DE COMPOSANTE CORRESPONDANT
C ---            AU DDL JEQUA :
C                ------------
                    NUMCM2 = ZI(IDEEQ+2* (JEQUA-1)+2-1)

C ---            RECUPERATION DU NOM DU NOEUD JNO :
C                --------------------------------
                    IF (JNO.GT.0) THEN
                      CALL JENUNO(JEXNUM(NOEUMA,JNO),NOMNO2)

C ---            RECUPERATION DU NOM DE LA COMPOSANTE NUMCM2 DANS LE CAS
C ---            OU IL S'AGIT D'UN DDL PHYSIQUE (I.E. NUMCM2 > 0) :
C                ------------------------------------------------
                      IF (NUMCM2.GT.0) THEN
                        NOMCM2 = ZK8(INOCMP+NUMCM2-1)
                      END IF

C ---            RECUPERATION DU NOMBRE DE COMPOSANTES DU NOEUD JNO :
C                --------------------------------------------------

C ---            AFFECTATION DES TABLEAUX DE TRAVAIL :
C                -----------------------------------
                      IF (NUMCM2.GT.0) THEN

                        K = K + 1

                        ZK8(IDNOMN+K-1) = NOMNO2
                        ZK8(IDNOMC+K-1) = NOMCM2
                        ZC(IDVALL+K-1) = ZC(JNDB)

                      END IF
                    END IF
                  END IF
   60             CONTINUE
                  JDEBLI = ZI(IDADIA+JEQUA-1) + 1
   70           CONTINUE

C ---       RECONSTRUCTION DES TABLEAUX DE TRAVAIL  PAR FILTRAGE
C ---       LONLI1 : LONGUEUR UTILE DES TABLEAUX DES NOMS DES NOEUDS,
C ---                DES COMPOSANTES ET DE LEURS VALEURS AVANT
C ---                UN FILTRAGE EVENTUEL SELON DES NOMS DE NOEUDS ET
C ---                DE COMPOSANTES SPECIFIES PAR L'UTILISATEUR :
C                    ------------------------------------------
                LONLI1 = K

C ---       RECONSTRUCTION DES TABLEAUX DE TRAVAIL  PAR FILTRAGE
C ---       EN FONCTION DES NOEUDS ET DES COMPOSANTES SPECIFIES
C ---       PAR L'UTILISATEUR :
C           -----------------
                CALL TRNOC1(NBNO,ZK8(IDLINO),NBCMP,ZK8(IDLICM),OPTIOZ,
     &                      LONLI1,ZK8(IDNOMN),ZK8(IDNOMC),ZC(IDVALL),
     &                      ZI(IDINCM),LONLI2)

C ---       IMPRESSION DE LA LIGNE COURANTE SELON LE GRAIN 'VALEUR' :
C           ------------------------------------------------------
                CALL IMPFC1(NOMNO1,NOMCM1,LONLI2,ZK8(IDNOMN),
     &                      ZK8(IDNOMC),ZC(IDVALL),NBCHIF,FICHIE)

              END IF
            END IF
          END IF

C ---    REACTUALISATION DE L'INDICE DE DEBUT DE LIGNE
C ---    ET DE L'INDICE DU TERME COURANT :
C        -------------------------------
   80     CONTINUE
          KIN = ZI(IDADIA+IEQUA-1)
          IDEBLI = ZI(IDADIA+IEQUA-1) + 1
   90   CONTINUE
        CALL JELIBE(JEXNUM(VALE,IBLOC))
  100 CONTINUE

C --- DESTRUCTION DU DESCRIPTEUR DE LA MATRICE :
C     ----------------------------------------
      CALL JEDETR(NOMSD//'.&INT')

      CALL JEDETC('V','&&IMCSMV',1)

      CALL JEDEMA()

      END
