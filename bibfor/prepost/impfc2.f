      SUBROUTINE IMPFC2 (NOMNOZ, NOMCMZ, NBNOMA, LONLIS, NOMNOE, NOMCMP,
     +                   VALE, INDCMP, INDNOE, NOMCM2, VALE2,
     +                   NBCM2, NCMPMX, NBCMP1, NOMCM1,
     +                   NBCMP2, LISCMP, NOEMAZ, NBCHIF, FICHIE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 17/06/98   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C TOLE CRS_602
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      IMPFC2 -- IMPRESSION D'UNE LIGNE OU D'UNE COLONNE D'UNE
C                MATR_ASSE SELON LE GRAIN 'NOEUD'
C                (I.E. LE GRAIN D'IMPRESSION EST CONSTITUE PAR
C                 UNE SOUS_MATRICE DONT LES TERMES COUPLENT 2 NOEUDS)
C                LA LIGNE OU LA COLONNE NE CONTIENT QUE LES VALEURS
C                DES COMPOSANTES LICITES DES NOEUDS LICITES
C                (LICITE VEUT DIRE 'CHOISI PAR L'UTILISATEUR')
C                LES VALEURS SONT COMPLEXES.
C
C   ARGUMENT        E/S  TYPE         ROLE
C    NOMNOZ          IN   K*     NOM DU NOEUD POUR LEQUEL ON VA
C                                IMPRIMER LA LIGNE DE LA MATRICE
C    NOMCMZ          IN   K*     NOM DE LA COMPOSANTE POUR LAQUELLLE
C                                ON VA IMPRIMER LA LIGNE DE LA MATRICE
C    NBNOMA          IN   I      NOMBRE DE NOEUDS DU MAILLAGE
C    LONLIS          IN   I      LONGUEUR 'UTILE' DES TABLEAUX NOMNOE,
C                                NOMCMP ET  VALE
C    NOMNOE(*)       IN   K8     LISTE DES NOEUDS SUR-LESQUELS PORTENT
C                                LES TERMES DU TABLEAU VALE
C    NOMCMP(*)       IN   K8     LISTE DES COMPOSANTES SUR-LESQUELLES
C                                PORTENT LES TERMES DU TABLEAU VALE
C    VALE(*)         IN   C      TABLEAU DES VALEURS DE LA LIGNE OU
C                                DE LA COLONNE A IMPRIMER
C    INDCMP(*)       IN   I      INDCMP(INDICE_COMPOSANTE)=INDICE_LIGNE
C    INDNOE(*)       IN   I      INDICATEUR DISANT SI UN NOEUD A ETE
C                                IMPRIME OU NON
C    NOMCM2(*)       IN   K8     NOM DES COMPOSANTES A IMPRIMER SUR UNE
C                                LIGNE
C    VALE2(*)        IN   C      VALEUR DES COMPOSANTES A IMPRIMER SUR
C                                UNE LIGNE
C    NBCM2(*)        IN   I      TABLEAU DU NOMBRE DE COMPOSANTES A
C                                IMPRIMER PAR LIGNE POUR UN  NOEUD DONNE
C    NCMPMX          IN   I      NOMBRE DE COMPOSANTES MAX DU NOEUD 1
C    NBCMP1          IN   I      NOMBRE DE COMPOSANTES EFFECTIVES DU
C                                NOEUD 1
C    NOMCM1(*)       IN   I      TABLEAU DES NOMS DE COMPOSANTES
C                                DU NOEUD 1
C    NOEMAZ          IN    K*    COLLECTION DES NOM DES NOEUDS DU
C                                MAILLAGE
C    NBCHIF          IN    I     NOMBRE DE CHIFFRES SIGNIFICATIFS
C    FICHIE          IN    K*     NOM DU FICHIER D'IMPRESSION
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
      CHARACTER*(*) NOMNOZ, NOMCMZ, FICHIE, NOEMAZ
      CHARACTER*8   NOMNOE(*), NOMCMP(*), NOMCM2(*), NOMCM1(*)
      CHARACTER*8   LISCMP(*)
      COMPLEX*16    VALE(*), VALE2(*)
      INTEGER       INDCMP(*), INDNOE(*), NBCM2(*)
C -----  VARIABLES LOCALES
      CHARACTER*1   SLACH
      CHARACTER*1   EXCLAM
      CHARACTER*1   CROIX
      CHARACTER*1   KBLAN2, KLONG1
      CHARACTER*2   KNBCH, KLVALR, KBLAN1, KNCMP
      CHARACTER*3   KTIRET
      CHARACTER*8   NOMNO1, NOCM1, M8BLAN
      CHARACTER*24  NOEUMA
      CHARACTER*32  JEXNOM
      CHARACTER*35  TIRE1
      CHARACTER*80  FORVAR
      CHARACTER*120 FORM1, FORM2, FORM3, FORM4
      CHARACTER*2000 TIRET
C
      INTEGER       LXLGUT
      LOGICAL       IMPLIG
C
      DATA TIRE1 /'___________________________________'/
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     ---------------
      EXCLAM = '!'
      CROIX  = 'X'
      M8BLAN = '        '
      NOMNO1 = NOMNOZ
      NOEUMA = NOEMAZ
      TIRET = TIRE1//TIRE1//TIRE1//TIRE1//TIRE1//TIRE1//TIRE1//TIRE1
     +      //TIRE1//TIRE1//TIRE1//TIRE1//TIRE1//TIRE1//TIRE1//TIRE1
     +      //TIRE1//TIRE1//TIRE1//TIRE1//TIRE1//TIRE1//TIRE1//TIRE1
     +      //TIRE1//TIRE1//TIRE1//TIRE1//TIRE1//TIRE1//TIRE1//TIRE1
     +      //TIRE1//TIRE1//TIRE1//TIRE1//TIRE1//TIRE1//TIRE1//TIRE1
     +      //TIRE1//TIRE1//TIRE1//TIRE1//TIRE1//TIRE1//TIRE1//TIRE1
C
C --- UNITE LOGIQUE DU FICHIER D'IMPRESSION :
C     -------------------------------------
      IFM = IUNIFI(FICHIE)
C
C --- NOMBRE DE CHIFFRES A METTRE APRES LA VIRGULE
C --- ON RAPPELLE QUE LA FONCTION NDRSEM() RENVOIE LE NOMBRE
C --- DE CHIFFRES EN DECIMAL D'UN REEL :
C     --------------------------------
C      NBCH = MIN (NDRSEM( ), NBCHIF)
      NBCH = MIN (14, NBCHIF)
      NBCH1 = NBCH - 1
C
C --- LONGUEUR PRISE POUR REPRESENTER UNE VALEUR REELLE :
C     -------------------------------------------------
      LVALRE = NBCH + 6
C
C --- FORMAT D'ECRITURE DES VALEURS REELLES :
C     -------------------------------------
      CALL CODENT(NBCH1,'G',KNBCH)
      CALL CODENT(LVALRE,'D',KLVALR)
C
      FORVAR = '"("'//'1PD'//KLVALR//'.'//KNBCH//'","'//
     +                '1PD'//KLVALR//'.'//KNBCH//'")"'
C
      DO 10 I = 1, NBNOMA
            INDNOE(I) = 0
 10   CONTINUE
C
      NBBLAN = 2*NBCH + 8
      CALL CODENT(NBBLAN,'D',KBLAN1)
C
C --- LONGUEUR UTILE DE LA CHAINE DE CARACTERES NOMNO1 :
C     ------------------------------------------------
      LGNOM1 = LXLGUT(NOMNO1)
      CALL CODENT(LGNOM1,'G',KLONG1)
C
C --- DECALAGE VERS LA DROITE DE LA CHAINE DE CARACTERES NOMNO1 :
C     --------------------------------------------------------
      IDECAL =  8 - LGNOM1
      CALL CODENT(IDECAL,'G',KBLAN2)
C
C --- BOUCLE SUR LES TERMES DE LA LISTE :
C     ---------------------------------
      DO 20 I = 1, LONLIS
C
C ---      RECUPERATION DU NUMERO DU NOEUD COURANT :
C          ---------------------------------------
             CALL JENONU(JEXNOM(NOEUMA,NOMNOE(I)),INO)
C
C ---      SI LE NOEUD A DEJA ETE TRAITE, ON NE FAIT RIEN :
C          ----------------------------------------------
             IF (INDNOE(INO).EQ.0) THEN
C
               INDNOE(INO) = 1
C
C ---      INITIALISATIONS :
C          ---------------
               DO 30 J = 1, NCMPMX
                 NBCM2(J) = 0
 30            CONTINUE
C
               NCMPX2 = NCMPMX*NCMPMX
               DO 40 J = 1, NCMPX2
                 VALE2(J) = 0
                 NOMCM2(J) = M8BLAN
 40            CONTINUE
C
               K = 0
C
C ---      RECUPERATION DES COMPOSANTES DU NOEUD COURANT :
C          ---------------------------------------------
               DO 50 J = I, LONLIS
                  IF (NOMNOE(I).EQ.NOMNOE(J)) THEN
                    ICMP = INDCMP(J)
                    NBCM2(ICMP) = NBCM2(ICMP) + 1
                    K = K + 1
                    VALE2(K) = VALE (J)
                    NOMCM2(K) = NOMCMP(J)
                  ENDIF
  50           CONTINUE
C
               NBCMP = 0
               DO 60 ICMP1 = 1, NBCMP1
                  IF (NBCM2(ICMP1).GT.NBCMP) THEN
                     NBCMP = NBCM2(ICMP1)
                  ENDIF
  60           CONTINUE
C
C ---         NOMBRE DE 'TIRETS' POUR FAIRE LE CADRE :
C             --------------------------------------
               NBTIRE = 19 + NBCMP*(2*NBCH+18)
               CALL CODENT(NBTIRE,'D',KTIRET)
C
C ---         ECRITURE DU CADRE :
C             -----------------
               FORM1 = '('//'2X,A'//KTIRET//')'
               FORM3 = '(2X,A'//KTIRET//')'
               WRITE(IFM,FORM1) TIRET(1:NBTIRE)
C
C ---         CODAGE DE NBCMP SOUS FORME D'UNE CHAINE DE CARACTERES :
C             -----------------------------------------------------
               CALL CODENT(NBCMP,'D',KNCMP)
C
               FORM2 = '(2X,A1,A'//KLONG1//',A1,A8,'//KBLAN2//'X,A1,'
     +                    //KNCMP//'(X,A8,'//KBLAN1//'X,A1))'
               FORM4 = '(2X,A1,A8,9X,A1,'//KNCMP//
     +                 '(X,'//FORVAR//',X,A1))'
C
C ---         ECRITURE DE L'ENTETE DU CADRE :
C             -----------------------------
               WRITE(IFM,FORM2) EXCLAM,NOMNO1,CROIX,NOMNOE(I),
     +             EXCLAM,(NOMCM2(JCMP),EXCLAM,JCMP=1,NBCMP)
C
C ---         BOUCLE SUR LE NOMBRE DE COMPOSANTES DU NOEUD 1 :
C             ----------------------------------------------
               N = 0
               DO 70 ICMP1 = 1, NBCMP1
                 IF (NBCM2(ICMP1).NE.0) THEN
               IF (IMPLIG(NOMCM1(ICMP1),NBCMP2,LISCMP)) THEN
                   WRITE(IFM,FORM3) TIRET(1:NBTIRE)
                   WRITE(IFM,FORM4) EXCLAM, NOMCM1(ICMP1),EXCLAM,
     +                        (DBLE(VALE2(N+IJ)),DIMAG(VALE2(N+IJ)),
     +                        EXCLAM,IJ=1,NBCM2(ICMP1))
               ENDIF
                   N = N + NBCM2(ICMP1)
                 ENDIF
  70             CONTINUE
                 WRITE(IFM,FORM3) TIRET(1:NBTIRE)
             ENDIF
 20   CONTINUE
      END
