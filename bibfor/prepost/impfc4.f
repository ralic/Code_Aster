      SUBROUTINE IMPFC4 (NOMAIL,NOMNOZ, NOMCMZ, LONLIS, NOMNOE,
     +                   NOMCMP, VALE, INDCMP, INDNOE, NUMNOE,
     +                   NOMCM2, VALE2,NBCM2, NCMPMX, NBCMP1, NOMCM1,
     +                   NBCMP2, LISCMP, NBCHIF, FICHIE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 10/03/98   AUTEUR VABHHTS J.PELLET 
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
C      IMPFC4 -- IMPRESSION D'UNE LIGNE D'UNE MATRICE ELEMENTAIRE
C                D 'UN MATR_ELEM SELON LE GRAIN 'NOEUD'
C                (I.E. ON IMPRIME LA SOUS-MATRICE DONT LES TERMES
C                      COUPLENT 2 NOEUDS)
C                LA LIGNE NE CONTIENT QUE LES VALEURS
C                DES COMPOSANTES LICITES
C                (LICITE VEUT DIRE 'CHOISI PAR L'UTILISATEUR')
C                LES VALEURS SONT COMPLEXES.
C
C   ARGUMENT        E/S  TYPE         ROLE
C    NOMAIL          IN   K8     NOM DE LA MAILLE A IMPRIMER
C    NOMNOZ          IN   K*     NOM DU NOEUD POUR LEQUEL ON VA
C                                IMPRIMER LA LIGNE DE LA MATRICE
C    NOMCMZ          IN   K*     NOM DE LA COMPOSANTE POUR LAQUELLLE
C                                ON VA IMPRIMER LA LIGNE DE LA MATRICE
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
C    NUMNOE(*)       IN   I      TABLEAU INDIQUANT A QUEL NOEUD
C                                APPARTIENT UNE COMPOSANTE A IMPRIMER
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
C    NBCMP2          IN   I      NOMBRE DE COMPOSANTES DE LA
C                                LISTE LISCMP
C    LISCMP          IN   K*     LISTE DES NOMS DES COMPOSANTES
C                                A IMPRIMER
C    NBCHIF          IN   I      NOMBRE DE CHIFFRES SIGNIFICATIFS
C    FICHIE          IN   K*     NOM DU FICHIER D'IMPRESSION
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
      CHARACTER*(*) NOMNOZ, NOMCMZ, FICHIE
      CHARACTER*8   NOMNOE(*), NOMCMP(*), NOMCM2(*), NOMCM1(*), NOMAIL
      CHARACTER*8   LISCMP(*)
      COMPLEX*16    VALE(*), VALE2(*)
      INTEGER       INDCMP(*), INDNOE(*), NBCM2(*), NUMNOE(*)
C -----  VARIABLES LOCALES
      CHARACTER*1   SLACH
      CHARACTER*1   EXCLAM
      CHARACTER*1   CROIX
      CHARACTER*1   KLONG1, KLONGM
      CHARACTER*2   KNBCH, KLVALR, KNCMP, KBLAN2, KBLAN1
      CHARACTER*3   KTIRET
      CHARACTER*8   NOMNO1, NOCM1, M8BLAN
      CHARACTER*35  TIRE1
      CHARACTER*80  FORVAR
      CHARACTER*120 FORM1, FORM2, FORM3, FORM4
      CHARACTER*2000 TIRET
C
      COMPLEX*16    ZEROC
C
      INTEGER       LXLGUT
      LOGICAL       IMPLIG
C
      DATA TIRE1 /'___________________________________'/
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     ---------------
      ZEROC = (0.0D0,0.0D0)
      SLACH  = '/'
      EXCLAM = '!'
      CROIX  = 'X'
      M8BLAN = '        '
      NOMNO1 = NOMNOZ
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
C --- NOMBRE DE CHIFFRES SIGNIFICATIFS
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
      NBBLAN = 2*LVALRE + 4 - 8
      CALL CODENT(NBBLAN,'D',KBLAN1)
C
C --- LONGUEUR UTILE DES CHAINES DE CARACTERES NOMNO1 ET NOMAIL :
C     ---------------------------------------------------------
      LGNOM1 = LXLGUT(NOMNO1)
      LGNOMA = LXLGUT(NOMAIL)
      LGNOMT = LGNOM1+LGNOMA+1
      CALL CODENT(LGNOM1,'G',KLONG1)
      CALL CODENT(LGNOMA,'G',KLONGM)
C
C --- DECALAGE VERS LA DROITE DE LA CHAINE DE CARACTERES
C --- NOMAIL/NOMNO1  :
C     -------------
      IDECAL =  17 - LGNOMT
      CALL CODENT(IDECAL,'G',KBLAN2)
C
C --- BOUCLE SUR LES TERMES DE LA LISTE :
C     ---------------------------------
      DO 10 I = 1, LONLIS
C
C ---    RECUPERATION DU NUMERO DU NOEUD COURANT :
C        ---------------------------------------
           INO = NUMNOE(I)
C
C ---    SI LE NOEUD A DEJA ETE TRAITE, ON NE FAIT RIEN :
C        ----------------------------------------------
           IF (INDNOE(INO).EQ.0) THEN
C
               INDNOE(INO) = 1
C
C ---      INITIALISATIONS :
C          ---------------
               DO 20 J = 1, NCMPMX
                 NBCM2(J) = 0
 20            CONTINUE
C
               NCMPX2 = NCMPMX*NCMPMX
C
               DO 30 J = 1, NCMPX2
                 VALE2(J) = ZEROC
                 NOMCM2(J) = M8BLAN
 30            CONTINUE
C
               K = 0
C
C ---      RECUPERATION DES COMPOSANTES DU NOEUD COURANT :
C          ---------------------------------------------
               DO 40 J = I, LONLIS
                  IF (NOMNOE(I).EQ.NOMNOE(J)) THEN
                    ICMP = INDCMP(J)
                    NBCM2(ICMP) = NBCM2(ICMP) + 1
                    K = K + 1
                    VALE2(K) = VALE (J)
                    NOMCM2(K) = NOMCMP(J)
                  ENDIF
  40           CONTINUE
C
               NBCMP = 0
               DO 50 ICMP1 = 1, NBCMP1
                  IF (NBCM2(ICMP1).GT.NBCMP) THEN
                     NBCMP = NBCM2(ICMP1)
                  ENDIF
  50           CONTINUE
C
C ---      NOMBRE DE 'TIRETS' POUR FAIRE LE CADRE :
C          --------------------------------------
               NBTIRE = 28 + NBCMP*(2*NBCH+18)
               CALL CODENT(NBTIRE,'G',KTIRET)
C
               FORM1 = '('//'2X,A'//KTIRET//')'
               WRITE(IFM,FORM1) TIRET(1:NBTIRE)
C
C ---      CODAGE DE NBCMP SOUS FORME D'UNE CHAINE DE CARACTERES :
C          -----------------------------------------------------
               CALL CODENT(NBCMP,'D',KNCMP)
C
               FORM2 = '(2X,A1,A'//KLONGM//',A1,A'//KLONG1//
     +                 ',A1,A8,'//KBLAN2//'X,A1,'
     +                 //KNCMP//'(X,A8,'//KBLAN1//'X,A1))'
               WRITE(IFM,FORM2) EXCLAM,NOMAIL,SLACH,NOMNO1,CROIX,
     +             NOMNOE(I),EXCLAM,(NOMCM2(JCMP),EXCLAM,JCMP=1,NBCMP)
C
C ---      BOUCLE SUR LE NOMBRE DE COMPOSANTES DU NOEUD 1 :
C          ----------------------------------------------
               N = 0
               DO 60 ICMP1 = 1, NBCMP1
                 IF (NBCM2(ICMP1).NE.0) THEN
                   FORM3 = '(2X,A'//KTIRET//')'
                   FORM4 = '(2X,A1,A8,18X,A1,'//KNCMP//'(X,'//FORVAR//
     +                     ',X,A1))'
                   IF (IMPLIG(NOMCM1(ICMP1),NBCMP2,LISCMP)) THEN
                     WRITE(IFM,FORM3) TIRET(1:NBTIRE)
                     WRITE(IFM,FORM4) EXCLAM, NOMCM1(ICMP1),EXCLAM,
     +                        (DBLE(VALE2(N+IJ)),DIMAG(VALE2(N+IJ)),
     +                        EXCLAM,IJ=1,NBCM2(ICMP1))
                   ENDIF
                   N = N + NBCM2(ICMP1)
                 ENDIF
  60           CONTINUE
               WRITE(IFM,FORM3) TIRET(1:NBTIRE)
           ENDIF
 10   CONTINUE
C
      END
