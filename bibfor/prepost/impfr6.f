      SUBROUTINE IMPFR6 (NOMAIL,LONLIS, NOMNOE, 
     +                   NOMCMP, VALE, INDCMP, INDNOE, NUMNOE,
     +                   NOMCM2, VALE2,NBCM2, NCMPMX,
     +                   NBCHIF, FICHIE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 17/01/97   AUTEUR VABHHTS J.PELLET 
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
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      IMPFR6 -- IMPRESSION D'UNE LIGNE D'UN VECTEUR ELEMENTAIRE
C                D 'UN VECT_ELEM SELON LE GRAIN 'NOEUD'
C                LA LIGNE NE CONTIENT QUE LES VALEURS
C                DES COMPOSANTES LICITES 
C                (LICITE VEUT DIRE 'CHOISI PAR L'UTILISATEUR')
C
C   ARGUMENT        E/S  TYPE         ROLE
C    NOMAIL          IN   K8     NOM DE LA MAILLE A IMPRIMER 
C    LONLIS          IN   I      LONGUEUR 'UTILE' DES TABLEAUX NOMNOE,
C                                NOMCMP ET  VALE
C    NOMNOE(*)       IN   K8     LISTE DES NOEUDS SUR-LESQUELS PORTENT
C                                LES TERMES DU TABLEAU VALE
C    NOMCMP(*)       IN   K8     LISTE DES COMPOSANTES SUR-LESQUELLES 
C                                PORTENT LES TERMES DU TABLEAU VALE
C    VALE(*)         IN   R      TABLEAU DES VALEURS DE LA LIGNE OU
C                                DE LA COLONNE A IMPRIMER 
C    INDCMP(*)       IN   I      INDCMP(INDICE_COMPOSANTE)=INDICE_LIGNE
C    INDNOE(*)       IN   I      INDICATEUR DISANT SI UN NOEUD A ETE 
C                                IMPRIME OU NON
C    NUMNOE(*)       IN   I      TABLEAU INDIQUANT A QUEL NOEUD 
C                                APPARTIENT UNE COMPOSANTE A IMPRIMER 
C    NOMCM2(*)       IN   K8     NOM DES COMPOSANTES A IMPRIMER SUR UNE
C                                LIGNE 
C    VALE2(*)        IN   R      VALEUR DES COMPOSANTES A IMPRIMER SUR 
C                                UNE LIGNE 
C    NBCM2(*)        IN   I      TABLEAU DU NOMBRE DE COMPOSANTES A
C                                IMPRIMER PAR LIGNE POUR UN  NOEUD DONNE
C    NCMPMX          IN   I      NOMBRE DE COMPOSANTES MAX DU NOEUD 1 
C    NBCHIF          IN   I      NOMBRE DE CHIFFRES SIGNIFICATIFS
C    FICHIE          IN   K*     NOM DU FICHIER D'IMPRESSION
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
      CHARACTER*(*) FICHIE
      CHARACTER*8   NOMNOE(*), NOMCMP(*), NOMCM2(*), NOMAIL
      REAL*8        VALE(*), VALE2(*)
      INTEGER       INDCMP(*), INDNOE(*), NBCM2(*), NUMNOE(*)
C -----  VARIABLES LOCALES
      PARAMETER     (NBNOMX = 27)
      CHARACTER*1   SLACH 
      CHARACTER*1   EXCLAM 
      CHARACTER*1   CROIX 
      CHARACTER*1   KBLAN1 
      CHARACTER*1   KLONGM 
      CHARACTER*2   KNBCH, KLVALR, KNCMP, KBLAN2
      CHARACTER*3   KTIRET
      CHARACTER*8   M8BLAN, FORVAR
      CHARACTER*35  TIRE1
      CHARACTER*72  FORM1, FORM2, FORM3, FORM4
      CHARACTER*160 TIRET
C
      INTEGER       LXLGUT
C
      DATA TIRE1 /'___________________________________'/
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     ---------------
      NBCMP1 = 1
      SLACH  = '/'
      EXCLAM = '!'
      M8BLAN = '        '
      TIRET = TIRE1//TIRE1//TIRE1//TIRE1//TIRE1(1:20)
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
      FORVAR = '1PD'//KLVALR//'.'//KNBCH
C
      NBBLAN = LVALRE + 1 - 8  
      CALL CODENT(NBBLAN,'D',KBLAN1)
C
C --- LONGUEUR UTILE DE  NOMAIL :
C     -------------------------
      LGNOMA = LXLGUT(NOMAIL)
      LGNOMT = LGNOMA+1
      CALL CODENT(LGNOMA,'G',KLONGM)
C
C --- DECALAGE VERS LA DROITE DE LA CHAINE DE CARACTERES NOMAIL :
C     ---------------------------------------------------------
      IDECAL =  9 - LGNOMT
      CALL CODENT(IDECAL,'G',KBLAN2)
C
      DO 10 I = 1, NBNOMX
          INDNOE(I) = 0
  10  CONTINUE
C
C --- BOUCLE SUR LES TERMES DE LA LISTE :
C     ---------------------------------
      DO 20 I = 1, LONLIS
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
               DO 30 J = 1, NCMPMX
                 NBCM2(J) = 0
 30            CONTINUE
C
               NCMPX2 = NCMPMX*NCMPMX
C
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
C ---      NOMBRE DE 'TIRETS' POUR FAIRE LE CADRE :
C          --------------------------------------
               NBTIRE = 19 + NBCMP*(NBCH+9)
               CALL CODENT(NBTIRE,'G',KTIRET)
C
               FORM1 = '('//'2X,A'//KTIRET//')'
               WRITE(IFM,FORM1) TIRET(1:NBTIRE)
C
C ---      CODAGE DE NBCMP SOUS FORME D'UNE CHAINE DE CARACTERES :
C          -----------------------------------------------------
               CALL CODENT(NBCMP,'D',KNCMP)
C
               FORM2 = '(2X,A1,A'//KLONGM//',A1,A8,'
     +                 //KBLAN2//'X,A1,'
     +                 //KNCMP//'(X,A8,'//KBLAN1//'X,A1))'
               WRITE(IFM,FORM2) EXCLAM,NOMAIL,SLACH,
     +             NOMNOE(I),EXCLAM,(NOMCM2(JCMP),EXCLAM,JCMP=1,NBCMP)
C
              FORM3 = '(2X,A'//KTIRET//')'
              FORM4 = '(2X,A1,17X,A1,'//KNCMP//'(X,'//FORVAR//
     +                ',X,A1))'
              WRITE(IFM,FORM3) TIRET(1:NBTIRE)
              WRITE(IFM,FORM4) EXCLAM,EXCLAM,
     +                        (VALE2(IJ),EXCLAM,IJ=1,NBCMP)
              WRITE(IFM,FORM3) TIRET(1:NBTIRE)
           ENDIF
 20   CONTINUE
C
      END
