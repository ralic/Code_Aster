      SUBROUTINE TRNOC1 ( NBNO, LISNOE, NBCMP, LISCMP, OPTIOZ, 
     +                    LONLI1, NOMNOE, NOMCMP, VALE, INDCMP, LONLI2)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 05/11/96   AUTEUR CIBHHGB G.BERTRAND 
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
C      TRNOC1 -- TRI   .DE LA LISTE DE NOMS DE NOEUDS NOMNOE
C                      .DE LA LISTE DE NOMS DE COMPOSANTES NOMCMP
C                      .DE LA LISTE DE VALEURS COMPLEXES VALE
C                              (RELATIVE A CES COMPOSANTES) 
C                EN FONCTION :
C                            .DE LA LISTE DE NOMS DE NOEUDS LISNOE
C                        ET  .DE LA LISTE DE NOMS DE COMPOSANTES LISCMP
C                
C               (I.E. ON NE RETIENT DU TABLEAU VALE QUE LES VALEURS
C                     RELATIVES AUX COMPOSANTES LISCMP DES NOEUDS DE 
C                     LISNOE)
C
C   ARGUMENT        E/S  TYPE         ROLE
C    NBNO            IN    I     NOMBRE DE NOEUDS DE LA LISTE LISNOE
C                                SI = 0 LA LISTE LISNOE EST VIDE ET
C                                L'ON PREND EN COMPTE TOUS LES NOEUDS
C                                DU TABLEAU NOMNOE
C    LISNOE(1)       IN    K8    LISTE DES NOEUDS POUR-LESQUELS ON
C                                DESIRE L'IMPRESSION DES VALEURS
C                                DU TABLEAU VALE
C    NBCMP           IN    I     NOMBRE DE COMPOSANTES DE LA LISTE
C                                LISCMP DES COMPOSANTES
C                                SI = 0 LA LISTE LISCMP EST VIDE ET
C                                L'ON PREND EN COMPTE TOUTES LES 
C                                COMPOSANTES DU TABLEAU NOCMP
C    LISCMP(1)       IN    K8    LISTE DES COMPOSANTES POUR-LESQUELLES
C                                ON DESIRE L'IMPRESSION DES VALEURS
C                                DU TABLEAU VALE
C    OPTIOZ          IN    K*     OPTION D'IMPRESSION
C                                     = 'SOUS_MATRICE'
C                                  OU = 'LIGNE'
C                                  OU = 'COLONNE'
C                                  SI = 'SOUS_MATRICE' ON IMPRIME 
C                                       UNIQUEMENT LES COMPOSANTES 
C                                       LICITES DES NOEUDS DE LA LISTE
C                                       LISNOE ET SI CELLE-CI EST VIDE
C                                       (NBNO = 0)
C                                       ON IMPRIME CELLES DE TOUS LES
C                                       NOEUDS (PHYSIQUES) DU MAILLAGE
C                                  SI = 'LIGNE' ON IMPRIME TOUTES LES
C                                       LES VALEURS RELATIVES AUX 
C                                       COMPOSANTES LICITES DES NOEUDS
C                                       DE LA LISTE LISNOE ET DE TOUS
C                                       LES NOEUDS SI CETTE LISTE EST 
C                                       VIDE (NBNO =0 )
C                                  SI = 'COLONNE' ON IMPRIME TOUTES LES
C                                       LES VALEURS RELATIVES AUX 
C                                       COMPOSANTES LICITES DES NOEUDS
C                                       DE LA LISTE LISNOE ET DE TOUS
C                                       LES NOEUDS SI CETTE LISTE EST 
C                                       VIDE (NBNO =0)      
C    LONLI1          IN    I     DIMENSION DES TABLEAUX NOMNOE, 
C                                NOMCMP ET VALE
C    NOMNOE(1)       VAR   K8    LISTE DES NOEUDS SUR-LESQUELS PORTENT
C                                LES TERMES DU TABLEAU VALE
C    NOMCMP(1)       VAR   K8    LISTE DES COMPOSANTES SUR-LESQUELLES 
C                                PORTENT LES TERMES DU TABLEAU VALE
C    VALE(1)         VAR   C     TABLEAU DES VALEURS A TRIER 
C    INDCMP(1)       IN    I     INDICATEUR DISANT SI UNE COMPOSANTE
C                                EST A PRENDRE EN COMPTE ( =0)
C                                OU NON ( =1)
C    LONLI2          OUT   I     LONGUEUR 'UTILE' DES TABLEAUX NOMNOE,
C                                NOMCMP ET  VALE
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
      CHARACTER*(*) OPTIOZ
      CHARACTER*8   LISNOE(1), LISCMP(1),  NOMNOE(1), NOMCMP(1)
      COMPLEX*16    VALE(1)
      INTEGER       INDCMP(1)
C -----  VARIABLES LOCALES
      CHARACTER*12  OPTION 
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      OPTION = OPTIOZ
C
      LONLI2 = 0
C
      DO 10 I = 1, LONLI1
        INDCMP(I) = 0
 10   CONTINUE
C
C --- CAS OU LA LISTE DES NOEUDS A RETENIR EST VIDE (I.E. NBNO = 0 
C --- ON GARDE TOUS LES NOEUDS) OU CAS OU L'OPTION EST 'SOUS_MATRICE'
C --- ET OU L'ON DOIT FAIRE UN TRI SUR LES COMPOSANTES (NBCMP > 0) :
C     ------------------------------------------------------------
      IF (NBNO.EQ.0.OR.OPTION.NE.'SOUS_MATRICE') THEN
          IF (NBCMP.NE.0) THEN
              DO 20 I = 1, LONLI1
                 INDCMP(I) = 1
                 DO 30 J=1, NBCMP
                    IF (LISCMP(J).EQ.NOMCMP(I)) THEN
                        INDCMP(I) = 0
                    ENDIF
  30             CONTINUE
  20          CONTINUE
          ENDIF
C
C --- CAS OU LA LISTE DES NOEUDS A RETENIR N'EST PAS VIDE 
C --- ET OU OPTION = 'SOUS_MATRICE' :
C     -----------------------------
      ELSE
C
C ---    CAS OU L'ON NE FAIT PAS DE TRI SELON LES COMPOSANTES :
C        ----------------------------------------------------
          IF (NBCMP.EQ.0) THEN
              DO 40 I = 1, LONLI1
                 INDCMP(I) = 1
                 DO 50 J=1, NBNO
                    IF (LISNOE(J).EQ.NOMNOE(I)) THEN
                        INDCMP(I) = 0
                    ENDIF
  50             CONTINUE
  40          CONTINUE
C
C ---    CAS OU L'ON FAIT UN TRI SELON LES COMPOSANTES :
C        ---------------------------------------------
          ELSE
              DO 60 I = 1, LONLI1
                INDCMP(I) = 1
                 DO 70 J=1, NBNO
                    IF (LISNOE(J).EQ.NOMNOE(I)) THEN
                      DO 80 K=1, NBCMP
                        IF (LISCMP(K).EQ.NOMCMP(I)) THEN
                          INDCMP(I) = 0
                        ENDIF
  80                 CONTINUE
                    ENDIF
  70             CONTINUE
  60          CONTINUE
          ENDIF
      ENDIF
C
C --- RECONSTRUCTION DES TABLEAUX DES NOMS DES NOEUDS, DES NOMS 
C --- DES COMPOSANTES ET DES VALEURS EN NE GARDANT QUE LES 
C --- COMPOSANTES LICITES :
C     -------------------
      INDLIS = 0
C
      DO 90 I = 1, LONLI1
         IF (INDCMP(I).EQ.0) THEN
             INDLIS = INDLIS + 1
             NOMNOE(INDLIS) = NOMNOE(I)
             NOMCMP(INDLIS) = NOMCMP(I)
             VALE(INDLIS)   = VALE(I)
         ENDIF
  90  CONTINUE
C
      LONLI2 = INDLIS
C
      END
