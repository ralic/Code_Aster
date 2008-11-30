      SUBROUTINE INISTB(MAXNOD,NBTYMA,NOMAIL,INDIC,PERMUT,
     &                  LIMAIL,INDICF,PERMUF,MAXFA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF STBTRIAS  DATE 01/12/2008   AUTEUR COURTOIS M.COURTOIS 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C     ====================================================
CA PRESUPER
C
C   ===================================================================
C   !                                                                 !
C   !  AUTEURS:J.M. PROIX                             DATE:20/11/89   !
C!J.F.LAMAUDIERED       DATE 15/05/91
C   !                                                                 !
C   !  LOGISCOPE STATIQUE :09/02/90                                   !
C   !                                                                 !
C   !  LOGISCOPE DYNAMIQUE (35 JEUX TESTS) :21/02/90                  !
C   !                                                                 !
C   ===================================================================
C   !                                                                 !
C   !  FONCTION: INITIALISATION DES NOMS DES MAILLES ASTER-TRIFOU EN  !
C   !            FONCTION DU CODE GRAPHIQUE SUPERTAB I-DEAS 4.0       !
C   !                                                                 !
C   !  REMARQUE: LA LISTE DES CODES DESCRIPTEURS (VERSION 5) EST      !
C   !            DONNEE ICI SEULEMENT A TITRE INDICATIF .             !
C   !            UNE CORRESPONDANCE EST FAITE AU NIVEAU DU SOUS_      !
C   !            PROGRAMME COV4V5 (CORRESPONDANCE DU CODE DESCRIPTEUR !
C   !            EN VERSION 5 ET DU CODE GRAPHIQUE EN VERSION 4       !
C   !                                                                 !
C   !   !!!! PERMUTATIONS EVENTUELLES DES NUMEROTATIONS LOCALES!!!!   !
C   !   !!!!       POUR LES MAILLES ET LES FACES               !!!!   !
C   !                                                                 !
C   !   (CF. DOCUMENT INTERFACE SUPERTAB-ASTER/SUPERTAB-TRIFOU)       !
C   !                                                                 !
C   ===================================================================
C   !                                                                 !
C   !  ROUTINE APPELANTE : PRESUP                                     !
C   !                                                                 !
C   ===================================================================
C   !                                                                 !
C   !                   ***************                               !
C   !                   *  ARGUMENTS  *                               !
C   !                   ***************                               !
C   !                                                                 !
C   !  ************************************************************** !
C   !  *  NOM    *  TYPE * MODE  *ALTERE *         ROLE             * !
C   !  ************************************************************** !
C   !  *         *       *       *       *                          * !
C   !  * MAXNOD  *INTEGER*ENTREE * NON   *NBRE MAXI DE NOEUDS POUR  * !
C   !  *         *       *       *       *  UNE  MAILLE             * !
C   !  *         *       *       *       *                          * !
C   !  * MAXFA   *INTEGER*ENTREE * NON   *NBRE MAXI DE FACES POUR   * !
C   !  *         *       *       *       *  UNE MAILLE
C   !  *         *       *       *       *                          * !
C   !  * NBTYMA  *INTEGER*SORTIE * NON   *NBRE DE TYPE DE MAILLES   * !
C   !  *         *       *       *       *   SUPERTAB               * !
C   !  *         *       *       *       *                          * !
C   !  * NOMAIL  *CHARACT*SORTIE * NON   *NOMS DES MAILES EN FONCT  * !
C   !  *         *       *       *       * DU CODE GRAPHIQUE S.TAB  * !
C   !  *         *       *       *       *                          * !
C   !  *         *       *       *       *                          * !
C   !  * INDIC   *INTEGER*SORTIE * NON   * INDIQUE S'IL FAUT FAIRE  * !
C   !  *         *       *       *       * UNE PERMUTATION POUR LA  * !
C   !  *         *       *       *       * NUMEROTION LOCALE DES    * !
C   !  *         *       *       *       * DES ELEMENTS             * !
C   !  *         *       *       *       *                          * !
C   !  * PERMUT  *INTEGER*SORTIE * NON   * TABLEAU DE PERMUTATION   * !
C   !  *         *       *       *       * POUR LA CONNECTIVITE     * !
C   !  *         *       *       *       *                          * !
C   !  * LIMAIL  *INTEGER*SORTIE * NON   * TABLEAU CONTENANT LE NBRE* !
C   !  *         *       *       *       * DE LIGNES POUR L'ECRITURE* !
C   !  *         *       *       *       * DE CHAQUE TYPE DE MAILLE * !
C   !  *         *       *       *       *                          * !
C   !  * INDICF  *INTEGER*SORTIE * NON   * INDIQUE S'IL FAUT FAIRE  * !
C   !  *         *       *       *       * UNE PERMUTATION POUR LA  * !
C   !  *         *       *       *       * NUMEROTATION LOCALE DES  * !
C   !  *         *       *       *       * FACES                    * !
C   !  *         *       *       *       *                          * !
C   !  * PERMUF  *INTEGER*SORTIE * NON   * TABLEAU DE PERMUTATION   * !
C   !  *         *       *       *       * POUR LA NUMEROTATION     * !
C   !  *         *       *       *       * LOCALE DES FACES         * !
C   !  *         *       *       *       *                          * !
C   !  ************************************************************** !
C   !                                                                 !
C   ===================================================================
C
C  --> DECLARATIONS DES ARGUMENTS
      CHARACTER*8 NOMAIL(*)
      INTEGER MAXFA,MAXNOD,NBTYMA,LIMAIL(*)
      INTEGER PERMUT(MAXNOD,*),INDIC(*),PERMUF(MAXFA,*),INDICF(*)
C  --> DECLARATIONS DES VARIABLES LOCALES
      INTEGER PERMU2(20,3:35),NBNOMA(3:35)
      INTEGER PERMU3(6,14:21),NBNOMF(14:21)
C
C  --> DECLARATIONS DES INDICES DE BOUCLES
      INTEGER I,J
C
C
C======================= SUPERTAB I-DEAS 4.0 ========================
C
C     CODE GRAPHIQUE     MAILLE             NOEUDS   FACE   TYPE
C     ______________     ______             ______   ____   ____
C
C          1             LINEIQUE              2           LINEAIRE
C          2             TRIANGLE              3           LINEAIRE
C          3             TRIANGLE              6           QUADRATIQUE
C          4             TRIANGLE              9           CUBIQUE
C          5             QUADRILATERE          4           LINEAIRE
C          6             QUADRILATERE          8           QUADRATIQUE
C          7             QUADRILATERE         12           CUBIQUE
C          8             PENTA COQUE EPAISSE   6           LINEAIRE=16
C          9             PENTA COQUE EPAISSE  12           QUADRATIQUE
C         10             PENTA COQUE EPAISSE  18           CUBIQUE
C         11             BRIQUE COQUE EPAISSE  8           LINEAIRE=19
C         12             BRIQUE COQUE EPAISSE 16           QUADRATIQUE
C         13             BRIQUE COQUE EPAISSE 24           CUBIQUE
C         14             TETRAEDRE             4     4     LINEAIRE
C         15             TETRAEDRE            10     4     QUADRATIQUE
C         16             PENTAEDRE             6     5     LINEAIRE
C         17             PENTAEDRE            15     5     QUADRATIQUE
C         18             PENTAEDRE            24     5     CUBIQUE
C         19             HEXAEDRE              8     6     LINEAIRE
C         20             HEXAEDRE             20     6     QUADRATIQUE
C         21             HEXAEDRE             32     6     CUBIQUE
C         22-28 IGNORES
C         29             RESSORT ENTRE NOEUDS  2           LINEAIRE
C         30             RESSORT FIXE AU SOL   1           LINEAIRE
C         31             AMORTIS ENTRE NOEUDS  2           LINEAIRE
C         32             AMORTIS FIXE AU SOL   1           LINEAIRE
C         33             MASSE CONCENTREE      1           LINEAIRE
C         34             BARRE RIGIDE          2           LINEAIRE
C         35             LINEIQUE              3           PARABOLIQUE
C
C
C======================= SUPERTAB I-DEAS 5.0 ========================
C
C     CODE DESCRIPTEUR   MAILLE             NOEUDS   FACE   TYPE
C     ________________   ______             ______   ____   ____
C
C     171                LINEIQUE              2           LINEAIRE
C     91_61_74_81        TRIANGLE              3           LINEAIRE
C     92_62_72_82        TRIANGLE              6           QUADRATIQUE
C     93_63_73           TRIANGLE              9           CUBIQUE
C     94_64_71_84        QUADRILATERE          4           LINEAIRE
C     95_65_75_85        QUADRILATERE          8           QUADRATIQUE
C     96_66_76           QUADRILATERE         12           CUBIQUE
C     101                PENTA COQUE EPAISSE   6           LINEAIRE=16
C     102                PENTA COQUE EPAISSE  12           QUADRATIQUE
C     103                PENTA COQUE EPAISSE  18           CUBIQUE
C     104                BRIQUE COQUE EPAISSE  8           LINEAIRE=19
C     105                BRIQUE COQUE EPAISSE 16           QUADRATIQUE
C     106                BRIQUE COQUE EPAISSE 24           CUBIQUE
C     111                TETRAEDRE             4     4     LINEAIRE
C     118                TETRAEDRE            10     4     QUADRATIQUE
C     112                PENTAEDRE             6     5     LINEAIRE
C     113                PENTAEDRE            15     5     QUADRATIQUE
C     114                PENTAEDRE            24     5     CUBIQUE
C     115                HEXAEDRE              8     6     LINEAIRE
C     116                HEXAEDRE             20     6     QUADRATIQUE
C     117                HEXAEDRE             32     6     CUBIQUE
C     136_137            RESSORT ENTRE NOEUDS  2           LINEAIRE
C     138_139            RESSORT FIXE AU SOL   1           LINEAIRE
C     141                AMORTIS ENTRE NOEUDS  2           LINEAIRE
C     142                AMORTIS FIXE AU SOL   1           LINEAIRE
C     161                MASSE CONCENTREE      1           LINEAIRE
C     121                BARRE RIGIDE          2           LINEAIRE
C     172                LINEIQUE              3           PARABOLIQUE
C
C --> DONNES POUR LA RENUMEROTATION LOCALE DES ELEMENTS
      DATA NBNOMA/6,9,4,8,12,6,12,18,8,16,24,4,10,6,15,24,8,20,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,3/
      DATA (PERMU2(I,3),I=1,6)/1,4,2,5,3,6/
      DATA (PERMU2(I,4),I=1,9)/1,4,5,2,6,7,3,8,9/
      DATA (PERMU2(I,6),I=1,8)/1,5,2,6,3,7,4,8/
      DATA (PERMU2(I,7),I=1,12)/1,5,6,2,7,8,3,9,10,4,11,12/
      DATA (PERMU2(I,9),I=1,12)/1,7,2,8,3,9,4,10,5,11,6,12/
      DATA (PERMU2(I,12),I=1,16)/1,9,2,10,3,11,4,12,5,13,6,14,7,15,8,16/
      DATA (PERMU2(I,15),I=1,10)/1,5,2,6,3,7,8,9,10,4/
      DATA (PERMU2(I,17),I=1,15)/1,7,2,8,3,9,10,11,12,4,13,5,14,6,15/
      DATA (PERMU2(I,20),I=1,20)/1,9,2,10,3,11,4,12,13,14,15,16,5,17,
     &     6,18,7,19,8,20/
      DATA (PERMU2(I,35),I=1,3)/1,3,2/
C
C --> DONNEES POUR LA RENUMEROTATION LOCALE DES FACES
      DATA NBNOMF/4,4,5,5,5,6,6,6/
      DATA (PERMU3(I,14),I=1,4)/1,2,3,4/
      DATA (PERMU3(I,15),I=1,4)/3,1,4,2/
      DATA (PERMU3(I,16),I=1,5)/1,2,3,4,5/
      DATA (PERMU3(I,17),I=1,5)/1,4,2,5,3/
      DATA (PERMU3(I,18),I=1,5)/1,4,2,5,3/
      DATA (PERMU3(I,19),I=1,6)/1,2,3,4,5,6/
      DATA (PERMU3(I,20),I=1,6)/3,6,1,5,4,2/
      DATA (PERMU3(I,21),I=1,6)/3,6,1,5,4,2/
C
C --> FIN DES DONNEES POUR LA RENUMEROTATION
C
      DO 1 J=1,MAXNOD
        LIMAIL(J)=0
 1    CONTINUE
C
      DO 2 I=1,MAXNOD
         INDIC(I)=-1
    2 CONTINUE
C
      DO 3 I=1,8
         INDIC(I)=0
    3 CONTINUE
C
C JMP 2/5/90
C
      INDIC(3)=1
      INDIC(4)=1
      INDIC(6)=1
      INDIC(7)=1
C
      INDIC(9)=1
      INDIC(12)=1
      INDIC(15)=1
      INDIC(17)=1
      INDIC(20)=1
      INDIC(35)=1
C
      INDIC(11)=0
      INDIC(14)=0
      INDIC(16)=0
      INDIC(19)=0
C
      DO 4 I=29,34
         INDIC(I)=0
    4 CONTINUE
C
      NBTYMA=35
C
      DO 5 I=1,NBTYMA
         IF (INDIC(I).EQ.1) THEN
C
            DO 6 J=1,NBNOMA(I)
               PERMUT(J,I)=PERMU2(J,I)
    6       CONTINUE
C
         END IF
    5 CONTINUE
C
      DO 10 I=1,MAXNOD
        INDICF(I)=-1
 10   CONTINUE
C
      DO 20 I=1,13
        INDICF(I)=0
 20   CONTINUE
C
      DO 30 I=14,21
        INDICF(I)=1
 30   CONTINUE
C
      DO 40 I=29,NBTYMA
        INDICF(I)=0
 40   CONTINUE
C
      DO 50 I=1,NBTYMA
        IF(INDICF(I).EQ.1) THEN
          DO 60 J=1,NBNOMF(I)
            PERMUF(J,I)=PERMU3(J,I)
 60       CONTINUE
        ENDIF
 50   CONTINUE
C
      NOMAIL( 1)='SEG2'
      NOMAIL( 2)='TRIA3'
      NOMAIL( 3)='TRIA6'
      NOMAIL( 4)='TRIA9'
      NOMAIL( 5)='QUAD4'
      NOMAIL( 6)='QUAD8'
      NOMAIL( 7)='QUAD12'
      NOMAIL( 8)='PENTAC6'
      NOMAIL( 9)='PENTAC12'
      NOMAIL(10)='PENTAC18'
      NOMAIL(11)='HEXACE8'
      NOMAIL(12)='HEXACE16'
      NOMAIL(13)='HEXACE24'
      NOMAIL(14)='TETRA4'
      NOMAIL(15)='TETRA10'
      NOMAIL(16)='PENTA6'
      NOMAIL(17)='PENTA15'
      NOMAIL(18)='PENTA24'
      NOMAIL(19)='HEXA8'
      NOMAIL(20)='HEXA20'
      NOMAIL(21)='HEXA32'
      NOMAIL(22)=' '
      NOMAIL(23)=' '
      NOMAIL(24)=' '
      NOMAIL(25)=' '
      NOMAIL(26)=' '
      NOMAIL(27)=' '
      NOMAIL(28)=' '
      NOMAIL(29)='SEG2'
      NOMAIL(30)='POI1'
      NOMAIL(31)='SEG2'
      NOMAIL(32)='POI1'
      NOMAIL(33)='POI1'
      NOMAIL(34)='SEG2'
      NOMAIL(35)='SEG3'
C
C  --> TABLEAU POUR LES NOMBRES DE LIGNE DECRIVANT CHAQUE MAILLE
      LIMAIL(1)=1
      LIMAIL(2)=1
      LIMAIL(3)=1
      LIMAIL(4)=2
      LIMAIL(5)=1
      LIMAIL(6)=1
      LIMAIL(7)=2
      LIMAIL(8)=1
      LIMAIL(9)=2
      LIMAIL(10)=3
      LIMAIL(11)=1
      LIMAIL(12)=2
      LIMAIL(13)=3
      LIMAIL(14)=1
      LIMAIL(15)=2
      LIMAIL(16)=1
      LIMAIL(17)=2
      LIMAIL(18)=3
      LIMAIL(19)=1
      LIMAIL(20)=3
      LIMAIL(21)=4
      LIMAIL(29)=1
      LIMAIL(30)=1
      LIMAIL(31)=1
      LIMAIL(32)=1
      LIMAIL(33)=1
      LIMAIL(34)=1
      LIMAIL(35)=1
      END
