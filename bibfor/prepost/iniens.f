      SUBROUTINE INIENS(MAXNOD,NBTYMX,NOMAST,NOMAEN,INDIC,PERMUT)
      IMPLICIT REAL*8 (A-H,O-Z)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 15/02/2005   AUTEUR NICOLAS O.NICOLAS 
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
C   ===================================================================
C   !  FONCTION: INITIALISATION DES NOMS DES MAILLES ASTER-TRIFOU EN  !
C   !            FONCTION D'UN NUMERO DE MAILLE ENSIGHT ARBITRAIRE    !
C   !   !!!! PERMUTATIONS EVENTUELLES DES NUMEROTATIONS        !!!!   !
C   !   !!!! LOCALES POUR LES MAILLES                          !!!!   !
C   ===================================================================
C   !  ROUTINE APPELANTE : IRADHE                                     !
C   ===================================================================
C   !                   ***************                               !
C   !                   *  ARGUMENTS  *                               !
C   !                   ***************                               !
C   !  ************************************************************** !
C   !  *  NOM    *  TYPE * MODE  *ALTERE *         ROLE             * !
C   !  ************************************************************** !
C   !  * MAXNOD  *INTEGER*ENTREE * NON   * NBRE MAXI DE NOEUDS POUR * !
C   !  *         *       *       *       *  UNE  MAILLE             * !
C   !  * NBTYMX  *INTEGER*SORTIE * NON   * NBRE MAXI DE TYPES DE    * !
C   !  *         *       *       *       *   MAILLE ENSIGHT         * !
C   !  * NOMAST  *CHARACT*SORTIE * NON   * NOMS DES MAILLES ASTER   * !
C   !  *         *       *       *       * EN FONCT DU TYPE DE      * !
C   !  *         *       *       *       * MAILLE ENSIGHT           * !
C   !  * NOMAEN  *CHARACT*SORTIE * NON   * NOMS DES MAILLES ENSIGHT * !
C   !  *         *       *       *       * EN FONCT DU TYPE DE      * !
C   !  *         *       *       *       * MAILLE ENSIGHT           * !
C   !  * INDIC   *INTEGER*SORTIE * NON   * INDIQUE S'IL FAUT FAIRE  * !
C   !  *         *       *       *       * UNE PERMUTATION POUR LA  * !
C   !  *         *       *       *       * NUMEROTION LOCALE DES    * !
C   !  *         *       *       *       * DES ELEMENTS             * !
C   !  * PERMUT  *INTEGER*SORTIE * NON   * TABLEAU DE PERMUTATION   * !
C   !  *         *       *       *       * POUR LA CONNECTIVITE     * !
C   !  ************************************************************** !
C
C  --> DECLARATIONS DES ARGUMENTS
      CHARACTER*8 NOMAST(*),NOMAEN(*)
      INTEGER MAXNOD,NBTYMX
      INTEGER PERMUT(MAXNOD,*),INDIC(*)
C  --> DECLARATIONS DES VARIABLES LOCALES
      INTEGER PERMU2(20,10:11),NBNOMA(10:11)
C  --> DECLARATIONS DES INDICES DE BOUCLES
      INTEGER I,J
C
C======================= ENSIGHT VERSION 5.5  =========================
C
C     NUMERO           MAILLE       NOM     NOEUDS  FACE  TYPE
C   ______________     ______      -----    ______  ____  ____
C
C   MAILLES SANS RENUMEROTATION LOCALE DES NOEUDS
C        1             LINEIQUE    bar2       2           LINEAIRE
C        2             TRIANGLE    tria3      3           LINEAIRE
C        3             TRIANGLE    tria6      6           QUADRATIQUE
C        4             QUADRANGLE  quad4      4           LINEAIRE
C        5             QUADRANGLE  quad8      8           QUADRATIQUE
C        6             TETRAEDRE   tetra4     4      4    LINEAIRE
C        7             TETRAEDRE   tetra10   10      4    QUADRATIQUE
C        8             PENTAEDRE   penta6     6      5    LINEAIRE
C        9             HEXAEDRE    hexa8      8      6    LINEAIRE
C   MAILLES AVEC RENUMEROTATION LOCALE DES NOEUDS
C       10             LINEIQUE    bar3       3           PARABOLIQUE
C       11             HEXAEDRE    hexa20    20      6    QUADRATIQUE
C   MAILLES DE DEGRE 2 NON SUPPORTEES ET REDUITES AU DEGRE 1
C       12             PENTAEDRE   penta15   15           QUADRATIQUE
C   MAILLES AVEC NOEUDS SUR LES FACES NON SUPPORTEES ET REDUITES
C       13             QUADRANGLE  quad9      9           QUADRATIQUE
C   MAILLES AVEC NOEUDS SUR LES FACES NON SUPPORTEES ET REDUITES
C       14             TRIANGLE    tria7      7           QUADRATIQUE
C
C --> DONNES POUR LA RENUMEROTATION LOCALE DES NOEUDS DES ELEMENTS
      DATA NBNOMA/3,20/
      DATA (PERMU2(I,10),I=1,3)/1,3,2/
      DATA (PERMU2(I,11),I=1,20)/1,2,3,4,5,6,7,8,9,10,11,12,17,18,19,
     &     20,13,14,15,16/
C --> FIN DES DONNEES POUR LA RENUMEROTATION
C
      DO 1 I=1,NBTYMX
        INDIC(I)=-1
    1 CONTINUE
      DO 3 I=1,9
        INDIC(I)=0
    3 CONTINUE
      INDIC(10)=1
      INDIC(11)=1
      INDIC(12)=0
      INDIC(13)=0
      INDIC(14)=0
C
      DO 5 I=1,NBTYMX
        IF(INDIC(I).EQ.1) THEN
          DO 6 J=1,NBNOMA(I)
            PERMUT(J,I)=PERMU2(J,I)
    6     CONTINUE
        END IF
    5 CONTINUE
C
      DO 7 I=1,NBTYMX
        NOMAST(I) = 'XXX'
        NOMAEN(I) = 'XXX'
    7 CONTINUE
C
      NOMAST( 1)='SEG2'
      NOMAEN( 1)='bar2'
C
      NOMAST( 2)='TRIA3'
      NOMAEN( 2)='tria3'
C
      NOMAST( 3)='TRIA6'
      NOMAEN( 3)='tria6'
C
      NOMAST( 4)='QUAD4'
      NOMAEN( 4)='quad4'
C
      NOMAST( 5)='QUAD8'
      NOMAEN( 5)='quad8'
C
      NOMAST( 6)='TETRA4'
      NOMAEN( 6)='tetra4'
C
      NOMAST( 7)='TETRA10'
      NOMAEN( 7)='tetra10'
C
      NOMAST( 8)='PENTA6'
      NOMAEN( 8)='penta6'
C
      NOMAST( 9)='HEXA8'
      NOMAEN( 9)='hexa8'
C
      NOMAST(10)='SEG3'
      NOMAEN(10)='bar3'
C
      NOMAST(11)='HEXA20'
      NOMAEN(11)='hexa20'
C
      NOMAST(12)='PENTA15'
      NOMAEN(12)='penta15'
C
      NOMAST(13)='QUAD9'
      NOMAEN(13)='quad9'
C
      NOMAST(14)='TRIA7'
      NOMAEN(14)='tria7'
C
      END
