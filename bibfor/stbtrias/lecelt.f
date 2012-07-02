      SUBROUTINE LECELT(IUNV,MAXNOD,NBTYMA,INDIC,PERMUT,CODGRA,
     &                  NODE,NBNODE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF STBTRIAS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
      IMPLICIT NONE
C     ================================================================
CA PRESUPER
C
C     =============================================================
C     !                                                           !
C     !  FONCTION: LECTURE DES NOEUDS DES MAILLES DE CODE GRAPH-  !
C     !            IQUE CODGRA.                                   !
C     !            !!!!!  PERMUTATION EVENTUELLE !!!!!            !
C     !  (CF. DOCUMENT INTERFACE SUPERTAB-ASTER )  !
C     =============================================================
C     !                                                           !
C     !  ROUTINE APPELE: IUNIFI (FONCTION)                        !
C     !                                                           !
C     !  ROUTINE APPELANTE: SLEELT                                !
C     !                                                           !
C     =============================================================
C     !                                                           !
C     !                 ***************                           !
C     !                 *  ARGUMENTS  *                           !
C     !                 ***************                           !
C     !                                                           !
C     !  ******************************************************** !
C     !  *   NOM     * TYPE  * MODE *ALTERE *     ROLE          * !
C     !  ******************************************************** !
C     !  *           *       *      *       *                   * !
C     !  * MAXNOD    *INTEGER*ENTREE* NON   *NBRE MAXI DE NOEUDS* !
C     !  *           *       *      *       *POUR UNE MAILLE    * !
C     !  *           *       *      *       *                   * !
C     !  * NBTYMA    *INTEGER*ENTREE* NON   *NBRE DE TYPES DE   * !
C     !  *           *       *      *       * MAILLES SUPERTAB  * !
C     !  *           *       *      *       *                   * !
C     !  * INDIC     *INTEGER*ENTREE* NON   * INDIQUE S'IL FAUT * !
C     !  *           *       *      *       *FAIRE 1 PERMUTATION* !
C     !  *           *       *      *       *                   * !
C     !  * PERMUT    *INTEGER*ENTREE* NON   * TABLEAU DE PERMU- * !
C     !  *           *       *      *       *  TATION           * !
C     !  *           *       *      *       *                   * !
C     !  * CODGRA    *INTEGER*ENTREE* NON   * CODE GRAPHIQUE DE * !
C     !  *           *       *      *       * LA MAILLE A LIRE  * !
C     !  *           *       *      *       *                   * !
C     !  * NBNODE    *INTEGER*ENTREE* NON   * NBRE DE NOEUDS DE * !
C     !  *           *       *      *       *  CETTE MAILLE     * !
C     !  *           *       *      *       *                   * !
C     !  * NODE      *INTEGER*SORTIE* NON   * TABLEAU DES NOEUDS* !
C     !  *           *       *      *       *(PARFOIS PERMUTES) * !
C     !  *           *       *      *       * DE LA MAILLE      * !
C     !  *           *       *      *       *                   * !
C     !  ******************************************************** !
C     !                                                           !
C     !   VARIABLES LOCALES:  NODLU(2)--> TABLEAU DE TRAVAIL      !
C     !   ------------------                                      !
C     =============================================================
C
C  --> DECLARATION DES ARGUMENTS
      INTEGER MAXNOD,NBTYMA
      INTEGER CODGRA,NODE(MAXNOD),NBNODE
      INTEGER PERMUT(MAXNOD,NBTYMA),INDIC(NBTYMA)
C  --> DECLARATION DES VARIABLES LOCALES
      INTEGER NODLU(32),II
C  --> DECLARATION DES INDICES DE BOUCLES
      INTEGER I
      INTEGER IMES ,IUNIFI ,IUNV
C-----------------------------------------------------------------------
C
      READ (IUNV,'(8I10)') (NODLU(I),I=1,NBNODE)
C
      IF (INDIC(CODGRA).EQ.-1) THEN
        IMES = IUNIFI('MESSAGE')
        WRITE (IMES,*) 'MAILLE DE TYPE ',CODGRA,' NON TRAITE'
      ELSE IF (INDIC(CODGRA).EQ.0) THEN
C
        DO 1 I = 1,NBNODE
          NODE(I) = NODLU(I)
    1   CONTINUE
C
      ELSE IF (INDIC(CODGRA).EQ.1) THEN
C
        DO 2 I = 1,NBNODE
          II = PERMUT(I,CODGRA)
          NODE(II) = NODLU(I)
    2   CONTINUE
C
      END IF
C
      END
