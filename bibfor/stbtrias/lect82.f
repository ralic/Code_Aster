      SUBROUTINE LECT82(IUNV,NODE,NBNODE,INUM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF STBTRIAS  DATE 20/12/2010   AUTEUR PELLET J.PELLET 
C TOLE CRS_1404
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C     ================================================================
CA PRESUPER
C
C     =============================================================
C     !                                                           !
C     !  FONCTION: LECTURE DES NOEUDS DES TRACELINES IDEAS .      !
C     !            !!!!!   (DATASET 82 !!!!!                      !
C     !       (CF. DOCUMENT INTERFACE SUPERTAB-ASTER )            !
C     =============================================================
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
C     !  * NBNODE    *INTEGER*ENTREE* NON   * NBRE DE NOEUDS DE * !
C     !  *           *       *      *       *  CETTE MAILLE     * !
C     !  *           *       *      *       *                   * !
C     !  * NODE      *INTEGER*SORTIE* NON   * TABLEAU DES NOEUDS* !
C     !  *           *       *      *       *(PARFOIS PERMUTES) * !
C     !  *           *       *      *       * DE LA MAILLE      * !
C     !  *           *       *      *       *                   * !
C     !  * INUM      *INTEGER*SORTIE* NON   * NOMBRE MAX DE     * !
C     !  *           *       *      *       * NOEUDS LUS        * !
C     !  *           *       *      *       *                   * !
C     !  ******************************************************** !
C     !                                                           !
C     !   VARIABLES LOCALES:  NODLU(2)--> TABLEAU DE TRAVAIL      !
C     !   ------------------                                      !
C     =============================================================
C
C
C  --> DECLARATION DES ARGUMENTS
C     ============================================================
C
      INTEGER NODE(*),NBNODE,INUM,IDRO,IUNV
C  --> DECLARATION DES VARIABLES LOCALES
      INTEGER NODLU(NBNODE)
C  --> DECLARATION DES INDICES DE BOUCLES
      INTEGER I,J
C
C     ---------- FIN DECLARATIONS -----------
C
      READ (IUNV,'(8I10)') (NODLU(J),J=1,NBNODE)
      IDRO=1
      INUM=1
      DO  669 I=1,NBNODE
        IF ((NODLU(I).NE.0).AND.(I.LT.NBNODE)) THEN
          NODE(IDRO)=NODLU(I)
          NODE(IDRO+1)=NODLU(I+1)
          IDRO=IDRO+2
          INUM = INUM + 1
C cas ou on commence par un 0
        ELSEIF ((NODLU(I).EQ.0).AND.(I.EQ.1)) THEN
          IDRO=IDRO
          INUM = INUM
        ELSEIF ((NODLU(I).EQ.0).AND.(I.LT.NBNODE)) THEN
          IDRO=IDRO-2
          INUM = INUM - 1
        ELSEIF ((NODLU(I).EQ.0).AND.(I.GE.NBNODE)) THEN
          IDRO=IDRO-2
          INUM = INUM - 1
          GOTO 669
        ENDIF
  669 CONTINUE
      END
