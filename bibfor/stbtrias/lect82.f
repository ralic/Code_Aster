      SUBROUTINE LECT82(NODE,NBNODE,INUM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF STBTRIAS  DATE 10/02/2004   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER NODE(NBNODE),NBNODE,INUM,IPO,IDRO,IUNV
C  --> DECLARATION DES VARIABLES LOCALES
      INTEGER NODLU(10000),NOD82(10000)
C  --> DECLARATION DES INDICES DE BOUCLES
      INTEGER I,J
C
C     ---------- FIN DECLARATIONS -----------
C
      IUNV = IUNIFI('IDEAS')
      READ (IUNV,'(8I10)') (NODLU(J),J=1,NBNODE)
      IPO=0
      DO  667 I=1,NBNODE
        DO 668 J=1,2
          NOD82(IPO+J) = NODLU(I-1+J)
  668   CONTINUE
        IPO=IPO+2
  667 CONTINUE
      IDRO=1
      INUM=1
      DO  669 I=1,NBNODE*2,2
        IF ((NOD82(I).NE.0)) THEN 
          NODE(IDRO)=NOD82(I)
          NODE(IDRO+1)=NOD82(I+1)
          IDRO=IDRO+2
          INUM = INUM + 1
        ELSE
          IF ((NOD82(I).EQ.0).AND.(I.GE.2*(NBNODE)-1)) THEN
          IDRO=IDRO-3
          INUM = INUM - 2
          GOTO 669
        ELSE
          IDRO=IDRO-2
          INUM = INUM - 1
        ENDIF  
      ENDIF  
  669 CONTINUE
      END
