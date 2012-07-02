      SUBROUTINE CODNOP(NOM1,NOM2,IC,NC)
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
C     ===================================
C
C     ===============================================================
C     !                                                             !
C     !  AUTEUR:J.F.LAMAUDIERE                         DATE:1/12/89 !
C     !                                                             !
C     !  LOGISCOPE STATIQUE :09/02/90                               !
C     !                                                             !
C     !  LOGISCOPE DYNAMIQUE (35 JEUX TESTS) :21/02/90              !
C     !                                                             !
C     ===============================================================
C     !                                                             !
C     !  FONCTION: CETTE ROUTINE PERMET L'ECRITURE D'UNE CHAINE !
C     !            DE CARACTERES DANS UNE AUTRE.                    !
C     !                                                             !
C     ===============================================================
C     !                                                             !
C     !  ROUTINE APPELANTES : ECRNEU                                !
C     !                           : SLECOR                          !
C     !                           : ECRELT                          !
C     !                           : SLEGRO                          !
C     !                           : SLEGEO                          !
C     !                           : ECFACH                          !
C     !                                                             !
C     ==============================================================!
C     !                                                             !
C     !                    ***************                          !
C     !                    *  ARGUMENTS  *                          !
C     !                    ***************                          !
C     !                                                             !
C     ! *********************************************************** !
C     ! *   NOM  *  TYPE  * MODE *ALTERE *        ROLE            * !
C     ! *********************************************************** !
C     ! * NOM1   *CHARACTE*SORTIE* OUI   *  CHAINE DE CARACTERE   * !
C     ! *        *        *      *       * RESULTAT DE LA CONCATE-* !
C     ! *        *        *      *       *   NATION               * !
C     ! *        *        *      *       *                        * !
C     ! * NOM2   *CHARACTE*ENTREE* NON   * CHAINE DE CARACTERES A * !
C     ! *        *        *      *       *   CONCATENER           * !
C     ! *        *        *      *       *                        * !
C     ! * IC     *INTEGER *ENTREE* NON   * DEBUT DE LA POSITION DE* !
C     ! *        *        *      *       *LA CHAINE NOM2 DANS NOM1* !
C     ! *        *        *      *       *                        * !
C     ! * NC     *INTEGER *ENTREE* NON   * FIN DE LA POSITION     * !
C     ! *        *        *      *       * ABSOLUE DE LA CHAINE   * !
C     ! *        *        *      *       *  NOM2 DANS NOM1        * !
C     ! *********************************************************** !
C     !                                                             !
C     ===============================================================
C
C  --> DECLARATION DES ARGUMENTS
      CHARACTER*(*) NOM1,NOM2
      INTEGER IC,NC
C
C  --> DECLARATION INDICE DE BOUCLE
      INTEGER I
C
C  ---------- FIN DECLARATIONS _________
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DO 10 I=IC,NC
        NOM1(I:I)=' '
 10   CONTINUE
C
      WRITE(NOM1(IC:NC),'(A)') NOM2
      END
