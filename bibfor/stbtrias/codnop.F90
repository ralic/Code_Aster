subroutine codnop(nom1, nom2, ic, nc)
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!     ===================================
!
!     ===============================================================
!     !                                                             !
!     !  AUTEUR:J.F.LAMAUDIERE                         DATE:1/12/89 !
!     !                                                             !
!     !  LOGISCOPE STATIQUE :09/02/90                               !
!     !                                                             !
!     !  LOGISCOPE DYNAMIQUE (35 JEUX TESTS) :21/02/90              !
!     !                                                             !
!     ===============================================================
!     !                                                             !
!     !  FONCTION: CETTE ROUTINE PERMET L'ECRITURE D'UNE CHAINE !
!     !            DE CARACTERES DANS UNE AUTRE.                    !
!     !                                                             !
!     ===============================================================
!     !                                                             !
!     !  ROUTINE APPELANTES : ECRNEU                                !
!     !                           : SLECOR                          !
!     !                           : ECRELT                          !
!     !                           : SLEGRO                          !
!     !                           : SLEGEO                          !
!     !                           : ECFACH                          !
!     !                                                             !
!     ==============================================================!
!     !                                                             !
!     !                    ***************                          !
!     !                    *  ARGUMENTS  *                          !
!     !                    ***************                          !
!     !                                                             !
!     ! *********************************************************** !
!     ! *   NOM  *  TYPE  * MODE *ALTERE *        ROLE            * !
!     ! *********************************************************** !
!     ! * NOM1   *CHARACTE*SORTIE* OUI   *  CHAINE DE CARACTERE   * !
!     ! *        *        *      *       * RESULTAT DE LA CONCATE-* !
!     ! *        *        *      *       *   NATION               * !
!     ! *        *        *      *       *                        * !
!     ! * NOM2   *CHARACTE*ENTREE* NON   * CHAINE DE CARACTERES A * !
!     ! *        *        *      *       *   CONCATENER           * !
!     ! *        *        *      *       *                        * !
!     ! * IC     *INTEGER *ENTREE* NON   * DEBUT DE LA POSITION DE* !
!     ! *        *        *      *       *LA CHAINE NOM2 DANS NOM1* !
!     ! *        *        *      *       *                        * !
!     ! * NC     *INTEGER *ENTREE* NON   * FIN DE LA POSITION     * !
!     ! *        *        *      *       * ABSOLUE DE LA CHAINE   * !
!     ! *        *        *      *       *  NOM2 DANS NOM1        * !
!     ! *********************************************************** !
!     !                                                             !
!     ===============================================================
!
!  --> DECLARATION DES ARGUMENTS
    character(len=*) :: nom1, nom2
    integer :: ic, nc
!
!  --> DECLARATION INDICE DE BOUCLE
    integer :: i
!
!  ---------- FIN DECLARATIONS _________
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    do 10 i = ic, nc
        nom1(i:i)=' '
10  end do
!
    write(nom1(ic:nc),'(A)') nom2
end subroutine
