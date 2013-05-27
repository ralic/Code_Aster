subroutine lecelt(iunv, maxnod, nbtyma, indic, permut,&
                  codgra, node, nbnode)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     ================================================================
!A PRESUPER
!
!     =============================================================
!     !                                                           !
!     !  FONCTION: LECTURE DES NOEUDS DES MAILLES DE CODE GRAPH-  !
!     !            IQUE CODGRA.                                   !
!     !            !!!!!  PERMUTATION EVENTUELLE !!!!!            !
!     !  (CF. DOCUMENT INTERFACE SUPERTAB-ASTER )  !
!     =============================================================
!     !                                                           !
!     !  ROUTINE APPELE: IUNIFI (FONCTION)                        !
!     !                                                           !
!     !  ROUTINE APPELANTE: SLEELT                                !
!     !                                                           !
!     =============================================================
!     !                                                           !
!     !                 ***************                           !
!     !                 *  ARGUMENTS  *                           !
!     !                 ***************                           !
!     !                                                           !
!     !  ******************************************************** !
!     !  *   NOM     * TYPE  * MODE *ALTERE *     ROLE          * !
!     !  ******************************************************** !
!     !  *           *       *      *       *                   * !
!     !  * MAXNOD    *INTEGER*ENTREE* NON   *NBRE MAXI DE NOEUDS* !
!     !  *           *       *      *       *POUR UNE MAILLE    * !
!     !  *           *       *      *       *                   * !
!     !  * NBTYMA    *INTEGER*ENTREE* NON   *NBRE DE TYPES DE   * !
!     !  *           *       *      *       * MAILLES SUPERTAB  * !
!     !  *           *       *      *       *                   * !
!     !  * INDIC     *INTEGER*ENTREE* NON   * INDIQUE S'IL FAUT * !
!     !  *           *       *      *       *FAIRE 1 PERMUTATION* !
!     !  *           *       *      *       *                   * !
!     !  * PERMUT    *INTEGER*ENTREE* NON   * TABLEAU DE PERMU- * !
!     !  *           *       *      *       *  TATION           * !
!     !  *           *       *      *       *                   * !
!     !  * CODGRA    *INTEGER*ENTREE* NON   * CODE GRAPHIQUE DE * !
!     !  *           *       *      *       * LA MAILLE A LIRE  * !
!     !  *           *       *      *       *                   * !
!     !  * NBNODE    *INTEGER*ENTREE* NON   * NBRE DE NOEUDS DE * !
!     !  *           *       *      *       *  CETTE MAILLE     * !
!     !  *           *       *      *       *                   * !
!     !  * NODE      *INTEGER*SORTIE* NON   * TABLEAU DES NOEUDS* !
!     !  *           *       *      *       *(PARFOIS PERMUTES) * !
!     !  *           *       *      *       * DE LA MAILLE      * !
!     !  *           *       *      *       *                   * !
!     !  ******************************************************** !
!     !                                                           !
!     !   VARIABLES LOCALES:  NODLU(2)--> TABLEAU DE TRAVAIL      !
!     !   ------------------                                      !
!     =============================================================
!
!  --> DECLARATION DES ARGUMENTS
    include 'asterfort/iunifi.h'
    integer :: maxnod, nbtyma
    integer :: codgra, node(maxnod), nbnode
    integer :: permut(maxnod, nbtyma), indic(nbtyma)
!  --> DECLARATION DES VARIABLES LOCALES
    integer :: nodlu(32), ii
!  --> DECLARATION DES INDICES DE BOUCLES
    integer :: i
    integer :: imes, iunv
!-----------------------------------------------------------------------
!
    read (iunv,'(8I10)') (nodlu(i),i=1,nbnode)
!
    if (indic(codgra) .eq. -1) then
        imes = iunifi('MESSAGE')
        write (imes,*) 'MAILLE DE TYPE ',codgra,' NON TRAITE'
    else if (indic(codgra).eq.0) then
!
        do 1 i = 1, nbnode
            node(i) = nodlu(i)
 1      continue
!
    else if (indic(codgra).eq.1) then
!
        do 2 i = 1, nbnode
            ii = permut(i,codgra)
            node(ii) = nodlu(i)
 2      continue
!
    endif
!
end subroutine
