subroutine lect82(iunv, node, nbnode, inum)
! TOLE CRS_1404
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
    implicit none
!     ================================================================
!A PRESUPER
!
!     =============================================================
!     !                                                           !
!     !  FONCTION: LECTURE DES NOEUDS DES TRACELINES IDEAS .      !
!     !            !!!!!   (DATASET 82 !!!!!                      !
!     !       (CF. DOCUMENT INTERFACE SUPERTAB-ASTER )            !
!     =============================================================
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
!     !  * NBNODE    *INTEGER*ENTREE* NON   * NBRE DE NOEUDS DE * !
!     !  *           *       *      *       *  CETTE MAILLE     * !
!     !  *           *       *      *       *                   * !
!     !  * NODE      *INTEGER*SORTIE* NON   * TABLEAU DES NOEUDS* !
!     !  *           *       *      *       *(PARFOIS PERMUTES) * !
!     !  *           *       *      *       * DE LA MAILLE      * !
!     !  *           *       *      *       *                   * !
!     !  * INUM      *INTEGER*SORTIE* NON   * NOMBRE MAX DE     * !
!     !  *           *       *      *       * NOEUDS LUS        * !
!     !  *           *       *      *       *                   * !
!     !  ******************************************************** !
!     !                                                           !
!     !   VARIABLES LOCALES:  NODLU(2)--> TABLEAU DE TRAVAIL      !
!     !   ------------------                                      !
!     =============================================================
!
!
!  --> DECLARATION DES ARGUMENTS
!     ============================================================
!
    integer :: node(*), nbnode, inum, idro, iunv
!  --> DECLARATION DES VARIABLES LOCALES
    integer :: nodlu(nbnode)
!  --> DECLARATION DES INDICES DE BOUCLES
    integer :: i, j
!
!     ---------- FIN DECLARATIONS -----------
!
    read (iunv,'(8I10)') (nodlu(j),j=1,nbnode)
    idro=1
    inum=1
    do 669 i = 1, nbnode
        if ((nodlu(i).ne.0) .and. (i.lt.nbnode)) then
            node(idro)=nodlu(i)
            node(idro+1)=nodlu(i+1)
            idro=idro+2
            inum = inum + 1
! cas ou on commence par un 0
        else if ((nodlu(i).eq.0).and.(i.eq.1)) then
            idro=idro
            inum = inum
        else if ((nodlu(i).eq.0).and.(i.lt.nbnode)) then
            idro=idro-2
            inum = inum - 1
        else if ((nodlu(i).eq.0).and.(i.ge.nbnode)) then
            idro=idro-2
            inum = inum - 1
            goto 669
        endif
669  end do
end subroutine
