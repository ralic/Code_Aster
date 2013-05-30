subroutine xerfis(ndime, ninter, npts, nptm)
    implicit none
!
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/u2mess.h'
    integer :: ndime, ninter, npts, nptm
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!                 AFFICHER DES MESSAGES D'ERREUR LORSQUE LES
!                CONFIGURATIONS DE FISSURE SUR L'ELEMENT SONT :
!       (1) DOUTEUSES (AURAIT DU ETRE RETIRE)/ IMPROBABLES (FAUX)
!       (2) INTERDITES
!     ENTREE
!       NDIME   : DIMENSION DE L'ELEMENT FINI
!       NINTER  : NOMBRE DE POINTS D'INTERSECTION
!       NPTS    : NB DE POINTS INTERSEPTES EN UN DES 3 NOEUDS SOMMETS
!       NPTM    : NB DE POINT INTERSEPTES EN UN DES 3 NOEUDS MILIEUX
!......................................................................
!
    call jemarq()
!
! --- POUR LES TRIA6
!
    if (ndime .eq. 2) then
!
! PLUTOT QUE DE VERIFIER LES SEULES CONFIGURATIONS QUE L'ON RETIENT
! ON EXCLUT CELLES QUE L'ON NE VEUT PAS POUR AVOIR UNE MEILLEURE
! VISIBILITE DES DIFFERENTES CONFIGURATIONS QUI SE PRESENTENT
!
!       NBRE DE POINT D'INTERSECTION INCORRECT (1) OU (2)
        if (ninter .le. 1) then
            call u2mess('F', 'XFEM_64')
!
!       NBRE DE POINT D'INTERSECTION INCORRECT (1) OU (2)
        else if (ninter.gt.3) then
            call u2mess('F', 'XFEM_64')
!
!       NBRE PT INTER SOMMET > NBRE PT INTER TOTAL (1)
        else if (npts.gt.ninter) then
            call u2mess('F', 'XFEM_64')
!
!       LA FISSURE INTERCEPTE DEUX NOEUDS SOMMETS UNIQUEMENT (1) OU (2)
        else if (ninter.eq.2 .and. npts.eq.2) then
            call u2mess('F', 'XFEM_64')
!
!       LA FISSURE INTERCEPTE LES 3 ARETES STRICTEMENT (2)
        else if (ninter.eq.3 .and. npts.eq.0) then
            call u2mess('F', 'XFEM_64')
!
!       (2)
        else if (ninter.eq.3 .and. npts.eq.1 .and. nptm.ne.1) then
            call u2mess('F', 'XFEM_64')
!
!       LA FISSURE JOUXTE UN BORD DE L'ELEMENT (1)
        else if (ninter.eq.3 .and. npts.eq.2 .and. nptm.eq.1) then
            call u2mess('F', 'XFEM_64')
!
!       (1) OU (2)
        else if (ninter.eq.3 .and. npts.eq.2 .and. nptm.ne.1) then
            call u2mess('F', 'XFEM_64')
!
!       (1) OU (2)
        else if (ninter.eq.3 .and. npts.eq.3) then
            call u2mess('F', 'XFEM_64')
!
        endif
!
! --- POUR LES SEG3
!
    else if (ndime.eq.1) then
!
!       NBRE DE POINT D'INTERSECTION INCORRECT (1) OU (2)
        if (ninter .ne. 1 .and. npts .ne. 0) then
            call u2mess('F', 'XFEM_64')
        endif
!
    endif
!
    call jedema()
end subroutine
