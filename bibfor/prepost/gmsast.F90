subroutine gmsast(nfie, nfis)
! aslint: disable=
    implicit none
    include 'asterfort/pregms.h'
    include 'asterfort/ulisop.h'
    include 'asterfort/ulopen.h'
    integer :: nfie, nfis
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!      GMSAST --   INTERFACE GMSH-->ASTER
!                  OUVERTURE ET LECTURE DU FICHIER GMSH
!                  ECRITURE DU FICHIER MAILLAGE ASTER
!
!   ARGUMENT        E/S  TYPE         ROLE
!    NFIE           IN    I         UNITE LOGIQUE DU FICHIER GMSH
!    NFIE           IN    I         UNITE LOGIQUE DU FICHIER GMSH
!
! ......................................................................
!
    character(len=16) :: k16nom
!
! ----------------------------------------------------------------------
!
    k16nom='                '
    if (ulisop ( nfie, k16nom ) .eq. 0) then
        call ulopen(nfie, ' ', 'GMSH', 'OLD', 'O')
    endif
    if (ulisop ( nfis, k16nom ) .eq. 0) then
        call ulopen(nfis, ' ', 'FICHIER-MODELE', 'NEW', 'O')
    endif
!
    call pregms(nfie, nfis)
!
    write(nfis,*) 'FIN'
    rewind nfis
!
    if (ulisop ( nfie, k16nom ) .ne. 0) then
        call ulopen(-nfie, ' ', 'GMSH', 'NEW', 'O')
    endif
end subroutine
