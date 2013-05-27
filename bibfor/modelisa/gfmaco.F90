subroutine gfmaco(noma, nbnoeu, nbno, icoova, axep)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!-----------------------------------------------------------------------
!
    implicit none
!     IN
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    integer :: nbnoeu, nbno, icoova
    character(len=8) :: noma
    real(kind=8) :: axep(2)
!     ------------------------------------------------------------------
!     NOMA   : NOM DU MAILLAGE GLOBAL DE LA SD G_FIBRE
!     NBNOEU : NB DE NOEUDS DEJA ENTRES DANS .COORDO  .VALE
!     NBNO   : NB DE NOEUDS DU GROUPE DE FIBRES CONCERNE
!     ICOVA  : ADRESSE DU .COORDO .VALE DU MAILLAGE DU GROUPE DE FIBRES
!     AXEP   : COOR_AXE_POUTRE DU GROUPE DE FIBRES
!     ------------------------------------------------------------------
!     COPIE DES COORDONNEES DES NOEUDS DU MAILLAGE DU GROUPE DE FIBRES
!     (MOT CLE SECTION) DANS LE MAILLAGE GLOBAL DE SECTION
!     (DEFI_GEOM_FIBRE)
!
!
!
! ----- DECLARATIONS
!
    integer :: icoorv, i
    character(len=24) :: cooval
!
    call jemarq()
!
    cooval = noma// '.COORDO    .VALE'
!
! --- REMPLISSAGE DE L'OBJET .COORDO    .VALE :
!     -------------------------
    call jeveuo(cooval, 'E', icoorv)
!
    do 20 i = 1, nbno
        zr(icoorv+(nbnoeu+i-1)*3-1+1)=zr(icoova+3*i-1+1)-axep(1)
        zr(icoorv+(nbnoeu+i-1)*3-1+2)=zr(icoova+3*i-1+2)-axep(2)
20  end do
!
    call jedema()
end subroutine
