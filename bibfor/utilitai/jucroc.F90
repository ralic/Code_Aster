subroutine jucroc(nomc, nooc, nuoc, dim, ldec)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jecroc.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    character(len=*) :: nomc, nooc
    integer :: nuoc, dim, ldec
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     CREATION D'UN OBJET DE COLLECTION
!     ------------------------------------------------------------------
! IN  NOMC  : CH*24 : NOM (COMPLET)  DE LA COLLECTION
! IN  NOOC  : CH*8  : NOM  DE L'OBJET (SI NUM <=0)
! IN  NUOC  : IS    : OU NUM  DE L'OBJET (>0)
! IN  DIM   : IS    : TAILLE DE L'OBJET
! OUT LDEC  : IS    : DECALAGE
!     ------------------------------------------------------------------
    character(len=4) :: cbid
    character(len=32) :: nom
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if (nuoc .gt. 0) then
        nom = jexnum(nomc,nuoc)
    else
        nom = jexnom(nomc,nooc)
    endif
    call jecroc(nom)
    call jeecra(nom, 'LONMAX', dim, cbid)
    call jeecra(nom, 'LONUTI', dim, cbid)
    call jeveuo(nom, 'E', ldec)
end subroutine
