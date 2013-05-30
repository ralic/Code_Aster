subroutine jeveut(nomlu, cel, jctab)
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
! ----------------------------------------------------------------------
! ALLOUE UN OBJET EM MEMOIRE DE FACON PERMANENTE (MARQUE = -1)
!
! IN  NOMLU  : NOM DE L'OBJET A ALLOUER
! IN  CEL    : TYPE D'ACCES 'E' OU 'L'
! OUT JCTAB  : ADRESSE DANS LE COMMUN DE REFERENCE ASSOCIE
! ----------------------------------------------------------------------
    implicit none
    include 'asterfort/jeveuo.h'
    integer :: jctab
    character(len=*) :: nomlu, cel
!     ------------------------------------------------------------------
    integer :: ipgc, kdesma(2), lgd, lgduti, kposma(2), lgp, lgputi
    common /iadmje/  ipgc,kdesma,   lgd,lgduti,kposma,   lgp,lgputi
! DEB ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ipgcex
!-----------------------------------------------------------------------
    ipgcex = ipgc
    ipgc = -1
    call jeveuo(nomlu, cel, jctab)
    ipgc = ipgcex
! FIN ------------------------------------------------------------------
end subroutine
