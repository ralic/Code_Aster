subroutine xnbddl(ndim, nfh, nfe, ddlc, ddld, ddls, singu)
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
! BUT : CALCULER L INDICE DANS LES MATRICES ELEMENTAIRES
!          POUR LES DDLS D ENRICHISSEMENT VECTORIEL
!-----------------------------------------------------------------------
!
! ARGUMENTS :
!------------
!-----------------------------------------------------------------------
    implicit none
!-----------------------------------------------------------------------
    integer :: ndim, nfh, nfe, ddlc, ddld, ddls
    integer, optional :: singu
!-----------------------------------------------------------------------
!
!   NOMBRE DE DDL DE DEPLACEMENT À CHAQUE NOEUD SOMMET
    ddld = ndim*(1+nfh+nfe)
!   NOMBRE DE DDL TOTAL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
    ddls = ddld+ddlc
    if (present(singu)) then
      singu=min(1,nfe)
      ddld = ndim*(1+nfh+ndim*singu)    
    endif
!
end subroutine
