subroutine xplmat(ndim, nfh, nfe, ddlc, ddlm,&
                  nnos, nnom, n, pl)
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
    integer :: ndim, nfh, nfe, ddlc, nnos, nnom, n, pl, ddlm
!
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
! person_in_charge: samuel.geniaut at edf.fr
!
!    CADRE : X-FEM ET CONTACT CONTINU
!             CALCULE LA PLACE DU LAMBDA(N) NORMAL DANS LA MATRICE
!             DE RAIDEUR DUE AU CONTACT
!
! IN  NDIM    : DIMENSION (=3)
! IN  NFH     : NOMBRE DE FONCTIONS HEAVYSIDE
! IN  NFE     : NOMBRE DE FONCTIONS SINGULIÈRES
! IN  DDLC    : NOMBRE DE DDL DE CONTACT (PAR NOEUD)
! IN  NNO     : NOMBRE DE NOEUDS SOMMET
! IN  NNOM    : NOMBRE DE NOEUDS MILIEU
! IN  N       : NUMÉRO DU NOEUD PORTANT LE LAMBDA
!
! OUT PL      : PLACE DU LMBDA DANS LA MATRICE
!     ------------------------------------------------------------------
!
    integer :: ddls
!
! ----------------------------------------------------------------------
    call assert(n.le.(nnos+nnom))
!
!     NOMBRE DE DDL PAR NOEUD SOMMET
    ddls=ndim*(1+nfh+nfe)+ddlc
!
!     PLACE DU PREMIER DDL DE CONTACT POUR CHAQUE N
    if (n .le. nnos) then
        pl=ddls*n-ddlc+1
    else
        pl=ddls*nnos+ddlm*(n-nnos)-ddlc+1
    endif
!
end subroutine
