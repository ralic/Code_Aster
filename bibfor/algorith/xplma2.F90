subroutine xplma2(ndim, nne, nnes, ndls, n,&
                  nfhe, pl)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    implicit none
    include 'asterfort/assert.h'
    integer :: ndim, nne, nnes, n, pl, ndls, nfhe
!
! ----------------------------------------------------------------------
!
!    CADRE :  CALCULE LA PLACE DU LAMBDA(N) NORMAL DANS LA MATRICE
!             ET LE SECOND MEMBRE DE CONTACT
!
!----------------------------------------------------------------------
! ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
! ----------------------------------------------------------------------
!
! IN  NDIM    : DIMENSION
! IN  DNE     : NOMBRE TOTAL DE NOEUDS ESCLAVES
! IN  NNES    : NOMBRE DE NOEUDS ESCLAVES SOMMETS
! IN  NDLS    : NOMBRE DE DDLS D'UN NOEUD SOMMET ESCLAVE
! IN  N       : NUMÉRO DU NOEUD PORTANT LE LAMBDA
! IN  NFHE    : NOMBRE DE DDL HEAVISIDE ESCLAVES
!
! OUT PL      : PLACE DU LAMBDA DANS LA MATRICE
!
! ----------------------------------------------------------------------
!
    call assert(n.le.nne)
!
    if (n .le. nnes) then
        pl= ndls*n - ndim*max(1,nfhe) + 1
    else
        pl=nnes*3*ndim+ndim*(n-nnes-1)+1
    endif
!
end subroutine
