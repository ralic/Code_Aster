subroutine xhmini(nomte, nfh, ddld, ddlm)
!
    implicit none
!
#   include "asterfort/assert.h"
#   include "asterfort/elref1.h"
#   include "asterfort/elrefe_info.h"
#   include "asterfort/teattr.h"
#   include "asterfort/tecach.h"
#   include "jeveux.h"
!
    character(len=16) :: nomte
    integer :: nfh, ddlm
    integer :: nfiss
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!          BUT : INITIALISER LES DIMENSIONS DES DDL DANS UN TE
!                POUR LES ELEMENTS X-FEM
!
!
! IN   NOMTE  : NOM DU TYPE ELEMENT
! OUT  NFH    : NOMBRE DE FONCTIONS HEAVISIDES
! OUT  NNOM   : NB DE NOEUDS MILIEU
! OUT  DDLS   : NOMBRE DE DDL (DEPL) À CHAQUE NOEUD SOMMET
! OUT  NDDL   : NOMBRE DE DDL TOTAL DE L'ÉLÉMENT
! OUT  DDLM   : NOMBRE DE DDL A CHAQUE NOEUD MILIEU
! OUT  NFISS  : NOMBRE DE FISSURES
!     ------------------------------------------------------------------
!
    integer :: ndim, nnop, ier, nnops
    integer :: ddld, jtab(7), iret
    character(len=8) :: elrefp, enr
!
! ----------------------------------------------------------------------
!
    call elref1(elrefp)
    call elrefe_info(elrefe=elrefp, fami='RIGI', ndim=ndim, nno=nnop, nnos=nnops)
!
! --- INITIALISATIONS
!
    nfh = 0
    ddlm = 0
    nfiss = 1
!
    call teattr('S', 'XFEM', enr, ier, typel=nomte)
!
! --- DDL ENRICHISSEMENT : HEAVYSIDE
!
    if (enr(1:2) .eq. 'XH') then
        nfh = 1
!       NOMBRE DE FISSURES
        call tecach('NOO', 'PLST', 'L', iret, nval=7,&
                    itab=jtab)
        nfiss = jtab(7)
!       ON N'AUTORISE PAS LA MULTI-FISSURATION POUR HM-XFEM
        ASSERT(nfiss.eq.1)
    endif
!
! --- NOMBRE DE DDL AUX NOEUDS SOMMETS (MECANIQUES)
!
    ddld=ndim*(1+nfh)
!
! --- NOMBRE DE DDL AUX NOEUDS MILIEUX (MECANIQUES)
!
    ddlm=ddld
!
end subroutine
