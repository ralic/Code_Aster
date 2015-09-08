subroutine xhmini(nomte, nfh, ddld, ddlm, ddlp, nfiss)
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
    integer :: nfh, ddlm, ddlp
    integer :: nfiss
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: daniele.colombo at ifpen.fr
!
!          BUT : INITIALISER LES DIMENSIONS DES DDL DANS UN TE
!                POUR LES ELEMENTS HM-XFEM
!
!
! IN   NOMTE  : NOM DU TYPE ELEMENT
! OUT  NFH    : NOMBRE DE FONCTIONS HEAVISIDES
! OUT  DDLD   : NOMBRE DE DDL (DEPL) A CHAQUE NOEUD SOMMET
! OUT  DDLM   : NOMBRE DE DDL (DEPL) A CHAQUE NOEUD MILIEU
! OUT  DDLP   : NOMBRE DE DDL (PRES) A CHAQUE NOEUD SOMMET
! OUT  NFISS  : NOMBRE DE FISSURES
! OUT  NFFH   : NOMBRE DE DDL HEAVISIDE PAR NOEUD
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
    ddld = 0
    ddlp = 0
    nfiss = 1
!
    call teattr('S', 'XFEM', enr, ier, typel=nomte)
!
! --- DDL ENRICHISSEMENT : HEAVYSIDE
!
    if (enr(1:2) .eq. 'XH') then
        nfh = 1
        if (enr(1:3) .eq. 'XH2') nfh = 2
        if (enr(1:3) .eq. 'XH3') nfh = 3
        if (enr(1:3) .eq. 'XH4') nfh = 4
!       NOMBRE DE FISSURES
        call tecach('NOO', 'PLST', 'L', iret, nval=7,&
                    itab=jtab)
        nfiss = jtab(7)
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
! --- NOMBRE DE DDL AUX NOEUDS SOMMETS (HYDRAULIQUES)
!
    ddlp=1+nfh
end subroutine
