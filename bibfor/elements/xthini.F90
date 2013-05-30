subroutine xthini(nomte, nfh, nfe)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: sam.cuvilliez at edf.fr
!
    implicit none
!
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/elref4.h'
    include 'asterfort/teattr.h'
    include 'asterfort/tecach.h'
    character(len=16) :: nomte
    integer :: nfh, nfe, nddl
!
!
!          BUT : INITIALISER LES DIMENSIONS DES DDL DANS UN TE
!                POUR LES ELEMENTS X-FEM EN THERMIQUE
!
!
! IN   NOMTE  : NOM DU TYPE ELEMENT
! OUT  NFH    : NOMBRE DE FONCTIONS HEAVISIDES
! OUT  NFE    : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT
! OUT  NDDL   : NOMBRE DE DDL TOTAL DE L'ÉLÉMENT
!     ------------------------------------------------------------------
!
    integer :: nno, ibid, ier, nnos, nfiss
    integer :: ddld, iadzi, jtab(7), iret
    character(len=8) :: enr
!
! ----------------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ibid, nno, nnos,&
                ibid, ibid, ibid, ibid, ibid)
!
! --- INITIALISATIONS
!
    nfh = 0
    nfe = 0
!
    call teattr(nomte, 'S', 'XFEM', enr, ier)
!
! --- DDL ENRICHISSEMENT : HEAVYSIDE, ENRICHIS (FOND)
!
    if (enr(1:2) .eq. 'XH') then
        nfh = 1
!       NOMBRE DE FISSURES :
        call tecach('NOO', 'PLST', 'L', 7, jtab,&
                    iret)
        nfiss = jtab(7)
!       ON NE TRAITE PAS LA JONCTION DE FISSURES EN THERMMIQUE
        call assert(nfiss.eq.1)
    endif
!
    if (enr(1:2) .eq. 'XT' .or. enr(3:3) .eq. 'T') then
        nfe = 1
    endif
!
    call assert(((nfh.eq.1).and.(nfe.eq.0)) .or. ((nfh.eq.0).and.(nfe.eq.1)) .or.&
                ((nfh.eq.1).and.(nfe.eq.1)))
!
end subroutine
