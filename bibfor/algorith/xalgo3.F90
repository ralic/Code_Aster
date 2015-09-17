subroutine xalgo3(ndim, elrefp, nnop, it, nnose, cnset, typma, ndime,&
                  geom, lsnelp, pmilie, ninter, ainter, ar, npts, nptm, &
                  pmmax, nmilie, mfis, lonref, pinref, pintt, pmitt, jonc)
    implicit none
!
#    include "jeveux.h"
#    include "asterfort/assert.h"
#    include "asterfort/xalg30.h"
#    include "asterfort/xalg31.h"
#    include "asterfort/xalg40.h"
#    include "asterfort/xalg41.h"
#    include "asterfort/xalg42.h"
#    include "asterfort/xalg20.h"
    character(len=8) :: typma, elrefp
    integer ::  ndim, ndime, nnop, it, nnose, cnset(*)
    integer ::  ninter, pmmax, npts, nptm, nmilie, mfis, ar(12, 3)
    real(kind=8) :: lonref, ainter(*), pmilie(*), lsnelp(27)
    real(kind=8) :: pinref(*), pintt(*), pmitt(*), geom(81)
    aster_logical :: jonc
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
!            BUT :  TROUVER LES PTS MILIEUX DANS L ELEMENT COUPE EN 3D
!
!     ENTREE
!       NDIM     : DIMENSION DE L ELEMENT
!       TYPMA    : TYPE DE MAILLE
!       TABCO    : COORDONNES DES NOEUDS DE LE ELEMENT PARENT
!       PINTER   : COORDONNES DES POINTS D INTERSECTION
!       PMILIE   : COORDONNES DES POINTS MILIEUX
!       NINTER   : NOMBRE DE POINTS D INTERSECTION
!       AINTER   : INFOS ARETE ASSOCIÉE AU POINTS D'INTERSECTION
!       AR       : CONNECTIVITE DU TETRA
!       PMMAX    : NOMBRE DE POINTS MILIEUX MAXIMAL DETECTABLE
!       NPTS     : NB DE PTS D'INTERSECTION COINCIDANT AVEC UN NOEUD SOMMET
!       LSNELP   : LSN AUX NOEUDS DE L'ELEMENT PARENT POUR LA FISSURE COURANTE
!       PINTT    : COORDONNEES REELLES DES POINTS D'INTERSECTION
!       PMITT    : COORDONNEES REELLES DES POINTS MILIEUX
!       JONC     : L'ELEMENT PARENT EST-IL TRAVERSE PAR PLUSIEURS FISSURES
!
!     SORTIE
!       NMILIE   : NOMBRE DE POINTS MILIEUX
!       PMILIE   : COORDONNES DES POINS MILIEUX
!     ----------------------------------------------------------------
!
!
    if (ndime .eq. 2) then
        call xalg20(ndim, elrefp, it, nnose, cnset, typma, ndime,&
                      geom, lsnelp, pmilie, ninter, ainter, ar, npts, nptm,&
                      pmmax, nmilie, mfis, lonref, pinref, pintt, pmitt, jonc)
    else if (ninter .eq. 3 .and. npts .eq. 0) then
         call xalg30(ndim, elrefp, it, nnose, cnset, typma, ndime,&
                      geom, lsnelp, pmilie, ninter, ainter, ar, npts, nptm,&
                      pmmax, nmilie, mfis, lonref, pinref, pintt, pmitt, jonc)
    else if (ninter .eq. 3 .and. npts .eq. 1) then
         call xalg31(ndim, elrefp, it, nnose, cnset, typma, ndime,&
                      geom, lsnelp, pmilie, ninter, ainter, ar, npts, nptm,&
                      pmmax, nmilie, mfis, lonref, pinref, pintt, pmitt, jonc)
    else if (ninter .eq. 4 .and. npts.eq. 0) then
         call xalg40(ndim, elrefp, nnop, it, nnose, cnset, typma, ndime,&
                      geom, lsnelp, pmilie, ninter, ainter, ar, npts, nptm,&
                      pmmax, nmilie, mfis, lonref, pinref, pintt, pmitt, jonc)
    else if (ninter .eq. 4 .and. npts.eq. 2) then
        call xalg42(ndim, elrefp, it, nnose, cnset, typma, ndime,&
                      geom, lsnelp, pmilie, ninter, ainter, ar, npts, nptm,&
                      pmmax, nmilie, mfis, lonref, pinref, pintt, pmitt, jonc)
    else if (ninter .eq. 4 .and. npts.eq. 1) then
        call xalg41(ndim, elrefp, nnop, it, nnose, cnset, typma, ndime,&
                      geom, lsnelp, pmilie, ninter, ainter, ar, npts, nptm,&
                      pmmax, nmilie, mfis, lonref, pinref, pintt, pmitt, jonc)
    else
        ASSERT(.false.)
    endif
!
end subroutine
