subroutine reeret(elrefp, nnop, geom, xg, ndim,&
                  deriv, xe, ff, dfdi)
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
!.......................................................................
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elrfdf.h"
#include "asterfort/elrfvf.h"
#include "asterfort/invjax.h"
#include "asterfort/iselli.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/matini.h"
#include "asterfort/reereg.h"
    character(len=3) :: deriv
    character(len=8) :: elrefp
    integer :: nnop, ndim, idepl
    real(kind=8) :: xg(ndim)
    real(kind=8) :: xe(ndim), ff(nnop), dfdi(nnop, ndim)
    real(kind=8) :: geom(*)
!
! ----------------------------------------------------------------------
!
! ELEMENTS XFEM THERMIQUES LINEAIRES:
!
! TROUVER LES COORDONNEES DANS L'ELEMENT DE REFERENCE D'UN
! POINT DONNE DANS L'ELEMENT REEL PAR LA METHODE NEWTON
! ET CALCUL DES FONCTIONS DE FORMES ET DE LEURS DERIVEES
!
! ----------------------------------------------------------------------
!
!
! IN  ELREFP : TYPE DE L'ELEMENT DE REF PARENT
! IN  NNOP   : NOMBRE DE NOEUDS DE L'ELT DE RÉF PARENT
!   L'ORDRE DES DDLS DOIT ETRE 'DC' 'H1' 'E1' 'E2' 'E3' 'E4' 'LAGC'
! IN  GEOM   : COORDONNEES DES NOEUDS
! IN  XG     : COORDONNES DU POINT DANS L'ELEMENT REEL
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  DERIV  : CALCUL DES QUANTITÉS CINÉMATIQUES
!               'NON' : ON S'ARRETE APRES LE CALCUL DES FF
!               'OUI' : ON CALCULE AUSSI LES DERIVEES DES FF
! OUT XE     : COORDONNÉES DU POINT DANS L'ÉLÉMENT DE RÉF PARENT
! OUT FF     : FONCTIONS DE FORMES EN XE
! OUT DFDI   : DÉRIVÉES DES FONCTIONS DE FORMES EN XE
!
    integer :: nbnomx
    parameter   (nbnomx = 27)
!
    real(kind=8) :: zero
    integer :: i, j, k, n, p, ig, cpt
    integer :: nno, nderiv, iret, nn
    real(kind=8) :: invjac(3, 3)
    real(kind=8) :: dff(3, nbnomx)
    real(kind=8) :: kron(3, 3), tmp, epstab(3, 3)
    logical :: ldec
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    ASSERT(iselli(elrefp))
    ASSERT(deriv.eq.'NON'.or.deriv.eq.'OUI')
    zero = 0.d0
!
! --- RECHERCHE DE XE PAR NEWTON-RAPHSON
!
    call reereg('S', elrefp, nnop, geom, xg,&
                ndim, xe, iret)
!
! --- VALEURS DES FONCTIONS DE FORME EN XE: FF
!
    call elrfvf(elrefp, xe, nbnomx, ff, nno)
!
! --- DERIVEES PREMIERES DES FONCTIONS DE FORME EN XE: DFF
!
    call elrfdf(elrefp, xe, ndim*nbnomx, dff, nno,&
                nderiv)
!
! --- CALCUL DE L'INVERSE DE LA JACOBIENNE EN XE: INVJAC
!
    call invjax('S', nno, ndim, nderiv, dff,&
                geom, invjac, iret)
!
    if (deriv .eq. 'NON') goto 9999
!
! --- DERIVEES DES FONCTIONS DE FORMES CLASSIQUES EN XE : DFDI
!
    call matini(nnop, ndim, zero, dfdi)
    do 310 i = 1, ndim
        do 300 n = 1, nno
            do 311 k = 1, ndim
                dfdi(n,i)= dfdi(n,i) + invjac(k,i)*dff(k,n)
311          continue
300      continue
310  end do
!
9999  continue
!
    call jedema()
!
end subroutine
