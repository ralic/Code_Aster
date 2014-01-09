subroutine bsthpl(nomte, bsigth, indith)
    implicit none
#include "jeveux.h"
#include "asterfort/dxbsig.h"
#include "asterfort/dxefgt.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/jevech.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
    real(kind=8) :: bsigth(24)
    logical :: indith
    character(len=16) :: nomte
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
!
!      CALCUL DU BSIGMA POUR LES CONTRAINTES THERMIQUES
!      (I.E. BT*D*ALPHA(T-TREF)) POUR LES ELEMENTS
!                                DE PLAQUE (DKT,DKQ,DST,DSQ,Q4G)
!     ------------------------------------------------------------------
!     IN  NOMTE  : NOM DU TYPE D'ELEMENT
!     OUT BSIGTH : BT*SIGMA POUR LES CONTRAINTES THERMIQUES
!     OUT INDITH : LOGICAL = .TRUE.  YA DES DEFORMATIONS THERMIQUES
!                          = .FALSE. SINON
!     ------------------------------------------------------------------
    integer :: i, jgeom, nno, iret
    real(kind=8) :: pgl(3, 3), xyzl(3, 4), sigth(32), zero
!     ------------------------------------------------------------------
!
! --- INITIALISATIONS :
!     ---------------
    zero = 0.0d0
    indith = .false.
!
    do 10 i = 1, 24
        bsigth(i) = zero
10  end do
!
!
! --- RECUPERATION DES COORDONNEES DES NOEUDS DE L'ELEMENT :
!     ----------------------------------------------------
    call jevech('PGEOMER', 'L', jgeom)
!
    if (nomte .eq. 'MEDKTR3' .or. nomte .eq. 'MEDSTR3' .or. nomte .eq. 'MEDKTG3' .or.&
        nomte .eq. 'MET3TR3' .or. nomte .eq. 'MET3GG3') then
        nno = 3
        call dxtpgl(zr(jgeom), pgl)
        else if (nomte.eq.'MEDKQU4' .or.&
     &         nomte.eq.'MEDKQG4' .or.&
     &         nomte.eq.'MEDSQU4' .or.&
     &         nomte.eq.'MEQ4QU4' .or.&
     &         nomte.eq.'MEQ4GG4' ) then
        nno = 4
        call dxqpgl(zr(jgeom), pgl, 'S', iret)
    else
        call utmess('F', 'ELEMENTS_14', sk=nomte)
    endif
!
! --- DETERMINATION DES COORDONNEES LOCALES XYZL DES NOEUDS
! --- DE L'ELEMENT :
!     ------------
    call utpvgl(nno, 3, pgl, zr(jgeom), xyzl)
!
! --- CALCUL DES EFFORTS GENERALISES D'ORIGNIE THERMIQUE AUX POINTS
! --- D'INTEGRATION :
!     -------------
    call dxefgt(pgl, sigth)
!
! --- CALCUL DE BT*SIGTH :
!     ------------------
    call dxbsig(nomte, xyzl, pgl, sigth, bsigth,'FORC_NODA')
!
!
end subroutine
