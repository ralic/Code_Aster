subroutine mmmvex(nnl, nbcps, ndexfr, vectff)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterfort/isdeco.h"
    integer :: ndexfr
    integer :: nnl, nbcps
    real(kind=8) :: vectff(18)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CALCUL)
!
! CALCUL DES VECTEURS - MODIFICATIONS EXCLUSION
!
! ----------------------------------------------------------------------
!
!
! IN  NNL    : NOMBRE DE NOEUDS PORTANT UN LAGRANGE DE CONTACT/FROTT
! IN  NBCPS  : NOMBRE DE COMPOSANTES/NOEUD DES LAGR_C+LAGR_F
! IN  NDEXFR : ENTIER CODE POUR EXCLUSION DIRECTION DE FROTTEMENT
! OUT VECTFF : VECTEUR ELEMENTAIRE LAGR_F
!
! ----------------------------------------------------------------------
!
    integer :: i, l, ii, nbcpf
    integer :: ndexcl(10)
!
! ----------------------------------------------------------------------
!
    nbcpf = nbcps - 1
!
! --- MODIFICATION DES TERMES SI EXCLUSION DIRECTION FROTT. SANS_NO_FR
!
    if (ndexfr .ne. 0) then
        call isdeco(ndexfr, ndexcl, 10)
        do 30 i = 1, nnl
            if (ndexcl(i) .eq. 1) then
                do 40 l = 1, nbcpf
                    if ((l.eq.2) .and. (ndexcl(10).eq.0)) then
                        goto 40
                    endif
                    ii = (i-1)*nbcpf+l
                    vectff(ii) = 0.d0
40              continue
            endif
30      continue
    endif
!
end subroutine
