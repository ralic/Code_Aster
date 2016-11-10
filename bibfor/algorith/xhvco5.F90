subroutine xhvco5(ndim, nnop, nnops, pla, nd,&
                  tau1, tau2, mu, nddls, jac,&
                  ffc, ffp, nddlm, wsaut,&
                  saut, vect, ifiss, nfiss, nfh,&
                  ifa, jheafa, ncomph,&
                  jheavn, ncompn, pf)

!
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/hmdeca.h"
#include "asterfort/matini.h"
#include "asterfort/prmave.h"
#include "asterfort/transp.h"
#include "asterfort/vecini.h"
#include "asterfort/xcalc_code.h"
#include "asterfort/xcalc_saut.h"
    integer :: ndim, nnop, nnops, nddlm
    integer :: nddls, pla(27), ifiss, nfiss, nfh, ifa, ncompn
    integer :: jheafa, ncomph, jheavn
    real(kind=8) :: vect(560)
    real(kind=8) :: ffp(27), jac, pf
    real(kind=8) :: mu(3), wsaut(3), saut(3)
    real(kind=8) :: ffc(16)
    real(kind=8) :: nd(3), tau1(3), tau2(3)
!
! ======================================================================
! person_in_charge: daniele.colombo at ifpen.fr
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! ROUTINE CONTACT (METHODE XFEM HPP - CALCUL ELEM.)
!
! --- CALCUL DES SECONDS MEMBRES DE COHESION, LOI CZM_LIN_MIX
! --- PARTIE INDEPENDANTE DE LA LOI D'INTERFACE
!
! ----------------------------------------------------------------------
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  NNO    : NOMBRE DE NOEUDS DE L'ELEMENT DE REF PARENT
! IN  NNOL   : NOMBRE DE NOEUDS LAGRANGE
! IN  PLA    : PLACE DES INCONNUS DE CONTACT DANS LA NUMEROTATION
! IN  ND     : DIRECTION NORMALE
! IN  TAU1   : DIRECTION TANGENTE 1
! IN  TAU2   : DIRECTION TANGENTE 2
! IN  MU     : VECTEUR DE MEME NOM (FORCES D'INTERFACE MOYENNES)
! IN  DDLS   : NOMBRE DE DDLS DES NOEUDS SOMMET
! IN  JAC    : PRODUIT DU JACOBIEN ET DU POIDS
! IN  FFC    : FONCTIONS DE FORME DE CONTACT
! IN  FFP    : FONCTIONS DE FORME DE L'ELEMENT PARENT
! IN  NNOS   : NOMBRE DE NOEUDS SOMMET 
! IN  DDLM   : NOMBRE DE DDLS DES NOEUDS MILIEU
! IN  SAUT   : SAUT DE DEPLACEMENT (BASE FIXE)
! IN  WSAUT  : SAUT DE DEPLACEMENT MOYEN W (BASE LOCAL)
! I/O VTMP   : VECTEUR ELEMENTAIRE DE COHESION
!
    integer :: i, j, pli, ier, hea_fa(2), in, dec, ifh
    real(kind=8) :: p(3, 3), ptr(3, 3), mug(3), am(3), coefi, ffi
    aster_logical :: lmultc
!
! ---------------------------------------------------------------------
! on va commencer par construire une matrice de passage
    call matini(3, 3, 0.d0, p)
    call matini(3, 3, 0.d0, ptr)
    call vecini(3, 0.d0, mug)
    call vecini(3, 0.d0, am)
    lmultc = nfiss.gt.1
    if (.not.lmultc) then
      hea_fa(1)=xcalc_code(1,he_inte=[-1])
      hea_fa(2)=xcalc_code(1,he_inte=[+1])
    else
      hea_fa(1) = zi(jheafa-1+ncomph*(ifiss-1)+2*(ifa-1)+1)
      hea_fa(2) = zi(jheafa-1+ncomph*(ifiss-1)+2*(ifa-1)+2)
    endif
!
    do 17 i = 1, ndim
        p(1,i) = nd(i)
17  continue
    do 18 i = 1, ndim
        p(2,i) = tau1(i)
18  continue
    if (ndim .eq. 3) then
        do 19 i = 1, ndim
            p(3,i) = tau2(i)
19      continue
    endif
! calcul du saut de deplacement am en base locale
    call prmave(0, p, 3, ndim, ndim,&
                saut, ndim, am, ndim, ier)
! calcul de la contrainte MUG en base globale
    call transp(p, 3, ndim, ndim, ptr,&
                3)
    call prmave(0, ptr, 3, ndim, ndim,&
                mu, ndim, mug, ndim, ier)
! on reecrit tout
! remplissage L1mu
    do i = 1, nnop
        call hmdeca(i, nddls, nddlm, nnops, in, dec)
!
        do ifh = 1, nfh
           coefi = xcalc_saut(zi(jheavn-1+ncompn*(i-1)+ifh),&
                              hea_fa(1), &
                              hea_fa(2),&
                              zi(jheavn-1+ncompn*(i-1)+ncompn))

           do j = 1, ndim
               vect(in+(ndim+dec)*ifh+j) = vect(in+(ndim+dec)*ifh+j)+ coefi*mug(j)*ffp(i)*jac
           end do
        end do
    end do
! remplissage L1u
    do i = 1, nnops
        pli=pla(i)
        ffi = ffc(i)
        do j = 1, ndim
            vect(pli+3+2*ndim-1+j) = vect(pli+3+2*ndim-1+j) - am(j)*ffi*jac
        end do
    end do
! remplissage L1w
    do i = 1, nnops
        pli=pla(i)
        ffi = ffc(i)
        do j = 1, ndim
            vect(pli+3+2*ndim-1+j) = vect(pli+3+2*ndim-1+j) - wsaut(j)*ffi*jac
        end do
    end do
! remplissage L2mu
    do i = 1, nnops
        pli = pla(i)
        ffi = ffc(i)
        do j = 1, ndim
            vect(pli-1+3+ndim+j) = vect(pli-1+3+ndim+j) - mu(j)*ffi* jac
        end do
        vect(pli-1+3+ndim+1) = vect(pli-1+3+ndim+1) - pf*ffi* jac
    end do
!
end subroutine
