subroutine calflu(chamno, modele, mate, nu, vectas,&
                  nd, nr, nv, opt)
    implicit none
#include "asterfort/assvec.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/vectfl.h"
    integer :: nr, nd, nv
    character(len=*) :: chamno, modele, mate, nu, vectas, opt
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
!
!------- CALCUL DES VECTEURS ASSEMBLES DE FLUX FLUIDES
!
!----------------------------------------------------------------------
!  IN  : K19 : CHAMNO : CHAMP AUX NOEUDS DE DEPL_R
!  IN  : K1  : OPT    : CARACTERE RELATIF A L OPTION CHOISIE
!  IN  : K8  : MODELE : MODELE FLUIDE
!  IN  : K24 : MATE   : MATERIAU THERMIQUE (PRIS POUR LE FLUIDE)
!  IN  : K14 : NU     : NUMEROTATION DES DDLS FLUIDES
!  OUT : K19 : VECTAS : CHAMNO DE FLUX FLUIDE
!  OUT : I   : ND,NR,NV : LONGUEURS DES .DESC, .REFE, .VALE
!------------------------------------------------------------------
    real(kind=8) :: r8bid
    character(len=24) :: ve
!------------------------------------------------------------------
!
!     --- CALCUL DU VECTEUR ELEMENTAIRE DE FLUX ---
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    ve ='&&CALFLU.VE'
    call vectfl(opt, modele, ' ', mate, chamno(1:19),&
                r8bid, ve)
!
!     --- ASSEMBLAGE DU VECTEUR ELEMENTAIRE DE FLUX SUR LA
!                       NUMEROTATION NU DU MODELE THERMIQUE ---
!
    call assvec('V', vectas, 1, ve(1:19), 1.0d0,&
                nu, ' ', 'ZERO', 1)
    call jedetr(ve)
!
    call jelira(vectas(1:19)//'.DESC', 'LONMAX', nd)
    call jelira(vectas(1:19)//'.REFE', 'LONMAX', nr)
    call jelira(vectas(1:19)//'.VALE', 'LONMAX', nv)
!
end subroutine
