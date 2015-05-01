subroutine hujayd(nmat, mater, nvi, vind, vinf,&
                  nr, yd, bnews, mtrac)
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
! person_in_charge: alexandre.foucault at edf.fr
    implicit none
!     ----------------------------------------------------------------
!     CHOIX DES VALEURS DE VIND A AFFECTER A YD
!     ----------------------------------------------------------------
!     IN   MATER  :  PROPRIETES MATERIAU
!          NMAT   :  DIMENSION TABLEAU DONNEES MATERIAU
!          NVI    :  NOMBRE DE VARIABLES INTERNES
!          VIND   :  VARIABLES INTERNES A T
!          VINF   :  VARIABLES INTERNES A T+DT (BASE SUR PRED_ELAS)
!          NR     :  DIMENSION MAXIMALE DE YD
!     OUT  YD     :  VECTEUR INITIAL
!          VIND   :  IMAGE DE VINF (COHERENCE AVEC ROUTINE HUJMID.F)
!          NR     :  DIMENSION DU SYSTEME NL A RESOUDRE
!     ----------------------------------------------------------------
#include "asterf_types.h"
#include "asterfort/lceqvn.h"
    integer :: nvi, nr, nmat
    real(kind=8) :: vind(nvi), vinf(nvi), yd(nr), mater(nmat, 2)
    aster_logical :: bnews(3), mtrac
!
    integer :: i, ii, nbmeca, ndt
    real(kind=8) :: zero, un
!
    parameter (zero = 0.d0)
    parameter (un   = 1.d0)
    parameter (ndt  = 6)
!     ----------------------------------------------------------------
! ---  DEFINITION DU NOMBRE DE MECANISMES POTENTIELS ACTIFS
    nbmeca = 0
    do 10 i = 1, 8
        if (vinf(23+i) .eq. un) nbmeca = nbmeca + 1
 10 end do
! ---  DIMENSION DU SYSTEME NL A RESOUDRE FONCTION DE NBMECA
    nr = ndt + 1 + 2*nbmeca
!
! --- AFFECTATION DE VIND A VINF
!    (COHERENCE AVEC SCHEMA D'INTEGRATION SPECIFIQUE)
!
    call lceqvn(nvi, vinf, vind)
!
! ---  YD(NDT+1) = EPS_V^P = VIND(23) A T
    yd(ndt+1) = vind(23)
!
    ii = 1
    do 30 i = 1, 8
        if (vind(23+i) .eq. un) then
!
            if (i .ne. 4) then
                yd(ndt+1+ii) = vind(i)
                yd(ndt+1+nbmeca+ii) = zero
                ii = ii + 1
            else
                yd(ndt+1+nbmeca) = vind(i)
                yd(ndt+1+2*nbmeca) = zero
            endif
!
        endif
 30 end do
!
! --- REDIMENSIONNEMENT DE YD ET YF POUR S'ADAPTER A HUJJID
! --- SIGMA/E0, R * PREF/ E0
    do 40 i = 1, 6
        yd(i) = yd(i)/mater(1,1)
 40 end do
!
    do 50 i = 1, nbmeca
        yd(ndt+1+i) = yd(ndt+1+i)/mater(1,1)*abs(mater(8,2))
 50 end do
!
! --- VARIABLE DE GESTION DES MECANISMES DE TRACTION
    do 60 i = 1, 3
        bnews(i) = .true.
 60 end do
    mtrac = .false.
!
! --- MISE A ZERO DU COMPTEUR D'ITERATIONS LOCALES
    vinf(35) = zero
!
!
end subroutine
