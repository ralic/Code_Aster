subroutine nmvexi(sigi, grj2, dj2ds, nb, mate,&
                  nmat, xhi, dxhids)
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
    implicit none
!
#include "asterc/r8gaem.h"
#include "asterfort/calcj0.h"
#include "asterfort/r8inir.h"
    integer :: nb, nmat
!
    real(kind=8) :: mate(nmat, 2), sigi(nb)
    real(kind=8) :: xhi, dxhids(nb), grj2, dj2ds(nb)
!-----------------------------------------------------------------------
!     MODELE VISCOPLASTIQUE A ECROUISSAGE ISOTRROPE COUPLE A DE
!     L ENDOMMAGEMENT ISOTROPE
!
!    CALCUL DE LA CONTRAINTE EQUIVALENTE D ENDOMMAGEMENT VISCOPLASTIQUE
!    ET DE SA DERIVEE PAR RAPPORT A LA CONTRAINTE ASSOCIEE
!-----------------------------------------------------------------------
!     IN   SIGI(NB)     : INCONNUES ASSOCIEES AUX CONTRAINTES
!          GRJ2         : CONTRAINTE EQUIVALENTE DE VON MISES
!          DJ2DS(NB)    : DERIVEE DE LA CONTRAINTE DE VON MISES
!          NB           : NOMBRE D'INCONNUES ASSOCIEES AUX CONTRAINTES
!          MATE(*,2)    : PARAMETRES MATERIAUX ELASTQUES ET PLASTIQUES
!          NMATE        : NOMBRE DE PARAMETRES MATERIAUX
!     OUT  XHI          : CONTRAINTE EQUIVALENTE D'ENDOMMAGEMENT
!                         VISCOPLASTIQUE
!          DXHIDS(NB)   : DERIVEE DE XHI/SIGI
!-----------------------------------------------------------------------
    integer :: itens, ivec
    real(kind=8) :: trsig, kron(6), tole, dj0ds(6), dj1ds(6)
    real(kind=8) :: grj0, grj1, valp(3)
    real(kind=8) :: sedvp1, sedvp2, zero
!
    data kron  /1.d0, 1.d0, 1.d0, 0.d0, 0.d0, 0.d0/
!-----------------------------------------------------------------------
!-- 1- INITIALISATIONS:
    tole = 1.d0 / r8gaem()
    zero = 0.d0
    call r8inir(nb, 0.d0, dj0ds, 1)
    call r8inir(nb, 0.d0, dj1ds, 1)
!-- COEFFICIENTS MATERIAU
    sedvp1 = mate(2,2)
    sedvp2 = mate(3,2)
!     ----------------------------------------------------------------
!     KD VA SERA CALCULEE PLUS LOIN UNE FOIS QUE XHI
!     (CONTRAINTE EQUIVALENTE D ENDOMMAGEMENT VISCOPLASTIQUE)
!     SERA CALCULEE
!     ----------------------------------------------------------------
    trsig=(sigi(1)+sigi(2)+sigi(3))
!
!-- CALCUL DE GRJ0(SIGI) : MAX DES CONTRAINTES PRINCIPALES
    if (sedvp1 .le. tole) then
        grj0 =zero
        valp(1)=zero
        valp(2)=zero
        valp(3)=zero
    else
        call calcj0(sigi, grj0, valp)
    endif
!
!-- CALCUL DE GRJ1(SIGI) : PREMIER INVARIANT (TRACE)
    grj1= trsig
!
!-- CALCUL DE LA CONTRAINTE EQUIVALENTE DE FLUAGE
    xhi=sedvp1*grj0+sedvp2*grj1+(1-sedvp1-sedvp2)*grj2
!
!-- DERIVEE DE LA CONTRAINTE EQUIVALENTE DE FLUAGE / CONTRAINTE
    do 10 ivec = 1, 3
        if (valp(ivec) .eq. grj0) dj0ds(ivec) = 1.d0
10  end do
!
    do 20 itens = 1, nb
        dj1ds(itens) = kron(itens)
        dxhids(itens)=sedvp1*dj0ds(itens)+sedvp2*dj1ds(itens)+&
        (1-sedvp1-sedvp2)*dj2ds(itens)
20  end do
end subroutine
