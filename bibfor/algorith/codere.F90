subroutine codere(cod, npg, codret)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    implicit none
    integer :: npg, cod(npg), codret
!     ------------------------------------------------------------------
!     SYNTHESE DES CODES RETOURS : EN ENTREE, ON A UN TABLEAU
!     DE DIM. NPG CONTENANT LES CODES RETOURS DE TOUS LES PTS DE
!     GAUSS. EN SORTIE, ON A UN SEUL CODE RETOUR RESUME
!     ------------------------------------------------------------------
!     IN  COD     : TABLEAU CONTENANT LES CODES RETOURS DE TOUS
!                   LES PTS DE GAUSS
!     IN  NPG     : NBRE DE PTS DE GAUSS DE L'ELELEMT TRAITE
!     OUT CODRET  : CODE RETOUR RESUME
!         CODRET=1 : ECHEC INTEGRATION LOI DE COMPORTEMENT
!         CODRET=3 : C_PLAN DEBORST SIGZZ NON NUL
!     ------------------------------------------------------------------
    integer :: i
!
    codret=0
    do 10 i = 1, npg
        if (cod(i) .eq. 1) then
            codret=1
            goto 9999
        else if (cod(i).ne.0) then
            codret=cod(i)
        endif
10  end do
!
!
9999  continue
end subroutine
