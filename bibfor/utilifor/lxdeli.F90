subroutine lxdeli(tab, nbval)
    implicit none
    character(len=1) :: tab(*)
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     DEFINITION DES SEPARATEURS ADMIS PAR L'ANALYSEUR LEXICAL
!     ------------------------------------------------------------------
!                CETTE ROUTINE A VOCATION A ETRE SURCHARGEE
!     ------------------------------------------------------------------
! OUT TAB   : TABLEAU CONTENANT LES SEPARATEURS
! VAR NBVAL : NOMBRE MAXIMUM DE SEPARATEURS   (EN ENTREE)
!           : NOMBRE EFFECTIF DE SEPARATEURS  (EN SORTIE)
!     ------------------------------------------------------------------
!     ROUTINE(S) UTILISEE(S) :
!         -
!     ROUTINE(S) FORTRAN     :
!         -
!     ------------------------------------------------------------------
! FIN LXDELI
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: nbval
!-----------------------------------------------------------------------
    if (nbval .ge. 11) then
        tab( 1) = '='
        tab( 2) = '('
        tab( 3) = ')'
        tab( 4) = ':'
        tab( 5) = ';'
        tab( 6) = ','
        tab( 7) = '%'
        tab( 8) = '&'
        tab( 9) = '*'
        tab(10) = '/'
        tab(11) = '!'
        nbval = 11
    else
        nbval = 0
    endif
!
end subroutine
