subroutine fopro1(vec, i, prolgd, interp)
    implicit none
    integer :: i
    character(len=*) :: vec(*), prolgd, interp
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
!     RECUPERE LES PROLONGEMENTS ET TYPE D'INTERPOLATION DANS
!     LE VECTEUR DESCRIPTEUR D'UN OBJET DE TYPE FONCTION
!     ------------------------------------------------------------------
! IN  VEC   : VECTEUR DESCRIPTEUR
! IN  I     : NUMERO DE LA FONCTION DANS LE CAS D'UNE NAPPE (0 SINON)
! OUT PROLGD: PROLONGEMENTS A GAUCHE ET A DROITE DE LA FONCTION I
! OUT INTERP: TYPE D'INTERPOLATION DE LA FONCTION I
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if (i .eq. 0) then
        interp = vec(2)
        prolgd = vec(5)
    else
        interp = vec(7+(2*i-1))
        prolgd = vec(7+(2*i ))
    endif
end subroutine
