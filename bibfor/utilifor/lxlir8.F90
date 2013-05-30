subroutine lxlir8(chaine, rval, ier)
    implicit none
    include 'asterfort/lxscan.h'
    character(len=*) :: chaine
    real(kind=8) :: rval
    integer :: ier
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
!     DECODAGE D'UN REEL ECRIT EN CHAINE DE CARACTERES
!     ------------------------------------------------------------------
! IN  CHAINE : CH*(*) : CHAINE DE CARACTERES CONTENANT L'ENTIER
! OUT IVAL   : R8     : REEL DECODE
! OUT IER    : IS     : CODE RETOUR
!              = 0  PAS D'ERREUR ON A BIEN LU UN ENTIER (IVAL)
!              = 1  ON A LU AUTRE CHOSE QU'UN ENTIER
!     ------------------------------------------------------------------
!     ROUTINE(S) UTILISEE(S) :
!         LXSCAN
!     ROUTINE(S) FORTRAN     :
!         -
!     ------------------------------------------------------------------
! FIN LXLIR8
!     ------------------------------------------------------------------
    character(len=80) :: cval
    integer :: ival
!
!-----------------------------------------------------------------------
    integer :: iclass, icol
!-----------------------------------------------------------------------
    ier = 0
    icol = 1
    call lxscan(chaine, icol, iclass, ival, rval,&
                cval)
!     ------------------------------------------------------------------
!                          ICLASS      CODE DE CE QUE L'ON A TROUVE
!           -- TYPE -----    ---- INFORMATION --------------------------
!          -1 FIN DE LIGNE   (RIEN A LIRE)
!           0 ERREUR         CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
!           1 ENTIER         IVAL DE TYPE INTEGER
!           2 REEL           RVAL DE TYPE REAL*8
!           3 IDENTIFICATEUR CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
!           4 TEXTE          CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
!           5 SEPARATEUR     CVAL DE TYPE CHARACTER*(*) DE LONGUEUR 1
!     ------------------------------------------------------------------
    if (iclass .ne. 2) ier = 1
end subroutine
