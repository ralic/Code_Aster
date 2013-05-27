subroutine almulr(czero, table, nbval, mantis, expo)
    implicit none
    character(len=*) :: czero
    integer :: nbval, expo
    real(kind=8) :: table(nbval), mantis
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     PRODUIT DE N NOMBRE AVEC TEST DE L OVERFLOW ET DE L'UNDERFLOW
!     AVEC CUMUL DE VALEUR ANTERIEUR OU REMISE A ZERO.
!     ------------------------------------------------------------------
! IN  CZERO  : K4  : DEMANDE DE REMISE A ZERO 'ZERO' OU DE CUMUL
! IN  TABLE  : R8  : TABLEAU DES VALEURS A MULTIPLIER
! IN  NBVAL  : R8  : NOMBRE DE VALEURS A MULTIPLIER
! VAR MANTIS : R8  : MANTISSE DU RESULTAT
! VAR EXPO   : IS  : EXPOSANT DU RESULTAT
!     ------------------------------------------------------------------
!     LE RESULTAT DU PRODUIT EST    MANTISS * 10 ** EXPO
!     UN "BON FORMAT" D'IMPRESSION EST
!         WRITE(?,'(10X,A,F16.10,A,I8)') 'PRODUIT = ',MANTIS,' E',EXPO
!     ------------------------------------------------------------------
!
    real(kind=8) :: trent, trent1, zero, dix
    integer :: itrent
!
!-----------------------------------------------------------------------
    integer :: ie, ival
!-----------------------------------------------------------------------
    trent = 1.d30
    itrent = 30
    trent1 = 1.d-30
    zero = 0.d0
    dix = 10.d0
!
    if (czero .eq. 'ZERO') then
        mantis = 1.d0
        expo = 0
    endif
!
    do 10 ival = 1, nbval
        mantis = mantis*table(ival)
        if (abs(mantis) .ge. trent) then
            mantis = mantis*trent1
            expo = expo + itrent
        else if (abs(mantis).le.trent1) then
            mantis = mantis*trent
            expo = expo - itrent
        endif
10  end do
!
    if (mantis .ne. zero) then
        ie = nint(log10(abs(mantis)))
        mantis = mantis/ (dix**ie)
        expo = expo + ie
    endif
!
end subroutine
