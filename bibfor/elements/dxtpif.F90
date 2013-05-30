subroutine dxtpif(temp, ltemp)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    real(kind=8) :: temp(3)
    logical :: ltemp(3)
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     GESTION DE TEMP_INF ET TEMP_SUP DANS LES COQUES / TUYAUX :
!
!     IN LTEMP(3) :
!        LTEMP(1) = .T. / .F.   TEMP_MIL EST AFFECTE
!        LTEMP(2) = .T. / .F.   TEMP_INF EST AFFECTE
!        LTEMP(3) = .T. / .F.   TEMP_SUP EST AFFECTE
!
!     IN/OUT TEMP(3) :
!        TEMP(1) =  VALEUR DE TEMP_MIL
!        TEMP(2) =  VALEUR DE TEMP_INF
!        TEMP(3) =  VALEUR DE TEMP_SUP
!
!        SI TEMP_INF (OU TEMP_SUP) N'EST PAS AFFECTE :
!          SI TEMP_MIL EST AFFECTE : TEMP_INF = TEMP_SUP = TEMP_MIL
!          SINON : ERREUR 'F'
    integer :: iadzi, iazk24
    character(len=8) :: nomail
!
!
    if (.not.ltemp(1)) then
        call tecael(iadzi, iazk24)
        nomail=zk24(iazk24-1+3)
        call u2mesk('F', 'ELEMENTS_53', 1, nomail)
    endif
!
    if (.not.ltemp(2)) temp(2)=temp(1)
    if (.not.ltemp(3)) temp(3)=temp(1)
!
end subroutine
