subroutine tesmcl(icl, iv, cv, mtcl, irteti)
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
    implicit none
!       ----------------------------------------------------------------
!       TESTE SI L ITEM LU EN DEBUT DE LIGNE  EST
!       LE MOT CLE RECHERCHE (EVENTUALITE)
!       ----------------------------------------------------------------
!       MOT CLE         =       MOT / RECONNU / EN DEBUT DE LIGNE
!
!       IN      IV      =       TAILLE ITEM CARACTERE
!               CV      =       ITEM LU
!               MTCL    =       MOT CLE RECHERCHE
!
!       OUT     (RETURN 1)      FAUX    MOT # MOT CLE RECHERCHE
!               (RETURN )       VRAI    MOT = MOT CLE RECHERCHE
!       ----------------------------------------------------------------
    integer :: iv
    character(len=*) :: cv
    character(len=8) :: mtcl, mcl
!
!-----------------------------------------------------------------------
    integer :: icl, irteti
!-----------------------------------------------------------------------
    irteti = 0
    if (icl .eq. 3 .and. iv .le. 8 .and. iv .ne. 0) then
        mcl = '        '
        mcl(1:iv) = cv(1:iv)
        if (mcl .eq. mtcl) then
            irteti = 0
            goto 9999
        endif
    endif
!
    irteti = 1
9999  continue
end subroutine
