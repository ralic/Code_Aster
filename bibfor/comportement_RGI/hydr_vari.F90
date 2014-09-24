subroutine hydr_vari(vari0, vari1, hydra0, hydra1, hydras,&
                     erreur)
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
! ======================================================================
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!     effets de l hydratation sur les varibles internes
!     declaration externes
!=====================================================================
    implicit none
    real(kind=8) :: vari0, vari1, hydra0, hydra1, hydras
    integer :: erreur
!     varibles locales
    real(kind=8) ::  yymin
    parameter (yymin=0.001d0)
!      print*,'hdratation actuelle:',hydra1
!      print*,'hydratation precedente:',hydra0
!      print*,'hydratation seuil:',hydras
    if (hydra1 .lt. yymin) then
!      le materiau est non coherent les varibles internes
!      sont conservees inchangées
        vari1=0.d0
    else
        if ((hydra1.ge.hydra0) .and. (hydra0.gt.hydras)) then
!        apres le seuil on conserve les vari ou on les attenue
            vari1=vari0*(hydra0-hydras)/(hydra1-hydras)
        else
            if (hydra1 .le. hydras) then
                vari1=0.d0
            else
                vari1=vari0
                end if
                end if
                end if
end subroutine
