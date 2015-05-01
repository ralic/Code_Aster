subroutine projmg(np1, np2, ic, nbm, phii,&
                  depg, xglo)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! DESCRIPTION : PASSAGE BASE MODALE -> BASE PHYSIQUE GLOBALE
! -----------
!               APPELANTS : CALND1, CALND2, CHVERI, COMPTR, INIALG,
!                           MDCHOE, MDCHOF, NEWTON, PROJMP, TESTCH,
!                           TSTCNT
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    integer :: np1, np2, ic, nbm
    real(kind=8) :: phii(np2, np1, *), depg(*), xglo(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: i
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    xglo(1) = 0.0d0
    xglo(2) = 0.0d0
    xglo(3) = 0.0d0
!
    do 10 i = 1, nbm
        xglo(1) = xglo(1) + phii(ic,i,1) * depg(i)
        xglo(2) = xglo(2) + phii(ic,i,2) * depg(i)
        xglo(3) = xglo(3) + phii(ic,i,3) * depg(i)
10  end do
!
! --- FIN DE PROJMG.
end subroutine
