subroutine projac(np1, np2, nbm, ic, phii,&
                  jacob, mtmp1, mtmp6)
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
! DESCRIPTION : PROJECTION DES MATRICES JACOBIENNES SUR BASE MODALE
! -----------
!               APPELANT : MDCHOE
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    integer :: np1, np2, nbm, ic
    real(kind=8) :: phii(np2, np1, *), jacob(3, 3), mtmp1(np1, *), mtmp6(3, *)
!
! VARIABLES LOCALES
! -----------------
    integer :: i, j, k
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    do 110 i = 1, 3
        do 120 j = 1, nbm
            mtmp6(i,j) = 0.0d0
            do 130 k = 1, 3
                mtmp6(i,j) = mtmp6(i,j) + jacob(i,k) * phii(ic,j,k)
130          continue
120      continue
110  end do
!
    do 210 i = 1, nbm
        do 220 j = 1, nbm
            mtmp1(i,j) = 0.0d0
            do 230 k = 1, 3
                mtmp1(i,j) = mtmp1(i,j) + phii(ic,i,k) * mtmp6(k,j)
230          continue
220      continue
210  end do
!
! --- FIN DE PROJAC.
end subroutine
