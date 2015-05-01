subroutine projmd(testc, np1, nb1, nb2, mat,&
                  vg, vd, matpr, mtmp1, mtmp2)
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
! DESCRIPTION : PROJECTION DE LA MATRICE MAT SUR LA NOUVELLE BASE
! -----------   MODALE DEFINIE PAR SES VECTEURS PROPRES A GAUCHE
!               ET A DROITE, VG ET VD
!
!               APPELANTS : ALITMI, NEWTON
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
#include "asterfort/prmama.h"
#include "asterfort/utmess.h"
    integer :: testc, np1, nb1, nb2
    real(kind=8) :: mat(np1, *), vg(np1, *), vd(np1, *), matpr(*), mtmp1(np1, *)
    real(kind=8) :: mtmp2(np1, *)
!
! VARIABLES LOCALES
! -----------------
    integer :: i, ier, iprod
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL  PRMAMA
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    if (testc .eq. 1) then
!
        iprod = 1
        ier = 0
!
        call prmama(iprod, mat, np1, nb1, nb1,&
                    vd, np1, nb1, nb2, mtmp1,&
                    np1, nb1, nb2, ier)
        if (ier .ne. 0) then
            call utmess('F', 'ALGORITH10_2')
        endif
!
        call prmama(iprod, vg, np1, nb2, nb1,&
                    mtmp1, np1, nb1, nb2, mtmp2,&
                    np1, nb2, nb2, ier)
        if (ier .ne. 0) then
            call utmess('F', 'ALGORITH10_2')
        endif
!
        do 10 i = 1, nb2
            matpr(i) = mtmp2(i,i)
10      continue
!
    else
!
        do 20 i = 1, nb2
            matpr(i) = mat(i,i)
20      continue
!
    endif
!
! --- FIN DE PROJMD.
end subroutine
