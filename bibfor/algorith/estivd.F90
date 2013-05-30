subroutine estivd(nbm, dt, vitg, depg, accg0,&
                  vitg0, depg0, tetaes, maxvit, inewto)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! DESCRIPTION : ESTIMATION DES VECTEURS VITESSES ET DEPLACEMENTS
! -----------   GENERALISES A L'INSTANT N+1 PAR LE SCHEMA D'EULER
!
!               APPELANTS : ALITMI, NEWTON
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    include 'asterfort/u2mess.h'
    integer :: nbm
    real(kind=8) :: dt, vitg(*), depg(*), accg0(*), vitg0(*), depg0(*), tetaes
    real(kind=8) :: maxvit
    integer :: inewto
!
! VARIABLES LOCALES
! -----------------
    integer :: i, j
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC  ABS
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
! 1.  CALCUL DU VECTEUR VITESSES GENERALISEES A L'INSTANT N+1
!     -------------------------------------------------------
!
    do 10 i = 1, nbm
        vitg(i) = vitg0(i) + dt * accg0(i)
10  end do
!
! --- SI L'APPELANT EST NEWTON : CORRECTION DU VECTEUR VITESSES
!     GENERALISEES DANS LE CAS D'UN CHANGEMENT DE SIGNE D'UNE
!     DES COMPOSANTES AU MOINS
!
    if (inewto .eq. 1) then
        do 20 i = 1, nbm
            if (abs(vitg0(i)) .gt. (1.0d-04*maxvit)) then
                if ((vitg(i)*vitg0(i)) .lt. 0.0d0) then
                    do 21 j = 1, nbm
                        vitg(j) = vitg0(j)
21                  continue
                    call u2mess('I', 'ALGORITH3_58')
                    goto 22
                endif
            endif
20      continue
22      continue
    endif
!
! 2.  CALCUL DU VECTEUR DEPLACEMENTS GENERALISES A L'INSTANT N+1
!     ----------------------------------------------------------
!
    do 30 i = 1, nbm
        depg(i) = depg0(i) + dt * ( vitg(i) * tetaes + vitg0(i) * ( 1.0d0-tetaes) )
30  end do
!
! --- FIN DE ESTIVD.
end subroutine
