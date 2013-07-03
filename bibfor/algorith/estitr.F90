subroutine estitr(nbm, amori, masgi, eps, ttrans,&
                  npf, npfmax, text, ier)
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
! DESCRIPTION : ESTIMATION DE LA DUREE DU TRANSITOIRE
! -----------
!               APPELANT : TRANSI
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
#include "asterfort/u2mess.h"
    integer :: nbm
    real(kind=8) :: amori(*), masgi(*), eps, ttrans
    integer :: npf, npfmax
    real(kind=8) :: text(*)
    integer :: ier
!
! VARIABLES LOCALES
! -----------------
    integer :: i
    real(kind=8) :: ttemp, tempf, ksiomm, ksiomc
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC  ABS, DBLE, LOG
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    ier = 0
!
!--1. RECHERCHE DE 2*KSI0*OMEGA0 MINIMUM POUR LES DIFFERENTS MODES
!
    ksiomm = 1.0d+10
    do 10 i = 1, nbm
        ksiomc = dble(abs(amori(i)/masgi(i)))
        if (ksiomc .lt. ksiomm) ksiomm = ksiomc
10  end do
!
!--2. CALCUL DU TRANSITOIRE POUR UNE PRECISION EPS INDEPENDANTE DU JEU
!
    ttrans = - log(eps) / ksiomm
!
!--3. CALCUL DU TEMPS DE TRANSITOIRE MINIMUM EN FONCTION DU PAS DE
!     DISCRETISATION DE LA FORCE EXTERIEURE
!
    ttemp = 0.0d0
    do 20 i = 1, 15
        tempf = ( (2.0d0 ** (dble(i))) - 1.0d0 ) * (text(2) - text(1))
        if (ttrans .lt. tempf) then
            goto 21
        else
            ttemp = tempf
        endif
20  end do
21  continue
!
    npf = i - 1
    ttrans = ttemp
    if (i .eq. 16) call u2mess('A', 'ALGORITH3_56')
!
    if (ttrans .gt. text(npfmax)) then
        ier = 1
        call u2mess('A', 'ALGORITH3_57')
    endif
!
! --- FIN DE ESTITR.
end subroutine
