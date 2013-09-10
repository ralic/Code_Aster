subroutine dilata(imate, phi, alphfi, t, aniso,&
                  angmas, tbiot, phenom)

    implicit none
! ======================================================================
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
! --- CALCUL DE ALPHAFI ------------------------------------------------
! ======================================================================
#include "asterc/r8pi.h"
#include "asterfort/rcvala.h"
    integer :: nelas2
    integer :: nelas1,nelas3
    parameter  ( nelas1=1 )
    parameter  ( nelas2=2 )
    parameter  ( nelas3=3 )
    real(kind=8) :: elas1(nelas1),elas2(nelas2),elas3(nelas3)
    character(len=8) :: ncra1(nelas1)
    character(len=8) :: ncra2(nelas2)
    character(len=8) :: ncra3(nelas3)
    integer :: icodre1(nelas1),icodre2(nelas2),icodre3(nelas3)
    integer :: imate, aniso, i, anisoi
    real(kind=8) :: phi, t, tbiot(6), alpha(6)
    real(kind=8) :: kron(6), angmas(3), alphfi
    character(len=16) :: phenom
    real(kind=8) :: talpha(3, 3),talphal(3, 3)
    real(kind=8) :: passag(3,3),work(3,3)
! =====================================================================
! --- DONNEES POUR RECUPERER LES CARACTERISTIQUES MECANIQUES ----------
! =====================================================================
    data ncra1 /'ALPHA'/
    data ncra2 /'ALPHA_L','ALPHA_N'/
    data ncra3 /'ALPHA_L','ALPHA_N','ALPHA_T'/
    anisoi = aniso
! ======================================================================
! --- INITIALISATION DU TENSEUR ----------------------------------------
! ======================================================================
    call matini(3, 3, 0.d0, work)
    call matini(3, 3, 0.d0, talpha)
    call matini(3, 3, 0.d0, talphal)
    call matini(3, 3, 0.d0, passag)
! =====================================================================
! --- DEFINITION DU SYMBOLE DE KRONECKER UTILE POUR LA SUITE ----------
! =====================================================================
    do 10 i = 1, 3
            kron(i) = 1.d0
10  continue
    do 20 i = 4, 6
            kron(i) = 0.d0
20  continue
!
! =====================================================================
! --- CALCUL CAS ISOTROPE ---------------------------------------------
! =====================================================================
    999 if (anisoi.eq.0) then
! =====================================================================
! --- RECUPERATION DES COEFFICIENTS MECANIQUES ------------------------
! =====================================================================
     call rcvala(imate, ' ', 'ELAS', 1, 'TEMP',&
                t, 1, ncra1(1), elas1(1), icodre1,0)
     talpha(1,1)=elas1(1)
     talpha(2,2)=elas1(1)
     talpha(3,3)=elas1(1)

! =====================================================================
! --- CALCUL CAS ISOTROPE TRANSVERSE 3D-------------------------------
! =====================================================================
    else if (anisoi.gt.0) then
     if (phenom .eq. 'ELAS') then
        anisoi=0
        goto 999
     else if ((phenom.eq.'ELAS_ISTR')) then
        call rcvala(imate, ' ', 'ELAS_ISTR', 1, 'TEMP',&
                    t, 2, ncra2(1), elas2(1), icodre2,0)
          talpha(1,1)=elas2(1)
          talpha(2,2)=elas2(1)
          talpha(3,3)=elas2(2)
     else if ( phenom.eq.'ELAS_ORTH')then
        call rcvala(imate, ' ', 'ELAS_ORTH', 1, 'TEMP',&
                    t, 3, ncra3(1), elas3(1), icodre3,0)
          talpha(1,1)=elas3(1)
          talpha(2,2)=elas3(3)
          talpha(3,3)=elas3(2)
     endif
    endif
!
! matrice de passage du local au global
    call matrot(angmas, passag)
! ======================================================================
! --- CALCUL DU TENSEUR DE ALPHA BGL DANS LE REPERE GLOBAL -------------
! ======================================================================
    call utbtab('ZERO', 3, 3, talpha, passag,work, talphal)
! =====================================================================
! --- DEFINITION DU TENSEUR DE CONDUCTIVITE THERMIQUE -----------------
! =====================================================================
    alpha(1)=talphal(1,1)
    alpha(2)=talphal(2,2)
    alpha(3)=talphal(3,3)
    alpha(4)=talphal(1,2)
    alpha(5)=talphal(1,3)
    alpha(6)=talphal(2,3)
! =====================================================================
! --- DEFINITION DU COEFFICIENT DE DILATATION DIFFERENTIEL ------------
! =====================================================================
    alphfi = 0.d0
    do 40 i = 1, 6
        alphfi = alphfi+(tbiot(i)-phi*kron(i))*alpha(i)/3.d0
40  enddo
end subroutine
