subroutine cribif(mod, dsidep, vbifur, nbrac4, racine)
! =====================================================================
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! =====================================================================
    implicit none
#include "asterc/r8nnem.h"
#include "asterc/r8prem.h"
#include "asterc/r8rddg.h"
#include "asterfort/fbifur.h"
#include "asterfort/utmess.h"
#include "asterfort/zerop3.h"
#include "asterfort/zeropn.h"
    integer :: nbrac4
    real(kind=8) :: dsidep(6, 6), racine(4), vbifur
    character(len=8) :: mod
! =====================================================================
! --- RECHERCHE DE ZONES DE LOCALISATION PAR LE CRITERE DE RICE -------
! =====================================================================
    integer :: ii, degre, compt, nbrac3, ier
    real(kind=8) :: zero, un, deux, trois, quatre
    real(kind=8) :: a0, a1, a2, a3, a4, lamba, lambb, lambc
    real(kind=8) :: valeur
    real(kind=8) :: ai(4), rac4(8), signe, rac3(3)
! =====================================================================
    parameter  ( zero   = 0.0d0 )
    parameter  ( un     = 1.0d0 )
    parameter  ( deux   = 2.0d0 )
    parameter  ( trois  = 3.0d0 )
    parameter  ( quatre = 4.0d0 )
! =====================================================================
! --- INITIALISATIONS ET COHERENCES -----------------------------------
! =====================================================================
    vbifur = zero
    nbrac4 = 0
    signe = 1.0d0
    valeur = r8nnem()
    racine(1) = 0.0d0
    racine(2) = 0.0d0
    racine(3) = 0.0d0
    racine(4) = 0.0d0
    if ((mod(1:6).ne.'D_PLAN') .and. (mod(1:6).ne.'C_PLAN') .and. (mod(1:4).ne.'AXIS')) then
        call utmess('F', 'ALGORITH2_43')
    endif
! =====================================================================
! --- AFFECTATION DES VARIABLES ---------------------------------------
! =====================================================================
    a0 = dsidep(1,1)*dsidep(4,4) - dsidep(1,4)*dsidep(4,1)
    a1 = dsidep(1,1)*(dsidep(4,2)+dsidep(2,4)) - dsidep(1,4)*dsidep(2,1) - dsidep(1,2)*dsidep(4,1&
         &)
    a2 = dsidep(1,1)*dsidep(2,2) + dsidep(1,4)*dsidep(4,2) + dsidep(4,1)*dsidep(2,4) - dsidep(1,2&
         &)*dsidep(4,4) - dsidep(1,2)*dsidep(2,1) - dsidep(4,4)*dsidep(2,1)
    a3 = dsidep(2,2)*(dsidep(1,4) + dsidep(4,1)) - dsidep(1,2)*dsidep(2,4) - dsidep(4,2)*dsidep(2&
         &,1)
    a4 = dsidep(4,4)*dsidep(2,2) - dsidep(4,2)*dsidep(2,4)
    if (a4 .lt. -r8prem()) then
        signe = -1.0d0
    endif
! =====================================================================
! --- CAS OU A4 = 0 ---------------------------------------------------
! =====================================================================
    if (abs(a4) .lt. r8prem()) then
! =====================================================================
! --- ON LOCALISE POUR A4 = 0 -----------------------------------------
! =====================================================================
        vbifur = un
        nbrac4 = 1
        racine(1) = 90.0d0
        racine(2) = 0.0d0
        racine(3) = 0.0d0
        racine(4) = 0.0d0
        goto 9998
    endif
    lamba = trois*a3/quatre/a4
    lambb = a2/deux/a4
    lambc = a1/quatre/a4
! =====================================================================
! --- RESOLUTION DU POLYNOME DE DEGRE 3 -------------------------------
! =====================================================================
    call zerop3(lamba, lambb, lambc, rac3, nbrac3)
    do 10 ii = 1, nbrac3
        valeur = signe * fbifur(a0,a1,a2,a3,a4,rac3(ii))
        if (valeur .lt. -r8prem()) vbifur = un
10  continue
! =====================================================================
! --- RECHERCHE DES RACINES DU POLYNOME -------------------------------
! =====================================================================
    if (vbifur .eq. un) then
        degre = 4
        ai(1) = a0/a4
        ai(2) = a1/a4
        ai(3) = a2/a4
        ai(4) = a3/a4
        call zeropn('F', degre, ai(1), rac4, ier)
! =====================================================================
! --- ON RECUPERE LES RACINES REELLES ---------------------------------
! =====================================================================
        compt = 0
        do 20 ii = 1, 4
            if (abs(rac4((ii-1)*2+2)) .lt. r8prem()) then
                compt = compt + 1
                racine(compt) = atan2(rac4((ii-1)*2+1),1.0d0)
                racine(compt) = racine(compt)*r8rddg()
            endif
20      continue
        nbrac4 = compt
    endif
! =====================================================================
9998  continue
! =====================================================================
end subroutine
