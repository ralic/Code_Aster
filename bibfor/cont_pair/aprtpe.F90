subroutine aprtpe(corint, nbpint,& 
                  typmaa,corinp, ndim, nudec)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    implicit none
#include "asterfort/reerel.h"
#include "asterfort/assert.h"
!
    integer, intent(in) :: ndim
    real(kind=8), intent(in) :: corint(ndim-1,16)
    integer, intent(in) :: nbpint
    character(len=8), intent(in) :: typmaa
    real(kind=8), intent(out) :: corinp(ndim-1,16)
    integer, intent(in), optional :: nudec
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE LAC - UTILITAIRE)
!
! RETOUR DANS L'ESPACE PARAMETRIQUE d'une maille
! D'UN TRIANGLE D'INTEGRATION depuis l'espace parametrique 
! d'une autre maille (pour un triangle)
!
! ----------------------------------------------------------------------
! IN  CORTRI : COORDONEES DU TRIANGLE
! IN  GEOMD  : COORDONEE DE LA MAILLE DE DEPART
! IN  TYPMAD : TYPE DE MAILLE DU DEPART
! IN  GEOMA  : COORDONEE DE LA MAILLE DESTINATION 
! IN  TYPMAA : TYPE DE MAILLE DE LA DESTINATION
! IN  NNDA    : NOMBRE DE NOEUD
! OUT TRIPAM : COORDONEES PARAMETRIQUE
!
! ----------------------------------------------------------------------
!
    integer :: ind1
    real(kind=8) :: aux(3), xd(2)
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: norm(3), noor
    real(kind=8) :: geoax3(3,3), geoax2(2,2)
    character(len=8) :: typmax
!
! ----------------------------------------------------------------------
! Initialisation
    do ind1=1, nbpint
        corinp(1,ind1)=0.d0
        corinp(2,ind1)=0.d0
    end do
    tau1(1:3) = 0.d0
    tau2(1:3) = 0.d0
    norm(1:3) = 0.d0
    noor=0.d0
!
    do ind1=1, nbpint
        tau1(1:3) = 0.d0
        tau2(1:3) = 0.d0
        norm(1:3) = 0.d0
        noor=0.d0
        xd(1)=corint(1,ind1)
        if ((ndim-1) .eq. 2) then
            xd(2)=corint(2,ind1)
        end if
! ---- CAS QUAD 8 et QUAD 4 et SE3 et TRIA6
        if ((typmaa.eq.'QU4' ) .and. present(nudec)) then
            if (nudec .eq. 1) then
                geoax3(1,1) = -1.d0
                geoax3(2,1) = -1.d0
                geoax3(3,1) =  0.d0
                geoax3(1,2) =  1.d0
                geoax3(2,2) = -1.d0
                geoax3(3,2) =  0.d0
                geoax3(1,3) =  1.d0
                geoax3(2,3) =  1.d0
                geoax3(3,3) =  0.d0
                typmax='TR3'
                call reerel(typmax,3, 3, geoax3, xd, aux)
                corinp(1,ind1) = aux(1)
                corinp(2,ind1) = aux(2)
            elseif (nudec .eq. 2) then
                geoax3(1,1) =  1.d0
                geoax3(2,1) =  1.d0
                geoax3(3,1) =  0.d0
                geoax3(1,2) = -1.d0
                geoax3(2,2) =  1.d0
                geoax3(3,2) =  0.d0
                geoax3(1,3) = -1.d0
                geoax3(2,3) = -1.d0
                geoax3(3,3) =  0.d0
                typmax='TR3'
                call reerel(typmax,3, 3, geoax3, xd, aux)
                corinp(1,ind1) = aux(1)
                corinp(2,ind1) = aux(2)
            else
                ASSERT(.false.)
            endif
! =================================================================================================
        elseif (typmaa.eq.'SE3' .and. present(nudec)) then
            if (nudec .eq. 1) then
                geoax2(1,1) = -1.d0
                geoax2(2,1) = 0.d0
                geoax2(1,2) =  0.d0
                geoax2(2,2) = 0.d0
                typmax='SE2'
                call reerel(typmax,2, 2, geoax2, xd, aux)
                corinp(1,ind1) = aux(1)
                corinp(2,ind1) = aux(2)
            elseif (nudec .eq. 2) then
                geoax2(1,1) = 0.d0
                geoax2(2,1) = 0.d0
                geoax2(1,2) =  1.d0
                geoax2(2,2) = 0.d0
                typmax='SE2'
                call reerel(typmax,2, 2, geoax2, xd, aux)
                corinp(1,ind1) = aux(1)
                corinp(2,ind1) = aux(2)
            else
                ASSERT(.false.)
            endif
! =================================================================================================
        elseif (typmaa.eq.'TR6' .and. present(nudec)) then
            if (nudec .eq. 1) then
                geoax3(1,1) = 0.d0
                geoax3(2,1) = 0.d0
                geoax3(3,1) = 0.d0
                geoax3(1,2) = 5.d-1
                geoax3(2,2) = 0.d0
                geoax3(3,2) = 0.d0
                geoax3(1,3) = 0.d0
                geoax3(2,3) = 5.d-1
                geoax3(3,3) = 0.d0
                typmax='TR3'
                call reerel(typmax,3, 3, geoax3, xd, aux)
                corinp(1,ind1) = aux(1)
                corinp(2,ind1) = aux(2)            
            elseif (nudec .eq. 2) then
                geoax3(1,1) = 5.d-1
                geoax3(2,1) = 0.d0
                geoax3(3,1) = 0.d0
                geoax3(1,2) = 1.d0
                geoax3(2,2) = 0.d0
                geoax3(3,2) = 0.d0
                geoax3(1,3) = 5.d-1
                geoax3(2,3) = 5.d-1
                geoax3(3,3) = 0.d0
                typmax='TR3'
                call reerel(typmax,3, 3, geoax3, xd, aux)
                corinp(1,ind1) = aux(1)
                corinp(2,ind1) = aux(2)
            elseif (nudec .eq. 3) then
                geoax3(1,1) = 5.d-1
                geoax3(2,1) = 0.d0
                geoax3(3,1) = 0.d0
                geoax3(1,2) = 5.d-1
                geoax3(2,2) = 5.d-1
                geoax3(3,2) = 0.d0
                geoax3(1,3) = 0.d0
                geoax3(2,3) = 5.d-1
                geoax3(3,3) = 0.d0
                typmax='TR3'
                call reerel(typmax,3, 3, geoax3, xd, aux)
                corinp(1,ind1) = aux(1)
                corinp(2,ind1) = aux(2)
            elseif (nudec .eq. 4) then
                geoax3(1,1) = 5.d-1
                geoax3(2,1) = 5.d-1
                geoax3(3,1) = 0.d0
                geoax3(1,2) = 0.d0
                geoax3(2,2) = 1.d0
                geoax3(3,2) = 0.d0
                geoax3(1,3) = 0.d0
                geoax3(2,3) = 5.d-1
                geoax3(3,3) = 0.d0
                typmax='TR3'
                call reerel(typmax,3, 3, geoax3, xd, aux)
                corinp(1,ind1) = aux(1)
                corinp(2,ind1) = aux(2)
            end if
! =================================================================================================
        elseif (typmaa.eq.'QU8' .and. present(nudec)) then
            if (nudec .eq. 1) then
                geoax3(1,1) = -1.d0
                geoax3(2,1) = -1.d0
                geoax3(3,1) =  0.d0
                geoax3(1,2) =  0.d0
                geoax3(2,2) = -1.d0
                geoax3(3,2) =  0.d0
                geoax3(1,3) = -1.d0
                geoax3(2,3) =  0.d0
                geoax3(3,3) =  0.d0
                typmax='TR3'
                call reerel(typmax,3, 3, geoax3, xd, aux)
                corinp(1,ind1) = aux(1)
                corinp(2,ind1) = aux(2)
            elseif (nudec .eq. 2) then
                geoax3(1,1) =  0.d0
                geoax3(2,1) = -1.d0
                geoax3(3,1) =  0.d0
                geoax3(1,2) =  1.d0
                geoax3(2,2) = -1.d0
                geoax3(3,2) =  0.d0
                geoax3(1,3) =  1.d0
                geoax3(2,3) =  0.d0
                geoax3(3,3) =  0.d0
                typmax='TR3'
                call reerel(typmax,3, 3, geoax3, xd, aux)
                corinp(1,ind1) = aux(1)
                corinp(2,ind1) = aux(2)                      
            elseif (nudec .eq. 3) then
                geoax3(1,1) =  1.d0
                geoax3(2,1) =  0.d0
                geoax3(3,1) =  0.d0
                geoax3(1,2) =  1.d0
                geoax3(2,2) =  1.d0
                geoax3(3,2) =  0.d0
                geoax3(1,3) =  0.d0
                geoax3(2,3) =  1.d0
                geoax3(3,3) =  0.d0
                typmax='TR3'
                call reerel(typmax,3, 3, geoax3, xd, aux)
                corinp(1,ind1) = aux(1)
                corinp(2,ind1) = aux(2)
            elseif (nudec .eq. 4) then
                geoax3(1,1) =  0.d0
                geoax3(2,1) =  1.d0
                geoax3(3,1) =  0.d0
                geoax3(1,2) = -1.d0
                geoax3(2,2) =  1.d0
                geoax3(3,2) =  0.d0
                geoax3(1,3) = -1.d0
                geoax3(2,3) =  0.d0
                geoax3(3,3) =  0.d0
                typmax='TR3'
                call reerel(typmax,3, 3, geoax3, xd, aux)
                corinp(1,ind1) = aux(1)
                corinp(2,ind1) = aux(2)
            elseif (nudec .eq. 5) then
                geoax3(1,1) =  0.d0
                geoax3(2,1) = -1.d0
                geoax3(3,1) =  0.d0
                geoax3(1,2) =  1.d0
                geoax3(2,2) =  0.d0
                geoax3(3,2) =  0.d0
                geoax3(1,3) =  0.d0
                geoax3(2,3) =  1.d0
                geoax3(3,3) =  0.d0
                typmax='TR3'
                call reerel(typmax,3, 3, geoax3, xd, aux)
                corinp(1,ind1) = aux(1)
                corinp(2,ind1) = aux(2)
            elseif (nudec .eq. 6) then
                geoax3(1,1) =  0.d0
                geoax3(2,1) =  1.d0
                geoax3(3,1) =  0.d0
                geoax3(1,2) = -1.d0
                geoax3(2,2) =  0.d0
                geoax3(3,2) =  0.d0
                geoax3(1,3) =  0.d0
                geoax3(2,3) = -1.d0
                geoax3(3,3) =  0.d0
                typmax='TR3'
                call reerel(typmax,3, 3, geoax3, xd, aux)
                corinp(1,ind1) = aux(1)
                corinp(2,ind1) = aux(2)                     
            else
                ASSERT(.false.)
            end if
        else
! ----- STOCKAGE RESULTAT (pas d'espace para support)----------------------
            corinp(1,ind1) = xd(1)
            if ((ndim-1) .eq. 2) then
                corinp(2,ind1) = xd(2)
            end if
        end if
    end do

end subroutine
