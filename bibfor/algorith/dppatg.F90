function dppatg(mater, pplus, plas)
!
    implicit      none
#include "asterc/r8nnem.h"
    real(kind=8) :: mater(5, 2), pplus, plas, dppatg
! ======================================================================
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
! --- BUT : INITIALISATION POUR L OPERATEUR TANGENT POUR LA LOI --------
! --- DRUCKER-PRAGER PARABOLIQUE ---------------------------------------
! ======================================================================
    real(kind=8) :: un, deux, trois, young, nu, troisk, deuxmu, phi, c, pult
    real(kind=8) :: alpha1, alpha, douze
! ======================================================================
    parameter  ( un    =  1.0d0 )
    parameter  ( deux  =  2.0d0 )
    parameter  ( trois =  3.0d0 )
    parameter  ( douze = 12.0d0 )
! ======================================================================
    young = mater(1,1)
    nu = mater(2,1)
    troisk = young / (un-deux*nu)
    deuxmu = young / (un+nu)
    alpha1 = mater(1,2)
    phi = mater(2,2)
    c = mater(3,2)
    pult = mater(4,2)
    alpha = deux*sin(phi)/(trois-sin(phi))
    dppatg = r8nnem()
    if (plas .eq. 1.0d0) then
        if (pplus .lt. pult) then
            dppatg = trois*deuxmu/deux + trois*troisk*alpha*alpha - douze*c*cos(phi)/(trois-sin(p&
                     &hi))* (un-(un-alpha1)/pult* pplus)*(un-alpha1)/pult
        else
            dppatg = trois*deuxmu/deux + trois*troisk*alpha*alpha
        endif
    else if (plas.eq.2.0d0) then
        if (pplus .lt. pult) then
            dppatg = trois*troisk*alpha*alpha - douze*c*cos(phi)/( trois-sin(phi))* (un-(un-alpha&
                     &1)/pult*pplus)*(un-alpha1)/ pult
        else
            dppatg = trois*troisk*alpha*alpha
        endif
    endif
! ======================================================================
end function
