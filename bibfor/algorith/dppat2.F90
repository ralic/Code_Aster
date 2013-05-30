function dppat2(mater, pmoins, pplus, plas)
!
    implicit      none
    include 'asterfort/betaps.h'
    include 'asterfort/u2mess.h'
    real(kind=8) :: mater(5, 2), pmoins, pplus, plas, dppat2
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
    real(kind=8) :: alpha1, alpha, douze, psi, beta, betam, dp
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
    psi = mater(5,2)
    alpha = deux*sin(phi)/(trois-sin(phi))
    beta = deux*sin(psi)/(trois-sin(psi))
    dp = pplus - pmoins
    if (plas .eq. 1.0d0) then
        if (pplus .lt. pult) then
            betam = betaps (beta, pmoins, pult)
            dppat2 = trois*deuxmu/deux + trois*troisk*alpha*betam - 6.0d0*troisk*alpha*dp*beta/pu&
                     &lt - douze*c*cos(phi)/(trois- sin(phi))* (un-(un-alpha1)/pult*pplus)*(un-al&
                     &pha1)/pult
        else
            dppat2 = trois*deuxmu/deux
        endif
    else if (plas.eq.2.0d0) then
        call u2mess('F', 'ALGORITH3_43')
        if (pplus .lt. pult) then
            betam = betaps (beta, pmoins, pult)
            dppat2 = trois*troisk*alpha*betam - douze*c*cos(phi)/( trois-sin(phi))* (un-(un-alpha&
                     &1)/pult*pplus)*(un-alpha1)/ pult
        else
            dppat2 = 0.0d0
        endif
    else
        dppat2 = 0.0d0
    endif
! ======================================================================
end function
