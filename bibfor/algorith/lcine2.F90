function lcine2(d, gm, pm, c, dgamma,&
                alpham, x)
    implicit none
    include 'asterc/r8prem.h'
    real(kind=8) :: d, gm, pm, c, dgamma, alpham, absdga, lcine2, x, alpha
    real(kind=8) :: signe
! person_in_charge: jean-michel.proix at edf.fr
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    absdga=abs(dgamma)
    lcine2=x*(1.d0+d*absdga)+d*alpham*absdga-dgamma
    alpha=alpham+x
    if (abs(alpha) .gt. r8prem()) then
        signe=alpha/abs(alpha)
        lcine2=lcine2+((c*abs(alpha)/gm)**pm)*signe
    endif
end function
