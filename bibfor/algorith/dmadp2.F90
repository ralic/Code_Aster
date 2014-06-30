function dmadp2(rho22, sat, phi, cs, mamolg,&
                kh, dp21p2, emmag, em)
    implicit none
    real(kind=8) :: rho22, sat, phi, cs, mamolg, kh, dp21p2, dmadp2
    real(kind=8) :: dphip2, em
    logical(kind=1) :: emmag
! ======================================================================
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
! --- CALCUL DE LA DERIVEE DE L AIR DISSOUS PAR RAPPORT A LA PRESSION --
! --- DE GAZ -----------------------------------------------------------
! ======================================================================
    if (emmag) then
        dphip2 = em
        dmadp2 = rho22*sat*(phi*mamolg*dp21p2/rho22/kh+dphip2)
    else
        dmadp2 = rho22*sat*(phi*mamolg*dp21p2/rho22/kh+cs)
    endif
! ======================================================================
end function
