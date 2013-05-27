function sgmxve(nbterm, vect)
    implicit none
    real(kind=8) :: sgmxve
    integer :: nbterm
    real(kind=8) :: vect(*)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!              SIGNE DE LA VALEUR ABSOLUE MAXIMALE
!
!  IN
!     NBTERM   :  NOMBRE DE VALEURS
!     VECT     :  TABLEAU
!  OUT
!     SGMXVE   : 1.0D0 OU -1.0D0
!
! --- ------------------------------------------------------------------
!
    integer :: ii
    real(kind=8) :: vmax
! --- ------------------------------------------------------------------
!
    vmax = vect(1)
    do 10 ii = 2, nbterm
        if (abs(vect(ii)) .ge. abs(vmax)) vmax = vect(ii)
10  end do
    if (vmax .ge. 0.0d0) then
        sgmxve = 1.0d0
    else
        sgmxve = -1.0d0
    endif
end function
