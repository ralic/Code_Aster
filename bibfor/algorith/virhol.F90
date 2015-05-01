subroutine virhol(nbvari, vintm, vintp, advihy, vihrho,&
                  rho110, dp1, dp2, dpad, cliq,&
                  dt, alpliq, signe, rho11, rho11m,&
                  retcom)
    implicit      none
    integer :: nbvari, retcom, advihy, vihrho
    real(kind=8) :: vintm(nbvari), vintp(nbvari), rho110, dp1, dp2, dpad, dt
    real(kind=8) :: cliq, signe, alpliq, rho11, rho11m
! ======================================================================
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
! ======================================================================
! --- CALCUL ET STOCKAGE DE LA VARIABLE INTERNE DE LA MASSE ------------
! --- VOLUMIQUE DE L EAU -----------------------------------------------
! ======================================================================
    real(kind=8) :: varbio, epxmax
    parameter    (epxmax = 5.d0)
! ======================================================================
! --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
! ======================================================================
! --- CALCUL DES ARGUMENTS EN EXPONENTIELS -----------------------------
! --- ET VERIFICATION DE LA COHERENCE ----------------------------------
! ======================================================================
    varbio = (dp2-signe*dp1-dpad)*cliq - 3.d0*alpliq*dt
    if (varbio .gt. epxmax) then
        retcom = 2
        goto 30
    endif
    vintp(advihy+vihrho) =&
     &              - rho110 + (vintm(advihy+vihrho)+rho110)*exp(varbio)
!
    rho11 = vintp(advihy+vihrho) + rho110
    rho11m = vintm(advihy+vihrho) + rho110
! ======================================================================
30  continue
! ======================================================================
! ======================================================================
end subroutine
