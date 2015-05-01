function schdp2(seq, i1e, phi, alpha, c,&
                pult, pmoins)
!
    implicit      none
    real(kind=8) :: seq, i1e, phi, alpha, c, pult, pmoins, schdp2
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
! --- BUT : CALCUL DU CRITERE PLASTIQUE --------------------------------
! ======================================================================
    real(kind=8) :: un, deux, trois, six, gamarp, gamapm
! ======================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------------
! ======================================================================
    parameter       ( un     =  1.0d0  )
    parameter       ( deux   =  2.0d0  )
    parameter       ( trois  =  3.0d0  )
    parameter       ( six    =  6.0d0  )
! ======================================================================
    gamarp = sqrt ( trois / deux ) * pult
    gamapm = sqrt ( trois / deux ) * pmoins
    if (pmoins .lt. pult) then
        schdp2 = seq + deux*sin(phi)*i1e/(trois-sin(phi)) - six*c*cos( phi)/(trois-sin(phi)) * (u&
                 &n-(un-alpha)*gamapm/gamarp) * (un-(un-alpha)*gamapm/gamarp)
    else
        schdp2 = seq + deux*sin(phi)*i1e/(trois-sin(phi)) - six*c*cos( phi)/(trois-sin(phi)) * al&
                 &pha * alpha
    endif
! ======================================================================
end function
