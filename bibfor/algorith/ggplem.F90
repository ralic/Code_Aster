subroutine ggplem(s, dpc, valden, unsurk, unsurm,&
                  theta, deuxmu, g, dgdst, dgdev)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!DEB
!---------------------------------------------------------------
!     VITESSE DE DEF. VISQUEUSE ET SA DERIVEE PAR RAPPORT A SIGMA
!---------------------------------------------------------------
! IN  S     :R: CONTRAINTE EQUIVALENTE SIGMA
!     DPC   :R: SCALAIRE RESUMANT L'ETAT VISCOPLASTIQUE DU POINT
!               CONSIDERE DU MATERIAU (DEFORM. PLASTIQUE CUMULEE)
!     VALDEN:R: VALEUR DE N
!     UNSURK:R: PARAMETRE 1/K
!     UNSURM:R: PARAMETRE 1/M
!     THETA :R: PARAMETRE DU SCHEMA D'INTEGRATION (0.5 OU 1)
!                  THETA = 0.5 -> SEMI-IMPLICITE
!                  THETA = 1.0 -> IMPLICITE
! OUT G     :R: VALEUR DE LA FONCTION G
!     DGDST :R: DERIVEE TOTALE DE G PAR RAPPORT A SIGMA
!     DGDEV :R: DERIVEE PARTIELLE DE G PAR RAPPORT A EV (I.E. DPC)
!---------------------------------------------------------------
!            DANS LE CAS DE LA LOI DE LEMAITRE,
!     CETTE ROUTINE CALCULE LA FONCTION G DE LA FORMULATION
!       "STRAIN HARDENING" DE L'ECOULEMENT VISCOPLASTIQUE
!       (LOI DONNEE SOUS FORME "STRAIN HARDENING")
!            .
!            EV = G(SIGMA,LAMBDA)
!
!     ET LA DERIVEE TOTALE DE CETTE FONCTION G PAR RAPPORT A SIGMA
!---------------------------------------------------------------
!FIN
!
!-----------------------------------------------------------------------
    real(kind=8) :: deuxmu, dgdev, dgdst, dpc, g, s, theta
    real(kind=8) :: unsurk, unsurm, valden
!-----------------------------------------------------------------------
    if (s .eq. 0.d0 .or. dpc .eq. 0.d0 .or. unsurk .eq. 0.d0) then
        g = 0.d0
        dgdst = 0.d0
        dgdev = 0.d0
        goto 99
    else
        if (unsurm .eq. 0.d0) then
            g = exp(valden*log(s*unsurk))
            dgdst = valden*g/s
            dgdev = 0.d0
        else
            g = exp(valden*(log(s*unsurk)-unsurm*log(dpc)))
            dgdst = valden*(1.d0/s+unsurm/(1.5d0*deuxmu*dpc))*g
            dgdev = - valden*g*unsurm/dpc
        endif
    endif
    g = g*theta
    dgdst = dgdst*theta
    dgdev = dgdev*theta
99  continue
!
end subroutine
