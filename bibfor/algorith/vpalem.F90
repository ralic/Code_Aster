function vpalem(x)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
    real(kind=8) :: x
    real(kind=8) :: vpalem
!
!DEB
!---------------------------------------------------------------
!     FONCTION A(X)  :  CAS DE LA LOI DE LEMAITRE
!---------------------------------------------------------------
! IN  X     :R: ARGUMENT RECHERCHE LORS DE LA RESOLUTION SCALAIRE
!---------------------------------------------------------------
!     L'ETAPE LOCALE DU CALCUL VISCOPLASTIQUE (CALCUL DU TERME
!       ELEMENTAIRE DE LA MATRICE DE RIGIDITE TANGENTE) COMPORTE
!       LA RESOLUTION D'UNE EQUATION SCALAIRE NON LINEAIRE:
!
!           A(X) = 0
!
!       (DPC,SIELEQ,DEUXMU,DELTAT JOUENT LE ROLE DE PARAMETRES)
!---------------------------------------------------------------
!FIN
!     COMMON POUR LES PARAMETRES DES LOIS VISCOPLASTIQUES
    common / nmpavp / dpc,sieleq,deuxmu,deltat,tschem,prec,theta,niter
    real(kind=8) :: dpc, sieleq, deuxmu, deltat, tschem, prec, theta, niter
!
!     COMMON POUR LES PARAMETRES DE LA LOI DE LEMAITRE (NON IRRADIEE)
    common / nmpale / unsurk,unsurm,valden
    real(kind=8) :: unsurk, unsurm, valden
!
    real(kind=8) :: g
!
    if (unsurk .eq. 0.d0 .or. x .eq. 0.d0) then
        g = 0.d0
    else
        if (unsurm .eq. 0.d0) then
            g = log(x*unsurk)
            g = exp(valden*g)
            g = g*theta
        else
            g = log(x*unsurk)-unsurm*log(dpc+(sieleq-x)/(1.5d0*deuxmu) )
            g = exp(valden*g)
            g = g*theta
        endif
    endif
    vpalem = 1.5d0*deuxmu*deltat*g + x - sieleq
end function
