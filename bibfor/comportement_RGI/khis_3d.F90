subroutine khis_3d(tau1, khi, xid, alpha, ar,&
                   asr, dth0, coth, xidtot)
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
! ======================================================================
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!      provient de rsi_3d : 
!     calcul de l'évolution de khi au cours du temps
!=====================================================================
    implicit none
    real(kind=8) :: tau1
    real(kind=8) :: khi
    real(kind=8) :: xid
    real(kind=8) :: alpha
    real(kind=8) :: ar
    real(kind=8) :: asr
    real(kind=8) :: dth0
    real(kind=8) :: coth
!     declaration variables locales
    real(kind=8) :: tau0, tau2, c1, c2, id1, id2, idref, beta, nom, tau, xm, xidtot
!
!     modif micro diffusion sellier nov 2012
!     on definit les points caracteristique de la courbe
!     temps caracteristique de micro def / indice de desstructuration
!     **** temps caracteristique pour le beton non hydrate *************
!     modifiable si necessaire
    tau0=tau1*1.d-2
!     ******************************************************************
!     tau2 est le temps caracteristique pour le beton totalement hydrate
!     et pas destructure (id2=0)
    tau2=tau1*1.d2
    c2=tau2/tau0
    id2=0.d0
!     ***** temps carateristique pour le beton destructure à id1********
!     on peut deplacer ce point si necessaire      
    id1=3.d0 
!     ******************************************************************      
    c1=tau1/tau0
!     calcul des coeffs idref et beta en fonction des points de passage
    idref = -(log( c1) * id1 - log( c2) * id2) / (log( c1) - log( c2))
    beta = -log( c2) * log( c1) * (id1 - id2) / (log( c1) - log( c2))
!     *****  modifiable si necessaire **********************************
!     exposant de non linearite de la dependance de tau à alpha
    xm=3.d0  
!     ******************************************************************
    nom=beta      
!      nom=(alpha**xm)*beta
!     pour prendre en compte l endo thermique on le rajoute à xid
!     coth:coeff de couplage vitesse de reaction endo thermique
    xidtot=xid+(coth*dth0/(1.d0-dth0))     
    tau=tau0*dexp(nom/(xidtot+idref))/(ar*asr)
    tau=tau1
    khi=(1.d0/tau1)*dexp(coth*dth0/(1.d0-dth0))*ar*asr
!      print*,tau,khi,xid,alpha,ar,asr
!      read*      
end subroutine
