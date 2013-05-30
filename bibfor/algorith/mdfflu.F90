subroutine mdfflu(dnorm, vnorm, anorm, vitloc, accloc,&
                  cost, sint, coefa, coefb, coefc,&
                  coefd, ffluid, flocal)
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
!
!***********************************************************************
! 01/01/91    G.JACQUART AMV/P61 47 65 49 41
!***********************************************************************
!     FONCTION  : CALCULE LA FORCE FLUIDE NORMALE A L'OBSTACLE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
!        NOCM       MODE                    ROLE
!  ________________ ____ ______________________________________________
!                         VARIABLES DU SYSTEME DYNAMIQUE MODAL
!  ________________ ____ ______________________________________________
!    DNORM          <--   DISTANCE NORMALE
!    VNORM          -->   VITESSE  NORMALE
!    ANORM          -->   ACCELERATION NORMALE
!    VITLOC         <--   VITESSE DANS LE REPERE LOCAL
!    ACCLOC         <--   ACCELERATION DANS LE REPERE LOCAL
!    COST,SINT      <--   DIRECTION DE LA NORMALE A L'OBSTACLE
!    COEFA          <--   COEFFICIENT DE MASSE AJOUTEE
!    COEFB          <--   COEFFICIENT DE PERTE DE CHARGE >0
!    COEFC          <--   COEFFICIENT DE FORCE VISQUEUSE
!    COEFD          <--   COEFFICIENT DE PERTE DE CHARGE <0
!    CNORM          <--   AMORTISSEUR NORMALE DE CHOC
!    FFLUID          -->  FORCE NORMALE FLUIDE
!    FLOCAL          -->  FORCE NORMALE DE CHOC REP. LOCAL
!-----------------------------------------------------------------------
    implicit none
    real(kind=8) :: vitloc(3), accloc(3), flocal(3), ffluid
    real(kind=8) :: coefa, coefb, coefc, coefd, cost, sint
!-----------------------------------------------------------------------
    real(kind=8) :: anorm, dnorm, vnorm
!-----------------------------------------------------------------------
    vnorm = vitloc(2)*cost + vitloc(3)*sint
    anorm = accloc(2)*cost + accloc(3)*sint
    ffluid = coefa*anorm/dnorm + coefb*(vnorm/dnorm)**2 + coefc*vnorm/dnorm**3 + coefd*vnorm*abs(&
             &vnorm)/dnorm/dnorm
    flocal(1)=0.d0
    flocal(2)=ffluid*cost
    flocal(3)=ffluid*sint
end subroutine
