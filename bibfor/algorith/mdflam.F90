subroutine mdflam(dnorm, vitloc, knorm, cost, sint,&
                  flim, fseuil, rigifl, defpla, fnorma,&
                  flocal, vnorm)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     FONCTION  : CALCULE LA DISTANCE NORMALE A L'OBSTACLE (<0 SI CHOC)
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
!    DNORM          <--   DISTANCE NORMALE A L'OBSTACLE
!    VITLOC         <--   VITESSE DANS LE REPERE LOCAL
!    COST,SINT      <--   DIRECTION NORMALE A L'OBSTACLE
!    KNORM          <--   RAIDEUR NORMALE DE CHOC
!    FNORMA          -->  FORCE NORMALE DE CHOC  (MODULE)
!    FLOCAL          -->  FORCE NORMALE DE CHOC REP. LOCAL
!-----------------------------------------------------------------------
    real(kind=8) :: vitloc(3), flocal(3), knorm, fnorma
!-----------------------------------------------------------------------
    real(kind=8) :: cost, defpla, dnorm, flim, fseuil, rigifl, sint
    real(kind=8) :: vnorm
!-----------------------------------------------------------------------
    vnorm = vitloc(2)*cost + vitloc(3)*sint
!
    if (defpla .le. 0.d0) then
!     --- FLAMBAGE NON ENCORE RENCONTRE ---
        if (-dnorm .lt. 0.d0) then
            fnorma = 0.0d0
        else
            if (-dnorm .lt. flim/knorm) then
                fnorma = -knorm*dnorm
            else
!           --- DEBUT DU FLAMBAGE ---
                fnorma = flim
                defpla = flim/knorm - fseuil/rigifl
                if (defpla .le. 0.d0) defpla = 1.d-20
            endif
        endif
    else
!     --- LE FLAMBAGE A DEJA EU LIEU ---
        if (-dnorm .lt. defpla) then
            fnorma = 0.0d0
        else
            if (vnorm .gt. 0.d0 .or. -dnorm .le. (fseuil/rigifl+defpla)) then
                fnorma = -rigifl*(dnorm+defpla)
                if (fnorma .lt. 0.d0) fnorma = 0.d0
            else
                fnorma = fseuil
                defpla = -dnorm - fseuil/rigifl
            endif
        endif
    endif
    flocal(1)=0.d0
    flocal(2)=fnorma*cost
    flocal(3)=fnorma*sint
end subroutine
