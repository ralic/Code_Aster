subroutine sricvg(nr,itmax,toler,iter,r,nvi,vinf,dy,irtet)

!
! ===================================================================================
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
! ===================================================================================

!!!
!!! MODELE LKR : CONTROLE DE LA CONVERGENCE DU NEWTON LOCAL DE LKR
!!!                   - CONTROLE DU NOMBRE D ITERATIONS
!!!                   - CONTROLE DE LA PRECISION DE CONVERGENCCE
!!!

! ===================================================================================
! IN  : ITMAX     :  NB MAXI D ITERATIONS LOCALES
!     : TOLER     :  TOLERANCE A CONVERGENCE
!     : ITER      :  NUMERO ITERATION COURANTE
!     : NR        :  DIMENSION R
!     : R         :  RESIDU DU SYSTEME NL A L'ITERATION COURANTE
!     : NVI       :  NOMBRE DE VARIABLES INTERNES
!     : VINF(NVI) :  VARIABLES INTERNES A L'INSTANT T+DT
!     : DY        :  SOLUTION DU SYSTEME NL A L'INSTANT T+DT
! OUT : IRET = 0  :  CONVERGENCE
!     : IRET = 1  :  ITERATION SUIVANTE
!     : IRET = 2  :  RE-INTEGRATION
!     : IRET = 3  :  REDECOUPAGE DU PAS DE TEMPS
!     : VINF(7)   :  SI ETAT PLASTIQUE NON VERIFIE - VINF(7)=0
!     : DY(NDT+1) :  SI ETAT PLASTIQUE NON VERIFIE - DY(NDT+1)=0
! ===================================================================================
    
    implicit none

    !!!
    !!! Variables globales
    !!!
    
    integer :: nr,itmax,iter,irtet,ndt,ndi,nvi
    real(kind=8) :: toler,r(nr),vinf(nvi),dy(nr)
    
    !!!
    !!! Variables locales
    !!!

    integer :: i
    real(kind=8) :: er
    common /tdim/ ndt,ndi
    
    !!!
    !!! Calcul de la norme de rini et dy
    !!!
    
    er=0.d0
    
    do i=1,nr
        er=er+r(i)*r(i)
    end do
    
    er=sqrt(er)
    
    !!!
    !!! Tets de la convergence par rapport a toler
    !!!

    if (er.lt.toler) then
        if ((dy(ndt+1).ge.0.d0).and.(vinf(7).gt.0.d0)) then
            irtet=0
        else if (vinf(7).le.0.d0) then
            irtet=0
        else
            irtet=2
            vinf(7)=0.d0
            do i=1,nr
                dy(i)=0.d0
            end do
        endif
        goto 9999
    endif
    
    !!!
    !!! Si non convergence, test du num. d'iteration
    !!!
    
    if (iter.lt.itmax) then
        irtet=1
    else
        irtet=3
    endif

9999  continue

end subroutine
