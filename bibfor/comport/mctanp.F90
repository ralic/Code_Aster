subroutine mctanp(dpstrs, rprops, pstrs, edge, right,&
                  apex)
!
    implicit none
! Declaration of real type variables
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    real(kind=8) :: dpstrs(3, 3)
    real(kind=8) :: rprops(*)
    real(kind=8) :: pstrs(3)
    real(kind=8) :: edge
    real(kind=8) :: right
    real(kind=8) :: apex
!
#include "asterf_types.h"
#include "asterfort/matini.h"
!
! Declaration of integer type variables
    integer :: i, j, ii, jj, mm
    real(kind=8) :: degr
!
    aster_logical :: epflag
!
    parameter ( degr=0.017453292519943295d0 )
! Real arrays and variables
    real(kind=8) :: young, poiss, sinphi, sinpsi, constb, gmodu, bulk
    real(kind=8) :: r2g, r4g, r2bulk, r1d3, r2d3, r2gd3, r4gd3, pstmax, pstmin, sphsps, consta
    real(kind=8) :: denom, b1, b2, b3, drvaa, drvab, drvba, drvbb, aux1
    real(kind=8) :: aux2, aux3, r1ddet, r1, r2, r3, r4, r0
    data&
     &    r0    ,r1    ,r2    ,r3    ,r4    /&
     &    0.0d0 ,1.0d0 ,2.0d0 ,3.0d0 ,4.0d0 /
!
! Declaration of Common space variables
    common / debug / epflag
!***********************************************************************
! COMPUTATION OF CONSISTENT TANGENT MODULUS FOR MOHR-COULOMB TYPE
! ELASTO-PLASTIC MATERIAL WITH ASSOCIATIVE/NON-ASSOCIATIVE FLOW RULE
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT DE MOHR-COULOMB
! IN  PSTRS   : CONTRAINTES PRINCIPALES
! IN  EDGE    : Y-A-T-IL DEUX  MECANISMES ACTIFS
! IN  RIGHT   : SI EDGE, LA PROJECTION S'EFFECTUE-T-IL A DROITE?
! IN  APEX    : Y-A-T-IL TROIS MECANISMES ACTIFS
!
! OUT DPSTRS  : MATRICE TANGENTE DANS L'ESPACE DES DIRECTIONS PRINCIPALE
!***********************************************************************
! Stops program if neither plane strain nor axisymmetric state
!      IF(NTYPE.NE.2.AND.NTYPE.NE.3)CALL ERRPRT('EI0026')
! Set material properties
    young=rprops(2)
    poiss=rprops(3)
    sinphi=sin(degr*rprops(4))
    sinpsi=sin(degr*rprops(5))
! Set some constants
    gmodu=young/(r2*(r1+poiss))
    bulk=young/(r3*(r1-r2*poiss))
    r2g=r2*gmodu
    r4g=r4*gmodu
    r2bulk=r2*bulk
    r1d3=r1/r3
    r2d3=r2*r1d3
    r2gd3=r2g*r1d3
    r4gd3=r4g*r1d3
! Spectral decomposition of the elastic trial strain
! Identify directions of maximum and minimum principal trial stresses
    ii=1
    jj=1
    pstmax=pstrs(ii)
    pstmin=pstrs(jj)
    do i = 2, 3
        if (pstrs(i) .ge. pstmax) then
            ii=i
            pstmax=pstrs(ii)
        endif
        if (pstrs(i) .lt. pstmin) then
            jj=i
            pstmin=pstrs(jj)
        endif
    end do
    if (ii .ne. 1 .and. jj .ne. 1) mm=1
    if (ii .ne. 2 .and. jj .ne. 2) mm=2
    if (ii .ne. 3 .and. jj .ne. 3) mm=3
!
    if (epflag) then
        write(6,'(3(A,E10.3))') '> IN MCTANP :: EDGE=',edge,' RIGHT=',&
        right,' APEX=',apex
        write(6,'(A,3(1X,E12.5))') '> IN MCTANP :: PSTRA =',(pstrs(i),&
        i=1,3)
    endif
!
! Compute elastoplastic consistent tangent
! ----------------------------------------
    if (edge .eq. r1) then
! Tangent consistent with 2-vector return to edge
        sphsps=sinphi*sinpsi
        consta=r4g*(r1+r1d3*sphsps)+r4*bulk*sphsps
        if (right .eq. r1) then
            constb=r2g*(r1+sinphi+sinpsi-r1d3*sphsps)+r4*bulk*sphsps
        else
            constb=r2g*(r1-sinphi-sinpsi-r1d3*sphsps)+r4*bulk*sphsps
        endif
        drvaa=-consta
        drvab=-constb
        drvba=-constb
        drvbb=-consta
        aux1=r2g*(r1+r1d3*sinpsi)+r2bulk*sinpsi
        aux2=(r4gd3-r2bulk)*sinpsi
        aux3=r2g*(r1-r1d3*sinpsi)-r2bulk*sinpsi
        r1ddet=r1/(drvaa*drvbb-drvab*drvba)
        if (right .eq. r1) then
! ...returned to right edge
            dpstrs(ii,ii)=bulk+r4gd3+aux1*(-drvab+drvbb+drvaa-drvba)*&
            (r2g+(r2bulk+r2gd3)*sinphi)*r1ddet
            dpstrs(ii,mm)=bulk-r2gd3+aux1*(r2g*(drvab-drvaa)+ ((-&
            drvab+drvbb+drvaa-drvba)*(r2bulk+r2gd3)+ (drvba-drvbb)*&
            r2g)*sinphi)*r1ddet
            dpstrs(ii,jj)=bulk-r2gd3+aux1*(r2g*(drvba-drvbb)+ ((-&
            drvab+drvbb+drvaa-drvba)*(r2bulk+r2gd3)+ (drvab-drvaa)*&
            r2g)*sinphi)*r1ddet
            dpstrs(mm,ii)=bulk-r2gd3+(aux2*(drvab-drvbb)+aux3*(drvba-&
            drvaa))*(r2g+(r2bulk+r2gd3)*sinphi)*r1ddet
            dpstrs(mm,mm)=bulk+r4gd3+(aux2*((r2bulk*(drvab-drvbb)+&
            (drvab*r2gd3+drvbb*r4gd3))*sinphi-drvab*r2g)+ aux3*(drvaa*&
            r2g+(r2bulk*(drvba-drvaa)- (drvaa*r2gd3+drvba*r4gd3))*&
            sinphi))*r1ddet
            dpstrs(mm,jj)=bulk-r2gd3+(aux2*((r2bulk*(drvab-drvbb)-&
            (drvbb*r2gd3+drvab*r4gd3))*sinphi+drvbb*r2g)+ aux3*((&
            r2bulk*(drvba-drvaa)+(drvaa*r4gd3+ drvba*r2gd3))*sinphi-&
            drvba*r2g))*r1ddet
            dpstrs(jj,ii)=bulk-r2gd3+((aux2*(drvba-drvaa)+aux3*(drvab-&
            drvbb))*((r2bulk+r2gd3)*sinphi+r2g))*r1ddet
            dpstrs(jj,mm)=bulk-r2gd3+(aux2*(((r2bulk*(drvba-drvaa)-&
            (drvba*r4gd3+drvaa*r2gd3))*sinphi)+drvaa*r2g)+ aux3*(((&
            r2bulk*(drvab-drvbb)+(drvab*r2gd3+ drvbb*r4gd3))*sinphi)-&
            drvab*r2g))*r1ddet
            dpstrs(jj,jj)=bulk+r4gd3+(aux2*(((r2bulk*(drvba-drvaa)+&
            (drvaa*r4gd3+drvba*r2gd3))*sinphi)-drvba*r2g)+ aux3*(((&
            r2bulk*(drvab-drvbb)-(drvab*r4gd3+ drvbb*r2gd3))*sinphi)+&
            drvbb*r2g))*r1ddet
        else
! ...returned to left edge
            dpstrs(ii,ii)=bulk+r4gd3+(aux1*(((r2bulk*(drvbb-drvab)+&
            (drvab*r4gd3+drvbb*r2gd3))*sinphi)+drvbb*r2g)+ aux2*(((&
            r2bulk*(drvba-drvaa)+(drvaa*r4gd3+ drvba*r2gd3))*sinphi)+&
            drvba*r2g))*r1ddet
            dpstrs(ii,mm)=bulk-r2gd3+(aux1*(((r2bulk*(drvbb-drvab)-&
            (drvab*r2gd3+drvbb*r4gd3))*sinphi)-drvab*r2g)+ aux2*(((&
            r2bulk*(drvba-drvaa)-(drvaa*r2gd3+ drvba*r4gd3))*sinphi)-&
            drvaa*r2g))*r1ddet
            dpstrs(ii,jj)=bulk-r2gd3+((aux1*(drvbb-drvab)+aux2*(drvba-&
            drvaa))*(((r2bulk+r2gd3)*sinphi)-r2g))*r1ddet
            dpstrs(mm,ii)=bulk-r2gd3+(aux1*(((r2bulk*(drvaa-drvba)-&
            (drvaa*r4gd3+drvba*r2gd3))*sinphi)-drvba*r2g)+ aux2*(((&
            r2bulk*(drvab-drvbb)-(drvab*r4gd3+ drvbb*r2gd3))*sinphi)-&
            drvbb*r2g))*r1ddet
            dpstrs(mm,mm)=bulk+r4gd3+(aux1*(((r2bulk*(drvaa-drvba)+&
            (drvaa*r2gd3+drvba*r4gd3))*sinphi)+drvaa*r2g)+ aux2*(((&
            r2bulk*(drvab-drvbb)+(drvab*r2gd3+ drvbb*r4gd3))*sinphi)+&
            drvab*r2g))*r1ddet
            dpstrs(mm,jj)=bulk-r2gd3+((aux1*(drvaa-drvba)+aux2*(drvab-&
            drvbb))*(((r2bulk+r2gd3)*sinphi)-r2g))*r1ddet
            dpstrs(jj,ii)=bulk-r2gd3+(aux3*(((r2bulk*(drvab-drvbb-&
            drvaa+ drvba)+(drvaa-drvab)*r4gd3+(drvba-drvbb)* r2gd3)*&
            sinphi)+(drvba-drvbb)*r2g))*r1ddet
            dpstrs(jj,mm)=bulk-r2gd3+(aux3*(((r2bulk*(drvab-drvbb-&
            drvaa+ drvba)+(drvab-drvaa)*r2gd3+(drvbb-drvba)* r4gd3)*&
            sinphi)+(drvab-drvaa)*r2g))*r1ddet
            dpstrs(jj,jj)=bulk+r4gd3+(aux3*(drvab-drvbb-drvaa+drvba)*&
            (((r2bulk+r2gd3)*sinphi)-r2g))*r1ddet
        endif
    else if (apex.eq.r1) then
! Tangent consistent with multi-vector return to apex
        call matini(3, 3, r0, dpstrs)
!         DPSTRS(II,II)=R1
!         DPSTRS(JJ,JJ)=R1
!         DPSTRS(MM,MM)=R1
    else
! Tangent consistent with 1-vector return to main active plane
        sphsps=sinphi*sinpsi
        consta=r4g*(r1+r1d3*sphsps)+r4*bulk*sphsps
        denom=-consta
        b1=(r2g*(r1+r1d3*sinpsi)+r2bulk*sinpsi)/denom
        b2=(r4g*r1d3-r2bulk)*sinpsi/denom
        b3=(r2g*(r1-r1d3*sinpsi)-r2bulk*sinpsi)/denom
        dpstrs(ii,ii)=r2g*(r2d3+b1*(r1+r1d3*sinphi))+ bulk*(r1+r2*b1*&
        sinphi)
        dpstrs(ii,mm)=r1d3*(r3*bulk-r2g)*(r1+r2*b1*sinphi)
        dpstrs(ii,jj)=r2g*(-r1d3-b1*(r1-r1d3*sinphi))+ bulk*(r1+r2*b1*&
        sinphi)
        dpstrs(mm,ii)=r2g*(-r1d3-b2*(r1+r1d3*sinphi))+ bulk*(r1-r2*b2*&
        sinphi)
        dpstrs(mm,mm)=r4g*r1d3*(r1+b2*sinphi)+bulk*(r1-r2*b2*sinphi)
        dpstrs(mm,jj)=r2g*(-r1d3+b2*(r1-r1d3*sinphi))+ bulk*(r1-r2*b2*&
        sinphi)
        dpstrs(jj,ii)=r2g*(-r1d3-b3*(r1+r1d3*sinphi))+ bulk*(r1-r2*b3*&
        sinphi)
        dpstrs(jj,mm)=r1d3*(r3*bulk-r2g)*(r1-r2*b3*sinphi)
        dpstrs(jj,jj)=r2g*(r2d3+b3*(r1-r1d3*sinphi))+ bulk*(r1-r2*b3*&
        sinphi)
    endif
!
    if (epflag) then
        write(6,'(A)')'> IN MCTANP :: DPSTRS='
        do i = 1, 3
            write(6,'(I3,3(1X,E12.5))')i,(dpstrs(i,j),j=1,3)
        enddo
    endif
!
end subroutine
