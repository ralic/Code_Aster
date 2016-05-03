subroutine lcmohr(ndim, typmod, imate, crit, option, tmpp,&
                  dstrai0, stresm0, stres, vim, vip,&
                  dsidep, codret)
! ----------------------------------------------------------------------
!
! OBJET:    LOI DE COMPORTEMENT DE MOHR-COULOMB
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  CRIT    : CRITERES  LOCAUX
!                 CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
!                           (ITER_INTE_MAXI == ITECREL)
!                 CRIT(2) = TYPE DE JACOBIEN A T+DT
!                           (TYPE_MATR_COMP == MACOMP)
!                           0 = EN VITESSE     > SYMETRIQUE
!                           1 = EN INCREMENTAL > NON-SYMETRIQUE
!                 CRIT(3) = VALEUR DE LA TOLERANCE DE CONVERGENCE
!                           (RESI_INTE_RELA == RESCREL)
!                 CRIT(5) = NOMBRE D'INCREMENTS POUR LE
!                           REDECOUPAGE LOCAL DU PAS DE TEMPS
!                           (ITER_INTE_PAS == ITEDEC)
!                           0 = PAS DE REDECOUPAGE
!                           N = NOMBRE DE PALIERS
! IN  IMATE   : NATURE DU MATERIAU
! IN  DSTRAI0 : INCREMENT DE DEFORMATION
! IN  STRESM0 : CONTRAINTE EN T-
! IN  VIM     : VARIABLES INTERNES EN T-
! IN  TMPP    : TEMPERATURE EN T+
! IN  TMPREF  : TEMPERATURE DE REFERENCE
! IN  OPTION  : OPTION DEMANDEE
!                 RIGI_MECA_TANG -> DSIDEP
!                 FULL_MECA      -> STRES DSIDEP VIP
!                 RAPH_MECA      -> STRES        VIP
!
! OUT STRES   : CONTRAINTE
! OUT VIP     : VARIABLES INTERNES:
!                 1   -> DEFORMATION PLASTIQUE VOLUMIQUE
!                 2   -> NORME DE LA DEFORMATION PLASTIQUE DEVIATORIQUE
!                 3   -> INDICATEUR DE PLASTICITE = | 0 : NON
!                                                   | 1 : OUI
! OUT DSIDEP  : MATRICE TANGENTE
! OUT CODRET  : CODE RETOUR = | 0: OK
!                             | 1: NOOK
!
! ----------------------------------------------------------------------
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    character(len=8)  :: typmod(*)
    character(len=16) :: option
    integer           :: ndim, imate, codret
    real(kind=8)      :: crit(*)
    real(kind=8)      :: tmpp
    real(kind=8)      :: dstrai(6), dstrai0(6)
    real(kind=8)      :: stresm(6), stresm0(6), stres(6)
    real(kind=8)      :: vim(*), vip(*)
    real(kind=8)      :: dsidep(6, 6)
!
#include "asterf_types.h"
#include "asterfort/bptobg.h"
#include "asterfort/jacobi.h"
#include "asterfort/lceqvn.h"
#include "asterfort/mcpstr.h"
#include "asterfort/mctg2d.h"
#include "asterfort/mctg3d.h"
#include "asterfort/mctgel.h"
#include "asterfort/rcvala.h"
#include "asterfort/vecini.h"
#include "asterfort/lcinma.h"
#include "asterfort/mgauss.h"
!
! Declaration of constant parameters
    integer      :: mmax, nmax
    real(kind=8) :: degr
    parameter    (degr=0.017453292519943295d0, mmax=3, nmax=6)
!
! Declaration of real type variables
    real(kind=8) :: rprops(nmax), strait(nmax)
    real(kind=8) :: strest(nmax), young, poiss
    real(kind=8) :: sinphi, cosphi, sinpsi, phi(mmax)
    real(kind=8) :: cohe, eigprj(mmax, mmax), pstrs(mmax)
    real(kind=8) :: gmodu, bulk, r2g, r4g, mat(mmax, mmax)
    real(kind=8) :: r2bulk, r2cphi, r2spsi, r1d3, eetv
    real(kind=8) :: pt, eetvd3, pstrs1, pstrs2, pstrs3
    real(kind=8) :: phia, phib, phic, res, scaprd, sphsps
    real(kind=8) :: consta, r4c2ph, ddgama, dgama, delta, dmax1
    real(kind=8) :: smcta, smctb, smctc, constb, drvaa, dgamc
    real(kind=8) :: drvab, drvba, drvbb, r1ddet, ddgamb, dgamb
    real(kind=8) :: resnor, factor, aux1, aux2, aux3
    real(kind=8) :: cotphi, ddepv, s1, s2, s3, sttv
    real(kind=8) :: r0, r1, r2, r3, r4, small, tol, sqr
    real(kind=8) :: apex, edge, ifplas, right, sufail
    real(kind=8) :: tr(nmax), p, r1nu, r1ny, tolcv
!
! Declaration of integer type variables
    integer :: icode(nmax), ndt, ndi, ii, jj, mm
    integer :: mxiter, i
!
! Declaration of integer type variables
!     aster_logical :: epflag
    aster_logical :: tridim, outofp
!
! Declaration of character type variables
    character(len=8)  :: mod
    character(len=16) :: nomres(3)
!
! Declaration of constant variables
    data  r0   ,r1   ,r2   ,r3   ,r4   ,small ,tol   ,sqr /&
          0.0d0,1.0d0,2.0d0,3.0d0,4.0d0,1.d-06,1.d-10,     &
          1.4142135623730951d0                            /
    data  mxiter / 50 /
!
! Declaration of Common space variables
    common / tdim  / ndt, ndi
!    common / debug / epflag
!
! Is the problem tridimensional?
    if (ndim .eq. 3) then
        tridim=.true.
    else
        tridim=.false.
    endif
!
! Modelisation type:
! 3D, D_PLAN, C_PLAN, AXIS
    mod = typmod(1)
!
    if (mod(1:6) .eq. 'C_PLAN') then
        outofp=.false.
    else
        outofp=.true.
    endif
!
! Remove SQRT(2) from extradiagonal terms of input strain and stress
! ------------------------------------------------------------------
    call lceqvn(nmax, dstrai0, dstrai)
    call lceqvn(nmax, stresm0, stresm)
!
    dstrai(4)=dstrai(4)/sqr
    stresm(4)=stresm(4)/sqr
    if (tridim) then
        dstrai(5)=dstrai(5)/sqr
        dstrai(6)=dstrai(6)/sqr
        stresm(5)=stresm(5)/sqr
        stresm(6)=stresm(6)/sqr
    endif
!
! Reading material linear elastic properties
!    nomres(1)= 'ALPHA   '
    nomres(1)= 'E       '
    nomres(2)= 'E       '
    nomres(3)= 'NU      '
    call rcvala(imate, ' ', 'ELAS', 0, '   ', &
                [tmpp], 3, nomres, rprops, icode, 2)
!
! Reading material Mohr-Coulomb properties
    nomres(1)= 'PHI     '
    nomres(2)= 'ANGDIL  '
    nomres(3)= 'COHESION'
    call rcvala(imate, ' ', 'MOHR_COULOMB', 0, '   ', &
                [tmpp], 3, nomres, rprops(4), icode(4), 2)
!
! Initialize some algorithmic and internal variables
    dgama =r0
    dgamb =r0
    dgamc =r0
    ifplas=r0
    sufail=r0
    edge  =r0
    apex  =r0
    tolcv =crit(3)
!
! Set some material properties
    young =rprops(2)
    poiss =rprops(3)
    sinphi=sin(degr*rprops(4))
    cosphi=cos(degr*rprops(4))
    sinpsi=sin(degr*rprops(5))
    cohe  =rprops(6)
!
! Set some constants
    gmodu =young/(r2*(r1+poiss))
    bulk  =young/(r3*(r1-r2*poiss))
    r2g   =r2*gmodu
    r4g   =r4*gmodu
    r2bulk=r2*bulk
    r2cphi=r2*cosphi
    r2spsi=r2*sinpsi
    r1d3  =r1/r3
!
! Compute elastic trial state
! ---------------------------
    call vecini(nmax, r0, strest)
    eetv  =dstrai(1)+dstrai(2)+dstrai(3)
    pt    =bulk*eetv
    eetvd3=eetv*r1d3
    strest(1)=stresm(1)+r2g*(dstrai(1)-eetvd3)+pt
    strest(2)=stresm(2)+r2g*(dstrai(2)-eetvd3)+pt
    strest(3)=stresm(3)+r2g*(dstrai(3)-eetvd3)+pt
    strest(4)=(stresm(4)+r2g*dstrai(4))
    if (tridim) then
        strest(5)=(stresm(5)+r2g*dstrai(5))
        strest(6)=(stresm(6)+r2g*dstrai(6))
    endif
!
! Compute elastic trial strain
! ----------------------------
! Elastic trial volumetric strain and pressure stress
! Be careful that we have the following form of input strain:
!
!          (Sqrt(2)*EPXY Sqrt(2)*EPXZ Sqrt(2)*EPYZ)
!
    call vecini(nmax, r0, strait)
    sttv  =strest(1)+strest(2)+strest(3)
    r1nu  =(r1+poiss)/young
    r1ny  =poiss/young
    strait(1)=r1nu*strest(1)-r1ny*sttv
    strait(2)=r1nu*strest(2)-r1ny*sttv
    strait(3)=r1nu*strest(3)-r1ny*sttv
    strait(4)=r1nu*strest(4)
    if (tridim) then
        strait(5)=r1nu*strest(5)
        strait(6)=r1nu*strest(6)
    endif
!
! Compute eigen-stress for prediction: pstrs1, pstrs2, pstrs3
    call mcpstr(strest, tridim, pstrs, eigprj, ii, &
                jj, mm, codret)
!
    pstrs1=pstrs(ii)
    pstrs2=pstrs(mm)
    pstrs3=pstrs(jj)
!
! ==================================================================
!
! 0. Is the behaviour plastic?
!
! ==================================================================
!
! Compute trial yield function and check for plastic consistency
! --------------------------------------------------------------
    smcta=pstrs1-pstrs3+(pstrs1+pstrs3)*sinphi
    phia =smcta-r2cphi*cohe
    res  =phia
    if (cohe .ne. r0) res=res/abs(cohe)
!
    if (res .gt. tol) then
!
! Plastic step: Apply return mapping
! ==================================
        ifplas=r1
! identify possible edge return: either right or left of main plane
! A partir de la prediction, il est possible de dire a coup sur si le
! retour radial est a gauche ou a droite
        scaprd=pstrs1*(r1-sinpsi)+pstrs2*(-r2)+pstrs3*(r1+sinpsi)
        if (scaprd .ge. r0) then
            right=r1
        else
            right=r0
        endif
! ==================================================================
!
! 1. Apply one-vector return mapping first (return to MAIN PLANE)
!
! ==================================================================
        sphsps=sinphi*sinpsi
        consta=r4g*(r1+r1d3*sphsps)+r4*bulk*sphsps
        r4c2ph=r2cphi*r2cphi
!
! Compute Newton-Raphson increment and update variable DGAMA
        ddgama=phia/consta
        dgama =dgama+ddgama
!
! Check validity of 1-vector return (check sextant of converged stress)
        s1=pstrs1-(r2g*(r1+r1d3*sinpsi)+r2bulk*sinpsi)*dgama
        s2=pstrs2+(r4g*r1d3-r2bulk)*sinpsi*dgama
        s3=pstrs3+(r2g*(r1-r1d3*sinpsi)-r2bulk*sinpsi)*dgama
        delta=dmax1(abs(s1),abs(s2),abs(s3))*small
!
! Compute new residual
        phia=smcta-consta*dgama-r2cphi*cohe
!
! Check convergence
        resnor=abs(phia)
        factor=abs(smcta)
        if (factor .ne. r0) resnor=resnor/factor
!
        if ( (s1+delta.ge.s2) .and. (s2+delta.ge.s3) &
             .and.(resnor.le.tolcv) ) then
!
! converged stress is in the same sextant as trial stress -> 1-vector
! return is valid.
!
            p=(s1+s2+s3)*r1d3
            goto 70
        else
!
! converged stress is not in the same sextant -> 1-vector result is
! not valid. Go to two-vector return map to edge
            goto 30
        endif
! failure of stress update procedure
        sufail=r1
        codret=1
!
! (-_-) goto 999 if a problem occured
        goto 999
 30     continue
! ==================================================================
!
! 2. Apply two-vector return mapping to appropriate EDGE
!
! ==================================================================
        ifplas = r2
!
        dgama=r0
        dgamb=r0
        smcta=pstrs1-pstrs3+(pstrs1+pstrs3)*sinphi
        if (right .eq. r1) then
            smctb=pstrs1-pstrs2+(pstrs1+pstrs2)*sinphi
        else
            smctb=pstrs2-pstrs3+(pstrs2+pstrs3)*sinphi
        endif
        phia=smcta-r2cphi*cohe
        phib=smctb-r2cphi*cohe
        if (right .eq. r1) then
            constb=r2g*(r1+sinphi+sinpsi-r1d3*sphsps)+r4*bulk*sphsps
        else
            constb=r2g*(r1-sinphi-sinpsi-r1d3*sphsps)+r4*bulk*sphsps
        endif
!
! Compute residual derivative matrix
        drvaa=-consta
        drvab=-constb
        drvba=-constb
        drvbb=-consta
!
! Compute Newton-Raphson increment and update variables DGAMA and DGAMB
        r1ddet=r1/(drvaa*drvbb-drvab*drvba)
        ddgama=(-drvbb*phia+drvab*phib)*r1ddet
        ddgamb=(drvba*phia-drvaa*phib)*r1ddet
        dgama=dgama+ddgama
        dgamb=dgamb+ddgamb
!
! Compute new residual
        phia=smcta-consta*dgama-constb*dgamb-r2cphi*cohe
        phib=smctb-constb*dgama-consta*dgamb-r2cphi*cohe
!
! Check convergence
        resnor=(abs(phia)+abs(phib))
        factor=(abs(smcta)+abs(smctb))
        if (factor .ne. r0) resnor=resnor/factor
!
        if (resnor .le. tolcv) then
! Check validity of 2-vector return to edge
            aux1=r2g*(r1+r1d3*sinpsi)+r2bulk*sinpsi
            aux2=(r4g*r1d3-r2bulk)*sinpsi
            aux3=r2g*(r1-r1d3*sinpsi)-r2bulk*sinpsi
            if (right .eq. r1) then
                s1=pstrs1-aux1*(dgama+dgamb)
                s2=pstrs2+aux2*dgama+aux3*dgamb
                s3=pstrs3+aux3*dgama+aux2*dgamb
            else
                s1=pstrs1-aux1*dgama+aux2*dgamb
                s2=pstrs2+aux2*dgama-aux1*dgamb
                s3=pstrs3+aux3*(dgama+dgamb)
            endif
            delta=dmax1(abs(s1),abs(s2),abs(s3))*small
!
            if (s1+delta .ge. s2 .and. s2+delta .ge. s3) then
!
! converged stress is in the same sextant as trial stress -> 2-vector
! return to edge is valid.
                edge=r1
                p=(s1+s2+s3)*r1d3
!
                goto 70
            else
!
! converged stress is not in the same sextant -> 2-vector return to edge
! is not valid. Go to three-vector return map to APEX
                goto 50
            endif
        endif
! failure of stress update procedure
        sufail=r2
        codret=1
!
! (-_-) goto 999 if a problem occured
        goto 999
 50     continue
! ==================================================================
!
! 3. Apply multi-vector return mapping to APEX
!
! ==================================================================
        ifplas = r3
!
! Check conditions for which return to apex does not make sense
        if (sinphi .eq. r0) then
            codret=1
            goto 999
        endif
!
        if (sinpsi .eq. r0) then
            codret=1
            goto 999
        endif
!
        smcta =pstrs1-pstrs3+(pstrs1+pstrs3)*sinphi
        smctb =pstrs1-pstrs2+(pstrs1+pstrs2)*sinphi
        smctc =pstrs2-pstrs3+(pstrs2+pstrs3)*sinphi
        phi(1)  =(smcta-r2cphi*cohe)/r2
        phi(2)  =(smctb-r2cphi*cohe)/r2
        phi(3)  =(smctc-r2cphi*cohe)/r2
!  C =
        consta=gmodu*r1d3+bulk
!  B =
        constb=-r2g*r1d3+bulk
!  D =
        drvaa=(consta+constb)*sinphi*sinpsi
!  E =
        drvbb=r2*(gmodu+consta*sinphi*sinpsi)
!
        mat(1,1)=drvbb
        mat(2,2)=drvbb
        mat(3,3)=drvbb
        mat(1,2)=drvaa+gmodu*(r1+sinpsi+sinphi)
        mat(1,3)=drvaa+gmodu*(r1-sinpsi-sinphi)
        mat(2,3)=drvaa+gmodu*(-r1-sinpsi+sinphi)
        mat(2,1)=drvaa+gmodu*(r1+sinpsi+sinphi)
        mat(3,1)=drvaa+gmodu*(r1-sinpsi-sinphi)
        mat(3,2)=drvaa+gmodu*(-r1+sinpsi-sinphi)
!
! Compute Plastic Multipliers
        call mgauss('NFSP', mat, phi, 3, 3, 1, r1ddet, codret)
        
        if (codret .eq. 1) then
            goto 999
        endif
!
        dgama=phi(1)
        dgamb=phi(2)
        dgamc=phi(3)
!
! Compute new residual   
        phia  =smcta- r2cphi*cohe &
               - r2*drvbb*dgama &
               - r2*(drvaa+gmodu*(r1+sinpsi+sinphi))*dgamb &
               - r2*(drvaa+gmodu*(r1-sinpsi-sinphi))*dgamc
!
        phib  =smctb- r2cphi*cohe &
               - r2*drvbb*dgamb &
               - r2*(drvaa+gmodu*(r1+sinpsi+sinphi))*dgama &
               - r2*(drvaa+gmodu*(-r1-sinpsi+sinphi))*dgamc
!
        phic  =smctc- r2cphi*cohe &
               - r2*drvbb*dgamc &
               - r2*(drvaa+gmodu*(r1-sinpsi-sinphi))*dgama &
               - r2*(drvaa+gmodu*(-r1+sinpsi-sinphi))*dgamb
!
! Check convergence
        resnor=(abs(phia)+abs(phib)+abs(phic))
        factor=(abs(smcta)+abs(smctb)+abs(smctc))
        if (factor .ne. r0) resnor=resnor/factor
!
        if (resnor .le. tolcv) then
!
! Set initial guess for volumetric plastic strain increment DEPV
            cotphi=cosphi/sinphi
            res   =cotphi*cohe-pt
! volumetric plastic strain
            ddepv =-res/bulk
! final volumetric stress
            p     =pt-bulk*ddepv
!
! update principal stresses
            s1  =p
            s2  =p
            s3  =p
            apex=r1
!
            goto 70
        endif
! Projection to apex failure
        sufail=r3
        codret=1
!
! (-_-) goto 999 if a problem occured
        goto 999
! ==================================================================
!
! 4. END OF INTEGRATION: CONTINUE IF NO PROBLEM OCCURED
!
! ==================================================================
 70     continue
! ------------------------------------------------------
!
! 4.1. update internal variables and output stress components
!
! ------------------------------------------------------
        call vecini(nmax, r0, tr)
        tr(ii)=s1
        tr(jj)=s3
        tr(mm)=s2
        call bptobg(tr, stres, eigprj)
!
! Attention: On re-projete les contraintes dans la base de Voigt
        do 80 i = mmax+1, nmax
            stres(i)=sqr*stres(i)
 80     continue
!
! Update internal variables
        if (apex .eq. r1) then
! return to apex
            vip(1)=vim(1)+r2spsi*(dgama+dgamb+dgamc)
!
            aux1=r1d3*sinpsi+r1
            aux2=r2*r1d3*sinpsi
            aux3=r1d3*sinpsi-r1
!
            smcta =dgama*aux1+dgamb*aux1+dgamc*aux2
            smctb =dgama*aux2+dgamb*aux3+dgamc*aux1
            smctc =dgama*aux3+dgamb*aux2+dgamc*aux3
!
            vip(2)=vim(2)+ sqrt(r3/r2*(smcta*smcta+ &
                           smctb*smctb+smctc*smctc))
!
        else if ((edge.eq.r1) .and. (right.eq.r1)) then
! return to the right edge
            vip(1)=vim(1)+r2spsi*(dgama+dgamb)
!
            aux1=r1d3*sinpsi+r1
            aux2=r2*r1d3*sinpsi
            aux3=r1d3*sinpsi-r1
!
            smcta =dgama*aux1+dgamb*aux1
            smctb =dgama*aux2+dgamb*aux3
            smctc =dgama*aux3+dgamb*aux2
!
            vip(2)=vim(2)+ &
            sqrt(r3/r2*(smcta*smcta+smctb*smctb+smctc*smctc))
!
        else if (edge.eq.r1) then
! return to the left edge
            vip(1)=vim(1)+r2spsi*(dgama+dgamb)
!
            aux1=r1d3*sinpsi+r1
            aux2=r2*r1d3*sinpsi
            aux3=r1d3*sinpsi-r1
!
            smcta =dgama*aux1+dgamb*aux2
            smctb =dgama*aux2+dgamb*aux1
            smctc =dgama*aux3+dgamb*aux3
!
            vip(2)=vim(2)+ &
            sqrt(r3/r2*(smcta*smcta+smctb*smctb+smctc*smctc))
!
        else
! return to plane
            vip(1)=vim(1)+r2spsi*dgama
            vip(2)=vim(2)+sqrt(sinpsi*sinpsi+r3)*dgama
        endif
! ------------------------------------------------------
!
! 4.2. Elastic step: update stress using linear elastic law
!
! ------------------------------------------------------
    else
!
        do 90 i = 1, mmax
            stres(i)     =strest(i)
            stres(mmax+i)=sqr*strest(mmax+i)
 90     continue
!
! Update internal variables
        vip(1)=vim(1)
        vip(2)=vim(2)
    endif
! ------------------------------------------------------
!
! 4.3. Update algorithmic variables
!      and Check criterium before exit
!
! ------------------------------------------------------
    vip(3)=ifplas
!
! ================================================================
!
!         If OPTION ='FULL_MECA', compute
!         Consistent Stiffness Matrix
!         ---------------------------
!
!         If OPTION ='RIGI_MECA', compute
!         Tangent Stiffness Matrix
!         ---------------------------
!
! ================================================================
!
    if (option(1:9) .eq. 'RIGI_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
!
! --------------------------------------
!
!   Compute Elastic Stiffness Matrix
!
! --------------------------------------
        call mctgel(dsidep, rprops)
!
        if (ifplas .eq. r0) goto 888
!
! -----------------------------------------
!
! Plastic Stiffness Matrix in the principal base
! Compute DPSTRS needed to compute the general stiffness matrix
!
! =========================================
!
!           Tridimensionnal case
!
! =========================================
!
        if (sufail .eq. r0 .and. tridim) then
!
            call mctg3d(stres, strait, rprops, dsidep, ii, jj, mm, &
                        edge, right, apex, codret)
!
            if (codret .eq. 1) goto 999
!
! =========================================
!
!            Bidimensionnal case
!
! =========================================
!
        else if (sufail.eq.r0) then
!
!
            call mctg2d(stres, strait, rprops, dsidep, ii, jj, mm, &
                        edge, right, apex, outofp)
!
! Endif SUFAIL
        endif
! Endif OPTION
    endif
!
888 continue
    codret=0
999 continue
!
end subroutine
