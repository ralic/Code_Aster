subroutine lcmohr(ndim, typmod, imate, option, tmpp,&
                  dstrai, stresm, stres, vim, vip,&
                  dsidep, codret)
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT DE MOHR-COULOMB
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  IMATE   : NATURE DU MATERIAU
! IN  STRAIN  : DEFORMATION EN T-
! IN  DSTRAI  : INCREMENT DE DEFORMATION
! IN  STRESM  : CONTRAINTE EN T-
! IN  VIM     : VARIABLES INTERNES EN T-
! IN  TMPM    : TEMPERATURE EN T-
! IN  TMPP    : TEMPERATURE EN T+
! IN  TMPREF  : TEMPERATURE DE REFERENCE
! IN  OPTION  : OPTION DEMANDEE
!                 RIGI_MECA_TANG ->     DSIDEP
!                 FULL_MECA      -> STRES DSIDEP VIP
!                 RAPH_MECA      -> STRES        VIP
!
! OUT STRES   : CONTRAINTE
! OUT VIP     : VARIABLES INTERNES
!                 1   -> DEFORMATION PLASTIQUE VOLUMIQUE
!                 2   -> NORME DE LA DEFORMATION PLASTIQUE DEVIATORIQUE
!                 3   -> INDICATEUR DE PLASTICITE:
!                        0: NON  1: OUI
!
! OUT DSIDEP  : MATRICE TANGENTE
! OUT CODRET  : CODE RETOUR
!                 0: OK   1: NOOK
! ----------------------------------------------------------------------
    implicit none
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
    integer :: ndim
    character(len=8) :: typmod(*)
    integer :: imate
    character(len=16) :: option
    real(kind=8) :: tmpp
    real(kind=8) :: dstrai(6)
    real(kind=8) :: stresm(6)
    real(kind=8) :: stres(6)
    real(kind=8) :: vim(*)
    real(kind=8) :: vip(*)
    real(kind=8) :: dsidep(*)
    integer :: codret
!
#include "asterf_types.h"
#include "asterfort/bptobg.h"
#include "asterfort/jacobi.h"
#include "asterfort/lceqvn.h"
#include "asterfort/mctg2d.h"
#include "asterfort/mctg3d.h"
#include "asterfort/mctgel.h"
#include "asterfort/rcvala.h"
#include "asterfort/vecini.h"
!
! Declaration of constant parameters
    integer :: mmax, nmax
    real(kind=8) :: degr
    parameter&
     &(   degr=0.017453292519943295d0   ,mmax=3   ,nmax=6   )
!
! Declaration of real type variables
    real(kind=8) :: rprops(nmax), strait(nmax), vaux(mmax)
    real(kind=8) :: strest(nmax), young, poiss
    real(kind=8) :: sinphi, cosphi, sinpsi
    real(kind=8) :: cohe, eigprj(mmax, mmax), pstrs(mmax), gmodu, bulk, r2g, r4g
    real(kind=8) :: r2bulk, r2cphi, r2spsi, r1d3, eetv
    real(kind=8) :: pt, eetvd3, pstrs1, pstrs2, pstrs3, smct, phia, phib, res, scaprd, sphsps
    real(kind=8) :: consta, r4c2ph, ddgama, dgama, delta, dmax1, smcta, smctb, constb, drvaa
    real(kind=8) :: drvab, drvba, drvbb, r1ddet, ddgamb, dgamb, resnor, factor, aux1, aux2, aux3
    real(kind=8) :: depv, cotphi, ddepv, s1, s2, s3, sttv
    real(kind=8) :: r0, r1, r2, r3, r4, small, tol, sqr, apex, edge, ifplas, right, sufail
    real(kind=8) :: tu(nmax), tr(nmax), p, r1nu, r1ny, t1(nmax)
!
! Declaration of integer type variables
    integer :: icode(nmax), itri, iorder, ndt, ndi, ii, jj, mm
    integer :: mxiter, i, j, itjac1
!
! Declaration of integer type variables
    aster_logical :: epflag, tridim, outofp
! Declaration of character type variables
    character(len=8) :: mod
    character(len=16) :: nomres(3)
!
! Declaration of constant variables
    data&
     &    r0   ,r1   ,r2   ,r3   ,r4   ,small ,tol   ,sqr   /&
     &    0.0d0,1.0d0,2.0d0,3.0d0,4.0d0,1.d-06,1.d-10,&
     &    1.4142135623730951d0                              /
    data&
     &    mxiter / 50 /
!
! Declaration of Common space variables
    common / tdim  / ndt   ,ndi
    common / debug / epflag
!
! Switch debugging On or Off
! ------------------------------------
    epflag=.false.
!      EPFLAG=.TRUE.
!
! Is the problem tridimensional?
    if (ndim .eq. 3) then
        tridim=.true.
    else
        tridim=.false.
    endif
! Modelisation type:
! 3D, D_PLAN, C_PLAN, AXIS
    mod=typmod(1)
    if (mod(1:6) .eq. 'C_PLAN') then
        outofp=.false.
    else
        outofp=.true.
    endif
!
    if (epflag) then
        write(6,'(A)')
        write(6,'(A)')'----- IN MOHR_COULOMB -----'
        write(6,'(A,A)')'> MODELISATION=',mod
        write(6,'(3(A,I3))')'> NDIM=',ndim,' NDT=',ndt,' NDI=',ndi
    endif
!
! Reading material linear elastic properties
    nomres(1)= 'ALPHA   '
    nomres(2)= 'E       '
    nomres(3)= 'NU      '
    call rcvala(imate, ' ', 'ELAS', 0, '   ',&
                [tmpp], 3, nomres, rprops, icode,&
                2)
!
! Reading material Mohr-Coulomb properties
    nomres(1)= 'PHI     '
    nomres(2)= 'ANGDIL  '
    nomres(3)= 'COHESION'
    call rcvala(imate, ' ', 'MOHR_COULOMB', 0, '   ',&
                [tmpp], 3, nomres, rprops(4), icode(4),&
                2)
!
! Initialize some algorithmic and internal variables
    dgama =r0
    dgamb =r0
    ifplas=r0
    sufail=r0
    edge  =r0
    apex  =r0
! Initialize unit matrix = (1 0 0 1 0 1) for Jacobi
    call vecini(nmax, r0, t1)
    t1(1) =r1
    t1(4) =r1
    t1(6) =r1
! Set some material properties
    young =rprops(2)
    poiss =rprops(3)
    sinphi=sin(degr*rprops(4))
    cosphi=cos(degr*rprops(4))
    sinpsi=sin(degr*rprops(5))
    cohe  =rprops(6)
! Set some constants
    gmodu =young/(r2*(r1+poiss))
    bulk  =young/(r3*(r1-r2*poiss))
    r2g   =r2*gmodu
    r4g   =r4*gmodu
    r2bulk=r2*bulk
    r2cphi=r2*cosphi
    r2spsi=r2*sinpsi
    r1d3  =r1/r3
! Compute elastic trial state
! ---------------------------
    call vecini(nmax, r0, strest)
    eetv  =dstrai(1)+dstrai(2)+dstrai(3)
    pt    =bulk*eetv
    eetvd3=eetv*r1d3
    strest(1)=stresm(1)+r2g*(dstrai(1)-eetvd3)+pt
    strest(2)=stresm(2)+r2g*(dstrai(2)-eetvd3)+pt
    strest(3)=stresm(3)+r2g*(dstrai(3)-eetvd3)+pt
    strest(4)=(stresm(4)+r2g*dstrai(4))/sqr
    if (tridim) then
        strest(5)=(stresm(5)+r2g*dstrai(5))/sqr
        strest(6)=(stresm(6)+r2g*dstrai(6))/sqr
    endif
!
    if (epflag) then
        write(6,'(A,6(1X,E12.5))')'> DSTRAI =',(dstrai(i),i=1,ndt)
        write(6,'(A,6(1X,E12.5))')'> STRESM =',(stresm(i),i=1,ndt)
        write(6,'(A,6(1X,E12.5))')'> STREST =',(strest(i),i=1,ndt)
    endif
! Compute elastic trial strain
! ---------------------------
! Elastic trial volumetric strain and pressure stress
! Be careful that we have the following form of input strain:
!          (Sqrt(2)*EPXY Sqrt(2)*EPXZ Sqrt(2)*EPYZ)
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
    if (epflag) write(6,'(A,6(1X,E12.5))')'> STRAIT =',(strait(i),i=1,ndt)
!
! Spectral decomposition of the trial stress
!
! ITRI =  0 : TRI EN VALEUR RELATIVE
!         1 : TRI EN VALEUR ABSOLUE
!         2 : PAS DE TRI
    itri  =2
! IORDER =  0 : TRI PAR ORDRE CROISSANT
!           1 : TRI PAR ORDRE DECROISSANT
!           2 : PAS DE TRI
    iorder=2
! Matrix  TR = (SIXX SIXY SIXZ SIYY SIYZ SIZZ) for Jacobi
! Produce EIGPRJ: Base Projection Matrix from initial base
!                 to principal directions base
!         PSTRS : principal stresses
    if (tridim) then
        tr(1)=strest(1)
        tr(2)=strest(4)
        tr(3)=strest(5)
        tr(4)=strest(2)
        tr(5)=strest(6)
        tr(6)=strest(3)
    else
        tr(1)=strest(1)
        tr(2)=strest(4)
        tr(3)=r0
        tr(4)=strest(2)
        tr(5)=r0
        tr(6)=r0
    endif
! Unit matrix = (1 0 0 1 0 1) for Jacobi
    call lceqvn(nmax, t1, tu)
!
    call jacobi(mmax, mxiter, tol, tol, tr,&
                tu, eigprj, pstrs, vaux, itjac1,&
                itri, iorder)
    if (.not.tridim) then
        eigprj(mmax,mmax)=r1
        pstrs(mmax) =strest(3)
    endif
!
    if (epflag) then
        write(6,'(A,3(1X,E12.5))')'> PSTRS =',(pstrs(i),i=1,3)
        write(6,'(A)')'> DIRPRJ='
        do i = 1, 3
            write(6,'(3(1X,E12.5))')(eigprj(i,j),j=1,3)
        enddo
        write(6,'(A)')'> EIGPRJ='
        do i = 1, 3
            write(6,'(3(1X,E12.5))') eigprj(i,1)*eigprj(i,1),eigprj(i,2)*&
        eigprj(i,2)
        enddo
        write(6,'(3(1X,E12.5))') eigprj(1,1)*eigprj(2,1),eigprj(1,2)*&
        eigprj(2,2)
    endif
!
! Identify minor (PSTRS1) and major (PSTRS3) principal stresses
    ii=1
    jj=1
    pstrs1=pstrs(ii)
    pstrs3=pstrs(jj)
    do i = 2, mmax
        if (pstrs(i) .ge. pstrs1) then
            ii=i
            pstrs1=pstrs(ii)
        endif
        if (pstrs(i) .lt. pstrs3) then
            jj=i
            pstrs3=pstrs(jj)
        endif
    end do
    if (ii .ne. 1 .and. jj .ne. 1) mm=1
    if (ii .ne. 2 .and. jj .ne. 2) mm=2
    if (ii .ne. 3 .and. jj .ne. 3) mm=3
    pstrs2=pstrs(mm)
!
    if (epflag) then
        write(6,'(3(A,I3))')'> II=',ii,' JJ=',jj,' MM=',mm
        write(6,'(3(A,1X,E12.5))') '> PSTRS1=',pstrs1,' PSTRS2=',&
        pstrs2,' PSTRS3=',pstrs3
    endif
!
! Compute trial yield function and check for plastic consistency
! --------------------------------------------------------------
    smct=pstrs1-pstrs3+(pstrs1+pstrs3)*sinphi
    phia=smct-r2cphi*cohe
    res =phia
    if (cohe .ne. r0) res=res/abs(cohe)
!
    if (epflag) write(6,'(A,3(1X,E12.5))')'> SEUIL EN INPUT =',res,phia
!
!
    if (res .gt. tol) then
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
! Apply one-vector return mapping first (return to MAIN PLANE)
! ------------------------------------------------------------
        sphsps=sinphi*sinpsi
        consta=r4g*(r1+r1d3*sphsps)+r4*bulk*sphsps
        r4c2ph=r2cphi*r2cphi
! Compute Newton-Raphson increment and update variable DGAMA
        ddgama=phia/consta
        dgama=dgama+ddgama
! Check validity of 1-vector return (check sextant of converged stress)
        s1=pstrs1-(r2g*(r1+r1d3*sinpsi)+r2bulk*sinpsi)*dgama
        s2=pstrs2+(r4g*r1d3-r2bulk)*sinpsi*dgama
        s3=pstrs3+(r2g*(r1-r1d3*sinpsi)-r2bulk*sinpsi)*dgama
        delta=dmax1(abs(s1),abs(s2),abs(s3))*small
! Compute new residual
        phia=smct-consta*dgama-r2cphi*cohe
        if (s1+delta .ge. s2 .and. s2+delta .ge. s3) then
! converged stress is in the same sextant as trial stress -> 1-vector
! return is valid.
!
            if (epflag) write(6, '(4(A,E12.5))') '> MAIN PLANE OK : S1=', s1, ' S2=', s2, ' S3=',&
                        s3, ' DELTA=', delta
!
            p=(s1+s2+s3)*r1d3
            goto 70
        else
! converged stress is not in the same sextant -> 1-vector result is
! not valid. Go to two-vector return map to edge
            if (epflag) write(6, '(4(A,E12.5))')'> MAIN PLANE NOOK : S1=', s1, ' S2=', s2,&
                        ' S3=', s3, ' DELTA=', delta
!
            goto 30
        endif
! failure of stress update procedure
        sufail=r1
        codret=1
!
        if (epflag) write (6,'(A)')'LCMOHR :: Projection Failure on plane'
!
        goto 999
 30     continue
!
! Apply two-vector return mapping to appropriate EDGE
! ---------------------------------------------------
        dgama=r0
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
! Compute residual derivative matrix
        drvaa=-consta
        drvab=-constb
        drvba=-constb
        drvbb=-consta
! Compute Newton-Raphson increment and update variables DGAMA and DGAMB
        r1ddet=r1/(drvaa*drvbb-drvab*drvba)
        ddgama=(-drvbb*phia+drvab*phib)*r1ddet
        ddgamb=(drvba*phia-drvaa*phib)*r1ddet
        dgama=dgama+ddgama
        dgamb=dgamb+ddgamb
! Compute new residual
        phia=smcta-consta*dgama-constb*dgamb-r2cphi*cohe
        phib=smctb-constb*dgama-consta*dgamb-r2cphi*cohe
! Check convergence
        resnor=(abs(phia)+abs(phib))
        factor=(abs(smcta)+abs(smctb))
        if (factor .ne. r0) resnor=resnor/factor
        if (resnor .le. tol) then
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
            if (s1+delta .ge. s2 .and. s2+delta .ge. s3) then
! converged stress is in the same sextant as trial stress -> 2-vector
! return to edge is valid.
                edge=r1
                p=(s1+s2+s3)*r1d3
!
                if (epflag) write(6, '(4(A,E12.5))') '> EDGE OK : S1=', s1, ' S2=', s2, ' S3=',&
                            s3, ' DELTA=', delta
!
                goto 70
            else
! converged stress is not in the same sextant -> 2-vector return to edge
! is not valid. Go to two-vector return map to APEX
                if (epflag) write(6, '(4(A,E12.5))') '> EDGE NOOK : S1=', s1, ' S2=', s2, ' S3=',&
                            s3, ' DELTA=', delta
!
                goto 50
            endif
        endif
! failure of stress update procedure
        sufail=r2
        codret=1
        if (epflag) write(6,'(A)')'LCMOHR :: Projection Failure on edge'
!
        goto 999
 50     continue
! Apply multi-vector return mapping to APEX
! ---------------------------------------
! Check conditions for which return to apex does not make sense
        if (sinphi .eq. r0) then
            write(6,'(A)') 'ECHEC DANS LCMOHR :: SINPHI = 0'
            codret=1
            goto 999
        endif
        if (sinpsi .eq. r0) then
            write(6,'(A)') 'ECHEC DANS LCMOHR :: SINPSI = 0'
            codret=1
            goto 999
        endif
! Set initial guess for volumetric plastic strain increment DEPV
        depv=r0
        cotphi=cosphi/sinphi
        res=cotphi*cohe-pt
        ddepv=-res/bulk
        depv=depv+ddepv
        p=pt-bulk*depv
        res=cotphi*cohe-p
! check for convergence
        resnor=abs(res)
!        IF(PT.NE.R0)RESNOR=RESNOR/ABS(PT)
        if (abs(pt) .gt. r1) resnor=resnor/abs(pt)
        if (resnor .le. small) then
            apex=r1
            dgama=depv
            dgamb=r0
! update principal stresses
            s1=p
            s2=p
            s3=p
!
            if (epflag) write (6,'(4(A,E12.5))') '> APEX OK : S1=',s1, ' S2=',s2,' S3=',s3
!
            goto 70
        endif
! Projection to apex failure
        sufail=r3
        codret=1
        if (epflag) write (6,'(A)') 'LCMOHR :: Projection Failure on apex'
!
        goto 999
 70     continue
! update internal variables and output stress components
! ------------------------------------------------------
        call vecini(nmax, r0, tr)
        tr(ii)=s1
        tr(jj)=s3
        tr(mm)=s2
        call bptobg(tr, stres, eigprj)
        do 80 i = mmax+1, nmax
            stres(i)=sqr*stres(i)
 80     continue
! Update internal variables
        if (apex .eq. r1) then
            vip(1)=vim(1)+p/bulk
            vip(2)=vim(2)
        else
            vip(1)=vim(1)+r2spsi*(dgama+dgamb)
            vip(2)=vim(2)+sqrt(sinpsi*sinpsi+r3)*(dgama+dgamb)
        endif
!
    else
! Elastic step: update stress using linear elastic law
! ====================================================
        do 90 i = 1, mmax
            stres(i) =strest(i)
            stres(mmax+i)=sqr*strest(mmax+i)
 90     continue
! Update internal variables
        vip(1)=vim(1)
        vip(2)=vim(2)
    endif
! Update algorithmic variables before exit
! ========================================
    vip(3)=ifplas
!
    if (epflag) then
        write(6,'(A)')'> END OF INTEGRATION :'
        write(6,'(A,6(1X,E12.5))')'> STRES =',(stres(i),i=1,6)
        write(6,'(A,15(1X,E10.3))')'> VIP =',(vip(i),i=1,15)
    endif
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
        if (epflag) then
            write(6,'(A,A)')'> OPTION =',option
            write(6,'(A)')'  ---- TANGENT MATRIX ----'
            if (ifplas .eq. r0) then
                write(6,'(A)')'> ELASTIC'
            else
                write(6,'(A)')&
     &'> ++++++++++++++++ PLASTIC ++++++++++++++++'
            endif
        endif
!
! Compute Elastic Stiffness Matrix
! --------------------------------------
        call mctgel(dsidep, rprops)
        if (ifplas .eq. r0) goto 888
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
            call mctg3d(stres, strait, rprops, dsidep, edge,&
                        right, apex, codret)
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
            if (epflag) then
                write(6,'(A)')'> 2D CASE'
                write(6,'(A,6(1X,E12.5))')'> STRES  =',(stres(i),i=1,ndt)
                write(6,'(A,6(1X,E12.5))')'> STRAIT =',(strait(i),i=1,&
                ndt)
            endif
!
            call mctg2d(stres, strait, rprops, dsidep, edge,&
                        right, apex, outofp)
!
! Endif SUFAIL
        endif
! Endif OPTION
    endif
!
888 continue
    codret=0
999 continue
end subroutine
