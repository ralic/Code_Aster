subroutine cjsinp(mater, epsd, deps, sigf, vinf,&
                  niter, nvi, nivcjs, ndec, epscon)
!
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!       ----------------------------------------------------------------
! CETTE ROUTINE COMPLETE LES VARIABLES INTERNES EN VUE DE DEPOUILLEMENT
!     LOI CJS
!     IN
!          MATER    :  COEFFICIENTS MATERIAU
!          NVI      :  NB DE VARIABLES INTERNES
!          EPSD     :  DEFORMATIONS A T
!          DEPS     :  INCREMENTS DE DEFORMATION
!          SIGF     :  CONTRAINTE  A T+DT
!     VAR
!          VINF     :  VARIABLES INTERNES  A T+DT
!             NDT  6 EN 3D ET 4 EN 2D
!
! DANS TOUS LES CAS
! -----------------
!       VINF(NDT+6) = NITER
!       VINF(NDT+7) = EPSCON
!       VINF(NDT+8) = NDEC
!
!       VINF(NDT+5) = ABS((I1F+QINIT)/(VINF(1)*TROIS))
!                     ETAT CONTRAINTE / CRITERE ISOTROPE
!                     DE 0 A 1
!
! EN CJS1
! -------
!         VINF(NDT+3)=ABS(QII*HTQ/(RM*(I1F+QINIT)))
!                     ETAT CONTRAINTE / CRITERE DEVIATOIRE
!                     DE 0 A 1
! EN CJS2
! -------
!
!         VINF(NDT+3)=ABS(QII*HTQ/(R*(I1F+QINIT)))
!                     ETAT CONTRAINTE / CRITERE DEVIATOIRE
!                     DE 0 A 1
!         VINF(NDT+4) = R/RM
!                     RAYON SURFACE PLASTIQUE DEVIATOIRE /
!                        RAYON SURFACE LIMITE DEVIATOIRE
!
! EN CJS3
! -------
!
!        VINF(NDT+3)=ABS(QII*HTQ/(R*(I1F+QINIT)))
!                     ETAT CONTRAINTE / CRITERE DEVIATOIRE
!                     DE 0 A 1
!        VINF(NDT+4) = XII/XIIL
!                     DEPLACEMENT SURFACE PLASTIQUE DEVIATOIRE /
!                        POSITION SURFACE LIMITE DEVIATOIRE
! ======================================================================
#include "asterfort/cjsqco.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lcprsc.h"
#include "asterfort/trace.h"
    integer :: ndt, ndi, nvi, niter, ndec, i
    real(kind=8) :: epsd(6), deps(6), sigf(6), vinf(nvi), epscon
    real(kind=8) :: mater(14, 2), rm, rc, pco, pa, pc
    real(kind=8) :: gamma, mucjs, epssig, pref, qinit
    real(kind=8) :: i1f, c, s(6), q(6), xf(6)
    real(kind=8) :: r, xii, epsv, phio, rr, cosa, cosdif
    real(kind=8) :: sii, hts, dets, cos3ts
    real(kind=8) :: siirel, qiirel, zero, un, deux, trois
    real(kind=8) :: qii, htq, detq, cos3tq
    real(kind=8) :: tangs, tangq, tetas, tetaq
    real(kind=8) :: xiil
    character(len=4) :: nivcjs
! ======================================================================
    parameter     ( un   = 1.d0   )
    parameter     ( zero = 0.d0   )
    parameter     ( deux = 2.d0   )
    parameter     ( trois= 3.d0   )
    parameter     ( epssig = 1.d-8   )
! ======================================================================
    common /tdim/   ndt, ndi
! ======================================================================
    call jemarq()
! ======================================================================
! --- PROPRIETES CJS MATERIAU ------------------------------------------
! ======================================================================
    rm = mater(2,2)
    rc = mater(5,2)
    c = mater(8,2)
    gamma = mater(9,2)
    mucjs = mater(10,2)
    pco = mater(11,2)
    pa = mater(12,2)
    qinit = mater(13,2)
! ======================================================================
! --- PREMIER INVARIANT ET AUTRES GRANDEURS UTILES ---------------------
! ======================================================================
    do 5 i = 1, ndt
        xf(i) = vinf(i+2)
 5  continue
! ======================================================================
    i1f = trace(ndi,sigf)
    if ((i1f+qinit) .eq. 0.d0) then
        i1f = -qinit+1.d-12 * pa
        pref = abs(pa)
    else
        pref = abs(i1f+qinit)
    endif
! ======================================================================
    call cjsqco(gamma, sigf, xf, pref, epssig,&
                i1f, s, sii, siirel, cos3ts,&
                hts, dets, q, qii, qiirel,&
                cos3tq, htq, detq)
! ======================================================================
    call lcprsc(xf, xf, xii)
    xii = sqrt(xii)
!
    epsv = zero
    do 30 i = 1, ndi
        epsv = epsv + epsd(i)+ deps(i)
30  continue
! ======================================================================
! --- CAS CJS3 ---------------------------------------------------------
! ======================================================================
    if (nivcjs .eq. 'CJS3') then
        pc = pco*exp(-c*epsv)
        if (xii .le. epssig) then
            phio= un
        else if (siirel .le. epssig) then
            cosa = un
            cosdif = un
            rr = rc + mucjs*max(zero,log(trois*pc/(i1f+qinit)))
            phio = cosa/( rr - hts/htq*rm*cosdif)
        else
            cosa = ( qii*qii - sii*sii - i1f*i1f*xii*xii ) / (deux* sii*i1f*xii)
!
            tangs = sqrt(un-cos3ts*cos3ts) / cos3ts
            tangq = sqrt(un-cos3tq*cos3tq) / cos3tq
            tetas = atan2(tangs,1.d0) / trois
            tetaq = atan2(tangq,1.d0) / trois
            cosdif = cos(tetas-tetaq)
!
            rr = rc + mucjs*max(zero,log(trois*pc/(i1f+qinit)))
            phio = cosa/( rr - hts/htq*rm*cosdif)
        endif
        xiil=un/(phio*hts)
    endif
!
    if (nivcjs .eq. 'CJS2' .or. nivcjs .eq. 'CJS3') then
        vinf(ndt+5) = abs((i1f+qinit)/(vinf(1)*trois))
    endif
    vinf(ndt+6) = niter
    vinf(ndt+7) = epscon
    vinf(ndt+8) = ndec
!
    if (nivcjs .eq. 'CJS1') then
        if ((abs(i1f+qinit)/pref) .lt. epssig) then
            vinf(ndt+3) = un
        else
            vinf(ndt+3)=abs(qii*htq/(rm*(i1f+qinit)))
        endif
    else if (nivcjs.eq.'CJS2') then
        r = vinf(2)
        vinf(ndt+4) = r/rm
        if ((abs(r*(i1f+qinit))/pref) .lt. epssig) then
            vinf(ndt+3) = un
        else
            vinf(ndt+3)=abs(qii*htq/(r*(i1f+qinit)))
        endif
    else if (nivcjs.eq.'CJS3') then
        r = vinf(2)
        vinf(ndt+4) = xii/xiil
        if ((abs(r*(i1f+qinit))/pref) .lt. epssig) then
            vinf(ndt+3) = un
        else
            vinf(ndt+3)=abs(qii*htq/(r*(i1f+qinit)))
        endif
    endif
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
