subroutine cjssmd(mater, sig, vin, seuild)
    implicit none
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!       CJS        :  SEUIL DU MECANISME DEVIATOIRE
!                     FD = QII HTQ + R I1
!       ----------------------------------------------------------------
!       IN  SIG    :  CONTRAINTE
!       IN  VIN    :  VARIABLES INTERNES = ( Q, R, X )
!       OUT SEUILD :  SEUIL  ELASTICITE DU MECANISME DEVIATOIRE
!       ----------------------------------------------------------------
    include 'asterfort/cjsqij.h'
    include 'asterfort/cos3t.h'
    include 'asterfort/hlode.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/lcdevi.h'
    include 'asterfort/lceqvn.h'
    include 'asterfort/lcprsc.h'
    include 'asterfort/trace.h'
    integer :: ndt, ndi
    real(kind=8) :: mater(14, 2), sig(6), vin(*), seuild, rcos3t, htq
    real(kind=8) :: r, x(6), i1, dev(6), q(6), qii, gamma, pa, qinit
    real(kind=8) :: pref, epssig
!       ----------------------------------------------------------------
    common /tdim/   ndt , ndi
! ======================================================================
! --- DEFINITION DE PARAMETRE ------------------------------------------
! ======================================================================
    parameter     ( epssig = 1.d-8 )
! ======================================================================
    call jemarq()
! ======================================================================
! --- VARIABLES INTERNES -----------------------------------------------
! ======================================================================
    r = vin(2)
    call lceqvn(ndt, vin(3), x)
! ======================================================================
! --- CARACTERISTIQUES MATERIAU ----------------------------------------
! ======================================================================
    gamma = mater( 9,2)
    pa = mater(12,2)
    qinit = mater(13,2)
! ======================================================================
! --- PRESSION DE REFERENCE --------------------------------------------
! ======================================================================
    i1 = trace(ndi,sig)
    if ((i1+qinit) .eq. 0.0d0) then
        i1 = -qinit + 1.d-12*pa
        pref = abs(pa)
    else
        pref = abs(i1+qinit)
    endif
! ======================================================================
! --- CALCUL DU TENSEUR Q ----------------------------------------------
! ======================================================================
    call lcdevi(sig, dev)
    call cjsqij(dev, i1, x, q)
    call lcprsc(q, q, qii)
    qii = sqrt(qii)
! ======================================================================
! --- CALCUL DE HTQ ----------------------------------------------------
! ======================================================================
    rcos3t = cos3t(q, pref, epssig)
    htq = hlode(gamma,rcos3t)
! ======================================================================
! --- CALCUL DU SEUIL DU MECANISME DEVIATOIRE --------------------------
! ======================================================================
    seuild = qii*htq + r*(i1+qinit)
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
