subroutine lkilnf(nvi, vind, nmat, materf, dt,&
                  sigd, nr, yd, yf, deps,&
                  vinf)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
! RESPOSABLE FOUCAULT A.FOUCAULT
! ----------------------------------------------------------------
!   POST-TRAITEMENTS SPECIFIQUES AU MODELE LETK
!
!   CORRESPONDANCE ENTRE LES VARIABLES INTERNES ET LES EQUATIONS
!          DU SYSTEME DIFFERENTIEL APRES INTEGRATION
!   VINF(1) = XIP (=YF(NDT+2)
!   VINF(2) = GAMMAP (=VIND(2)+YF(NDT+1)*G_II
!   VINF(3) = XIVP (=YF(NDT+3)
!   VINF(4) = GAMMAVP (=VIND(4)+DGAMMAVP)
!   VINF(5) = 0 OU 1 (CONTRACTANCE/DILATANCE)
!   VINF(6) = 0 OU 1 (INDICATEUR DE VISCOPLASTICITE)
!   VINF(7) = 0 OU 1 (INDICATEUR PLASTICITE)
!
! ----------------------------------------------------------------
!  IN
!     NVI    : NOMBRE DE VARIABLES INTERNES
!     VIND   : VARIABLE INTERNES A T
!     NMAT   : DIMENSION TABLEAU MATERIAU
!     MATERF : COEF MATERIAU A T+DT
!     DT     : INCREMENT DE TEMPS
!     SIGD   : ETAT DE CONTRAINTES A T
!     NR     : DIMENSION VECTEUR INCONNUES (YF/DY)
!     YD     : INCONNUES DU COMPORTEMENT INTEGRES A T
!     YF     : INCONNUES DU COMPORTEMENT INTEGRES A T+DT
!     DEPS   : INCREMENT DE DEFORMATIONS
!  OUT
!     VINF   :  VARIABLES INTERNES A T+DT
!       ----------------------------------------------------------------
    include 'asterfort/lcdevi.h'
    include 'asterfort/lkbpri.h'
    include 'asterfort/lkcalg.h'
    include 'asterfort/lkcaln.h'
    include 'asterfort/lkcrip.h'
    include 'asterfort/lkcriv.h'
    include 'asterfort/lkdfds.h'
    include 'asterfort/lkdgde.h'
    include 'asterfort/lkdhds.h'
    include 'asterfort/lkds2h.h'
    include 'asterfort/lkvacp.h'
    include 'asterfort/lkvarp.h'
    integer :: val, ndt, nvi, nmat, ndi, nr
    real(kind=8) :: materf(nmat, 2)
    real(kind=8) :: yd(*), vind(*), dt, deps(6)
    real(kind=8) :: yf(*), vinf(*), sigd(6)
!
    integer :: retcom, i
    real(kind=8) :: devsig(6), i1, xivmax, xippic, ucriv, seuilv
    real(kind=8) :: depsv(6), dgamv, seuilp, ucrip
    real(kind=8) :: varv, zero, seuivm, dhds(6), ds2hds(6)
    real(kind=8) :: paraep(3), varpl(4), dfdsp(6), bprimp
    real(kind=8) :: sigt(6)
    real(kind=8) :: vecnp(6), gp(6), devgii, deux, trois, un
!
    parameter       (zero  =  0.d0 )
    parameter       (un    =  1.d0 )
    parameter       (deux  =  2.d0 )
    parameter       (trois =  3.d0 )
!
    common /tdim/   ndt  , ndi
!     ------------------------------------------------------------------
! --- REMPLISSAGE DIRECT DE VINF(1) ET VINF(3)
    vinf(1) = max(yf(ndt+2),zero)
    vinf(3) = max(yf(ndt+3),zero)
!
! --------------------------------------------------------------------
! --- PASSAGE EN CONVENTION MECANIQUE DES SOLS
! --------------------------------------------------------------------
    do 10 i = 1, ndt
        sigt(i) = -yf(i)
10  continue
! ----------------------------------------------------------------------
! --- VARIABLES LOCALES TEMPORAIRES
! ----------------------------------------------------------------------
! --- CONSTRUCTION TENSEUR DEVIATOIRE DES CONTRAINTES ET 1ER INVARIANT
    call lcdevi(sigt, devsig)
    i1 = sigt(1)+sigt(2)+sigt(3)
!
! --- DONNEES MATERIAU : VALEUR MAX DE XIV; XI_PIC
    xivmax = materf(20,2)
    xippic = materf(18,2)
!
! ----------------------------------------------------------------------
! --- I) - BUT : CALCUL DE LA DEFORMATION VISQUEUSE -DEPSV- ET DU
! ---      PARAMETRE D ECROUISSAGE VISQUEUX -DGAMV-
! ----------------------------------------------------------------------
! --- I-1) INDICATEUR SUR ANGLE DE DILATANCE VISQUEUX PSI -> VAL = 0
    val = 0
!
! --- I-2) VARIABLE D'ECROUISSAGE VISQUEUSE VINTR = YF(NDT+3)
! --- I-3) CALCUL SEUIL VISQUEUX PAR RAPPORT A YF(1:6)=SIGF -> SEUILV
! --- I-3-1)  XIT   = YF(NDT+3)
    call lkcriv(yf(ndt+3), i1, devsig, vinf, nmat,&
                materf, ucriv, seuilv)
    if (seuilv .ge. zero) then
        call lkdgde(val, yf(ndt+3), dt, seuilv, ucriv,&
                    i1, devsig, vinf, nmat, materf,&
                    depsv, dgamv, retcom)
        vinf(4) = vind(4)+dgamv
! --- INDICATEUR DE VISCO-PLASTICITE
        vinf(6) = un
    else
        vinf(4) = vind(4)
        vinf(6) = zero
    endif
! ----------------------------------------------------------------------
! --- II) - BUT : CALCUL DE LA DEFORMATION PLASTIQUE -DEPSP- ET DU
! ---       PARAMETRE D ECROUISSAGE PLASTIQUE -DGAMP-
! ----------------------------------------------------------------------
! --- II-1) CALCUL FONCTION SEUIL PLASTIQUE EN YF
    seuilp = zero
!
    call lkcrip(i1, devsig, vinf, nmat, materf,&
                ucrip, seuilp)
!
! --- II-2)SI YF(NDT+1) > 0 ALORS PLASTICITE A PRENDRE EN COMPTE
    if (yf(ndt+1) .gt. zero) then
!
! --- INDICATEUR DE PLASTICITE
        vinf(7) = un
!
! --- II-2-A-1) INDICATEUR ANGLE DE DILATANCE PLASTIQUE PSI -> 0 OU 1
        if (yf(ndt+2) .le. xippic) then
            val = 0
        else
            val = 1
        endif
!
! --- II-2-A-2) INDICATEUR CONTRACTANCE OU DILATANCE -> VARV = 0 OU 1
! --- II-2-A-2)-1) CALCUL POSITION YF PAR RAPPORT SEUIL VISQUEUX MAX
        call lkcriv(xivmax, i1, devsig, vinf, nmat,&
                    materf, ucriv, seuivm)
!
! --- II-2-B-2)-2) TEST SUR SEUIL >0 OU <0 POUR DEFINIR VARV
        if (seuivm .le. zero) then
            varv = 0
        else
            varv = 1
        endif
        vinf(5) = varv
!
! --- II-2-A-3) CALCUL DE DF/DSIG
        call lkdhds(nmat, materf, i1, devsig, dhds,&
                    retcom)
        call lkds2h(nmat, materf, i1, devsig, dhds,&
                    ds2hds, retcom)
        call lkvarp(vinf, nmat, materf, paraep)
        call lkvacp(nmat, materf, paraep, varpl)
        call lkdfds(nmat, materf, devsig, paraep, varpl,&
                    ds2hds, ucrip, dfdsp)
!
! --- II-2-A-4) CALCUL DE G
        bprimp = lkbpri (val,vinf,nmat,materf,paraep,i1,devsig)
        call lkcaln(devsig, bprimp, vecnp, retcom)
        call lkcalg(dfdsp, vecnp, gp, devgii)
!
        vinf(2) = yf(ndt+1)*devgii*sqrt(deux/trois)+vind(2)
!
    else
! --- II-2-B) PAS DE PLASTICITE A PRENDRE EN COMPTE - DLAMBAP = 0.D0
        vinf(2) = vind(2)
! --- INDICATEUR DE PLASTICITE
        vinf(7) = zero
!
        call lkcriv(xivmax, i1, devsig, vinf, nmat,&
                    materf, ucriv, seuivm)
!
! --- II-2-B-2)-2) TEST SUR SEUIL >0 OU <0 POUR DEFINIR VARV
        if (seuivm .le. zero) then
            varv = 0
        else
            varv = 1
        endif
        vinf(5) = varv
!
    endif
!
end subroutine
