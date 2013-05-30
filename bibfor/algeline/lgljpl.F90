subroutine lgljpl(mod, nbmat, mater, sig, devg,&
                  devgii, vin, dsde, codret)
!
    implicit      none
    include 'jeveux.h'
    include 'asterfort/calcds.h'
    include 'asterfort/cos3t.h'
    include 'asterfort/dervar.h'
    include 'asterfort/drfdrg.h'
    include 'asterfort/drfdrs.h'
    include 'asterfort/drudrg.h'
    include 'asterfort/drudrs.h'
    include 'asterfort/gdev.h'
    include 'asterfort/hlode.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/lcdevi.h'
    include 'asterfort/lceqvn.h'
    include 'asterfort/lcinma.h'
    include 'asterfort/lcopli.h'
    include 'asterfort/solren.h'
    include 'asterfort/trace.h'
    include 'asterfort/varecr.h'
    include 'asterfort/wkvect.h'
    include 'blas/ddot.h'
    integer :: nbmat, codret
    real(kind=8) :: mater(nbmat, 2), sig(6), vin(*), dsde(6, 6)
    real(kind=8) :: devg(6), devgii
    character(len=8) :: mod
! ======================================================================
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
!
!
! ======================================================================
! ======================================================================
! --- BUT : CALCUL DE DSIG/DEPS ----------------------------------------
! ======================================================================
! IN  : MOD    : TYPE DE MODELISATION ----------------------------------
! --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
! --- : MATER  : PARAMETRES MATERIAU -----------------------------------
! --- : SIG    : TENSEUR DES CONTRAINTES -------------------------------
! --- : DEVG   : DEVIATEUR DU TENSEUR G --------------------------------
! --- : DEVGII : NORME DU DEVIATEUR DE G -------------------------------
! --- : VIN    : VARIABLES INTERNES ------------------------------------
! OUT : DSDE   : DSIG/DEPS ---------------------------------------------
! ======================================================================
! ======================================================================
    integer :: jpara, jderiv, ndt, ndi
    real(kind=8) :: epssig, sigc, gamcjs, pref, sn(6), snii, invn, h0
    real(kind=8) :: mun, gampn, rcos3t, rn, gn
    real(kind=8) :: duds(6), dudg, dfds(6), dfdg
    real(kind=8) :: q(6), hook(6, 6)
    character(len=16) :: parecr, derive
! ======================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------------
! ======================================================================
    parameter       ( epssig  =  1.0d-8 )
    parameter       ( mun     = -1.0d0  )
! ======================================================================
    common /tdim/   ndt , ndi
! ======================================================================
    call jemarq()
! ======================================================================
! --- DEFINITIONS ------------------------------------------------------
! ======================================================================
    parecr = '&&LGLJPL.PARECR'
    derive = '&&LGLJPL.DERIVE'
    call wkvect(parecr, 'V V R', 5, jpara)
    call wkvect(derive, 'V V R', 4, jderiv)
    call lcinma(0.0d0, hook)
    call lcinma(0.0d0, dsde)
! ======================================================================
! --- RECUPERATION DE PARAMETRES MATERIAU ------------------------------
! ======================================================================
    sigc = mater ( 9,2)
    gamcjs = mater (12,2)
    pref = mater (15,2)
    call lceqvn(1, vin(1), gampn)
! ======================================================================
! --- RECUPERATION DE LA MATRICE DE HOOK -------------------------------
! ======================================================================
    call lcopli('ISOTROPE', mod, mater(1, 1), hook)
! ======================================================================
! --- CALCULS INITIAUX DE VARIABLES INTERMEDIAIRES ---------------------
! ======================================================================
    call lcdevi(sig, sn)
    snii=ddot(ndt,sn,1,sn,1)
    snii = sqrt (snii)
    invn = trace (ndi, sig)
    h0 = hlode (gamcjs, mun)
! ======================================================================
! --- CALCULS DES VARIABLES D'ECROUISSAGES ET DE SES DERIVEES ----------
! ======================================================================
    call varecr(gampn, nbmat, mater, zr(jpara))
    call dervar(gampn, nbmat, mater, zr(jpara), zr(jderiv))
! ======================================================================
! --- CALCUL DES VARIABLES INITIALES -----------------------------------
! ======================================================================
    rcos3t = cos3t (sn, pref, epssig)
    rn = hlode (gamcjs, rcos3t)
    gn = gdev (snii, rn)
! ======================================================================
! --- CALCUL DE Q A L'ITERATION COURANTE -------------------------------
! ======================================================================
    call solren(sn, nbmat, mater, q, codret)
    if (codret .ne. 0) goto 100
! ======================================================================
! --- CALCUL DES DIFFERENTES DERIVEES ----------------------------------
! ======================================================================
! **********************************************************************
! --- CALCUL DE DUDS ---------------------------------------------------
! **********************************************************************
    call drudrs(zr(jpara), q, h0, sigc, duds)
! **********************************************************************
! --- CALCUL DE DUDG ---------------------------------------------------
! **********************************************************************
    call drudrg(zr(jpara), zr(jderiv), h0, sigc, gn,&
                invn, dudg)
! **********************************************************************
! --- CALCUL DE DFDS ---------------------------------------------------
! **********************************************************************
    call drfdrs(q, zr(jpara), h0, sigc, gn,&
                duds, dfds)
! **********************************************************************
! --- CALCUL DE DFDG ---------------------------------------------------
! **********************************************************************
    call drfdrg(zr(jpara), zr(jderiv), h0, sigc, gn,&
                dudg, dfdg)
! **********************************************************************
! ======================================================================
! --- CALCUL DE DSIG/DEPS ----------------------------------------------
! ======================================================================
    call calcds(hook, devg, devgii, dfds, dfdg,&
                dsde)
! ======================================================================
! --- DESTRUCTION DES VECTEURS INUTILES --------------------------------
! ======================================================================
100  continue
    call jedetr(parecr)
    call jedetr(derive)
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
