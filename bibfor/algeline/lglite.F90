subroutine lglite(yf, nbmat, mater, f0, devg,&
                  devgii, traceg, dy, codret)
!
    implicit      none
    include 'jeveux.h'
    include 'asterfort/calcdr.h'
    include 'asterfort/calcdy.h'
    include 'asterfort/cos3t.h'
    include 'asterfort/dervar.h'
    include 'asterfort/gdev.h'
    include 'asterfort/hlode.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/lceqvn.h'
    include 'asterfort/solren.h'
    include 'asterfort/varecr.h'
    include 'asterfort/wkvect.h'
    include 'blas/ddot.h'
    integer :: nbmat, codret
    real(kind=8) :: yf(10), mater(nbmat, 2), f0
    real(kind=8) :: devg(6), devgii, traceg, dy(10)
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
! --- BUT : CALCUL DES DIFFERENTS INCREMENTS ---------------------------
! ======================================================================
! IN  : YF     : (SIG, I1, GAMP, EVP, DELTA) A L'INSTANT COURANT -------
! --- : NR     : DIMENSION DE YD ---------------------------------------
! --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
! --- : MATER  : PARAMETRES MATERIAU -----------------------------------
! --- : F0     : VALEUR SEUIL A L'INSTANT 0 ----------------------------
! --- : DEVG   : DEVIATEUR DU TENSEUR G --------------------------------
! --- : DEVGII : NORME DU DEVIATEUR DU TENSEUR G -----------------------
! --- : TRACEG : TRACE DU TENSEUR G ------------------------------------
! OUT : DY     : INCREMENTS (DSIG, DI1, DGAMP, DEVP, DDELTA) -----------
! ======================================================================
! ======================================================================
    integer :: jpara, jderiv, ndt, ndi
    real(kind=8) :: pref, epssig, gamcjs, mu, k, snii, rn, gn
    real(kind=8) :: rcos3t
    real(kind=8) :: dfdl, sn(6), invn, gampn, evpn, deltan, q(6)
    character(len=16) :: parecr, derive
! ======================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------------
! ======================================================================
    parameter       ( epssig  = 1.0d-8 )
! ======================================================================
    common /tdim/   ndt , ndi
! ======================================================================
    call jemarq()
! ======================================================================
! --- DEFINITIONS ------------------------------------------------------
! ======================================================================
    parecr = '&&LGLITE.PARECR'
    derive = '&&LGLITE.DERIVE'
    call wkvect(parecr, 'V V R', 5, jpara)
    call wkvect(derive, 'V V R', 4, jderiv)
! ======================================================================
! --- RECUPERATION DE DONNEES ------------------------------------------
! ======================================================================
    mu = mater ( 4,1)
    k = mater ( 5,1)
    gamcjs = mater (12,2)
    pref = mater (15,2)
    call lceqvn(ndt, yf(1), sn(1))
    call lceqvn(1, yf(ndt+1), invn)
    call lceqvn(1, yf(ndt+2), gampn)
    call lceqvn(1, yf(ndt+3), evpn)
    call lceqvn(1, yf(ndt+4), deltan)
! ======================================================================
! --- CALCUL DES VARIABLES D'ECROUISSAGES ET DE SES DERIVEES -----------
! ======================================================================
    call varecr(gampn, nbmat, mater, zr(jpara))
    call dervar(gampn, nbmat, mater, zr(jpara), zr(jderiv))
! ======================================================================
! --- CALCUL DES VARIABLES ELASTIQUES INITIALES ------------------------
! ======================================================================
    snii=ddot(ndt,sn,1,sn,1)
    snii = sqrt (snii)
    rcos3t = cos3t (sn, pref, epssig)
    rn = hlode (gamcjs, rcos3t)
    gn = gdev (snii, rn)
! ======================================================================
! --- CALCUL DE Q ------------------------------------------------------
! ======================================================================
    call solren(sn, nbmat, mater, q, codret)
    if (codret .ne. 0) goto 100
! ======================================================================
! --- CALCUL DES DIFFERENTES DERIVEES ----------------------------------
! ======================================================================
    call calcdr(nbmat, mater, zr(jpara), zr(jderiv), gn,&
                invn, q, devg, devgii, traceg,&
                dfdl)
! ======================================================================
! --- CALCUL DES DIFFERENTS INCREMENTS ---------------------------------
! ======================================================================
    call calcdy(mu, k, f0, devg, devgii,&
                traceg, dfdl, deltan, dy)
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
