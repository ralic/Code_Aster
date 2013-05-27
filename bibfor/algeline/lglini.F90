subroutine lglini(yd, nbmat, mater, f0, sigd,&
                  deps, devg, devgii, traceg, dy,&
                  codret)
!
    implicit      none
    include 'jeveux.h'
    include 'asterc/r8prem.h'
    include 'asterfort/cos3t.h'
    include 'asterfort/dervar.h'
    include 'asterfort/gdev.h'
    include 'asterfort/hlode.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/lcdevi.h'
    include 'asterfort/lceqvn.h'
    include 'asterfort/lglind.h'
    include 'asterfort/lglinn.h'
    include 'asterfort/solrei.h'
    include 'asterfort/trace.h'
    include 'asterfort/varecr.h'
    include 'asterfort/wkvect.h'
    include 'blas/ddot.h'
    integer :: nbmat, codret
    real(kind=8) :: yd(10), mater(nbmat, 2), f0, sigd(6), deps(6)
    real(kind=8) :: devg(6), devgii, traceg, dy(10)
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! --- BUT : CALCUL INITIAL POUR LE PREMIER MULTIPLICATEUR PLASTIQUE ----
! ======================================================================
! IN  : YD     : (DEVIATEUR,1ER INVAVRIANT,GAMP,EVP,DELTA) ITERATION 0 -
! --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
! --- : MATER  : PARAMETRES MATERIAU -----------------------------------
! --- : F0     : VALEUR SEUIL A L'ITERATION 0 --------------------------
! --- : SIGD   : TENSEUR DES CONTRAINTES A L'INSTANT MOINS -------------
! --- : DEPS   : TENSEUR D'ACCROISSEMENT DES DEFORMATIONS --------------
! OUT : DEVG   : DEVIATEUR DU TENSEUR G --------------------------------
! --- : DEVGII : NORME DU DEVIATEUR DE G -------------------------------
! --- : TRACEG : TRACE DE G --------------------------------------------
! --- : DY     : INCREMENTS (DEVIATEUR, 1ER INVARIANT, GAMP, EVP, DELTA)
! ======================================================================
! ======================================================================
    logical :: teste
    integer :: jpara, jderiv, ii, ndt, ndi
    real(kind=8) :: se(6), gamp, delta, siie, pref, epssig
    real(kind=8) :: gamcjs, rcos3t, re, ge
    real(kind=8) :: q(6), vecn(6), ie
    real(kind=8) :: si(6), invn
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
    parecr = '&&LGLINI.PARECR'
    derive = '&&LGLINI.DERIVE'
    call wkvect(parecr, 'V V R', 5, jpara)
    call wkvect(derive, 'V V R', 4, jderiv)
! ======================================================================
! --- INITIALISATION DE DONNEES ----------------------------------------
! ======================================================================
    gamcjs = mater(12,2)
    pref = mater(15,2)
    call lceqvn(ndt, yd(1), se(1))
    call lceqvn(1, yd(ndt+1), ie)
    call lceqvn(1, yd(ndt+2), gamp)
    call lceqvn(1, yd(ndt+4), delta)
! ======================================================================
! --- CALCUL DES VARIABLES D'ECROUISSAGES ------------------------------
! ======================================================================
    call varecr(gamp, nbmat, mater, zr(jpara))
! ======================================================================
! --- CALCUL DES VARIABLES ELASTIQUES INITIALES ------------------------
! ======================================================================
    siie=ddot(ndt,se,1,se,1)
    siie = sqrt (siie)
    rcos3t = cos3t (se, pref, epssig)
    re = hlode (gamcjs, rcos3t)
    ge = gdev (siie, re)
! ======================================================================
! --- CALCUL DE Q ET DE N ----------------------------------------------
! ======================================================================
    teste = .false.
    do 10 ii = 1, ndt
        if (abs(sigd(ii)) .gt. epssig) teste = .true.
10  end do
    if (teste) then
        call lcdevi(sigd, si)
        invn = trace (ndi,sigd)
        call solrei(gamp, si, invn, zr(jpara), nbmat,&
                    mater, q, vecn, codret)
    else
        call solrei(gamp, se, ie, zr(jpara), nbmat,&
                    mater, q, vecn, codret)
    endif
    if (codret .ne. 0) goto 100
! ======================================================================
! --- INITIALISATION ---------------------------------------------------
! ======================================================================
    if (gamp .lt. r8prem()) then
! ======================================================================
! --- PREMIERE INITIALISATION POUR GAMP = 0 ----------------------------
! ======================================================================
        call lglind(nbmat, mater, zr(jpara), ge, q,&
                    vecn, deps, devg, devgii, traceg,&
                    dy)
    else
! ======================================================================
! --- INITIALISATION DE NEWTON -----------------------------------------
! ======================================================================
        call dervar(gamp, nbmat, mater, zr(jpara), zr(jderiv))
        call lglinn(nbmat, mater, zr(jpara), zr(jderiv), ge,&
                    ie, q, vecn, f0, delta,&
                    devg, devgii, traceg, dy)
    endif
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
