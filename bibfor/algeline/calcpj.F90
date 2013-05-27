subroutine calcpj(nbmat, mater, gamp, evp, sigd,&
                  sige, epssig, invare, gamps, evps,&
                  invars, b)
!
    implicit      none
    include 'jeveux.h'
    include 'asterfort/bprime.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/lcdevi.h'
    include 'asterfort/lceqvn.h'
    include 'asterfort/trace.h'
    include 'asterfort/varecr.h'
    include 'asterfort/wkvect.h'
    include 'blas/ddot.h'
    integer :: nbmat
    real(kind=8) :: mater(nbmat, 2), gamp, evp, sigd(6), sige(6), epssig
    real(kind=8) :: invare, gamps, invars, evps, b
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
! --- BUT : CALCUL DE LA PROJECTION AU SOMMET --------------------------
! ======================================================================
! IN  : NDT    : NOMBRE DE COMPOSANTES TOTALES DU TENSEUR --------------
! --- : NDI    : NOMBRE DE COMPOSANTES DIAGONALES DU TENSEUR -----------
! --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
! --- : MATER  : PARAMETRES MATERIAU -----------------------------------
! --- : GAMP   : DEFORMATION DEVIATOIRE PLASTIQUE CUMULEE --------------
! --- : EVP    : DEFORMATION VOLUMIQUE PLASTIQUE CUMULEE ---------------
! --- : SIIE   : NORME DU TENSEUR --------------------------------------
! --- : EPSSIG : EPSILON -----------------------------------------------
! --- : INVARE : PREMIER INVARIANT DU TENSEUR DES CONTRAINTES ELASTIQUE-
! OUT : GAMPS  : DEFORMATION DEVIATOIRE PLASTIQUE CUMULEE AU SOMMET ----
! --- : INVARS : PREMIER INVARIANT DU TENSEUR DES CONTRAINTES AU SOMMET-
! --- : EVPS   : DEFORMATION VOLUMIQUE PLASTIQUE CUMULEE AU SOMMET -----
! --- : B      : PARAMETRE CONTROLANT LE COMPORTEMENT VOLUMIQUE --------
! ------------ : DU MATERIAU -------------------------------------------
! ======================================================================
! ======================================================================
    integer :: jpara, jpara2, ndt, ndi
    real(kind=8) :: mu, sigc, sig(6), sd(6), sgamp, mgamp
    real(kind=8) :: zero, un, deux, trois, se(6)
    real(kind=8) :: sigii, siie, invar, epsult, gamult, k
    character(len=16) :: parecr, parec2
! ======================================================================
! --- INITIALISATION DE PARAMETRE --------------------------------------
! ======================================================================
    parameter       ( zero     =  0.0d0   )
    parameter       ( un       =  1.0d0   )
    parameter       ( deux     =  2.0d0   )
    parameter       ( trois    =  3.0d0   )
    parameter       ( epsult   =  1.0d-03 )
! ======================================================================
    common /tdim/   ndt , ndi
! ======================================================================
    call jemarq()
! ======================================================================
! --- RECUPERATION DE PARAMETRES DU MODELE -----------------------------
! ======================================================================
    mu = mater(4,1)
    k = mater(5,1)
    gamult = mater(1,2)
    sigc = mater(9,2)
! ======================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------------
! ======================================================================
    parecr = '&&CALCPJ.PARECR'
    parec2 = '&&CALCPJ.PAREC2'
    call wkvect(parecr, 'V V R', 5, jpara)
    call wkvect(parec2, 'V V R', 5, jpara2)
! ======================================================================
! --- CALCUL DES PROJECTIONS AU SOMMET ---------------------------------
! ======================================================================
    call lcdevi(sige, se)
    siie=ddot(ndt,se,1,se,1)
    siie = sqrt (siie)
    gamps = gamp + sqrt(deux/trois)*siie/(deux*mu)
    call varecr(gamps, nbmat, mater, zr(jpara))
    sgamp = zr(jpara-1+1)
    mgamp = zr(jpara-1+4)
    invars = trois*sigc*sgamp/mgamp
    evps = evp + (invare - invars)/(trois * k)
! ======================================================================
! --- CALCUL DU PARAMETRE DE DILATANCE B A L'INSTANT MOINS -------------
! ======================================================================
! --- CAS OU GAMP > GAMULT(1-EPS) --------------------------------------
! ======================================================================
    if (gamp .gt. (gamult*(un-epsult))) then
        b = zero
    else
! ======================================================================
! --- CAS OU GAMP <= GAMULT(1-EPS) -------------------------------------
! ======================================================================
        sigii=ddot(ndt,sigd,1,sigd,1)
        if (sigii .lt. epssig) then
            call lceqvn(ndt, sige, sig)
        else
            call lceqvn(ndt, sigd, sig)
        endif
        call lcdevi(sig, sd)
        invar = trace (ndi,sig)
        call varecr(gamp, nbmat, mater, zr(jpara2))
        b = bprime(nbmat,mater,zr(jpara2),invar,sd,epssig)
    endif
! ======================================================================
! --- DESTRUCTION DES VECTEURS INUTILES --------------------------------
! ======================================================================
    call jedetr(parecr)
    call jedetr(parec2)
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
