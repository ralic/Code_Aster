subroutine glrcad(zimat, mp1, mp2, delas, rpara,&
                  dmax1, dmax2, dam1, dam2, curvcu,&
                  c1, c2, nbackn, deps, depsp,&
                  df, ddiss, dsidep, normm, normn,&
                  crit, codret)
!
! aslint: disable=W1504
    implicit none
!-----------------------------------------------------------------------
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
! ======================================================================
!
!     INTEGRE LA LOI DE COMPORTEMENT GLRC_DAMAGE POUR UN INCREMENT
!     DE DEFORMATION DEPS DEFINIT DANS LE REPERE D ORTHOTROPIE
!
! IN  ZIMAT : ADRESSE DE LA LISTE DE MATERIAU CODE
! IN  DELAS : MATRICE ELASTIQUE EN MEMBRANE, FLEXION ET COUPLAGE
! IN  MP1 : MOMENTS LIMITES ELASTIQUES EN FLEXION POSITIVE
! IN  MP2 : MOMENTS LIMITES ELASTIQUES EN FLEXION NEGATIVE
! IN  RPARA : PARAMETRE MATERIAU DE LA LOI D ENDOMMAGEMENT
! IN  DMAX1 : ENDOMMAGEMENT MAX EN FLEXION +
! IN  DMAX2 : ENDOMMAGEMENT MAX EN FLEXION -
! IN  CURVCU : TENSEUR DES COURBURES ELASTIQUES DANS LE REPERE ORTHO
! IN  C1 : TENSEUR D ECROUISSAGE CINEMATIQUE DE PRAGER EN MEMBRANE
! IN  C2 : TENSEUR D ECROUISSAGE CINEMATIQUE DE PRAGER EN FLEXION
! IN  DEPS : INCREMENT DE DEFORMATION DANS LE REPERE ORTHO
! IN  NORMM : NORME SUR LA FONCTION MP = F(N)
! IN  NORMN : NORME SUR LA FONCTION MP = F(N)
!
! IN/OUT DAM1 : ENDOMMAGEMENT EN FLEXION +
! IN/OUT DAM2 : ENDOMMAGEMENT EN FLEXION -
! IN/OUT NBACKN : EFFORT - EFFORT DE RAPPEL
!
! OUT DEPSP : INCREMENT DE DEFORMATION PLASTIQUE DANS LE REPERE ORTHO
! OUT DF : INCREMENT D EFFORT DANS LE REPERE ORTHO
! OUT DDISS : INCREMENT DE DISSIPATION
! OUT DSIDEP : MATRICE TANGENTE
! person_in_charge: sebastien.fayolle at edf.fr
!
    include 'asterfort/assert.h'
    include 'asterfort/brbagl.h'
    include 'asterfort/critnu.h'
    include 'asterfort/d0mpfn.h'
    include 'asterfort/ddmpfn.h'
    include 'asterfort/dndiss.h'
    include 'asterfort/dxktan.h'
    include 'asterfort/fplass.h'
    include 'asterfort/matmul.h'
    include 'asterfort/mppffn.h'
    include 'asterfort/norrm6.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/tanmat.h'
    include 'blas/dcopy.h'
    logical :: bbok
!
    integer :: ncrit, ncrit2, ier, zimat
    integer :: i, j, kk, kkk, ipara(4), codret, kmax
!
    real(kind=8) :: delas(6, 6), alpha, beta, gamma, k1, k2, dmax1
    real(kind=8) :: dmax2, curvcu(3), c1(6, 6), c2(6, 6), deps(6), mp1(*)
    real(kind=8) :: mp2(*)
    real(kind=8) :: dam1, dam2, nbackn(6), normm, normn
    real(kind=8) :: depsp(6), ddiss, df(6), dsidep(6, 6)
    real(kind=8) :: dc1(6, 6), dc2(6, 6), reps(6), depste(6)
    real(kind=8) :: ddisst, depspt(6), depst2(6), zerode
    real(kind=8) :: dtg(6, 6), curcup(3), dcc1(3, 3), dcc2(3, 3), rpara(5)
    real(kind=8) :: zero, dfp(6), dfp2(6)
    real(kind=8) :: dff(3, 3), crit(*)
!
!---------------------------------------------
    real(kind=8) :: nmnbn(6), newnbn(6)
!         = FORCE - BACKFORCE
    real(kind=8) :: nmplas(2, 3), newpla(2, 3)
!         = PLASMOM(BENDING,_X _Y _XY)
    real(kind=8) :: nmdpla(2, 2), newdpl(2, 2)
!         = DPLASMOM(BENDING,_X _Y)
    real(kind=8) :: nmddpl(2, 2), newddp(2, 2)
!         = DDPLASMOM(BENDING,_X _Y)
    real(kind=8) :: nmzef, newzef
!         ZERO ADIMENSIONNEL POUR LE CRITERE F
    real(kind=8) :: nmzeg, newzeg, newzfg(2)
!         ZERO ADIMENSIONNEL POUR LE CRITERE G
    integer :: nmief, newief
!         NMIEF > 0 : NBN HORS DE LA ZONE DE DEFINITION DE MP
    integer :: nmprox(2), newpro(2)
!         NMPROX > 0 : NBN DANS ZONE DE CRITIQUE
!---------------------------------------------
!
    zero = crit(3)
    kmax = nint(crit(1))
    ncrit = 0
!
    alpha = rpara(1)
    beta = rpara(2)
    gamma = rpara(3)
    k1 = rpara(4)
    k2 = rpara(5)
!
    zerode = zero * norrm6(deps)
!
    do 10, i = 1,6
!     COPIE DU TENSEUR DES EFFORT - EFFORT DE RAPPEL
    nmnbn(i) = nbackn(i)
    10 end do
!
!     CALCUL DES MOMENTS LIMITES DE PLASTICITE
!     ET DES ZEROS DES CRITERES
    call mppffn(zimat, nmnbn, nmplas, nmzef, nmzeg,&
                nmief, normm)
!
!     CALCUL DES DERIVEES DES MOMENTS LIMITES DE PLASTICITE
    call d0mpfn(zimat, nmnbn, nmdpla)
!
!     CALCUL DES DERIVEES SECONDES DES MOMENTS LIMITES DE PLASTICITE
    call ddmpfn(zimat, nmnbn, nmddpl)
!
    newzef = nmzef
    newzeg = nmzeg
!
    call assert(nmief.le.0)
!
    do 30, j = 1,6
    do 20, i = 1,6
!     DTG : MATRICE TANGENTE
    dtg(i,j) = delas(i,j)
20  continue
    30 end do
!
    ddiss=0.d0
    call r8inir(6, 0.0d0, df, 1)
    call r8inir(6, 0.0d0, depsp, 1)
!
    do 40, i = 1,3
    curcup(i) = curvcu(i)
    40 end do
!
!     METHODE MIXTE POUR S ASSURER
!     D AVOIR f(m,backm)<= 0 A CHAQUE PAS DE TEMPS
!
    do 50, i = 1,6
!     REPS EST LE RESIDU DE L INCREMENT DE DEFORMATION
    reps(i) = deps(i)
    50 end do
!
    do 502, j = 1,6
    do 501, i = 1,6
!     DC1 : MATRICE ELASTIQUE + CONSTANTES DE PRAGER
!     DC2 : MATRICE ELASTIQUE + CONSTANTES DE PRAGER
    dc1(i,j) = dtg(i,j)+c1(i,j)
    dc2(i,j) = dtg(i,j)+c2(i,j)
501  continue
    502 end do
!
    do 229, kk = 1,kmax
    if (norrm6(reps) .le. zerode) then
!     TEST DE CV DE L ALGO D INTEGRATION
        goto 230
    endif
!
    do 60, i = 1,6
!     AFFECTATION DE L INCREMENT DE DEFORMATION TEST
    depste(i) = reps(i)
60  continue
!
!     CALCUL DE L ENDOMMAGEMENT ET DE LA MATRICE TANGENTE
    call tanmat(alpha, beta, gamma, k1, k2,&
                dmax1, dmax2, dam1, dam2, curcup,&
                depste(4), dff)
!
    do 63, j = 1,3
    do 62, i = 1,3
    dtg(i+3,j+3) = dff(i,j)
62  continue
63  continue
!
    do 80, j = 1,6
    do 70, i = 1,6
    dc1(i,j) = dtg(i,j)+c1(i,j)
    dc2(i,j) = dtg(i,j)+c2(i,j)
70  continue
80  continue
!
!     CALCUL DU PREDICTEUR ELASTIQUE ET DU NOMBRE DE CRITERE ATTEINT
    ncrit = critnu(zimat,nmnbn,depste,dtg,normm)
!
    do 122, kkk = 1,kmax
    do 90, j = 1,6
    depst2(j) = 0.5d0*depste(j)
90  continue
!
!     CALCUL DU PREDICTEUR ELASTIQUE ET DU NOMBRE DE CRITERE ATTEINT
    ncrit2 = critnu(zimat,nmnbn,depst2,dtg,normm)
!
    if (ncrit2 .ne. ncrit) then
        do 100, j = 1,6
        depste(j) = depst2(j)
100      continue
        ncrit=ncrit2
    else
        newzfg(1) = newzef
        newzfg(2) = newzeg
!
        ipara(1) = zimat
        ipara(2) = ncrit
!
!     CALCUL DU NOUVEAU MOMENT
!     DE L INCREMENT DE COURBURE PLASTIQUE ET DE LA DISSIPATION
        call dndiss(ipara, nmnbn, nmplas, nmdpla, nmddpl,&
                    nmprox, depste, newnbn, newpla, newdpl,&
                    newddp, newzfg, depspt, ddisst, dc1,&
                    dc2, dtg, normm, normn)
!
        zimat = ipara(1)
        ncrit = ipara(2)
        newief = ipara(3)
        ier = ipara(4)
!
        newzef = newzfg(1)
        newzeg = newzfg(2)
!
        if (ier .gt. 0) then
            do 110, j = 1,6
            depste(j) = depst2(j)
110          continue
            ncrit=ncrit2
        else
!     LE POINT EST DANS LA ZONE G < 0
!     SI LE NOUVEAU MOMENT EST A L EXTERIEUR DE LA SURFACE DE PLAST
!     LA METHODE BRINGBACK EST UTILISEE
!     POUR ESSAYER DE LE RAMENER SUR LA SURFACE
!
            if (fplass(newnbn,newpla,1) .gt. newzef .or. fplass(newnbn,newpla,2) .gt.&
                newzef) then
!
!     PROCEDURE BRINGBACK
                call brbagl(zimat, newnbn, newpla, newdpl, newddp,&
                            newzef, newzeg, newief, newpro, depspt,&
                            ddisst, dc1, dc2, dtg, bbok,&
                            normm, normn)
!
!     BRINGBACK OK : INCREMENT VALIDE
!
                if (bbok) goto 123
!
!     BRINGBACK NOK : DICHOTOMIE
                do 120, j = 1,6
                depste(j) = depst2(j)
120              continue
!
                ncrit = ncrit2
            else
                goto 123
            endif
        endif
    endif
122  continue
!
!     NON CONVERGENCE DE L ALGO DE DICHOTOMIE
    codret = 1
!
123  continue
!
!     L INCREMENT EST VALIDE : MISE A JOUR DES VARIABLES
!
    do 125, j = 1,6
    nmnbn(j) = newnbn(j)
125  continue
!
    do 140, j = 1,3
    do 130, i = 1,2
    nmplas(i,j) = newpla(i,j)
130  continue
140  continue
!
    do 160, j = 1,2
    do 150, i = 1,2
    nmdpla(i,j) = newdpl(i,j)
    nmddpl(i,j) = newddp(i,j)
150  continue
160  continue
!
    nmzef = newzef
    nmzeg = newzeg
    nmief = newief
!
    do 170, j = 1,2
    nmprox(j) = newpro(j)
170  continue
!
    do 180, j = 1,6
    depsp(j) = depsp(j) + depspt(j)
180  continue
!
    ddiss = ddiss + ddisst
!
    do 190, j = 1,3
    curcup(j) = curcup(j) + depste(j+3) - depspt(j+3)
190  continue
!
    do 200, j = 1,6
    dfp2(j) = depste(j) - depspt(j)
200  continue
!
    call matmul(dtg, dfp2, 6, 6, 1,&
                dfp)
!
    do 210, j = 1,6
    df(j) = df(j) + dfp(j)
210  continue
!
    do 220, j = 1,6
    reps(j) = reps(j) - depste(j)
220  continue
    229 end do
!
!     NON CONVERGENCE DE L ALGO D INTEGRATION
    codret = 1
!
230  continue
!
    do 240, j = 1,6
    nbackn(j) = nmnbn(j)
    240 end do
!
    do 270 i = 1, 3
        do 260 j = 1, 3
            dcc1(j,i) = dc1(3+j,3+i)
            dcc2(j,i) = dc2(3+j,3+i)
260      continue
270  end do
!
    call dcopy(36, delas, 1, dsidep, 1)
!
!     REALISE LE CALCUL DE LA MATRICE TANGENTE
    call dxktan(dtg, mp1, mp2, nbackn, ncrit,&
                dcc1, dcc2, dsidep)
!
end subroutine
