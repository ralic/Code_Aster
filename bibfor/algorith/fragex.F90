subroutine fragex(ndim, imate, instam, instap, epsm,&
                  deps, vim, option, sigp, vip,&
                  typmod, dsidep, codret)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE CRP_7 TOLE CRP_21
!
! VARIABLES ENTREE SORTIE NECESSAIRES A LA ROUTINE
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rcvala.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    include 'blas/ddot.h'
    character(len=8) :: typmod(*)
    character(len=16) :: option
    integer :: ndim, imate
    real(kind=8) :: instam, instap, epsm(6), deps(6), vim(2)
    real(kind=8) :: sigp(6), vip(2), dsidep(6, 6)
    integer :: codret
!
! VARIABLES ENTREE SORTIE INUTILES POUR CETTE ROUTINE
!
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT ELASTIQUE FRAGILE (SANS REGULARISATION)
!           INTEGRATION PAR LE SCHEMA IMPL-EX
!
!
!       ARGUMENTS UTILES A CETTE ROUTINE
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  IMATE   : NATURE DU MATERIAU
! IN  EPSM    : DEFORMATION EN T-
! IN  DEPS    : INCREMENT DE DEFORMATION
! IN  VIM     : VARIABLES INTERNES EN T-
! IN  INSTAM  : INSTANT T-
! IN  INSTAP  : INSTANT T+DT
! IN  OPTION  : OPTION DEMANDEE
!                 RIGI_MECA_IMPLEX -> SIGP DSIDEP
!                 RAPH_MECA        -> SIGP        VIP
!                 FULL_MECA        ->  INTERDIT
!                 RIGI_MECA_ELAS   ->  INTERDIT
! OUT SIGP    : CONTRAINTE EN T+
! OUT VIP     : VARIABLES INTERNES
!                 1   -> VALEUR DE L'ENDOMMAGEMENT
!                 2   -> INCREMENT DE L'ENDOMMAGEMENT/
!                        ------------------------------
!                        INCREMENT DE L'INSTANT (T- - T-- necessaire)
!
! OUT DSIDEP  : MATRICE TANGENTE
! OUT CODRET  : INDICATEUR DE REDECOUPAGE DU PAS DE TEMPS
!
!
!         ARGUMENTS INUTILES A CETTE ROUTINE QUE L'ON PEUT DEMANDER
!
!       IN      KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
!               COMPOR  COMPORTEMENT DE L ELEMENT
!                       COMP(1) = RELATION DE COMPORTEMENT (CHABOCHE...)
!                       COMP(2) = NB DE VARIABLES INTERNES
!                       COMP(3) = TYPE DE DEFORMATION (PETIT,JAUMANN...)
!               CRIT    CRITERES  LOCAUX
!                       CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
!                                 (ITER_INTE_MAXI == ITECREL)
!                       CRIT(2) = TYPE DE JACOBIEN A T+DT
!                                 (TYPE_MATR_COMP == MACOMP)
!                                 0 = EN VITESSE     > SYMETRIQUE
!                                 1 = EN INCREMENTAL > NON-SYMETRIQUE
!                       CRIT(3) = VALEUR DE LA TOLERANCE DE CONVERGENCE
!                                 (RESI_INTE_RELA == RESCREL)
!                       CRIT(5) = NOMBRE D'INCREMENTS POUR LE
!                                 REDECOUPAGE LOCAL DU PAS DE TEMPS
!                                 (ITER_INTE_PAS == ITEDEC)
!                                 0 = PAS DE REDECOUPAGE
!                                 N = NOMBRE DE PALIERS
!               TAMPON  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
!                       AUX LOIS DE COMPORTEMENT (DIMENSION MAXIMALE
!                       FIXEE EN DUR)
!               SIGM    CONTRAINTE A T
! ======================================================================
    logical :: cplan, tang, raph
!
    integer :: ndimsi, k, l
!
    real(kind=8) :: eps(6), treps, coplan, sigel(6)
    real(kind=8) :: kron(6), valres(3), dmax, fmin
    real(kind=8) :: fd, d, dm, dd, ener, dt, dddt
    real(kind=8) :: e, nu, lambda, deuxmu, gamma, sy, wy
!
    integer :: idret(3)
    character(len=8) :: nomres(3)
!
    parameter  (dmax = 1.d0, fmin = 1.d-5)
    data  kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
!
! ======================================================================
!                            INITIALISATION
! ======================================================================
!
! -- OPTION ET MODELISATION
!
    raph = option .eq. 'RAPH_MECA'
    tang = option .eq. 'RIGI_MECA_IMPLEX'
    cplan = typmod(1) .eq. 'C_PLAN  '
    ndimsi = 2 * ndim
    codret = 0
!
! -- LECTURE DES CARACTERISTIQUES ELASTIQUES
!
    nomres(1) = 'E'
    nomres(2) = 'NU'
    call rcvala(imate, ' ', 'ELAS', 0, ' ',&
                0.d0, 2, nomres, valres, idret,&
                1)
!
    e = valres(1)
    nu = valres(2)
    lambda = e * nu / (1.d0+nu) / (1.d0 - 2.d0*nu)
    deuxmu = e/(1.d0+nu)
!
!
! -- LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
!
    nomres(1) = 'SY'
    nomres(2) = 'D_SIGM_EPSI'
    call rcvala(imate, ' ', 'ECRO_LINE', 0, ' ',&
                0.d0, 2, nomres, valres, idret,&
                1)
    sy = valres(1)
    gamma = - valres(2)/e
    wy = sy**2 / (2*e)
!
!
! -- DEFORMATIONS
!
    call dcopy(ndimsi, epsm, 1, eps, 1)
    if (raph) call daxpy(ndimsi, 1.d0, deps, 1, eps,&
                         1)
!
! -- EXTRACTION DES VARIABLES
    dm = vim(1)
!      ETAT = NINT(VIM(2))
    dddt = vim(2)
    dt = instap-instam
!
! ======================================================================
!     CAS RIGI_MECA_TANG : EXTRAPOLATION VARIABLES INTERNES ET MATRICE
! ======================================================================
!
    if (tang) then
        if (dm .ne. 1) then
! -- DISTINCTION DES CAS : SI AVANT PAS ENDO, EXTRAPOL SANS ENDO
            if (dddt .eq. 0.d0) then
                dd = 0.d0
                d = dm + dd
!           ETAT = 0.D0
            else
                dd = dddt * dt
                d = dm + dd
!            ETAT = 1
                if (d .gt. dmax) then
                    d = dmax
!             ETAT = 2
                endif
            endif
        else
! -- SI LE PTG EST CASSE, PAS D'INCREMENT
            if (dddt .eq. 0.d0) then
                dd = 0.d0
                d = dm + dd
!            ETAT = 2
            else
                dd = 0.d0
                d = dm + dd
!          ETAT = 2
            endif
        endif
!
! -- CALCUL DES CONTRAINTES EXTRAPOLEES
!
        if (cplan) then
            coplan = - nu/(1.d0-nu)
            eps(3) = coplan * (eps(1)+eps(2))
        endif
!
        treps = eps(1)+eps(2)+eps(3)
        do 60 k = 1, ndimsi
            sigel(k) = lambda*treps*kron(k) + deuxmu*eps(k)
60      continue
!
        do 320 k = 1, ndimsi
            sigp(k) = (1.d0-d) * sigel(k)
320      continue
!
! -- MATRICE TANGENTE
!
! -- CONTRIBUTION ELASTIQUE
!
        call r8inir(36, 0.d0, dsidep, 1)
        fd = 1-d
        fd = max(fmin, fd)
        do 100 k = 1, 3
            do 110 l = 1, 3
                dsidep(k,l) = fd*lambda
110          continue
100      continue
!
        do 120 k = 1, ndimsi
            dsidep(k,k) = dsidep(k,k) + fd*deuxmu
120      continue
!
! -- CORRECTION CONTRAINTES PLANES
!
        if (cplan) then
            do 300 k = 1, ndimsi
                if (k .eq. 3) goto 300
                do 310 l = 1, ndimsi
                    if (l .eq. 3) goto 310
                    dsidep(k,l)=dsidep(k,l) - 1.d0/dsidep(3,3)*dsidep(&
                    k,3)*dsidep(3,l)
310              continue
300          continue
        endif
! ======================================================================
!     CAS RAPH_MECA : CALCUL VARIABLES INTERNES ET CONTRAINTES A T+
! ======================================================================
    else if (raph) then
! ======================================================================
!                         CONTRAINTES ELASTIQUES
! ======================================================================
!
! -- SI CONTRAINTES PLANES
!
        if (cplan) then
            coplan = - nu/(1.d0-nu)
            eps(3) = coplan * (eps(1)+eps(2))
        endif
!
! -- CALCUL DES CONTRAINTES ELASTIQUES
!
        treps = eps(1)+eps(2)+eps(3)
        do 160 k = 1, ndimsi
            sigel(k) = lambda*treps*kron(k) + deuxmu*eps(k)
160      continue
        ener = 0.5d0 * ddot(ndimsi,eps,1,sigel,1)
!
! ======================================================================
!                 INTEGRATION DE LA LOI DE COMPORTEMENT
! ======================================================================
!
! -- POINT DEJA SATURE
!
        if (dm .eq. 1) then
            d = dm
!
! -- CALCUL DE L'ETAT D'ENDOMMAGEMENT
        else
            if (ener .le. wy*((1.d0+gamma)/(1.d0+gamma-dm))**2) then
                d = dm
!          ETAT = 0
            else
!            ETAT = 1
                d = max(dm, (1.d0+gamma)*(1.d0-sqrt(wy/ener)))
                if (d .gt. dmax) then
                    d = dmax
!              ETAT = 2
                endif
            endif
        endif
        dd=d-dm
!
! -- CALCUL DES CONTRAINTES
!
        do 30 k = 1, ndimsi
            sigp(k) = (1.d0-d) * sigel(k)
30      continue
!
! -- STOCKAGE DES VARIABLES INTERNES
!
        vip(1) = d
!        VIP(2) = ETAT
        vip(2) = (d-dm)/(instap-instam)
    else
        call assert(.false.)
    endif
!
end subroutine
