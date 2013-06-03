subroutine nm1vil(fami, kpg, ksp, icdmat, materi,&
                  crit, instam, instap, tm, tp,&
                  tref, deps, sigm, vim, option,&
                  defam, defap, angmas, sigp, vip,&
                  dsidep, iret, compo, nbvalc)
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
! ----------------------------------------------------------------------
!
! aslint: disable=W1504
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/granac.h'
    include 'asterfort/nmasse.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/rcvarc.h'
    include 'asterfort/u2mess.h'
    integer :: icdmat, kpg, ksp, iret, nbvalc
    real(kind=8) :: crit(*)
    real(kind=8) :: instam, instap
    real(kind=8) :: tm, tp, tref
    real(kind=8) :: irram, irrap
    real(kind=8) :: deps
    real(kind=8) :: sigm, vim(nbvalc)
    character(len=16) :: option, compo
    character(len=*) :: fami
    real(kind=8) :: defam, defap
    real(kind=8) :: angmas(3)
    real(kind=8) :: sigp, vip(nbvalc), dsidep, alpha
    character(len=8) :: materi
!
! ----------------------------------------------------------------------
!      VISCO_PLASTICITE FLUAGE SOUS IRRADIATION AVEC GRANDISSEMENT
!      VISC_IRRA_LOG OU GRAN_IRRA_LOG
!      LOI 1D PURE. MODIF JMP POUR ECRIRE SIMPLEMENT :
! DEPSVP=SIGMA+.EXP(-Q/T)*(A.OMEGA/(1+OMEGA*FLUENCE)+B*FLUENCE)*DFLUENCE
!
! IN  ICDMAT  : MATERIAU CODE
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  INSTAM  : INSTANT DU CALCUL PRECEDENT
! IN  INSTAP  : INSTANT DU CALCUL
! IN  TM      : TEMPERATURE A L'INSTANT PRECEDENT
! IN  TP      : TEMPERATURE A L'INSTANT DU CALCUL
! IN  TREF    : TEMPERATURE DE REFERENCE
! IN  DEPS    : INCREMENT DE DEFORMATION-INCREMENT DEFORMATION THERMIQUE
! IN  SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
! IN  VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
! IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
! IN  DEFAM   : DEFORMATIONS ANELASTIQUES A L'INSTANT PRECEDENT
! IN  DEFAP   : DEFORMATIONS ANELASTIQUES A L'INSTANT DU CALCUL
! IN  ANGMAS  : LES TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
! OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL
! OUT VIP     : VARIABLES INTERNES A L'INSTANT ACTUEL
! OUT DSIDEP  : MODULE TANGENT
! OUT IRET    : CODE RETOUR DE LA RECHERCHE DE ZERO DE F(X)=0
!                   IRET=0 => PAS DE PROBLEME
!                   IRET=1 => ECHEC
!
!
!
!     COMMON POUR LES PARAMETRES DES LOIS VISCOPLASTIQUES
    common / nmpavp / dpc,sieleq,deuxmu,deltat,tschem,prec,theta,niter
    real(kind=8) :: dpc, sieleq, deuxmu, deltat, tschem, prec, theta, niter
!     COMMON POUR LES PARAMETRES DES LOIS DE FLUAGE SOUS IRRADIATION
!     VISC_IRRA_LOG: FLUPHI A      B      CTPS    ENER
    common / nmpair / fluphi,&
     &                  a,b,ctps,ener
    real(kind=8) :: fluphi
    real(kind=8) :: a, b, ctps, ener
! PARAMETRES MATERIAUX
! ELASTIQUES
    real(kind=8) :: ep, nup, troikp, deumup
    real(kind=8) :: em, num, troikm, deumum
! AUTRES
    integer :: nbcgil, iret2
    parameter  (nbcgil=5)
    real(kind=8) :: coegil(nbcgil)
    character(len=8) :: nomgil(nbcgil)
    integer :: codgil(nbcgil)
    real(kind=8) :: t1, t2
    real(kind=8) :: degran, depsan, depsim, depsgr
    real(kind=8) :: coef1, coefb, expqt
    data nomgil /'A','B','CSTE_TPS','ENER_ACT','FLUX_PHI'/
!
    iret = 0
!     PARAMETRE THETA D'INTEGRATION
!
    theta = crit(4)
    t1 = abs(theta-0.5d0)
    t2 = abs(theta-1.d0)
    prec = 0.000001D0
    if ((t1.gt.prec) .and. (t2.gt.prec)) then
        call u2mess('F', 'ALGORITH6_55')
    endif
!
! TEMPERATURE AU MILIEU DU PAS DE TEMPS  (DANS COMMON / NMPAVP /)
    tschem = tm*(1.d0-theta)+tp*theta
! DEFORMATION PLASTIQUE CUMULEE  (DANS COMMON / NMPAVP /)
    dpc = vim(1)
! INCREMENT DE TEMPS (DANS COMMON / NMPAVP /)
    deltat = instap - instam
! CARACTERISTIQUES ELASTIQUES VARIABLES
    call nmasse(fami, kpg, ksp, '-', icdmat,&
                materi, instam, em, num, deumum,&
                troikm)
!
    call nmasse(fami, kpg, ksp, '+', icdmat,&
                materi, instap, ep, nup, deumup,&
                troikp)
!
!     IRRADIATION AU POINT CONSIDERE
!     FLUX NEUTRONIQUE
    call rcvarc('F', 'IRRA', '-', fami, kpg,&
                ksp, irram, iret2)
    if (iret2 .gt. 0) irram=0.d0
    call rcvarc('F', 'IRRA', '+', fami, kpg,&
                ksp, irrap, iret2)
    if (iret2 .gt. 0) irrap=0.d0
    irrap = irrap - irram + vim(2)
    irram = vim(2)
!
    fluphi = (irrap-irram)/deltat
!     RECUPERATION DES CARACTERISTIQUES DES LOIS DE FLUAGE
    call rcvalb(fami, kpg, ksp, '+', icdmat,&
                materi, compo, 0, ' ', 0.d0,&
                nbcgil, nomgil(1), coegil(1), codgil(1), 0)
!     TRAITEMENT DES PARAMETRES DE LA LOI DE FLUAGE
    if (codgil(1) .eq. 0) then
!         LOI DE TYPE VISC_IRRA_LOG
!         PARAMETRES DE LA LOI DE FLUAGE
!
        a = coegil(1)
        b = coegil(2)
        ctps = coegil(3)
        ener = coegil(4)
!
        if (coegil(5) .ne. 1.d0) then
            call u2mess('A', 'ALGORITH6_56')
        endif
        if (fluphi .lt. -prec) then
            call u2mess('F', 'ALGORITH6_57')
        endif
    else
        call u2mess('F', 'ALGORITH6_58')
    endif
!
!     CALCUL DE LA DEFORMATION DE GRANDISSEMENT
    degran = 0.0d0
    call granac(fami, kpg, ksp, icdmat, materi,&
                compo, irrap, irram, tm, tp,&
                depsgr)
!
    if (compo(1:10) .eq. 'GRAN_IRRA_') then
        vip(3) = vim(3)+depsgr
        if (depsgr .ne. 0.0d0) then
! --- RECUPERATION DU REPERE POUR LE GRANDISSEMENT
            alpha = angmas(1)
            if (angmas(2) .ne. 0.d0) then
                call u2mess('F', 'ALGORITH6_59')
            endif
!
!        INCREMENT DEFORMATION DE GRANDISSEMENT DANS LE REPERE
            degran = depsgr*cos(alpha)*cos(alpha)
        endif
    endif
!     INCREMENT DEFORMATION ANELASTIQUE
    depsan = defap-defam
!     INCREMENT DEFORMATION IMPOSEE
    depsim = depsan+degran
!
    expqt=exp(-ener/(tp+273.15d0))
!
    coefb=expqt*((a*ctps/(1.d0+ctps*irrap))+b)*(irrap-irram)
    coef1 = ep/(1.d0+ep*coefb)
!
! CONTRAINTE ACTUALISEE
!
!
    sigp =coef1*(sigm/em+deps-depsim)
!
! DEFORMATION PLASTIQUE CUMULEE ACTUALISEE
!
    vip(1) = vim(1)+(abs(sigp)*coefb)
! MODULE TANGENT POUR MATRICE TANGENTE
!
    dsidep = coef1
    vip(2) = irrap
!
end subroutine
