subroutine lcjoba(ndim, typmod, imate, crit, sum,&
                  dsu, vim, option, sig, vip,&
                  dsidep, iret)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
    implicit none
#include "asterfort/r8inir.h"
#include "asterfort/rcvala.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
    character(len=8) :: typmod(2)
    character(len=16) :: option
    integer :: ndim, imate, iret
    real(kind=8) :: sum(2), dsu(2), vim(6)
    real(kind=8) :: sig(6), vip(6), dsidep(6, 6), crit(3)
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT ENDOMMAGEABLE DE LA LIAISON ACIER-BETON :
!     COUPLAGE ENTRE L'EFFORT NORMAL ET LE CISAILLEMENT
!     COMBINABLE AVEC ELAS
!
! IN :
!     NDIM    : DIMENSION DE L'ESPACE
!     TYPMOD  : TYPE DE MODELISATION
!     IMATE   : NATURE DU MATERIAU
!     SUM     : SAUT DE DEFORMATION EN T-
!     DSU     : INCREMENT DE SAUT DE DEFORMATION
!     VIM     : VARIABLES INTERNES EN T-
!     OPTION  : OPTION DEMANDEE
!                 RIGI_MECA_TANG ->     DSIDEP
!                 FULL_MECA      -> SIG DSIDEP VIP
!                 RAPH_MECA      -> SIG        VIP
! OUT :
!     SIG     : CONTRAINTE SIG(1)= = SIG_N, SIG(2)=SIG_T
!     VIP     : VARIABLES INTERNES
!       1   -> VALEUR DE L'ENDOMMAGEMENT DIRECTION NORMALE
!       2   -> VALEUR DE L'ENDOMMAGEMENT DIRECTION TANGENTIELLE
!       3   -> VALEUR DE L'ECROUISSAGE ISOTROPE EN REGION 1
!       4   -> VALEUR DE L'ECROUISSAGE ISOTROPE EN REGION 2
!       5   -> DEFORMATION PAR FROTTEMENT DES FISSURES
!       6   -> VALEUR DE L'ECROUISSAGE CINEMATIQUE
!              PAR FROTTEMENT DES FISSURES
!     DSIDEP  : MATRICE TANGENTE
!     IRET    : CODE RETOUR DE  L'INTEGRATION DE LA LDC
!               IRET=0 => PAS DE PROBLEME
!               IRET=1 => DJ<0 ET INTEGRATION IMPOSSIBLE
!                      => ABSENCE DE CONVERGENCE DANS LE CALCUL
!                        IMPLICITE, FROTTEMENT DES FISSURES')
!
! ON A BESOIN DE
!   HPEN   = PENETRATION
!   GAMD0  = DEFORMATION TANGENTIELLE SEUIL ELASTIQUE REGION 1
!   AD1    = CONSTANTE A D'ENDOMMAGEMENT TANGENTIEL REGION 1
!   BD1    = CONSTANTE B D'ENDOMMAGEMENT TANGENTIEL REGION 1
!   GAMD2  = DEFORMATION TANG SEUIL COALESCENCE DES FISSURES REGION 2
!   AD2    = CONSTANTE A D'ENDOMMAGEMENT TANGENTIEL REGION 2
!   BD2    = CONSTANTE B D'ENDOMMAGEMENT TANGENTIEL REGION 2
!   VIFRO  = COEF INTERNE DE FROTTEMENT DES FISSURES
!   FA     = COEF ECROUISSAGE CINEMATIQUE EN FROTTEMENT DES FISSURES
!   FC     = PARAMETRE DE CONTROL DU CONFINEMENT
!   EPSTR0 = DEFORMATION NORMALE SEUIL OUVERTURE DES FISSURES
!   ADN    = CONSTANTE A D'ENDOMMAGEMENT NORMALE
!   BDN    = CONSTANTE B D'ENDOMMAGEMENT NORMALE
! ----------------------------------------------------------------------
    logical :: rigi, resi, conv, trac, adher
    integer :: icodre(14)
    character(len=8) :: nomres(14)
    integer :: k, itemax
    real(kind=8) :: su(2), eps(2), e, gtt, hpen
    real(kind=8) :: bdn, adn, epstr0
    real(kind=8) :: bd1, ad1, gamd0, bd2, ad2, gamd2
    real(kind=8) :: fc, fa, vifrot, signo, i1
    real(kind=8) :: y0t, yit, y2t, fd1, fd2
    real(kind=8) :: d0t, df0t, z0, zf0, df2t, z2, zf2, dft
    real(kind=8) :: gamfro, x0, taofro, fini
    real(kind=8) :: d0n, dfn, y0n, yin, confi, epsco
    real(kind=8) :: xmul, fx, dfds, dfdx, dphids, dphidx, lamdap
    real(kind=8) :: valres(14)
!
! ======================================================================
!                            INITIALISATION
! ======================================================================
!
! -- OPTION ET MODELISATION
    rigi = (option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL')
    resi = (option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL')
    itemax = nint(crit(1))
    adher = .true.
    epsco = 1.0d0
    d0n = 0.0d0
    d0t = 0.0d0
    df0t = 0.0d0
    df2t = 1.0d0
    dfn = 0.0d0
    dft = 0.0d0
!
!    LECTURE DES CARACTERISTIQUES ELASTIQUES
    nomres(1) = 'E'
    call rcvala(imate, ' ', 'ELAS', 1, ' ',&
                [0.d0], 1, nomres, valres, icodre, 1)
    e = valres(1)
!
!    LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
    nomres(1) = 'HPEN'
    nomres(2) = 'GTT'
    nomres(3) = 'GAMD0'
    nomres(4) = 'AD1'
    nomres(5) = 'BD1'
    nomres(6) = 'GAMD2'
    nomres(7) = 'AD2'
    nomres(8) = 'BD2'
    nomres(9) = 'VIFROT'
    nomres(10) = 'FA'
    nomres(11) = 'FC'
    nomres(12) = 'EPSTR0'
    nomres(13) = 'ADN'
    nomres(14) = 'BDN'
!
    call rcvala(imate, ' ', 'JOINT_BA', 1, ' ',&
                [0.d0], 14, nomres, valres, icodre, 1)
    hpen = valres(1)
    gtt = valres(2)
    gamd0 = valres(3)
    ad1 = valres(4)
    bd1 = valres(5)
    gamd2 = valres(6)
    ad2 = valres(7)
    bd2 = valres(8)
    vifrot = valres(9)
    fa = valres(10)
    fc = valres(11)
    epstr0 = valres(12)
    adn = valres(13)
    bdn = valres(14)
!
!
!   INITIALISATION DES VARIABLES INTERNES
!
    d0n = vim(1)
    d0t = vim(2)
    z0 = vim(3)
    z2 = vim(4)
    gamfro = vim(5)
    x0 = vim(6)
!
! ======================================================================
!       CALCUL DES GRANDEURS UTILES QUELQUE SOIT OPTION
! ======================================================================
!
!    1 - CALCUL DES DEFORMATIONS MECANIQUES
!--------------------------------------------------------
!
! CALCUL DU SAUT EN T+
!
    call dcopy(2, sum, 1, su, 1)
    if (resi) call daxpy(2, 1.d0, dsu, 1, su,&
                         1)
!
!  - TRANSFORMATION DES SAUTS EN DEFORMATIONS
!
!      DEFORMATIONS : EPS(1) = EPS_N , EPS(2) = EPS_T
    call r8inir(2, 0.d0, eps, 1)
!
    eps(1) = su(1)/hpen
    eps(2) = su(2)/hpen
!
!    DETERMINATION DE L'OUVERTURE OU FERMETURE DE L'ELEMENT
    if (eps(1) .gt. 0.d0) then
        trac = .true.
        if (eps(1) .gt. epstr0) adher = .false.
    else
        trac = .false.
    endif
!
! ======================================================================
!       CALCUL DES CONTRAINTES ET VARIABLES INTERNES
!           (OPTION FULL_MECA ET RAPH_MECA - (RESI) )
! ====================================================================
    if (resi) then
!
!    MATRICE DE COMPORTEMENT
        call r8inir(36, 0.d0, dsidep, 1)
        dsidep(1,1)=e
        dsidep(2,2)=gtt
!
!    CALCUL DU CONFINEMENT
        signo = dsidep(1,1)*eps(1)
        i1 = signo/3
        confi = fc*i1
        if (confi .gt. 0.d0) confi=0.d0
!
!   2 -     CALCUL DE L'ENDOMMAGEMENT DANS LA DIRECTION
!           NORMALE
!----------------------------------------------------------------
!
!    SEUIL D'ADHERENCE NORMALE PARFAITE
        if (trac) then
            y0n = 0.5d0*dsidep(1,1)*(epstr0**2)
        else
            y0n = 0.5d0*dsidep(1,1)*(epsco**2)
        endif
!
        yin = 0.5d0*dsidep(1,1)*(eps(1)**2)
!
!    ENDOMMAGEMENT DANS LA DIRECTION NORMALE
!
        if (.not.adher) then
!
            dfn = 1.d0 - 1.d0/(1.d0+adn*((yin-y0n)**bdn))
!
            dft = max(d0t,dfn)
!
            goto 100
!...........................................................
!      REMARQUE: ENDOMMAGEMENT DE L'ADHERENCE NORMALE ET
!                DISPARITION DE LA LIAISON DANS LA DIRECTION
!                TANGENTIELLE
!...........................................................
!
        endif
!
!   3 -     CALCUL DE L'ENDOMMAGEMENT DANS LA DIRECTION
!           TANGENTIELLE
!----------------------------------------------------------------
!
!      SEUILS D'ENDOMMAGEMENT DE LA LIAISON DANS LA
!      DIRECTION TANGENTIELLE
!
        y0t = 0.5d0*dsidep(2,2)*(gamd0**2)
        yit = 0.5d0*dsidep(2,2)*(eps(2)**2)
        y2t = 0.5d0*dsidep(2,2)*(gamd2**2)
!
!      CRITERES D'ENDOMMAGEMENT
!
        fd1 = yit - (y0t + z0)
        fd2 = yit - (y2t + z2)
!
        if (fd1 .gt. 0.d0) then
!
!         ENDOMMAGEMENT EN REGION 1
!
            df0t= (sqrt(y0t/yit))* exp(ad1*((sqrt(2.d0/dsidep(2,2))*&
            (sqrt(yit)-sqrt(y0t)))**bd1))
            zf0 = yit - y0t
!
            if (fd2 .gt. 0.d0) then
!
!         ENDOMMAGEMENT EN REGION 2
!
                df2t = abs(1.d0/(1.d0+ad2*((yit-y2t)**bd2)))
                zf2 = yit - y2t
            else
                df2t = 1.d0
                zf2 = z2
            endif
            dft = 1.d0 - df0t*df2t
!          write(6,*)eps(2),yit,df0t,df2t,dft
            dfn = d0n
        else
!
!         PAS DE PROGRESSION DE L'ENDOMMAGEMENT
!
            dft = d0t
            zf0 = z0
            zf2 = z2
            dfn = d0n
        endif
!
!
!   4 -     CALCUL DE LA CONTRAINTE PAR FROTTEMENT DES FISSURES
!----------------------------------------------------------------
!
        if (dft .gt. 0.d0) then
!
!         CALCUL DE LA CONTRAINTE PAR FROTTEMENT DES FISSURES
!
!
            taofro = dsidep(2,2)*dft*(eps(2)-gamfro)
            fini = abs(taofro-x0) + confi
!
            if (fini .gt. 0.d0) then
                conv = .false.
                do 40 k = 1, itemax
                    taofro = dsidep(2,2)*dft*(eps(2)-gamfro)
                    if ((taofro-x0) .ge. 0.d0) then
                        xmul = +1.d0
                    else
                        xmul = -1.d0
                    endif
!
                    fx = abs(taofro-x0) + confi
                    dfds = xmul
                    dfdx = -1.d0*xmul
                    dphids = xmul
                    dphidx = -1.d0*xmul+fa*x0
!
! --------EVALUATION DU MULTIPLICATEUR PLASTIQUE
!
                    lamdap = fx/( dfds*dsidep(2,2)*dft*dphids+ dfdx* vifrot*dphidx)
                    gamfro = gamfro + lamdap*dphids
                    x0 = x0 - vifrot*lamdap*dphidx
                    taofro = taofro - lamdap*dsidep(2,2)*dft*dphids
                    fx = abs(taofro-x0) + confi
!
! --------EVALUATION DE LA CONVERGENCE
                    conv = ((abs(fx/fini) .le. 0.d0) .or. (lamdap .le. crit(3)) )
                    if (conv) goto 100
40              continue
!
                if (.not. conv) then
                    iret = 1
                endif
            endif
        endif
!
100      continue
!
!   5 -  CALCUL DES CONTRAINTES REELLES
!----------------------------------------------------------------
!
        call r8inir(6, 0.d0, sig, 1)
!
        sig(1)=dsidep(1,1)*(1.d0-dfn)*eps(1)
        sig(2)=dsidep(2,2)*(1.d0-dft)*eps(2)
        taofro=dsidep(2,2)*dft*(eps(2)-gamfro)
        if (trac .and. (.not.adher)) taofro=0.d0
        sig(2)=sig(2)+taofro
!
!
!    6 -   MISE A JOUR DES VARIABLES INTERNES
! ------------------------------------------------------------
!
        vip(1) = dfn
        vip(2) = dft
        vip(3) = zf0
        vip(4) = zf2
        vip(5) = gamfro
        vip(6) = x0
!
    endif
!
! ======================================================================
!     CALCUL  DE LA MATRICE TANGENTE DSIDEP
!         OPTION RIGI_MECA_TANG ET FULL_MECA  (RIGI)
!    (PAR SIMPLICITE ON NE CALCULE QUE LA MATRICE SECANTE)
! ======================================================================
    if (rigi) then
!
!   1 -  CONTRIBUTION ELASTIQUE
! ------------------------------------------------------------
!
        call r8inir(36, 0.d0, dsidep, 1)
!
        dsidep(1,1)=e*(1.d0-dfn)
        dsidep(2,2)=gtt*(1.d0-dft)
!
    endif
!
!   2 -  TRANSFORMATION DES DIMENSIONS POUR UTILISER DANS
!        L ELEMENT JOINT
! ------------------------------------------------------------
!
    if (rigi) then
        dsidep(1,1)= dsidep(1,1)/hpen
        dsidep(2,2)= dsidep(2,2)/hpen
    endif
!
end subroutine
