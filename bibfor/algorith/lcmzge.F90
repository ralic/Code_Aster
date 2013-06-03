subroutine lcmzge(fami, kpg, ksp, ndim, typmod,&
                  imate, epstm, depst, vim, option,&
                  sig, vip, dsidpt, proj)
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
    include 'asterc/r8nnem.h'
    include 'asterfort/bptobg.h'
    include 'asterfort/jacobi.h'
    include 'asterfort/lcumvi.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/rcvarc.h'
    include 'asterfort/u2mess.h'
    character(len=8) :: typmod(*)
    character(len=*) :: fami
    character(len=16) :: option
    integer :: ndim, imate, kpg, ksp
    real(kind=8) :: epstm(12), depst(12), vim(4)
    real(kind=8) :: sig(6), vip(*), dsidpt(6, 6, 2)
    real(kind=8) :: proj(6, 6)
!
! ----------------------------------------------------------------------
!  LOI DE COMPORTEMENT ENDOMMAGEABLE : MODELE DE MAZARS (EN DELOCALISE)
!     NB. LES PARAMETRES MATERIAUX PEUVENT DEPENDRE DE LA TEMPERATURE,
!      DE L'HYDRATATION OU DU SECHAGE
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  IMATE   : NATURE DU MATERIAU
! IN  EPSM    : DEFORMATION EN T-
! IN  EPSRM   : DEFORMATION GENERALISEE EN T-
! IN  DEPS    : INCREMENT DE DEFORMATION
! IN  DEPSR   : INCREMENT DE DEFORMATION GENERALISEE
! IN  VIM     : VARIABLES INTERNES EN T-
! IN  OPTION  : OPTION DEMANDEE
!                 RIGI_MECA_TANG ->     DSIDEP
!                 FULL_MECA      -> SIG DSIDEP VIP
!                 RAPH_MECA      -> SIG        VIP
! OUT SIG     : CONTRAINTE
! OUT VIP     : VARIABLES INTERNES
!                 1   -> VALEUR DE L'ENDOMMAGEMENT
!                 2   -> INDICATEUR D'ENDOMMAGEMENT
!                 3   -> TEMPERATURE MAXIMALE VUE PAR LE MATERIAU
!                 3   -> VALEUR DE EPSEQ (NON lOCAL)
! OUT DSIDEP  : MATRICE TANGENTE
! OUT DSIDPR  : MATRICE TANGENTE DEFO GENERALISEE
! OUT PROJ    : PROJECTEUR DE COUPURE DU TERME DE REGULARISATION
! ----------------------------------------------------------------------
    logical :: rigi, resi, elas, rela, prog, cplan
    character(len=1) :: poum
    integer :: icodre(7)
    character(len=8) :: nomres(7), nompar
    integer :: ndimsi, nperm, nitjac, trij, ordrej
    integer :: i, j, l, iret, iret1, iret2, iret3
    real(kind=8) :: e, nu, epsthe, kdess, bendo
    real(kind=8) :: ac, at, bc, bt, epsd0
    real(kind=8) :: epsm(6), epsrm(6), deps(6), depsr(6), epsplu(6)
    real(kind=8) :: epse(6), epser(6), epspr(3), epsp(3)
    real(kind=8) :: epsr(6), eps(6), treps, epseq, epstil
    real(kind=8) :: sigel(6), sigelp(3)
    real(kind=8) :: temp, tmax, tmaxm, hydr, sech, sref, tref
    real(kind=8) :: tol, toldyn, tr(6), tu(6), trr(6), jacaux(3)
    real(kind=8) :: vecpe(3, 3), vecper(3, 3)
    real(kind=8) :: coplan, lambda, deuxmu
    real(kind=8) :: rac2, coef, tmp1, d, rap, gama, k, y
    real(kind=8) :: valres(7), valpar
    real(kind=8) :: kron(6)
    real(kind=8) :: epsfp(6), epscou(6), chi, vala, r, a, b
    integer :: idc
    logical :: coup
!
    data        kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
! ======================================================================
!                            INITIALISATION
! ======================================================================
!
! -- OPTION ET MODELISATION
    rigi = (option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL')
    resi = (option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL')
    rela = option(11:14) .eq. 'ELAS'
    cplan = (typmod(1).eq.'C_PLAN  ')
    prog = .false.
    elas = .true.
    ndimsi = 2*ndim
    rac2=sqrt(2.d0)
    iret1 = 0
    iret2 = 0
    iret3 = 0
! M.B.: NOUVELLE OPTION COUP POUR LE COUPLAGE AVEC UMLV
! MEME OPTION UTILISEE POUR LE COUPLAGE UMLV-ENDO_ISOT_BETON
    coup = (option(6:9).eq.'COUP')
! M.B.: INDICE POUR IDENTIFIER LES VARIABLES INTERNES DANS LES CAS:
! COUPLAGE ET ABSENCE DE COUPLAGE AVEC UMLV
    idc = 0
    if (coup) then
        idc = 21
    endif
! -- PROJECTEUR DE COUPURE
    call r8inir(36, 0.d0, proj, 1)
    if (vim(1) .lt. 1.d0-1.d-05) call r8inir(6, 1.d0, proj, 7)
!     DETERMINATION DE LA TEMPERATURE DE REFERENCE (TMAX) ET
!   DES CONDITIONS D HYDRATATION OU DE SECHAGE
    tmaxm = vim(3)
    call rcvarc(' ', 'TEMP', 'REF', fami, kpg,&
                ksp, tref, iret1)
    call rcvarc(' ', 'SECH', 'REF', fami, kpg,&
                ksp, sref, iret)
    if (iret .ne. 0) sref=0.d0
    if (resi) then
        poum='+'
        call rcvarc(' ', 'TEMP', poum, fami, kpg,&
                    ksp, temp, iret2)
        if (iret2 .eq. 1) then
            tmax = r8nnem()
        else
            tmax = max(tmaxm, temp)
            if (tmax .gt. tmaxm) vip(idc+3) = tmax
        endif
    else
        poum='-'
        call rcvarc(' ', 'TEMP', poum, fami, kpg,&
                    ksp, temp, iret3)
        if (iret3 .eq. 1) then
            tmax = r8nnem()
        else
            tmax = max(tmaxm, temp)
        endif
    endif
    call rcvarc(' ', 'HYDR', poum, fami, kpg,&
                ksp, hydr, iret)
    if (iret .ne. 0) hydr=0.d0
    call rcvarc(' ', 'SECH', poum, fami, kpg,&
                ksp, sech, iret)
    if (iret .ne. 0) sech=0.d0
!  RECUPERATION DES CARACTERISTIQUES MATERIAUX QUI PEUVENT VARIER
!  AVEC LA TEMPERATURE (MAXIMALE), L'HYDRATATION OU LE SECHAGE
!-----------------------------------------------------
    nompar = 'TEMP'
    valpar = tmax
!    LECTURE DES CARACTERISTIQUES ELASTIQUES
    nomres(1) = 'E'
    nomres(2) = 'NU'
    nomres(3) = 'ALPHA'
    call rcvalb(fami, kpg, ksp, poum, imate,&
                ' ', 'ELAS', 1, nompar, valpar,&
                2, nomres, valres, icodre, 1)
    call rcvalb(fami, kpg, ksp, poum, imate,&
                ' ', 'ELAS', 1, nompar, valpar,&
                1, nomres(3), valres(3), icodre(3), 0)
    if ((iret2+iret3) .eq. 0) then
        if ((iret1.ne.0) .or. (icodre(3).ne.0)) then
            call u2mess('F', 'CALCULEL_15')
        else
            epsthe = valres(3) * (temp - tref)
        endif
    else
        epsthe = 0.d0
    endif
    e = valres(1)
    nu = valres(2)
    lambda = e * nu / (1.d0+nu) / (1.d0-2.d0*nu)
    deuxmu = e/(1.d0+nu)
! --- LECTURE DU RETRAIT ENDOGENE ET RETRAIT DE DESSICCATION
    nomres(1)='B_ENDOGE'
    nomres(2)='K_DESSIC'
    call rcvalb(fami, 1, 1, '+', imate,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                1, nomres(1), valres(1), icodre(1), 0)
    if (icodre(1) .ne. 0) valres(1) = 0.d0
    bendo = valres(1)
    call rcvalb(fami, 1, 1, '+', imate,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                1, nomres(2), valres(2), icodre(2), 0)
    if (icodre(2) .ne. 0) valres(2) = 0.d0
    kdess = valres(2)
! --- LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
    nomres(1) = 'EPSD0'
    nomres(2) = 'AC'
    nomres(3) = 'BC'
    nomres(4) = 'AT'
    nomres(5) = 'BT'
    nomres(6) = 'K'
    call rcvalb(fami, kpg, ksp, poum, imate,&
                ' ', 'MAZARS', 1, nompar, valpar,&
                6, nomres, valres, icodre, 1)
    epsd0 = valres(1)
    ac = valres(2)
    bc = valres(3)
    at = valres(4)
    bt = valres(5)
    k = valres(6)
!    M.B.: LECTURE DU PARAMETRE DE COUPLAGE AVEC UMLV
    if (coup) then
        nomres(7) = 'CHI'
        call rcvalb(fami, kpg, ksp, poum, imate,&
                    ' ', 'MAZARS', 0, ' ', 0.d0,&
                    1, nomres(7), valres(7), icodre(7), 1)
        chi = valres(7)
        if (chi .eq. 0.d0) then
            call u2mess('I', 'COMPOR1_59')
        endif
    endif
! -- SEPARATION DE EPSM/EPSRM, DEPS/DEPSR DANS EPSTM, DEPST
    do 10 i = 1, ndimsi
        epsm(i)=epstm(i)
        epsrm(i)=epstm(i+6)
        deps(i)=depst(i)
        depsr(i)=depst(i+6)
10  end do
! -   M.B.: CALCUL DE LA DEFORMATION DE FLUAGE AU TEMP P
    if (coup .and. resi) then
        call lcumvi('FT', vip, epsfp)
    endif
! ======================================================================
!    CALCUL DES CONTRAINTES ET VARIABLES INTERNES
!    (OPTION FULL_MECA ET RAPH_MECA)
! ======================================================================
    call r8inir(6, 0.d0, eps, 1)
    call r8inir(6, 0.d0, epsr, 1)
!  -   MISE A JOUR DES DEFORMATIONS MECANIQUES
    if (resi) then
        do 20 j = 1, ndimsi
            eps(j) = epsm(j) + deps(j)
            epsr(j) = epsrm(j) + depsr(j)
20      continue
    else
        do 30 j = 1, ndimsi
            eps(j)=epsm(j)
            epsr(j)=epsrm(j)
30      continue
        d=vim(1)
    endif
! -  MODIF M.B.: ON MET DANS EPS LES DEFORMATIONS REELES
    do 40 j = 4, ndimsi
        eps(j) = eps(j)/rac2
        epsr(j)= epsr(j)/rac2
        if (coup .and. resi) then
            epsfp(j) = epsfp(j)/rac2
        endif
40  end do
!
!
!    CALCUL DE LA DEFORMATION ELASTIQUE (LA SEULE QUI CONTRIBUE
!    A FAIRE EVOLUER L'ENDOMMAGEMENT)
!
    call r8inir(6, 0.d0, epse, 1)
    call r8inir(6, 0.d0, epser, 1)
    do 35 j = 1, ndimsi
        epse(j) = eps(j) - ( epsthe - kdess * (sref-sech) - bendo * hydr ) * kron(j)
        epser(j) = epsr(j) - ( epsthe - kdess * (sref-sech) - bendo * hydr ) * kron(j)
35  end do
!
!
!  M.B.: SI CONTRAINTES PLAN (COUP)
!  ON CALCULE LA 3EME  COMPOSANTE NORMALE
!   AVANT DE DIAGONALISER
    if (coup .and. resi) then
        if (cplan) then
            coplan = - nu/(1.d0-nu)
            epse(3) = coplan * (epse(1)+epse(2))
            epser(3) = coplan * (epser(1)+epser(2))
        endif
    endif
!
    if (coup .and. resi) then
        call r8inir(6, 0.d0, epscou, 1)
        do 1010 j = 1, ndimsi
            epse(j) = epse(j) - epsfp(j)
            epscou(j) = epser(j) - (1.d0-chi)*epsfp(j)
1010      continue
    endif
!  -   ON PASSE DANS LE REPERE PROPRE DE EPS
    nperm = 12
    tol = 1.d-10
    toldyn = 1.d-2
!       MATRICE  TR = (XX XY XZ YY YZ ZZ) POUR JACOBI)
    tr(1) = epse(1)
    tr(2) = epse(4)
    tr(3) = epse(5)
    tr(4) = epse(2)
    tr(5) = epse(6)
    tr(6) = epse(3)
!       MATRICE UNITE = (1 0 0 1 0 1) (POUR JACOBI)
    tu(1) = 1.d0
    tu(2) = 0.d0
    tu(3) = 0.d0
    tu(4) = 1.d0
    tu(5) = 0.d0
    tu(6) = 1.d0
    trij = 2
    ordrej = 2
!
    call jacobi(3, nperm, tol, toldyn, tr,&
                tu, vecpe, epsp, jacaux, nitjac,&
                trij, ordrej)
! ON PASSE DANS LE REPERE PROPRE DE EPSR
    tu(1) = 1.d0
    tu(2) = 0.d0
    tu(3) = 0.d0
    tu(4) = 1.d0
    tu(5) = 0.d0
    tu(6) = 1.d0
    trij = 2
    ordrej = 2
    trr(1) = epser(1)
    trr(2) = epser(4)
    trr(3) = epser(5)
    trr(4) = epser(2)
    trr(5) = epser(6)
    trr(6) = epser(3)
    if (coup .and. resi) then
        trr(1) = epscou(1)
        trr(2) = epscou(4)
        trr(3) = epscou(5)
        trr(4) = epscou(2)
        trr(5) = epscou(6)
        trr(6) = epscou(3)
    endif
    call jacobi(3, nperm, tol, toldyn, trr,&
                tu, vecper, epspr, jacaux, nitjac,&
                trij, ordrej)
! -- SI CONTRAINTES PLANES
!    Modifie M.B.: if NOT COUP
    if (.not. coup) then
        if (cplan) then
            coplan = - nu/(1.d0-nu)
            epsp(3) = coplan * (eps(1)+eps(2))
            epspr(3) = coplan * (epsr(1)+epsr(2))
        endif
    endif
!--  ------------------------------
!      CALCUL DE L'ETAT D'ENDOMMAGEMENT
! -  ------------------------------
!     CALCUL DE EPSEQ (NON LOCAL) ET EPSTIL (LOCAL)
    epseq = 0.d0
    epstil = 0.d0
    do 50 j = 1, 3
        if (epspr(j) .gt. 0.d0) then
            epseq = epseq + (epspr(j)**2)
        endif
        if (epsp(j) .gt. 0.d0) then
            epstil = epstil + (epsp(j)**2)
        endif
50  end do
    epseq = sqrt(epseq)
    epstil = sqrt(epstil)
! -     CALCUL DES CONTRAINTES ELASTIQUES (REPERE PRINCIPAL)
    treps = epsp(1)+epsp(2)+epsp(3)
    do 60 j = 1, 3
        sigelp(j) = lambda*treps
60  end do
    do 70 j = 1, 3
        sigelp(j) = sigelp(j) + deuxmu*epsp(j)
70  end do
    tmp1 = 0.d0
    do 80 j = 1, 3
        if (sigelp(j) .lt. 0.d0) then
            tmp1 = tmp1 + sigelp(j)
        endif
80  end do
    if (resi) then
!   5 -     CALCUL DE R
!----------------------------------------------------------------
        vala=abs(sigelp(1))+abs(sigelp(2))+abs(sigelp(3))
        r=0.d0
        do 81 i = 1, 3
            r = r + max(0.00000000D0,sigelp(i))
81      end do
        if (vala .gt. 1.d-10) then
            r=(r/ vala)
        else
            r=1.d0
        endif
        if (r .lt. 0.00001D0) r=0.d0
        if (r .gt. 0.99999D0) r=1.d0
        gama=0.d0
        rap=0.d0
        do 69 i = 1, 3
            rap = rap + min(0.d0,sigelp(i))
            gama = gama + (min(0.d0,sigelp(i)))**2
69      end do
        if ((abs(rap).gt.1.d-10) .and. (r.eq.0.d0)) then
            gama = -(sqrt(gama)/ rap)
        else
            gama=1.d0
        endif
        if (gama .le. 0.d0) gama=1.d0
        y=gama*epseq
!      CALCUL DES PARAMETRES D'ENDOMMAGEMENT
        if (epseq .le. epsd0) then
            d=vim(1)
        else
            a=2.d0*r**2.d0*(at-2.d0*k*at+ac)-r*(at*(1.d0-4.d0*k)+3.d0*&
            ac)+ac
            b=r**2.d0*bt+(1.d0-r**2.d0)*bc
            d=1.d0-epsd0*(1.d0-a)/y -a*exp(-b*(y-epsd0))
            d = min(d , 0.99999D0)
            d = max ( vim(1), d)
            if (d .gt. vim(1)) prog = .true.
            if (d .gt. 0.d0) elas = .false.
        endif
    endif
!       MISE A JOUR DES CONTRAINTES ET VARIABLES D'ENDOMMAGEMENT
    if (resi) then
!        ON PASSE DANS LE REPERE INITIAL LES CONTRAINTES REELLES
        call r8inir(6, 0.d0, sig, 1)
        tr(1) = sigelp(1)*(1.d0-d)
        tr(2) = sigelp(2)*(1.d0-d)
        tr(3) = sigelp(3)*(1.d0-d)
        tr(4) = 0.d0
        tr(5) = 0.d0
        tr(6) = 0.d0
        call bptobg(tr, sig, vecpe)
        do 100 j = 4, ndimsi
            sig(j)=rac2*sig(j)
100      continue
        vip(idc+1) = d
        if (d .eq. 0.d0) then
            vip(idc+2) = 0.d0
        else
            vip(idc+2) = 1.d0
        endif
        vip(idc+4) = epseq
    endif
! ======================================================================
!     CALCUL  DE LA MATRICE TANGENTE DSIDEP
!         OPTION RIGI_MECA_TANG ET FULL_MECA
! ======================================================================
! ======================================================================
!                            MATRICE TANGENTE
! ======================================================================
    if (rigi) then
! - M.B.: OPTION FULL_MECA POUR LE COUPLAGE AVEC UMLV
        if (coup) d = vip(idc+1)
! -- CONTRIBUTION ELASTIQUE
        call r8inir(72, 0.d0, dsidpt, 1)
        do 110 j = 1, 3
            do 120 l = 1, 3
                dsidpt(j,l,1) = (1.d0-d)*lambda
120          continue
110      continue
        do 130 j = 1, ndimsi
            dsidpt(j,j,1) = dsidpt(j,j,1) + (1-d)*deuxmu
130      continue
        if ((.not.elas) .and. prog .and. (.not.rela) .and. (d.lt.0.99999D0)) then
            if (epseq .lt. 1.d-10) then
                coef=0.d0
            else
                coef =(epsd0*(1.d0 - a)/(gama*epseq)**2 + a*b/ exp (b*&
                ((gama*epseq) - epsd0)))
                coef = coef/epseq
                coef=gama*coef
            endif
!      CALCUL DE EPS+
            call r8inir(6, 0.d0, tr, 1)
            do 160 j = 1, 3
                if (epspr(j) .gt. 0.d0) then
                    tr(j) = epspr(j)
                endif
160          continue
            call bptobg(tr, epsplu, vecper)
            do 170 j = 4, ndimsi
                epsplu(j) = epsplu(j)*rac2
170          continue
            call r8inir(6, 0.d0, sigel, 1)
            tr(1) = sigelp(1)
            tr(2) = sigelp(2)
            tr(3) = sigelp(3)
            tr(4) = 0.d0
            tr(5) = 0.d0
            tr(6) = 0.d0
            call bptobg(tr, sigel, vecpe)
            do 180 j = 4, ndimsi
                sigel(j)=rac2*sigel(j)
180          continue
            do 190 i = 1, 6
                do 200 j = 1, 6
                    dsidpt(i,j,2) = - coef * sigel(i)* epsplu(j)
200              continue
190          continue
        endif
    endif
end subroutine
