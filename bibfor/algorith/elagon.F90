subroutine elagon(ndim, imate, crit, sat, biot,&
                  tm, tp, alpha, deps, e,&
                  nu, snetm, option, snetp, dsidep,&
                  p1, p2, dp1, dsidp1, dsidp2)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
! ROUTINE ELAGON
! MODELE POUR L ARGILE GONFLANTE (HOXHNA COLLIN) EN CONTRAINTES NET
!
! ======================================================================
! SNET : CONTRAINTES NET : SIGTOT=SIGNET-biot*Pgaz
!                          SIGNET=SIGTOT+biot*Pgaz
!       DANS CETTE VERSION SIP=-biot*Pgaz
! ======================================================================
    implicit none
    include 'asterfort/dpgfp1.h'
    include 'asterfort/prgonf.h'
    include 'asterfort/rcvalb.h'
    integer :: ndim, imate
    character(len=16) :: option
    real(kind=8) :: crit(*), tm, tp, alpha
    real(kind=8) :: deps(6), biot, sat, p1, p2, dp1
    real(kind=8) :: snetm(6), snetp(6), dsidep(6, 6)
    real(kind=8) :: dsidp1(6), dsidp2(6)
!
    real(kind=8) :: valres(2)
    real(kind=8) :: betam, pref
    real(kind=8) :: depsmo, sigmmo, e, nu, k0, deuxmu
    real(kind=8) :: kron(6), depsdv(6), sigmdv(6), sigpdv(6)
    real(kind=8) :: sigpmo
    real(kind=8) :: p1m
    real(kind=8) :: valpam(1)
    integer :: ndimsi
    integer :: k, l, kpg, spt
    integer :: icodre(2)
    character(len=8) :: nomres(2), fami, poum
    character(len=8) :: nprefr(1)
!
! ======================================================================
    integer :: ndt, ndi
    common /tdim/   ndt  , ndi
    data        kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
! ======================================================================
!
!
!     --  INITIALISATIONS :
!     ----------------------
    ndimsi = 2*ndim
!
    pref= 1.d6
    p1m=p1-dp1
!
!
!     --  RECUPERATION DES CARACTERISTIQUES
!     ---------------------------------------
    nomres(1)='BETAM'
    nomres(2)='PREF'
!
    nprefr(1) = 'TEMP'
    valpam(1) = tm
!
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, imate,&
                ' ', 'ELAS_GONF ', 1, nprefr, valpam,&
                1, nomres(1), valres(1), icodre(1), 2)
    betam=valres(1)
!
    call rcvalb(fami, kpg, spt, poum, imate,&
                ' ', 'ELAS_GONF ', 1, nprefr, valpam,&
                1, nomres(2), valres(2), icodre(2), 2)
    pref = valres(2)
!
!
    deuxmu = e/(1.d0+nu)
    k0 = e/(3.d0*(1.d0-2.d0*nu))
!
!
! ======================================================================
! --- RETRAIT DE LA DEFORMATION DUE A LA DILATATION THERMIQUE ----------
! ======================================================================
    do 110 k = 1, ndi
        deps(k) = deps(k) - alpha*(tp-tm)
110  end do
!
!     --  CALCUL DE DEPSMO ET DEPSDV :
!     --------------------------------
    depsmo = 0.d0
    do 111 k = 1, 3
        depsmo = depsmo + deps(k)
111  end do
!
    do 112 k = 1, ndimsi
        depsdv(k) = deps(k) - depsmo/3.d0 * kron(k)
112  end do
!
!     --  CALCUL DES CONTRAINTES
!     ----------------------------
! Contraintes moyenne (1/3 trace(sig) )
    sigmmo = 0.d0
    do 116 k = 1, 3
        sigmmo = sigmmo + snetm(k)/3.d0
116  end do
!
    do 117 k = 1, ndimsi
        sigmdv(k) = snetm(k) - sigmmo * kron(k)
117  end do
    sigpmo = 0.d0
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
!
!     --------------------------------
! MODELE DE GONFLEMENT APPLIQUE A LA CONTRAINTE MOYENNE
!
! ATTENTION ICI KO INDEP DE LA SUCCION
! ON N APPLIQUE PAS LA NON LINEARITE DEMANDANT LES PARAMETRES R ET BETA
! CE QUI POURRAIT ETRE ENVISAGE DANS UN SECOND TEMPS
!
        sigpmo = sigmmo+k0*depsmo +prgonf(biot,betam,pref,p1)-prgonf( biot,betam,pref,p1m)
    endif
!
    do 118 k = 1, ndimsi
        sigpdv(k) = sigmdv(k) + deuxmu * depsdv(k)
        snetp(k) = sigpdv(k) + sigpmo*kron(k)
118  end do
!
!
!     --  CALCUL DE L'OPERATEUR TANGENT :
!     --------------------------------
    if (option(1:14) .eq. 'RIGI_MECA_TANG' .or. option(1:9) .eq. 'FULL_MECA') then
!
!     --9.0 INITIALISATION DE L'OPERATEUR TANGENT
!     ---------------------------------------
        do 125 k = 1, 6
            dsidp1(k) = 0.d0
            dsidp2(k) = 0.d0
            do 126 l = 1, 6
                dsidep(k,l) = 0.d0
126          continue
125      continue
!
        do 127 k = 1, 3
            do 128 l = 1, 3
                dsidep(k,l) = k0-deuxmu/3.d0
128          continue
127      continue
        do 129 k = 1, ndimsi
            dsidep(k,k) = dsidep(k,k)+deuxmu
129      continue
!
    endif
!
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
!
        do 139 k = 1, ndimsi
            dsidp1(k) = dpgfp1(biot,betam,pref,p1)
139      continue
!
    endif
end subroutine
