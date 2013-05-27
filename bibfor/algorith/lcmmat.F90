subroutine lcmmat(fami, kpg, ksp, comp, mod,&
                  imat, nmat, angmas, pgl, materd,&
                  materf, matcst, nbcomm, cpmono, ndt,&
                  ndi, nr, nvi, hsr, nfs,&
                  nsg, toutms, vind, impexp)
    implicit none
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! person_in_charge: jean-michel.proix at edf.fr
! TOLE CRP_21 CRS_1404
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
!       ----------------------------------------------------------------
!       MONOCRISTAL : RECUPERATION DU MATERIAU A T(TEMPD) ET T+DT(TEMPF)
!                    NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES
!                    MATER(*,1) = E , NU , ALPHA
!                    MATER(*,2) = COEFFICIENT DE CHAQUE COMPORTEMENT
!                    VARIABLES INTERNES :
!                     EPSVP(6)+ALPHA,GAMMA,P PAR SYSTEME DE GLISSEMENT
!       ----------------------------------------------------------------
!       IN  FAMI   : FAMILLE DE POINT DE GAUSS
!            KPG   : NUMERO DU POINT DE GAUSS
!            KSP   : NUMERO DU SOUS-POINT DE GAUSS
!           COMP   :  GRANDEUR COMPOR
!           IMAT   :  ADRESSE DU MATERIAU CODE
!           MOD    :  TYPE DE MODELISATION
!           NMAT   :  DIMENSION  MAXIMUM DE MATER
!          ANGMAS  :  TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
!       OUT MATERD :  COEFFICIENTS MATERIAU A T
!           PGL    : MATRICE DE PASSAGE GLOBAL LOCAL
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!                     MATER(*,1) = CARACTERISTIQUES   ELASTIQUES
!                     MATER(*,2) = CARACTERISTIQUES   PLASTIQUES
!           MATCST :  'OUI' SI  MATERIAU A T = MATERIAU A T+DT
!                     'NON' SINON
!           NBCOMM : POSITION DES COEF POUR CHAQUE LOI DE CHAQUE SYSTEME
!           CPMONO : NOMS DES LOIS POUR CHAQUE FAMILLE DE SYSTEME
!
!           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
!           NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
!           NR     :  NB DE COMPOSANTES SYSTEME NL
!           NVI    :  NB DE VARIABLES INTERNES
!           HSR    : MATRICE D'INTERACTION POUR L'ECROUISSAGE ISOTROPE
!                    UTILISEE SEULEMENT POUR LE MONOCRISTAL IMPLICITE
!           TOUTMS : TOUS LES TENSEURS D'ORIENTATION POUR TOUS LES
!                    SYSTEMES DE GLISSEMENT
!           IMPEXP : 0 IMPLICITE, 1 EXPLICITE
!       ----------------------------------------------------------------
    include 'jeveux.h'
    include 'asterc/r8vide.h'
    include 'asterfort/calcmm.h'
    include 'asterfort/d1ma3d.h'
    include 'asterfort/dmat3d.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/lcmaec.h'
    include 'asterfort/lcmaei.h'
    include 'asterfort/lcmafl.h'
    include 'asterfort/lcmmjv.h'
    include 'asterfort/lcmmsg.h'
    include 'asterfort/matrot.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rccoma.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    integer :: nmat, ndt, ndi, nr, nvi, nbcomm(nmat, 3), nbval, nvini
    integer :: kpg, ksp, irota, impexp, nfs, nsg
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2)
    real(kind=8) :: hook(6, 6)
    real(kind=8) :: repere(7), xyz(3), kooh(6, 6), q(3, 3)
    real(kind=8) :: epsi, angmas(3), pgl(3, 3), hookf(6, 6)
    real(kind=8) :: valres(nmat), ms(6), ng(3), lg(3), vind(*)
    real(kind=8) :: hsr(nsg, nsg), toutms(nfs, nsg, 6)
    character(len=8) :: mod, nomc(14)
    integer :: cerr(14)
    character(len=3) :: matcst
    character(len=*) :: fami
    character(len=16) :: comp(*), nmater, necoul, necris, necrci
    character(len=16) :: phenom, nomfam
    character(len=24) :: cpmono(5*nmat+1)
    integer :: i, imat, nbfsys, ifa, j, dimtms, itbint
    integer :: nbsyst, nbsys
!     ----------------------------------------------------------------
!
! -   NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
!
!
    call jemarq()
!
    if (mod(1:2) .eq. '3D') then
        ndt = 6
        ndi = 3
    else if (mod(1:6).eq.'D_PLAN'.or.mod(1:4).eq.'AXIS') then
        ndt = 6
        ndi = 3
    else if (mod(1:6).eq.'C_PLAN') then
        ndt = 6
        ndi = 3
    endif
    dimtms=nfs*nsg*6
    call r8inir(dimtms, 0.d0, toutms, 1)
    call r8inir(2*nmat, 0.d0, materd, 1)
    call r8inir(2*nmat, 0.d0, materf, 1)
!
    read (comp(2),'(I16)') nvi
!
    call lcmmjv(comp, nmat, cpmono, nbfsys, irota,&
                itbint, nfs, nsg, hsr)
!
    if (impexp .eq. 1) then
        if (irota .ne. 0) then
            call u2mess('F', 'COMPOR2_11')
        endif
    endif
!
!     LA DERNIERE VARIABLE INTERNE EST L'INDICATEUR PLASTIQUE
!
    call matrot(angmas, pgl)
!
    do 111 i = 1, nmat
        do 111 j = 1, 3
            nbcomm(i,j)=0
111      continue
    nbcomm(1,1)=1
!
    do 6 ifa = 1, nbfsys
        nomfam=cpmono(5*(ifa-1)+1)
        call lcmmsg(nomfam, nbsys, 0, pgl, ms,&
                    ng, lg, 0, q)
!
        nmater=cpmono(5*(ifa-1)+2)
        necoul=cpmono(5*(ifa-1)+3)
        necris=cpmono(5*(ifa-1)+4)
        necrci=cpmono(5*(ifa-1)+5)
!
!        COEFFICIENTS MATERIAUX LIES A L'ECOULEMENT
        call lcmafl(fami, kpg, ksp, '-', nmater,&
                    imat, necoul, nbval, valres, nmat,&
                    itbint, nfs, nsg, hsr, nbsys)
        nvini=nbcomm(ifa,1)
        if (necoul .eq. 'MONO_DD_KR') then
            nbval=nbval+1
!           une seule matrice d'interaction pour le monocristal
            valres(nbval)=1
        endif
        do 501 i = 1, nbval
            materd(nvini-1+i,2)=valres(i)
501      continue
        nbcomm(ifa,2)=nvini+nbval
!
!        COEFFICIENTS MATERIAUX LIES A L'ECROUISSAGE CINEMATIQUE
        call lcmaec(fami, kpg, ksp, '-', nmater,&
                    imat, necrci, nbval, valres, nmat)
        nvini=nbcomm(ifa,2)
        do 502 i = 1, nbval
            materd(nvini-1+i,2)=valres(i)
502      continue
        nbcomm(ifa,3)=nvini+nbval
!
!        COEFFICIENTS MATERIAUX LIES A L'ECROUISSAGE ISOTROPE
        call lcmaei(fami, kpg, ksp, '-', nmater,&
                    imat, necris, necoul, nbval, valres,&
                    nmat, itbint, nfs, nsg, hsr,&
                    ifa, nomfam, nbsys)
        nbval=nbval+1
!        une seule matrice d'interaction pour le monocristal
        valres(nbval)=1
        nvini=nbcomm(ifa,3)
        do 503 i = 1, nbval
            materd(nvini-1+i,2)=valres(i)
503      continue
        nbcomm(ifa+1,1)=nvini+nbval
!
!
 6  end do
!     ON STOCKE A LA FIN LE NOMBRE TOTAL DE COEF MATERIAU
    nbcomm(nmat,2)=nbfsys
    nbcomm(nmat,3)=nbcomm(nbfsys+1,1)+1
    nbcomm(1,1)=1
!
    nbsyst=0
!
    do 61 ifa = 1, nbfsys
!
        nomfam=cpmono(5*(ifa-1)+1)
        call lcmmsg(nomfam, nbsys, 0, pgl, ms,&
                    ng, lg, 0, q)
        nmater=cpmono(5*(ifa-1)+2)
        necoul=cpmono(5*(ifa-1)+3)
        necris=cpmono(5*(ifa-1)+4)
        necrci=cpmono(5*(ifa-1)+5)
!
        nbsyst=nbsyst+nbsys
!
        call lcmafl(fami, kpg, ksp, '+', nmater,&
                    imat, necoul, nbval, valres, nmat,&
                    itbint, nfs, nsg, hsr, nbsys)
        nvini=nbcomm(ifa,1)
        if (necoul .eq. 'MONO_DD_KR') then
            nbval=nbval+1
!           une seule matrice d'interaction pour le monocristal
            valres(nbval)=1
        endif
        do 504 i = 1, nbval
            materf(nvini-1+i,2)=valres(i)
504      continue
        nbcomm(ifa,2)=nvini+nbval
!
        call lcmaec(fami, kpg, ksp, '+', nmater,&
                    imat, necrci, nbval, valres, nmat)
        nvini=nbcomm(ifa,2)
        do 505 i = 1, nbval
            materf(nvini-1+i,2)=valres(i)
505      continue
        nbcomm(ifa,3)=nvini+nbval
!
        call lcmaei(fami, kpg, ksp, '+', nmater,&
                    imat, necris, necoul, nbval, valres,&
                    nmat, itbint, nfs, nsg, hsr,&
                    ifa, nomfam, nbsys)
        nvini=nbcomm(ifa,3)
        nbval=nbval+1
!        une seule matrice d'interaction pour le monocristal
        valres(nbval)=1
        do 506 i = 1, nbval
            materf(nvini-1+i,2)=valres(i)
506      continue
        nbcomm(ifa+1,1)=nvini+nbval
!
61  end do
!
    call rccoma(imat, 'ELAS', 1, phenom, cerr)
!
    if (phenom .eq. 'ELAS') then
!
! -    ELASTICITE ISOTROPE
!
        nomc(1) = 'E       '
        nomc(2) = 'NU      '
        nomc(3) = 'ALPHA   '
!
! -     RECUPERATION MATERIAU A TEMPD (T)
!
        call rcvalb(fami, kpg, ksp, '-', imat,&
                    ' ', 'ELAS', 0, ' ', 0.d0,&
                    2, nomc(1), materd(1, 1), cerr(1), 1)
        call rcvalb(fami, kpg, ksp, '-', imat,&
                    ' ', 'ELAS', 0, ' ', 0.d0,&
                    1, nomc(3), materd(3, 1), cerr(3), 0)
        materd(nmat,1)=0
!
! -     RECUPERATION MATERIAU A TEMPF (T+DT)
!
        call rcvalb(fami, kpg, ksp, '+', imat,&
                    ' ', 'ELAS', 0, '   ', 0.d0,&
                    2, nomc(1), materf(1, 1), cerr(1), 1)
        call rcvalb(fami, kpg, ksp, '+', imat,&
                    ' ', 'ELAS', 0, '  ', 0.d0,&
                    1, nomc(3), materf(3, 1), cerr(3), 0)
        if (cerr(3) .ne. 0) materf(3,1) = 0.d0
        materf(nmat,1)=0
!
    else if (phenom.eq.'ELAS_ORTH') then
!
        repere(1)=1
        do 21 i = 1, 3
            repere(i+1)=angmas(i)
21      continue
!
! -    ELASTICITE ORTHOTROPE
!
!
! -     MATRICE D'ELASTICITE ET SON INVERSE A TEMPD(T)
!
        call dmat3d(fami, imat, r8vide(), '-', kpg,&
                    ksp, repere, xyz, hook)
        call d1ma3d(fami, imat, r8vide(), '-', kpg,&
                    ksp, repere, xyz, kooh)
!
        do 67 j = 4, 6
            do 67 i = 1, 6
                hook(i,j) = hook(i,j)*sqrt(2.d0)
67          continue
        do 68 j = 1, 6
            do 68 i = 4, 6
                hook(i,j) = hook(i,j)*sqrt(2.d0)
68          continue
        do 69 j = 4, 6
            do 69 i = 1, 6
                kooh(i,j) = kooh(i,j)/sqrt(2.d0)
69          continue
        do 70 j = 1, 6
            do 70 i = 4, 6
                kooh(i,j) = kooh(i,j)/sqrt(2.d0)
70          continue
!
        do 101 i = 1, 6
            do 102 j = 1, 6
                materd(6*(j-1)+i,1)=hook(i,j)
                materd(36+6*(j-1)+i,1)=kooh(i,j)
102          continue
101      continue
!
        materd(nmat,1)=1
!
        nomc(1) = 'ALPHA_L'
        nomc(2) = 'ALPHA_T'
        nomc(3) = 'ALPHA_N'
!
        call rcvalb(fami, kpg, ksp, '-', imat,&
                    ' ', phenom, 0, ' ', 0.d0,&
                    3, nomc, materd(73, 1), cerr, 0)
        if (cerr(1) .ne. 0) materd(73,1) = 0.d0
        if (cerr(2) .ne. 0) materd(74,1) = 0.d0
        if (cerr(3) .ne. 0) materd(75,1) = 0.d0
!
!
!
! -     MATRICE D'ELASTICITE ET SON INVERSE A A TEMPF (T+DT)
!
        call dmat3d(fami, imat, r8vide(), '+', kpg,&
                    ksp, repere, xyz, hookf)
        call d1ma3d(fami, imat, r8vide(), '+', kpg,&
                    ksp, repere, xyz, kooh)
!
        do 671 j = 4, 6
            do 671 i = 1, 6
                hookf(i,j) = hookf(i,j)*sqrt(2.d0)
671          continue
        do 681 j = 1, 6
            do 681 i = 4, 6
                hookf(i,j) = hookf(i,j)*sqrt(2.d0)
681          continue
        do 691 j = 4, 6
            do 691 i = 1, 6
                kooh(i,j) = kooh(i,j)/sqrt(2.d0)
691          continue
        do 701 j = 1, 6
            do 701 i = 4, 6
                kooh(i,j) = kooh(i,j)/sqrt(2.d0)
701          continue
        do 103 i = 1, 6
            do 104 j = 1, 6
                materf(6*(j-1)+i,1)=hookf(i,j)
                materf(36+6*(j-1)+i,1)=kooh(i,j)
104          continue
103      continue
!
        materf(nmat,1)=1
!
        call rcvalb(fami, kpg, ksp, '+', imat,&
                    ' ', phenom, 0, ' ', 0.d0,&
                    3, nomc, materd(73, 1), cerr, 0)
        if (cerr(1) .ne. 0) materf(73,1) = 0.d0
        if (cerr(2) .ne. 0) materf(74,1) = 0.d0
        if (cerr(3) .ne. 0) materf(75,1) = 0.d0
!
    else
        call u2mesk('F', 'ALGORITH4_65', 1, phenom)
    endif
!
    nr=ndt+nbsyst
    call calcmm(nbcomm, cpmono, nmat, pgl, nfs,&
                nsg, toutms, comp, nvi, vind,&
                irota)
!
! -   MATERIAU CONSTANT ?
!
    matcst = 'OUI'
    epsi=1.d-3
    do 30 i = 1, nmat
        if (abs(materd(i,1)-materf(i,1) ) .gt. epsi*materd(i,1)) then
            matcst = 'NON'
            goto 9999
        endif
30  end do
    do 40 i = 1, nmat
        if (abs(materd(i,2)-materf(i,2) ) .gt. epsi*materd(i,2)) then
            matcst = 'NON'
            call u2mess('F', 'COMPOR1_27')
            goto 9999
        endif
40  end do
!
9999  continue
!
!     ON STOCKE A LA FIN LE NOMBRE TOTAL DE COEF MATERIAU
!      MATERD(NMAT,2)=NBCOMM(NMAT,3)
!      MATERF(NMAT,2)=NBCOMM(NMAT,3)
!
    call jedema()
!
end subroutine
