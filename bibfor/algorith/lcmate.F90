subroutine lcmate(fami, kpg, ksp, comp, mod,&
                  imat, nmat, tempd, tempf, impexp,&
                  typma, hsr, materd, materf, matcst,&
                  nbcomm, cpmono, angmas, pgl, itmax,&
                  toler, ndt, ndi, nr, crit,&
                  nvi, vind, nfs, nsg, toutms,&
                  nhsr, numhsr, sigd)
    implicit   none
! ======================================================================
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE CRP_21
! person_in_charge: jean-michel.proix at edf.fr
!       ----------------------------------------------------------------
!       RECUPERATION DU MATERIAU A TEMPF ET TEMPD
!       IN  FAMI   :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!           KPG,KSP:  NUMERO DU (SOUS)POINT DE GAUSS
!           COMP   :  COMPORTEMENT
!           MOD    :  TYPE DE MODELISATION
!           IMAT   :  ADRESSE DU MATERIAU CODE
!           NMAT   :  DIMENSION 1 DE MATER
!           TEMPD  :  TEMPERATURE A T
!           TEMPF  :  TEMPERATURE A T + DT
!           IMPEXP : 0 IMPLICITE, 1 EXPLICITE
!          ANGMAS  :  LES TROIS ANGLES DU MOT_CLEF MASSIF
!           SIGD   :  ETAT DE CONTRAINTES A T
!       OUT MATERD :  COEFFICIENTS MATERIAU A T    (TEMPD )
!           MATERF :  COEFFICIENTS MATERIAU A T+DT (TEMPF )
!                     MATER(*,I) = CARACTERISTIQUES MATERIAU
!                                    I = 1  CARACTERISTIQUES ELASTIQUES
!                                    I = 2  CARACTERISTIQUES PLASTIQUES
!           MATCST :  'OUI' SI  MATERIAU A T = MATERIAU A T+DT
!                     'NON' SINON OU 'NAP' SI NAPPE DANS 'VECMAT.F'
!           NBCOMM : POSITION DES COEF POUR CHAQUE LOI DE CHAQUE SYSTEME
!           CPMONO : NOMS DES LOIS POUR CHAQUE FAMILLE DE SYSTEME
!           PGL    : MATRICE DE PASSAGE
!           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
!           NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
!           NR     :  NB DE COMPOSANTES SYSTEME NL
!           NVI    :  NB DE VARIABLES INTERNES
!           TOUTMS :  TOUS LES TENSEURS MS
!           HSR    : MATRICE D'INTERACTION POUR L'ECROUISSAGE ISOTROPE
!                    UTILISEE SEULEMENT POUR LE MONOCRISTAL IMPLICITE
!       ----------------------------------------------------------------
    include 'asterfort/assert.h'
    include 'asterfort/burmat.h'
    include 'asterfort/cvmmat.h'
    include 'asterfort/haymat.h'
    include 'asterfort/hbrmat.h'
    include 'asterfort/hujma2.h'
    include 'asterfort/irrmat.h'
    include 'asterfort/lcmatt.h'
    include 'asterfort/lcmmap.h'
    include 'asterfort/lcmmat.h'
    include 'asterfort/lglmat.h'
    include 'asterfort/lkimat.h'
    include 'asterfort/matect.h'
    include 'asterfort/rslmat.h'
    include 'asterfort/rsvmat.h'
    include 'asterfort/vecmat.h'
    integer :: imat, nmat, ndt, ndi, nr, nvi, i, itmax, kpg, ksp, impexp
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2), tempd, tempf
    real(kind=8) :: vind(*), pgl(3, 3), angmas(3), toler, crit(*), sigd(6)
    character(len=16) :: loi, comp(*)
    character(len=8) :: mod, typma
    character(len=3) :: matcst
    character(len=*) :: fami
!     SPECIFIQUE MONOCRISTAL
    integer :: numhsr(*), nbcomm(*), nfs, nsg, nhsr
    real(kind=8) :: hsr(*), toutms(*)
    character(len=24) :: cpmono(*)
!       ----------------------------------------------------------------
!
! -     INITIALISATION DE MATERD ET MATERF A 0.
!
    do 10 i = 1, nmat
        materd(i,1) = 0.d0
        materd(i,2) = 0.d0
        materf(i,1) = 0.d0
        materf(i,2) = 0.d0
10  end do
!
    loi = comp(1)
    if (loi(1:8) .eq. 'ROUSS_PR') then
        call rslmat(fami, kpg, ksp, mod, imat,&
                    nmat, materd, materf, matcst, ndt,&
                    ndi, nr, nvi, vind)
!
    else if (loi(1:10) .eq. 'ROUSS_VISC') then
        call rsvmat(fami, kpg, ksp, mod, imat,&
                    nmat, materd, materf, matcst, ndt,&
                    ndi, nr, nvi, vind)
!
    else if (loi(1:9) .eq. 'VISCOCHAB') then
        call cvmmat(fami, kpg, ksp, mod, imat,&
                    nmat, materd, materf, matcst, typma,&
                    ndt, ndi, nr, crit, vind,&
                    nvi, sigd)
!
    else if (loi.eq.'VENDOCHAB'.or.loi.eq.'VISC_ENDO_LEMA') then
        call vecmat(fami, kpg, ksp, mod, loi,&
                    imat, nmat, materd, materf, matcst,&
                    typma, ndt, ndi, nr, nvi)
!
    else if (loi(1:6) .eq. 'LAIGLE') then
        call lglmat(mod, imat, nmat, tempd, materd,&
                    materf, matcst, ndt, ndi, nr,&
                    nvi)
!
        elseif (( loi(1:10) .eq. 'HOEK_BROWN' ).or. ( loi(1:14) .eq.&
    'HOEK_BROWN_EFF' ))then
        call hbrmat(mod, imat, nmat, tempd, materd,&
                    materf, matcst, ndt, ndi, nr,&
                    nvi)
!
    else if (loi(1:8) .eq. 'MONOCRIS') then
        call lcmmat(fami, kpg, ksp, comp, mod,&
                    imat, nmat, angmas, pgl, materd,&
                    materf, matcst, nbcomm, cpmono, ndt,&
                    ndi, nr, nvi, hsr, nfs,&
                    nsg, toutms, vind, impexp)
        typma='COHERENT'
        if (mod .ne. '3D') then
            sigd(5)=0.d0
            sigd(6)=0.d0
        endif
!
    else if (loi(1:8) .eq. 'POLYCRIS') then
        call lcmmap(fami, kpg, ksp, comp, mod,&
                    imat, nmat, angmas, pgl, materd,&
                    materf, matcst, nbcomm, cpmono, ndt,&
                    ndi, nr, nvi, nfs, nsg,&
                    nhsr, numhsr, hsr)
        typma='COHERENT'
!
    else if (loi(1:7) .eq. 'IRRAD3M') then
        call irrmat(fami, kpg, ksp, mod, imat,&
                    nmat, itmax, toler, materd, materf,&
                    matcst, ndt, ndi, nr, nvi)
!
    else if (loi(1:15) .eq. 'BETON_BURGER_FP') then
        call burmat(fami, kpg, ksp, mod, imat,&
                    nmat, materd, materf, matcst, ndt,&
                    ndi, nr, nvi)
        typma='COHERENT'
    else if (loi(1:4) .eq. 'LETK') then
        call lkimat(mod, imat, nmat, materd, materf,&
                    matcst, ndt, ndi, nvi, nr)
        typma='COHERENT'
    else if (loi .eq. 'HAYHURST') then
        call haymat(fami, kpg, ksp, mod, imat,&
                    nmat, '-', materd(1, 1), materd(1, 2), nvi,&
                    nr)
        call haymat(fami, kpg, ksp, mod, imat,&
                    nmat, '+', materf(1, 1), materf(1, 2), nvi,&
                    nr)
        call matect(materd, materf, nmat, matcst)
        typma='COHERENT'
    else if (loi(1:6) .eq. 'HUJEUX') then
        call hujma2(mod, imat, nmat, tempf, angmas,&
                    sigd, vind, materd, materf, ndt,&
                    ndi, nvi, nr, matcst)
        typma='COHERENT'
    else
!
! CAS GENERAL
!
        call lcmatt(fami, kpg, ksp, mod, imat,&
                    nmat, '-', comp, materd(1, 1), materd(1, 2),&
                    typma, ndt, ndi, nr, nvi)
        call lcmatt(fami, kpg, ksp, mod, imat,&
                    nmat, '+', comp, materf(1, 1), materf(1, 2),&
                    typma, ndt, ndi, nr, nvi)
!
        call matect(materd, materf, nmat, matcst)
!
    endif
!
!     - DANS LCPLNL ON DIMENSIONNE DES TABLES AVEC (NDT+NVI) QUI SONT
!       ENSUITE UTILISEES PAR NEWTON
!     - LA DIMENSION DU SYSTEME DIFFERENTIEL EST NR
!     ==> IL FAUT DONC NDT+NVI >= NR
    call assert((ndt+nvi).ge.nr)
end subroutine
