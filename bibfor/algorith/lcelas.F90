subroutine lcelas(loi, mod, imat, nmat, materd,&
                  materf, matcst, nvi, angmas, deps,&
                  sigd, vind, sigf, vinf, theta,&
                  etatd, crit, iret)
    implicit   none
!       ================================================================
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
!       ----------------------------------------------------------------
!       INTEGRATION ELASTIQUE SUR DT
!       IN  LOI    :  NOM DU MODELE DE COMPORTEMENT
!           MOD    :  MODELISATION
!           NMAT   :  DIMENSION MATER
!           MATERD :  COEFFICIENTS MATERIAU A T
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!           ANGMAS :  ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
!           VIND   :  VARIABLES INTERNES A T
!           SIGD   :  CONTRAINTE  A T
!       VAR DEPS   :  INCREMENT DE DEFORMATION
!       OUT SIGF   :  CONTRAINTE A T+DT
!           VINF   :  VARIABLES INTERNES A T+DT
!           IRET   :  CODE RETOUR (O-->OK / 1-->NOOK)
!       ----------------------------------------------------------------
    include 'asterfort/hujpel.h'
    include 'asterfort/lcelin.h'
    include 'asterfort/lksige.h'
    include 'asterfort/rsllin.h'
    integer :: nmat, nvi, imat, iret
!
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2)
    real(kind=8) :: theta
    real(kind=8) :: sigd(6), sigf(6)
    real(kind=8) :: vind(*), vinf(*)
    real(kind=8) :: deps(6), crit(*)
    real(kind=8) :: angmas(3)
!
    character(len=8) :: mod
    character(len=16) :: loi
    character(len=3) :: matcst
    character(len=7) :: etatd
!
! --- INITIALISATION VARIABLE CODE RETOUR
    iret = 0
!
    if (loi(1:8) .eq. 'ROUSS_PR' .or. loi(1:10) .eq. 'ROUSS_VISC') then
        call rsllin(mod, nmat, materd, materf, matcst,&
                    deps, sigd, vind, sigf, theta)
    else if (loi(1:4).eq.'LETK') then
!        ELASTICITE NON LINEAIRE ISOTROPE POUR LETK
        call lksige(mod, nmat, materd, deps, sigd,&
                    sigf)
    else if (loi(1:6).eq.'HUJEUX') then
!        ELASTICITE NON LINEAIRE ISOTROPE POUR HUJEUX
        call hujpel(etatd, mod, crit, imat, nmat,&
                    materf, angmas, deps, sigd, nvi,&
                    vind, sigf, vinf, iret)
    else
!        ELASTICITE LINEAIRE ISOTROPE OU ANISOTROPE
        call lcelin(mod, nmat, materd, materf, deps,&
                    sigd, sigf)
!
    endif
!
end subroutine
