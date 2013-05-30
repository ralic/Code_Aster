subroutine lcpopl(loi, angmas, nmat, materd, materf,&
                  mod, deps, sigd, sigf, vind,&
                  vinf)
    implicit none
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
! ======================================================================
!     ----------------------------------------------------------------
!     ROUTINE DE POST-TRAITEMENT POUR CERTAINES LOIS
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!     IN  NMAT    :  NOMBRE DE PARAMETRES MATERIAU INELASTIQUE
!         MATERD :  COEFFICIENTS MATERIAU A T
!         MATERF :  COEFFICIENTS MATERIAU A T+DT
!         MOD    :  TYPE DE MODELISATION
!         ANGMAS :  ANGLES NAUTIQUES (AFFE_CARA_ELEM)
!     OUT SIGF   :  CONTRAINTE A T+DT
!         VINF   :  VARIABLES INTERNES A T+DT
!     ----------------------------------------------------------------
!
    include 'asterc/r8prem.h'
    include 'asterc/r8vide.h'
    include 'asterfort/hujori.h'
    include 'asterfort/lgldcm.h'
    include 'asterfort/u2mess.h'
    integer :: nmat
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2), sigf(*), vind(*), vinf(*)
    real(kind=8) :: angmas(3), sigd(6), deps(6)
    character(len=8) :: mod
    character(len=16) :: loi
!
    real(kind=8) :: bid66(6, 6), hill, dsig(6), nsig, neps
    real(kind=8) :: zero, un, deux, dix
    logical :: reorie
    integer :: i, ndt
!
    parameter (ndt  = 6   )
    parameter (zero = 0.d0)
    parameter (un   = 1.d0)
    parameter (deux = 2.d0)
    parameter (dix  = 1.d1)
!
    if (loi(1:6) .eq. 'LAIGLE') then
        call lgldcm(nmat, materf, sigf, vinf)
    endif
!
! --  CONTRAINTES PLANES
    if (mod(1:6) .eq. 'C_PLAN') sigf(3) = 0.d0
!
    if (loi .eq. 'HAYHURST') then
        materd(1,1)=materd(1,1)*(1.0d0-vind(11))
        materf(1,1)=materf(1,1)*(1.0d0-vinf(11))
    endif
    if (loi .eq. 'VENDOCHAB') then
        materd(1,1)=materd(1,1)*(1.0d0-vind(9))
        materf(1,1)=materf(1,1)*(1.0d0-vinf(9))
    endif
!
    if (loi(1:6) .eq. 'HUJEUX') then
! --- 1 ORIENTATION DES CONTRAINTES SELON ANGMAS VERS REPERE GLOBAL
        if (angmas(1) .eq. r8vide()) call u2mess('F', 'ALGORITH8_20')
        reorie =(angmas(1).ne.zero) .or. (angmas(2).ne.zero) .or. (&
        angmas(3).ne.zero)
        call hujori('GLOBA', 1, reorie, angmas, sigd,&
                    bid66)
        call hujori('GLOBA', 1, reorie, angmas, deps,&
                    bid66)
        call hujori('GLOBA', 1, reorie, angmas, sigf,&
                    bid66)
!
! --- TRAVAIL DU 2ND ORDRE
        hill = zero
        nsig = zero
        neps = zero
        do 10 i = 1, ndt
            dsig(i) = sigf(i) - sigd(i)
            hill = hill + dsig(i)*deps(i)
            nsig = nsig + dsig(i)**2.d0
            neps = neps + deps(i)**2.d0
10      continue
!
! --- NORMALISATION DU CRITERE : VARIE ENTRE -1 ET 1
        if ((neps.gt.r8prem()) .and. (nsig.gt.r8prem())) then
            vinf(32) = hill/sqrt(neps*nsig)
        else
            vinf(32) = zero
        endif
!
        vinf(34) = zero
        do 20 i = 1, 8
            if (abs(vinf(23+i)-un) .lt. r8prem()) then
                if (i .eq. 1) vinf(34)=vinf(34)+dix**zero
                if (i .eq. 2) vinf(34)=vinf(34)+dix**un
                if (i .eq. 3) vinf(34)=vinf(34)+dix**deux
                if (i .eq. 4) vinf(34)=vinf(34)+dix**3.d0
                if (i .eq. 5) vinf(34)=vinf(34)+dix**4.d0
                if (i .eq. 6) vinf(34)=vinf(34)+dix**5.d0
                if (i .eq. 7) vinf(34)=vinf(34)+dix**6.d0
                if (i .eq. 8) vinf(34)=vinf(34)+dix**7.d0
            endif
20      continue
!
    endif
!
end subroutine
