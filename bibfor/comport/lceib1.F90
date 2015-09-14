subroutine lceib1(fami, kpg, ksp, imate, compor,&
                  ndim, epsm, sref, sechm, hydrm,&
                  t, lambda, deuxmu, epsthe, kdess,&
                  bendo, gamma, seuil)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
#include "asterfort/verift.h"
    character(len=16) :: compor(*)
    character(len=*) :: fami
    integer :: imate, ndim, t(3, 3), kpg, ksp
    real(kind=8) :: epsm(6), lambda, deuxmu, epsthe(2), kdess, bendo
    real(kind=8) :: gamma, seuil
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT ENDO_ISOT_BETON - INITIALISATION
!
! IN  COMPOR     : NOM DE LA LOI DE COMPORTEMENT
! IN  IMATE      : CODE MATERIAU
! IN  EPSM       : DEFORMATION AU TEMPS MOINS
! IN  TM         : TEMPERATURE A T-
! IN  TREF       : TEMPERATURE DE REFERENCE
! IN  SREF       : SECHAGE DE REFEERNCE
! IN  SECHM      : SECHAGE AU TEMPS -
! IN  HYDRM      : HYDRATATION AU TEMPS-
! OUT T          : TENSEUR DE PLACEMENT (PASSAGE VECT -> MATRICE)
! OUT LAMBDA
! OUT DEUXMU
! OUT ALPHA
! OUT KDESS
! OUT BENDO
! OUT GAMMA
! OUT SEUIL
! ----------------------------------------------------------------------
!
    integer :: icodre(3)
    character(len=16) :: nomres(3)
    integer :: i, k, ndimsi
    real(kind=8) :: valres(3), e, nu
    real(kind=8) :: sref, sechm, hydrm
    real(kind=8) :: k0, k1, sicr, trepsm, eps(6), kron(6)
    data        kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
!
!
    ndimsi=2*ndim
    t(1,1)=1
    t(1,2)=4
    t(1,3)=5
    t(2,1)=4
    t(2,2)=2
    t(2,3)=6
    t(3,1)=5
    t(3,2)=6
    t(3,3)=3
!
    if ((.not.( compor(1)(1:15) .eq. 'ENDO_ISOT_BETON')) .and.&
        (.not.( compor(1)(1:6) .eq. 'KIT_HM')) .and.&
        (.not.( compor(1)(1:7) .eq. 'KIT_HHM')) .and.&
        (.not.( compor(1)(1:7) .eq. 'KIT_THM')) .and.&
        (.not.( compor(1)(1:8) .eq. 'KIT_THHM')) .and.&
        (.not.( compor(1)(1:7) .eq. 'KIT_DDI'))) then
        call utmess('F', 'ALGORITH4_50', sk=compor(1))
    endif
!    LECTURE DES CARACTERISTIQUES DU MATERIAU
    nomres(1) = 'E'
    nomres(2) = 'NU'
    nomres(3) = 'ALPHA'
!
    if (&
         (compor(1)(1:15) .eq. 'ENDO_ISOT_BETON')&
          .or.&
         (&
           (&
             (compor(1)(1:6) .eq. 'KIT_HM') .or. (compor(1)(1:7) .eq. 'KIT_HHM') .or.&
             (compor(1)(1:7) .eq. 'KIT_THM').or. (compor(1)(1:8) .eq. 'KIT_THHM')&
           )&
          .and. &
          (compor(11)(1:15) .eq. 'ENDO_ISOT_BETON')&
         )&
       ) then
!
        call rcvalb(fami, kpg, ksp, '+', imate,&
                    ' ', 'ELAS', 0, ' ', [0.d0],&
                    2, nomres, valres, icodre, 1)
        call verift(fami, kpg, ksp, '-', imate,&
                    epsth_=epsthe(1))
        call verift(fami, kpg, ksp, '+', imate,&
                    epsth_=epsthe(2))
!
        e = valres(1)
        nu = valres(2)
!
        lambda = e * nu / (1.d0+nu) / (1.d0 - 2.d0*nu)
        deuxmu = e/(1.d0+nu)
!
!    LECTURE DES CARACTERISTIQUES DE RETRAIT ENDOGENE ET DESSICCATION
        nomres(1)='B_ENDOGE'
        nomres(2)='K_DESSIC'
        call rcvalb(fami, 1, 1, '+', imate,&
                    ' ', 'ELAS', 0, ' ', [0.d0],&
                    2, nomres, valres, icodre, 0)
        if (icodre(1) .ne. 0) valres(1) = 0.d0
        if (icodre(2) .ne. 0) valres(2) = 0.d0
        bendo=valres(1)
        kdess=valres(2)
!
!    LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
        nomres(1) = 'D_SIGM_EPSI'
        nomres(2) = 'SYT'
        nomres(3) = 'SYC'
        call rcvalb(fami, 1, 1, '+', imate,&
                    ' ', 'BETON_ECRO_LINE', 0, ' ', [0.d0],&
                    3, nomres, valres, icodre, 0)
        if ((icodre(1).ne.0) .or. (icodre(2).ne.0)) then
            call utmess('F', 'ALGORITH4_51')
        endif
        gamma = - e/valres(1)
        k0=valres(2)**2 *(1.d0+gamma)/(2.d0*e) *(1.d0+nu-2.d0*nu**2)/(&
        1.d0+nu)
        if (nu .eq. 0) then
            if (icodre(3) .eq. 0) then
                call utmess('F', 'ALGORITH4_52')
            else
                seuil=k0
            endif
        else
            sicr=sqrt((1.d0+nu-2.d0*nu**2)/(2.d0*nu**2))*valres(2)
            if (icodre(3) .eq. 1) then
                seuil=k0
            else
                if (valres(3) .lt. sicr) then
                    call utmess('F', 'ALGORITH4_53')
                else
                    k1=valres(3)*(1.d0+gamma)*nu**2/(1.d0+nu)/(1.d0-&
                    2.d0*nu) -k0*e/(1.d0-2.d0*nu)/valres(3)
!      PASSAGE AUX DEFORMATIONS ELASTIQUES
                    call r8inir(6, 0.d0, eps, 1)
                    do 5 k = 1, ndimsi
                        eps(k) = epsm(k) - ( epsthe(1) - kdess * ( sref-sechm) - bendo * hydrm ) &
                                 &* kron(k)
  5                 continue
                    trepsm=0.d0
                    do 1 i = 1, ndim
                        trepsm=trepsm+eps(i)
  1                 continue
                    if (trepsm .gt. 0.d0) then
                        trepsm=0.d0
                    endif
                    seuil = k0-k1*trepsm
                endif
            endif
        endif
    endif
!
end subroutine
