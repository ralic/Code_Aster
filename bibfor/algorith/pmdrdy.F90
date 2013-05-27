subroutine pmdrdy(dsidep, coef, cimpo, valimp, y,&
                  sigp, r, drdy)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: jean-michel.proix at edf.fr
!-----------------------------------------------------------------------
!     OPERATEUR CALC_POINT_MAT : CONSTRUCTION RESIDU ET JACOBIENNE
!-----------------------------------------------------------------------
!      IDEM NMDORC MAIS SANS MODELE
! ----------------------------------------------------------------------
! IN  DSIDEP  : MATRICE TANGENTE
! IN   COEF   : COEF POUR ADIMENSIONNALISER LE PB
! IN  CIMPO   : = 1 POUR LA CMP DE EPSI OU SIGM IMPOSEE
! IN  VALIMP  : VALEUR DE LA CMP DE EPSI OU SIGM IMPOSEE
! IN  Y       : VECTEUR INCONNUE Y(1 A 6) = SIG/COEF, Y(7 A 12) = EPSI
! IN   SIGP   : CONTRAINTES ACTUELLES
! OUT R       : VECTEUR RESIDU ADIMENSIONNALISE
! OUT DRDY    : MATRICE JACOBIENNE ADIMENSIONNALISEE
    implicit none
    include 'asterfort/infniv.h'
    include 'asterfort/r8inir.h'
    real(kind=8) :: y(12), id(6, 6), sigp(6), dsidep(6, 6), cimpo(6, 12)
    real(kind=8) :: valimp(6)
    real(kind=8) :: r(12), drdy(12, 12), coef
    integer :: i, j, idbg, ifm, niv
!
    idbg=0
!
    call r8inir(6*6, 0.d0, id, 1)
    do 44 i = 1, 6
        id(i,i)=1.d0
44  end do
!
    do 6 i = 1, 6
        do 6 j = 1, 6
            drdy(i,j)=id(i,j)
            drdy(i,6+j)=-dsidep(i,j)/coef
 6      continue
    do 7 i = 1, 6
        do 7 j = 1, 12
            drdy(6+i,j)=cimpo(i,j)
 7      continue
    do 4 i = 1, 6
        r(i)=-y(i)+sigp(i)/coef
 4  end do
    do 5 i = 1, 6
        r(6+i)= valimp(i)
        do 5 j = 1, 12
            r(6+i)=r(6+i)-cimpo(i,j)*y(j)
 5      continue
    if (idbg .eq. 1) then
        call infniv(ifm, niv)
        write(ifm,*) 'DRDY'
        do 45 i = 1, 12
            write(ifm,'(12(1X,E12.5))')(drdy(i,j),j=1,12)
45      continue
    endif
end subroutine
