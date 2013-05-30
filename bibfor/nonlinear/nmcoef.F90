subroutine nmcoef(noeu1, noeu2, typpil, nbno, cnsln,&
                  compo, vect, i, n, coef1,&
                  coef2, coefi)
!
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
! TOLE CRS_1404
!
    implicit none
    include 'jeveux.h'
    include 'asterc/r8prem.h'
    include 'asterfort/jeveuo.h'
    integer :: noeu1, noeu2, nbno, i, n
    real(kind=8) :: coef1, coef2, coefi
    character(len=8) :: compo
    character(len=19) :: cnsln
!
! ----------------------------------------------------------------------
! CALCUL COEFFICIENT PILOTAGE DU VECTEUR INITIALISATION <S>
! FORMULATION XFEM
! ----------------------------------------------------------------------
!
!
! IN  NOEU1  : EXTREMITE 1 ARETE PILOTEE
! IN  NOEU1  : EXTREMITE 2 ARETE PILOTEE
! IN  TYPPIL : TYPE PILOTAGE
! IN  NBNO   : NOMBRE ARETES EFFECTIVEMENT PILOTEES
! IN  CNSLSN : NOM CHAM_NO_S LEVEL SET NORMALE
! IN  COMPO  : DIRECTION A PILOTER
! IN  VECT   : VECTEUR NORME DE CETTE DIRECTION DANS LA BASE FIXE
! IN  I      : NUMERO COMPOSANTE
! IN  N      : NUMERO ARETE
! OUT  COEF1 : COEF NOEUD 1
! OUT  COEF2 : COEF NOEUD 2
! OUT  COEFI : COEF REPERAGE
!
!
!
!
    integer :: jlsn, coefii
    real(kind=8) :: lsn1, lsn2, vect(3), nbnor, deno, eps
    character(len=24) :: typpil
!
    call jeveuo(cnsln//'.CNSV', 'E', jlsn)
    lsn1 = zr(jlsn-1+noeu1)
    lsn2 = zr(jlsn-1+noeu2)
    eps = r8prem()
    nbnor=1.d0*nbno
    if ((abs(lsn1).le.eps) .and. (abs(lsn2).le.eps)) then
        coef1=1.d0
        coef2=1.d0
        coefi=0.d0
    else
        deno = abs(lsn1) + abs(lsn2)
        coef1= abs(lsn2)/deno
        coef2= abs(lsn1)/deno
        coefii=nbno*(i-1)+n
        coefi=coefii*1.d0
    endif
!
    if (typpil .eq. 'SAUT_IMPO') then
        if (compo .eq. 'DNOR' .or. compo(1:4) .eq. 'DTAN') then
            coef2=2*coef2*vect(i)/nbnor
            coef1=2*coef1*vect(i)/nbnor
        else
            coef2=2*coef2/nbnor
            coef1=2*coef1/nbnor
        endif
    else if (typpil.eq.'SAUT_LONG_ARC') then
        if (compo .eq. 'DNOR' .or. compo(1:4) .eq. 'DTAN') then
            coef2=2*coef2*vect(i)/sqrt(nbnor)
            coef1=2*coef1*vect(i)/sqrt(nbnor)
        else
            coef2=2*coef2/sqrt(nbnor)
            coef1=2*coef1/sqrt(nbnor)
        endif
    endif
end subroutine
