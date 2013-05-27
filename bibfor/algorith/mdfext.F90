subroutine mdfext(tinit, dt, neqgen, nbexci, idescf,&
                  nomfon, coefm, liad, inumor, nbpas,&
                  f)
    implicit none
    include 'jeveux.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    integer :: neqgen, nbexci, nbpas
    integer :: idescf(*), liad(*), inumor(*)
    real(kind=8) :: tinit, dt, t, coefm(*), f(neqgen, *)
    character(len=8) :: nomfon(*)
!
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     CALCULE LES FORCES EXTERIEURES A CHAQUE PAS DE TEMPS
!     ------------------------------------------------------------------
! IN  : TINIT  : PAS DE TEMPS INITIAL POUR LE BLOC DE CALCUL
! IN  : NEQGEN : NOMBRE D'EQUATIONS GENERALISEES
! IN  : NBEXCI : NOMBRE D'EXCITATION (MC EXCIT ET EXCIT_RESU)
! IN  : IDESCF : TYPE D'EXCITATION (VECT_ASSE/NUME_ORDRE,FONC_MULT/
!                COEF_MULT)
! IN  : NOMFON : NOM DES FONC_MULT (QUAND IL Y EN A)
! IN  : COEFM  : VALEUR DU COEF_MULT
! IN  : LIAD   : VALEUR DU VECT_ASSE
! IN  : NUMOR  : NUME_ORDRE DU MODE EXCITE
! IN  : NBPAS  : NOMBRE DE PAS DE CALCUL
! OUT : F      : TABLEAU DES FORCES EXTERIEURES A CHAQUE PAS DE TEMPS
! ----------------------------------------------------------------------
!
!
!
!
    integer :: ier
    character(len=4) :: nompar
!
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, j, k
    real(kind=8) :: alpha
!-----------------------------------------------------------------------
    call jemarq()
!
    nompar='INST'
    do 20 i = 1, nbexci
        t=tinit
        do 10 k = 1, nbpas
            if (idescf(i) .eq. 1) then
                call fointe('F ', nomfon(i), 1, nompar, t,&
                            alpha, ier)
                do 30 j = 1, neqgen
                    f(j,k) = f(j,k) + alpha * zr(liad(i)+j-1)
30              continue
            else if (idescf(i).eq.2) then
                call fointe('F ', nomfon(i), 1, nompar, t,&
                            alpha, ier)
                f(inumor(i),k)=f(inumor(i),k)+alpha
            else if (idescf(i).eq.3) then
                do 40 j = 1, neqgen
                    f(j,k) = f(j,k) + coefm(i)* zr(liad(i)+j-1)
40              continue
            else if (idescf(i).eq.4) then
                f(inumor(i),k)=f(inumor(i),k)+coefm(i)
            endif
            t = tinit + ( k*dt )
10      continue
20  end do
!
!
    call jedema()
end subroutine
