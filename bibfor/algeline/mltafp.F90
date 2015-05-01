subroutine mltafp(n, ncol, adper, matper, matfi,&
                  local)
! person_in_charge: olivier.boiteau at edf.fr
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ASSEMBLAGE DES MATRICES FRONTALES VERSION SIMPLIFIEE
!  LA VERSION PRECEDENTE ASSEMBLAIT PAR 2 COLONES
! POUR UNE MEILLEURE UTILISATION DES REGISTRES SUR CRAY
! aslint: disable=W1304
    implicit none
    integer(kind=4) :: local(*)
    integer :: n, ncol, adper(*)
    real(kind=8) :: matper(*), matfi(*)
!     VARIABLES LOCALES
    integer :: decp1, decf1, j, i, ni
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    decf1 = 1
    do 120 i = 1, ncol, 1
        decp1 = adper(local(i))
        matper(decp1) = matper(decp1) + matfi(decf1)
        decp1 = decp1 - local(i)
        ni = n - i
        decf1 = decf1 + 1
        do 110 j = 1, ni
            matper(decp1+local(j+i)) = matper(decp1+local(j+i)) + matfi(decf1)
            decf1 = decf1 + 1
110      continue
120  end do
!
end subroutine
