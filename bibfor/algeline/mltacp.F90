subroutine mltacp(n, ncol, adper, matper, matfi,&
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
! VERSION COMPLEXE DE MLTAFP
! aslint: disable=W1304
    implicit none
    integer(kind=4) :: local(*)
    integer :: n, ncol, adper(*)
    complex(kind=8) :: matper(*), matfi(*)
!     VARIABLES LOCALES
    integer :: decp1, decp2, decf1, decf2, j, i, ni, decp
    integer :: ip
    decf1 = 1
    decf2 = n
    if (mod(ncol,2) .eq. 0) then
        do 120 i = 1, ncol, 2
            decp1 = adper(local(i))
            matper(decp1) = matper(decp1) + matfi(decf1)
            decp1 = decp1 - local(i)
            decp2 = adper(local(i+1)) - local(i+1)
            ni = n - i
            do 110 j = 1, ni
!             ID1 = DECP1 + LOCAL(J+I)
!             ID2 = DECP2 + LOCAL(J+I)
!             JD1 = DECF1+J
!             JD2 = DECF2 +J
                matper(decp1+local(j+i)) = matper( decp1+local(j+i)) + matfi(decf1+j)
                matper(decp2+local(j+i)) = matper( decp2+local(j+i)) + matfi(decf2+j)
110          continue
            decf1 = decf1 + 2*ni + 1
            decf2 = decf2 + 2*ni - 1
120      end do
    else
        do 140 i = 1, ncol-1, 2
            decp1 = adper(local(i))
            matper(decp1) = matper(decp1) + matfi(decf1)
            decp1 = decp1 - local(i)
            decp2 = adper(local(i+1)) - local(i+1)
            ni = n - i
            do 150 j = 1, ni
!             ID1 = DECP1 + LOCAL(J+I)
!             ID2 = DECP2 + LOCAL(J+I)
!             JD1 = DECF1+J
!             JD2 = DECF2 +J
                matper(decp1+local(j+i)) = matper( decp1+local(j+i)) + matfi(decf1+j)
                matper(decp2+local(j+i)) = matper( decp2+local(j+i)) + matfi(decf2+j)
150          continue
            decf1 = decf1 + 2*ni + 1
            decf2 = decf2 + 2*ni - 1
140      continue
!       TRAVAIL SUR LA COLONNE RESTANTE
        decp = adper(local(ncol)) - local(ncol)
        do 130 i = ncol, n
            ip = decp + local(i)
            matper(ip) = matper(ip) + matfi(decf1+i-ncol)
130      continue
    endif
end subroutine
