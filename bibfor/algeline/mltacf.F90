subroutine mltacf(n, ncol, adper, matper, matfi,&
                  local, p)
! person_in_charge: olivier.boiteau at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
! VERSION COMPLEXE DE MLTAFF
! aslint: disable=W1304
    implicit none
    integer(kind=4) :: local(*)
    integer :: n, ncol, adper(*), p
    complex(kind=8) :: matper(*), matfi(*)
!     VARIABLES LOCALES
    integer :: decp1, decp2, decf1, decf2, j, i, ni, id1, id2, jd1, jd2, decp
    integer :: ip, decp0
    decf1 = 1 + (n+ (n-ncol+1))*ncol/2
    decf2 = n - ncol + (n+ (n-ncol+1))*ncol/2
    decp0 = adper(p+1) - 1
    if (mod((n-ncol),2) .eq. 0) then
        do 120 i = ncol + 1, n, 2
            decp1 = adper(local(i)) - decp0
            matper(decp1) = matper(decp1) + matfi(decf1)
            decp1 = decp1 - local(i)
            decp2 = adper(local(i+1)) - local(i+1) - decp0
            ni = n - i
            do 110 j = 1, ni
                id1 = decp1 + local(j+i)
                id2 = decp2 + local(j+i)
                jd1 = decf1 + j
                jd2 = decf2 + j
                matper(id1) = matper(id1) + matfi(jd1)
                matper(id2) = matper(id2) + matfi(jd2)
110          continue
            decf1 = decf1 + 2*ni + 1
            decf2 = decf2 + 2*ni - 1
120      continue
    else
        do 140 i = ncol + 1, n - 1, 2
            decp1 = adper(local(i)) - decp0
            matper(decp1) = matper(decp1) + matfi(decf1)
            decp1 = decp1 - local(i)
            decp2 = adper(local(i+1)) - local(i+1) - decp0
            ni = n - i
            do 130 j = 1, ni
                id1 = decp1 + local(j+i)
                id2 = decp2 + local(j+i)
                jd1 = decf1 + j
                jd2 = decf2 + j
                matper(id1) = matper(id1) + matfi(jd1)
                matper(id2) = matper(id2) + matfi(jd2)
130          continue
            decf1 = decf1 + 2*ni + 1
            decf2 = decf2 + 2*ni - 1
140      continue
!       TRAVAIL SUR LA COLONNE RESTANTE
        decp = adper(local(n)) - decp0
        ip = decp
        matper(ip) = matper(ip) + matfi(decf1)
    endif
end subroutine
