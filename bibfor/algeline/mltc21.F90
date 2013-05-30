subroutine mltc21(p, front, frn, n, t1,&
                  t2, eps, ier)
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
!     VERSION COMPLEXE DE MLTF21
!     VERSION MODIFIEE POUR L' APPEL A CGEMV (PRODUITS MATRICE-VECTEUR)
!     LE STOCKAGE DES COLONNES DE LA FACTORISEE EST MODIFIE
    implicit none
    include 'asterfort/ccl11j.h'
    include 'asterfort/ccl21j.h'
    include 'asterfort/cclni1.h'
    include 'asterfort/cclni2.h'
    integer :: p, n, ier
    complex(kind=8) :: front(*), t1(*), t2(*), frn(*)
    real(kind=8) :: eps
!                                          VARIABLES LOCALES
    integer :: dia1, dia2, j, l, m, r, dia21, n1
    complex(kind=8) :: coef1
    m = p/2
    r = p - 2*m
    l = n
    dia2 = -n
    do 110 j = 1, m
!                                            TRAVAIL SUR LE BLOC TRIANGU
        dia1 = dia2 + n + 1
        dia2 = dia1 + n + 1
        dia21 = dia2 + 1
        l = l - 2
        coef1 = front(dia1+1)
        if (abs(front(dia1)) .lt. eps) then
            ier = 1
            goto 120
        else
            front(dia1+1) = front(dia1+1)/front(dia1)
            front(dia2) = front(dia2) - front(dia1+1)*coef1
!                                            TRAVAIL SUR LES 2 COLONNES
            call cclni2(front(dia1+2), front(dia21), l, front(dia1), front(dia2),&
                        coef1, t1, t2, eps, ier)
!                                            MISE A JOUR DES COLONNES SU
            n1 = p - 2*j
!     MODIFS POUR STOCKAGE CGEMV
            call ccl21j(front(dia1), front(dia21+n), frn, j, l,&
                        n, n1, t1, t2)
        endif
110  end do
!                                            TRAVAIL SUR LE RESTE DES CO
!                                            DU SUPERNOEUD
    if (r .eq. 1) then
!     MODIFS POUR STOCKAGE CGEMV
        dia1 = dia2 + n + 1
        dia2 = dia1 + l
        l = l - 1
        call cclni1(front(dia1+1), l, front(dia1), t1, eps,&
                    ier)
        call ccl11j(front(dia1), frn, l, t1)
    endif
120  continue
    goto 9999
9999  continue
end subroutine
