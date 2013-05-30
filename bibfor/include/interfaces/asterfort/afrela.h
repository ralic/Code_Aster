!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
interface
    subroutine afrela(coefr, coefc, ddl, noeud, ndim,&
                      direct, nbterm, betar, betac, betaf,&
                      typcoe, typval, typlag, epsi, lisrez)
        integer :: nbterm
        real(kind=8) :: coefr(nbterm)
        complex(kind=8) :: coefc(nbterm)
        character(len=8) :: ddl(nbterm)
        character(len=8) :: noeud(nbterm)
        integer :: ndim(nbterm)
        real(kind=8) :: direct(3, nbterm)
        real(kind=8) :: betar
        complex(kind=8) :: betac
        character(*) :: betaf
        character(len=4) :: typcoe
        character(len=4) :: typval
        character(len=2) :: typlag
        real(kind=8) :: epsi
        character(*) :: lisrez
    end subroutine afrela
end interface
