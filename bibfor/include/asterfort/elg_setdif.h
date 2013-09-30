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
            subroutine elg_setdif(inda,nba,indb,nbb,indc,nbc,remp)
              integer :: nbb
              integer :: nba
#             ifdef _HAVE_PETSC
                  PetscInt  :: inda(nba)
                  PetscInt  :: indb(nbb)
                  PetscInt  :: indc(nba)
#             else
                  integer(kind=4) :: inda(nba)
                  integer(kind=4) :: indb(nbb)
                  integer(kind=4) :: indc(nba)
#             endif
              integer :: nbc
              character(len=6) :: remp
            end subroutine elg_setdif
          end interface
