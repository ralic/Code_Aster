!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
            subroutine numer2(nb_ligr,list_ligr,base,nume_ddlz,         &
     &nume_ddl_oldz,modelocz,sd_iden_relaz)
              integer, intent(in) :: nb_ligr
              character(len=24) ,pointer, intent(in) :: list_ligr(:)
              character(len=2), intent(in) :: base
              character(len=*), intent(inout) :: nume_ddlz
              character(len=*), intent(in) :: nume_ddl_oldz
              character(len=*), intent(in) :: modelocz
              character(len=*) ,optional, intent(in) :: sd_iden_relaz
            end subroutine numer2
          end interface 
