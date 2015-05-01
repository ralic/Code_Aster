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
            subroutine noligr(ligrz,igrel,numel,nunoeu,code,inema, &
                       &nbno,typlaz,jlgns,rapide,jliel0,jlielc,jnema0,jnemac)
              character(len=*), intent(in) :: ligrz
              integer, intent(in) :: igrel
              integer, intent(in) :: numel
              integer, intent(in) :: nunoeu
              integer, intent(in) :: code
              integer, intent(inout) :: inema
              integer, intent(inout) :: nbno
              character(len=*), intent(in) :: typlaz
              integer, intent(in) :: jlgns
              character(len=3) ,optional, intent(in) :: rapide
              integer ,optional, intent(in) :: jliel0
              integer ,optional, intent(in) :: jlielc
              integer ,optional, intent(in) :: jnema0
              integer ,optional, intent(in) :: jnemac
            end subroutine noligr
          end interface 
