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
            subroutine nocart(carte,code,ncmp,groupma,mode,nma,limano,  &
                              limanu,ligrel)
              character(*), intent(in) :: carte
              integer, intent(in) :: code
              integer, intent(in) :: ncmp
              character(*) ,optional, intent(in) :: groupma
              character(*) ,optional, intent(in) :: mode
              integer ,optional, intent(in) :: nma
              character(*) ,optional, intent(in) :: limano(*)
              integer ,optional, intent(in) :: limanu(*)
              character(*) ,optional, intent(in) :: ligrel
            end subroutine nocart
          end interface
