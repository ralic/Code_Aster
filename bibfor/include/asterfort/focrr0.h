!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
            subroutine focrr0(nomfon,interp,base,resu,nomcha,maille,    &
     &noeud,cmp,npoint,nusp,ivari,nomvari,nbordr,lordr)
              character(len=19), intent(in) :: nomfon
              character(len=8), intent(in) :: interp
              character(len=1), intent(in) :: base
              character(len=19), intent(in) :: resu
              character(len=16), intent(in) :: nomcha
              character(len=8), intent(in) :: maille
              character(len=8), intent(in) :: noeud
              character(len=8), intent(in) :: cmp
              integer, intent(in) :: npoint
              integer, intent(in) :: nusp
              integer, intent(in) :: ivari
              character(len=16), intent(in) :: nomvari
              integer, intent(in) :: nbordr
              integer, intent(in) :: lordr(*)
            end subroutine focrr0
          end interface 
