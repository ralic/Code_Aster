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
            subroutine rcadlv(fami,kpg,ksp,poum,jmat,nomat,mfact,msimp, &
     &nbpar,nompar,valpar,jadr,nbres,icodre,iarret)
              integer, intent(in) :: nbpar
              character(len=*), intent(in) :: fami
              integer, intent(in) :: kpg
              integer, intent(in) :: ksp
              character(len=1), intent(in) :: poum
              integer, intent(in) :: jmat
              character(len=*), intent(in) :: nomat
              character(len=*), intent(in) :: mfact
              character(len=*), intent(in) :: msimp
              character(len=*), intent(in) :: nompar(nbpar)
              real(kind=8), intent(in) :: valpar(nbpar)
              integer, intent(out) :: jadr
              integer, intent(out) :: nbres
              integer, intent(out) :: icodre
              integer, intent(in) :: iarret
            end subroutine rcadlv
          end interface 
