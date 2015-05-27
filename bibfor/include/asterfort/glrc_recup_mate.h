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
            subroutine glrc_recup_mate(imate,compor,lambda,deuxmu,lamf,deumuf,gt, &
      gc,gf,seuil,alpha,alfmc,ep,lrgm)
              integer :: imate
              character(len=16) :: compor
              real(kind=8) :: lambda
              real(kind=8) :: deuxmu
              real(kind=8) :: lamf
              real(kind=8) :: deumuf
              real(kind=8) :: gt
              real(kind=8) :: gc
              real(kind=8) :: gf
              real(kind=8) :: seuil
              real(kind=8) :: alpha
              real(kind=8) :: alfmc
              real(kind=8) :: ep
              aster_logical :: lrgm
            end subroutine glrc_recup_mate
          end interface 
