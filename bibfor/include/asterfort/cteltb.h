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
            subroutine cteltb(nbma,mesmai,noma,nbval,nkcha,nkcmp,nkvari,&
     &toucmp,nbcmp,typac,ndim,nrval,resu,nomtb,nsymb,chpgs,tych,nival,  &
     &niord)
              integer :: nbma
              character(len=24) :: mesmai
              character(len=8) :: noma
              integer :: nbval
              character(len=24) :: nkcha
              character(len=24) :: nkcmp
              character(len=24) :: nkvari
              aster_logical :: toucmp
              integer :: nbcmp
              character(len=8) :: typac
              integer :: ndim
              character(len=24) :: nrval
              character(len=8) :: resu
              character(len=8) :: nomtb
              character(len=16) :: nsymb
              character(len=19) :: chpgs
              character(len=4) :: tych
              character(len=24) :: nival
              character(len=24) :: niord
            end subroutine cteltb
          end interface 
