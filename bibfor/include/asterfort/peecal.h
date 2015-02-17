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
            subroutine peecal(tych,resu,nomcha,lieu,nomlie,modele,ichagd&
     &,chpost,nbcmp,nomcmp,nomcp2,nuord,inst,iocc,ligrel,cespoi)
              integer :: nbcmp
              character(len=4) :: tych
              character(len=19) :: resu
              character(len=24) :: nomcha
              character(len=8) :: lieu
              character(len=*) :: nomlie
              character(len=8) :: modele
              integer :: ichagd
              character(len=19) :: chpost
              character(len=8) :: nomcmp(nbcmp)
              character(len=8) :: nomcp2(nbcmp)
              integer :: nuord
              real(kind=8) :: inst
              integer :: iocc
              character(len=19) :: ligrel
              character(len=19) :: cespoi
            end subroutine peecal
          end interface 
