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
            subroutine rsadpa(nomsd,cel,npara,lpara,iordr,itype,tjv,ttyp&
     &,sjv,styp,istop)
              integer, intent(in) :: npara
              character(len=*), intent(in) :: nomsd
              character(len=1), intent(in) :: cel
              character(len=*), intent(in) :: lpara(*)
              integer, intent(in) :: iordr
              integer, intent(in) :: itype
              integer ,optional, intent(out) :: tjv(*)
              character(len=*) ,optional, intent(out) :: ttyp(*)
              integer ,optional, intent(out) :: sjv
              character(len=*) ,optional, intent(out) :: styp
              integer ,optional, intent(in) :: istop
            end subroutine rsadpa
          end interface 
