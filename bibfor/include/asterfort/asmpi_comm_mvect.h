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
            subroutine asmpi_comm_mvect(optmpi,typsca,nbval,jtrav,bcrank&
     &,vi,vi4,vr,vc,sci,sci4,scr,scc)
              character(len=*), intent(in) :: optmpi
              character(len=*), intent(in) :: typsca
              integer ,optional, intent(in) :: nbval
              integer ,optional, intent(in) :: jtrav
              integer ,optional, intent(in) :: bcrank
              integer ,optional, intent(inout) :: vi(*)
              integer(kind=4) ,optional, intent(inout) :: vi4(*)
              real(kind=8) ,optional, intent(inout) :: vr(*)
              complex(kind=8) ,optional, intent(inout) :: vc(*)
              integer ,optional, intent(inout) :: sci
              integer(kind=4) ,optional, intent(inout) :: sci4
              real(kind=8) ,optional, intent(inout) :: scr
              complex(kind=8) ,optional, intent(inout) :: scc
            end subroutine asmpi_comm_mvect
          end interface 
