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
    subroutine mbxnlr(option,fami,nddl,nno,ncomp,kpg,ipoids,igeom,&
                  imate,ideplm,ideplp,ivectu,icontp,&
                  imatuu,dff,alpha,beta,&
                  vecteu,matric)
    character(len=16) :: option
    character(len=4) :: fami
    integer :: nddl, nno, ncomp
    integer :: kpg
    integer :: ipoids
    integer :: igeom, imate
    integer :: ideplm, ideplp
    integer :: ivectu, icontp, imatuu
    real(kind=8) :: dff(2, nno), alpha, beta
    aster_logical :: vecteu, matric
    end subroutine mbxnlr
end interface
