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
! aslint: disable=W1504
!
interface
    subroutine mbgnlr(option,vecteu,matric,nno,ncomp,imate,icompo,dff,alpha,beta,&
                  h,preten,igeom,ideplm,ideplp,kpg,fami,ipoids,icontp,ivectu,imatuu)
    character(len=4) :: fami
    character(len=16) :: option
    integer :: nno, ncomp, kpg
    integer :: imate, icompo, igeom, ideplm, ideplp, ipoids, icontp, ivectu
    integer :: imatuu
    real(kind=8) :: dff(2, nno), alpha, beta, h, preten
    aster_logical :: vecteu, matric
    end subroutine mbgnlr
end interface
