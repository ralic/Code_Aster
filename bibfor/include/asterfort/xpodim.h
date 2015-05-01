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
#include "asterf_types.h"
!
interface 
    subroutine xpodim(malini, mailc, modvis, licham, nsetot,&
                      nnntot, ncotot, listno, cns1, cns2,&
                      ces1, ces2, cel2, cesvi1, cesvi2,&
                      ior, resuco, nbnoc, nbmac, logrma,&
                      dirgrm, maxfem, ngfon, comps1, comps2,&
                      pre1)
        character(len=8) :: malini
        character(len=24) :: mailc
        character(len=8) :: modvis
        character(len=24) :: licham
        integer :: nsetot
        integer :: nnntot
        integer :: ncotot
        character(len=24) :: listno
        character(len=19) :: cns1
        character(len=19) :: cns2
        character(len=19) :: ces1
        character(len=19) :: ces2
        character(len=19) :: cel2
        character(len=19) :: cesvi1
        character(len=19) :: cesvi2
        integer :: ior
        character(len=8) :: resuco
        integer :: nbnoc
        integer :: nbmac
        character(len=24) :: logrma
        character(len=24) :: dirgrm
        character(len=8) :: maxfem
        integer :: ngfon
        character(len=19) :: comps1
        character(len=19) :: comps2
        aster_logical :: pre1
    end subroutine xpodim
end interface 
