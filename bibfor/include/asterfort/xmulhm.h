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
#include "asterf_types.h"
!
interface
    subroutine xmulhm(contac, ddls, ddlc, ddlm, jaint, ifiss,&
                      jheano, vstnc, lact, lcalel, lelim,&
                      nfh, nfiss, ninter,&
                      nlact, nno, nnol, nnom, nnos,&
                      pla, pos, typma, jstano)
        integer :: contac
        integer :: ddls
        integer :: ddlc
        integer :: ddlm
        integer :: jaint
        integer :: ifiss
        integer :: jheano
        integer :: vstnc(*)
        integer :: lact(16)
        aster_logical :: lcalel
        aster_logical :: lelim
        integer :: nfh
        integer :: nfiss
        integer :: ninter
        integer :: nlact(2)
        integer :: nno
        integer :: nnol
        integer :: nnom
        integer :: nnos
        integer :: pla(27)
        integer :: pos(16)
        character(len=8) :: typma
        integer, optional, intent(in) :: jstano
    end subroutine xmulhm
end interface
