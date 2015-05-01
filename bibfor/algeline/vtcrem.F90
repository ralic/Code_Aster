subroutine vtcrem(chamno, matass, base, typc)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/vtcrea.h"
    character(len=*) :: chamno, matass, base, typc
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     CREATION D'UN CHAM_NO S'APPUYANT SUR LA NUMEROTATION DE MATASS
!     ------------------------------------------------------------------
!     OUT CHAMNO : K19 : NOM DU CHAM_NO CONCERNE
!     IN  MATASS : K19 : NOM DE LA MATRICE
!     IN  BASE   : K1 : BASE JEVEUX ('G', 'V' , ... )
!     IN  TYPC   : K1 : TYPE JEVEUX DE BASE
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    character(len=24) :: refa, crefe(2)
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: jrefa, neq
!-----------------------------------------------------------------------
    data refa/'                   .REFA'/
!     ------------------------------------------------------------------
    call jemarq()
!
    refa(1:19) = matass
    call jeveuo(refa, 'L', jrefa)
    call dismoi('NB_EQUA', matass, 'MATR_ASSE', repi=neq)
    crefe(1)=zk24(jrefa-1+1)
    crefe(2)=zk24(jrefa-1+2)(1:14)//'.NUME'
    call vtcrea(chamno, crefe, base, typc, neq)
!
    call jedema()
end subroutine
