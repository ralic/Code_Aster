subroutine mechnc(noma, motcle, iocc, chnumc)
    implicit none
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/mecact.h"
    integer :: iocc
    character(len=*) :: noma, motcle
    character(len=24) :: chnumc
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CREE UNE CARTE POUR LES COQUES
!     ------------------------------------------------------------------
! IN  : NOMA   : NOM DU MAILLAGE
! IN  : MOTCLE : MOTCLE FACTEUR
! IN  : IOCC   : NUMERO D'OCCURENCE
! OUT : CHNUMC : NOM DE LA CARTE CREEE
!     ------------------------------------------------------------------
    integer :: ival(3), ncou, nangl
    real(kind=8) :: r8b
    character(len=3) :: ordo
    character(len=8) :: k8b, licmp(3)
    complex(kind=8) :: c16b
! DEB-------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: n1, n2, n3, nx3
!-----------------------------------------------------------------------
    call getvis(motcle, 'NUME_COUCHE', iocc=iocc, scal=ncou, nbret=n1)
    call getvtx(motcle, 'NIVE_COUCHE', iocc=iocc, scal=ordo, nbret=n2)
    call getvis(motcle, 'ANGLE', iocc=iocc, scal=nangl, nbret=n3)
    chnumc = ' '
    if (n2 .ne. 0) then
        if (ordo .eq. 'SUP') then
            nx3 = 1
        else if (ordo.eq.'MOY') then
            nx3 = 0
        else if (ordo.eq.'INF') then
            nx3 = -1
        endif
        chnumc = '&&MECHNC.NUMC'
        licmp(1) = 'NUMC'
        licmp(2) = 'ORDO'
        licmp(3) = 'ANGL'
        ival(1) = ncou
        ival(2) = nx3
        ival(3) = nangl
        call mecact('V', chnumc, 'MAILLA', noma, 'NUMC_I',&
                    3, licmp, ival, r8b, c16b,&
                    k8b)
    endif
!
end subroutine
