subroutine crnslv(nuz, metres, renum, base)
    implicit  none
#include "jeveux.h"
#include "asterfort/crsolv.h"
#include "asterfort/wkvect.h"
    character(len=*) :: nuz, base, metres, renum
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!---------------------------------------------------------
!  BUT : ENRICHIR LA SD NUME_DDL (NUZ) DE L'OBJET .NSLV
!        ET CREATION D'UNE SD SOLVEUR
!---------------------------------------------------------
    integer :: jnslv
    character(len=1) :: bas1
    character(len=14) :: nu
    character(len=19) :: solveu
!
!
    nu=nuz
    bas1=base
!
!     -- CREATION D'UNE SD SOLVEUR :
    solveu=nu//'.SOLV'
    call crsolv(metres, renum, solveu, bas1)
!
! --- CREATION DE L'OBJET .NSLV :
    call wkvect(nu//'.NSLV', bas1//' V K24', 1, jnslv)
    zk24(jnslv-1+1)=solveu
end subroutine
