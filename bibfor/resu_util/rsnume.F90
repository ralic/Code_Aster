subroutine rsnume(resu, nomsy, nu)
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
    implicit none
#include "jeveux.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/rs_getlast.h"
    character(len=*) :: nu, resu, nomsy
! ----------------------------------------------------------------------
! BUT : RECUPERER  UN NUME_DDL DANS UNE SD_RESULTAT
! ----------------------------------------------------------------------
! IN   K8   RESU    : NOM DE LA SD_RESULAT
! IN   K16  NOMSY   : NOM SYMBOLIQUE DU CHAM_NO : 'DEPL','TEMP', ...
! OUT  K14  NU      : NOM DU NUME_DDL  TROUVE (OU ' ' SINON)
! ----------------------------------------------------------------------
!
    integer :: nume_last, icode, iret, luti, iret2
    character(len=19) :: chamno, resu2
    character(len=24), pointer :: refe(:) => null()
!
! DEB ------------------------------------------------------------------
!
    nu=' '
    resu2=resu
    call jeexin(resu2//'.ORDR', iret)
    if (iret .gt. 0) then
        call jelira(resu2//'.ORDR', 'LONUTI', luti)
        if (luti .eq. 0) goto 999
        call rs_getlast(resu, nume_last)
!
        call rsexch(' ', resu, nomsy, nume_last, chamno,&
                    icode)
!
        if (icode .eq. 0) then
            call jeveuo(chamno//'.REFE', 'L', vk24=refe)
            call jeexin(refe(2)(1:19)//'.NEQU', iret2)
            if (iret2 .gt. 0) nu=refe(2)(1:14)
        endif
    endif
999 continue
!
end subroutine
