subroutine ssdmrc(mag)
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
    implicit none
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    character(len=8) :: mag
! ----------------------------------------------------------------------
!     BUT:
!        - INITILISER L' OBJET :  MAG.NOEUD_CONF    V V I DIM=NBNOE
!            POUR INO=1,NBNOE
!            SI (JNO= MAG.NOEUD_CONF(INO) .NE. INO) :
!               LE NOEUD INO A ETE  CONFONDU AVEC LE NOEUD JNO (<INO)
!               ATTENTION : LE NOEUD JNO PEUT ETRE LUI AUSSI CONFONDU
!                           AVEC UN AUTRE NOEUD KNO<JNO !!
!            SINON , LE NOEUD INO EST A CONSERVER.
!
!     IN:
!        MAG : NOM DU MAILLAGE QUE L'ON DEFINIT.
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iadime, iancnf, nnnoe
!-----------------------------------------------------------------------
    call jemarq()
    call jeveuo(mag//'.DIME', 'L', iadime)
    nnnoe=zi(iadime-1+1)
    call wkvect(mag//'.NOEUD_CONF', 'V V I', nnnoe, iancnf)
    do 1, i=1,nnnoe
    zi(iancnf-1+i)=i
    1 end do
    call jedema()
end subroutine
