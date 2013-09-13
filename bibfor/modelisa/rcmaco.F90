subroutine rcmaco(chmat, indmat, nbmat, imate)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeveut.h"
#include "asterfort/jexnum.h"
#include "asterfort/matcod.h"
#include "asterfort/utmess.h"
!
    character(len=8) :: chmat
    integer :: indmat, nbmat, imate
! ----------------------------------------------------------------------
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
! person_in_charge: j-pierre.lefebvre at edf.fr
!
!     BUT: CREER L'OBJET NOMMAT//'      .CODI' ,LE REMPLIR ET RENVOYER
!          SON ADRESSE PAR RAPPORT A ZI
!
! ----------------------------------------------------------------------
!
!
!
    integer :: nbcmp, jdesc, igrp, ier
    character(len=8) :: k8b, nommat, nomgd, materi
    character(len=19) :: codi
!
    call jemarq()
!
    call jeveut(chmat(1:8)//'.MATE_CODE.GRP', 'L', igrp)
    materi=zk8(igrp+indmat)
    nommat = materi
    call jeveuo(chmat(1:8)//'.CHAMP_MAT .DESC', 'L', jdesc)
    call jenuno(jexnum('&CATA.GD.NOMCMP', zi(jdesc)), nomgd)
    call dismoi('F', 'NB_CMP_MAX', nomgd, 'GRANDEUR', nbcmp,&
                k8b, ier)
    if (imate .gt. 9999) then
        call utmess('F', 'CALCULEL6_11')
    endif
!
    call matcod(chmat, indmat, nbmat, imate, igrp,&
                nommat, codi)
!
    call jedema()
!
end subroutine
