subroutine pmmaco(nommat, nbmat, codi)
    implicit   none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveut.h"
#include "asterfort/matcod.h"
#include "asterfort/wkvect.h"
    character(len=8) :: nommat(*)
    character(len=19) :: codi
! ----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean-michel.proix at edf.fr
!-----------------------------------------------------------------------
! OPERATEUR CALC_POINT_MAT : MATERIAU CODE COMME RCMACO MAIS SANS MODELE
!-----------------------------------------------------------------------
!
!     BUT: CREER L'OBJET NOMMAT//'      .CODI' ,LE REMPLIR ET RENVOYER
!          SON ADRESSE PAR RAPPORT A ZI
!
! IN   NOMMAT : NOM DU MATERIAU
! OUT  CODI   : OBJET MATERIAU CODE (VOIR DESCRIPTION DANS MATCOD)
!
! ----------------------------------------------------------------------
!
!
!
    integer :: indmat, nbmat, imate, igrp, ingrp, i
    character(len=8) :: nommats, matercod
! ----------------------------------------------------------------------
!
    call jemarq()
!
    nommats = '&chpoint'

    call jedetr(nommats//'.MATE_CODE.GRP')
    call jedetr(nommats//'.MATE_CODE.NGRP')
    call wkvect(nommats//'.MATE_CODE.GRP', 'V V K8', nbmat, igrp)
    call wkvect(nommats//'.MATE_CODE.NGRP', 'V V I', 1, ingrp)
    do i=1,nbmat
        zk8(igrp-1+i)=nommat(i)
    enddo
    zi(ingrp)=1
!
    call jeveut(nommats//'.MATE_CODE.GRP', 'L', igrp)
!
    codi=' '
    indmat=0
!   imate : numero de groupe ?
    imate=1
    matercod='matcod'
    call matcod(nommats, indmat, nbmat, imate, igrp,&
                matercod, codi)
    call jedema()
!
end subroutine
