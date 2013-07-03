subroutine pmmaco(nommat, codi)
    implicit   none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveut.h"
#include "asterfort/matcod.h"
#include "asterfort/wkvect.h"
    character(len=8) :: nommat
    character(len=19) :: codi
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
    integer :: indmat, nbmat, imate, igrp, ingrp
    character(len=8) :: k8b
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call jedetr(nommat//'.MATE_CODE.GRP')
    call jedetr(nommat//'.MATE_CODE.NGRP')
    call wkvect(nommat//'.MATE_CODE.GRP', 'V V K8', 1, igrp)
    call wkvect(nommat//'.MATE_CODE.NGRP', 'V V I', 1, ingrp)
    zk8(igrp)=nommat
    zi(ingrp)=1
!
    call jeveut(nommat//'.MATE_CODE.GRP', 'L', igrp)
!
    codi=' '
    indmat=0
    nbmat=1
    imate=1
    k8b = ' '
    call matcod(k8b, indmat, nbmat, imate, igrp,&
                nommat, codi)
!
    call jedema()
!
end subroutine
