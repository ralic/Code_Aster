subroutine mmcyc1(resoco, iptc, nompt, indco)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
#include "asterfort/iscode.h"
#include "asterfort/iscycl.h"
#include "asterfort/isdeco.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: resoco
    integer :: iptc
    character(len=16) :: nompt
    integer :: indco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE)
!
! DETECTION DU CYCLE DE TYPE CONTACT/PAS CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
! IN  INDCO  : STATUT DE CONTACT
! IN  NOMPT  : NOM DU POINT DE CONTACT
! IN  IPTC   : NUMERO DE LA LIAISON DE CONTACT
!
!
!
!
    character(len=24) :: cyclis, cycnbr, cyctyp, cycpoi
    integer :: jcylis, jcynbr, jcytyp, jcypoi
    integer :: statut(30)
    integer :: longcy, ccycle(1), ncycle, tcycle, icycl
    character(len=16) :: lcycle
    logical :: detect
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    longcy = 3
    detect = .false.
!
! --- ACCES OBJETS
!
    cyclis = resoco(1:14)//'.CYCLIS'
    cycnbr = resoco(1:14)//'.CYCNBR'
    cyctyp = resoco(1:14)//'.CYCTYP'
    cycpoi = resoco(1:14)//'.CYCPOI'
    call jeveuo(cyclis, 'E', jcylis)
    call jeveuo(cycnbr, 'E', jcynbr)
    call jeveuo(cyctyp, 'E', jcytyp)
    call jeveuo(cycpoi, 'E', jcypoi)
!
! --- ETAT PRECEDENT
!
    ccycle(1) = zi(jcylis-1+4*(iptc-1)+1)
    ncycle = zi(jcynbr-1+4*(iptc-1)+1)
    call isdeco(ccycle(1), statut, 30)
!
! --- MISE A JOUR
!
    ncycle = ncycle + 1
    statut(ncycle) = indco
    call iscode(statut, ccycle(1), 30)
!
! --- DETECTION D'UN CYCLE
!
    tcycle = 0
    lcycle = ' '
    if (ncycle .eq. longcy) then
        detect = iscycl(ccycle(1),longcy)
        if (detect) then
            tcycle = 1
            lcycle = nompt
        endif
    endif
!
    zi(jcytyp-1+4*(iptc-1)+1) = tcycle
    zk16(jcypoi-1+4*(iptc-1)+1) = lcycle
!
! --- REINITIALISATION DU CYCLE
!
    if (ncycle .eq. longcy) then
        call isdeco(ccycle(1), statut, 30)
        do 10 icycl = 1, longcy-1
            statut(icycl) = statut(icycl+1)
10      continue
        call iscode(statut, ccycle(1), 30)
        ncycle = longcy - 1
    endif
!
! --- SAUVEGARDE DU CYCLE
!
    zi(jcylis-1+4*(iptc-1)+1) = ccycle(1)
    zi(jcynbr-1+4*(iptc-1)+1) = ncycle
!
    call jedema()
end subroutine
