subroutine mmcycl(defico, resoco, typcyc, liecyc)
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
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: defico, resoco
    character(len=16) :: liecyc, typcyc
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE)
!
! RECUPERATION DU PREMIER POINT OU IL Y A CYCLAGE
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
! OUT TYPCYC : TYPE DE CYCLAGE
!              ' '               - AUCUN CYCLAGE
!              'CYCLAGE CONT'    - CYCLE CONTACT/PAS CONTACT
!              'CYCLAGE ADH/GLI' - CYCLE ADHERENT/GLISSANT
!              'CYCLAGE AV/AR'   - CYCLE GLISSANT AVANT/ARRIERE
!              'FLIP FLOP'       - FLIP FLOP HISTORIQUE
! OUT LIECYC : LIEU DE CYCLAGE
!
!
!
!
    integer :: ntpc
    integer :: iptc
    character(len=24) :: cyctyp, cycpoi
    integer :: jcytyp, jcypoi
    integer :: tcycle
    character(len=16) :: lcycle
    logical :: detect, lfrot
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES OBJETS
!
    cyctyp = resoco(1:14)//'.CYCTYP'
    cycpoi = resoco(1:14)//'.CYCPOI'
    call jeveuo(cyctyp, 'L', jcytyp)
    call jeveuo(cycpoi, 'L', jcypoi)
!
! --- INITIALISATIONS
!
    ntpc = cfdisi(defico,'NTPC')
    lfrot = cfdisl(defico,'FROTTEMENT')
    detect = .false.
    typcyc = ' '
    liecyc = ' '
!
! --- DETECTION DU PREMIER POINT OU IL Y A CYCLAGE
!
    do 50 iptc = 1, ntpc
        tcycle = zi(jcytyp-1+4*(iptc-1)+4)
        lcycle = zk16(jcypoi-1+4*(iptc-1)+4)
        detect = tcycle.eq.1
        if (detect) then
            typcyc = 'FLIP FLOP'
            liecyc = lcycle
            goto 99
        endif
        tcycle = zi(jcytyp-1+4*(iptc-1)+1)
        lcycle = zk16(jcypoi-1+4*(iptc-1)+1)
        detect = tcycle.eq.1
        if (detect) then
            typcyc = 'CYCLAGE CONT'
            liecyc = lcycle
            goto 99
        endif
        if (lfrot) then
            tcycle = zi(jcytyp-1+4*(iptc-1)+2)
            lcycle = zk16(jcypoi-1+4*(iptc-1)+2)
            detect = tcycle.eq.1
            if (detect) then
                typcyc = 'CYCLAGE ADH/GLI'
                liecyc = lcycle
                goto 99
            endif
            tcycle = zi(jcytyp-1+4*(iptc-1)+3)
            lcycle = zk16(jcypoi-1+4*(iptc-1)+3)
            detect = tcycle.eq.1
            if (detect) then
                typcyc = 'CYCLAGE AV/AR'
                liecyc = lcycle
                goto 99
            endif
        endif
50  end do
!
99  continue
!
    call jedema()
end subroutine
