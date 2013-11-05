subroutine refdag(resin)
!
    implicit none
#include "jeveux.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedup1.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: resin
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
! person_in_charge: hassan.berro at edf.fr
! ----------------------------------------------------------------------
!
!   DOUBLER LA TAILLE DES OBJETS CONTENEURS DES REFERENCES DYNAMIQUES (.REFD ET INDI)
!
    integer :: nbrefs, ibid, jbid, jindi, jindi2, jrefe, jrefe2
    character(len=1) :: jvb
    character(len=8) :: restmp, k8bid
    character(len=16) :: refd, indi
!
    call jemarq()
!
    restmp = '&&REFDAJ'
!
    refd = '           .REFD'
    indi = '           .INDI'
!
    jvb = 'G'
    if (resin(1:2) .eq. '&&') jvb = 'V'
!
!   Save the existing information in a temporary location
    call jedup1(resin//refd, 'V', restmp//refd)
    call jedup1(resin//indi, 'V', restmp//indi)
    call jelira(resin//refd, 'NUTIOC', nbrefs, k8bid)
!
!   Clear all initial information
    call jedetr(resin//refd)
    call jedetr(resin//indi)
!
!   Initialize the REFD and INDI with the new size (double nbrefs)
    call wkvect(resin//indi, jvb//' V I', 2*nbrefs, jindi)
    call jecrec(resin//refd, jvb//' V K24', 'NU', 'CONTIG', 'CONSTANT',&
                2*nbrefs)
    call jeecra(resin//refd, 'LONT', (2*nbrefs)*5, k8bid)
!
!   Set all INDI entries to -100 (default/empty reference value)
    do 10 ibid = 1, 2*nbrefs
        zi(jindi+ibid-1) = -100
10  continue
!
!   Copy the temporary-saved information to the newly created objects
    call jeveuo(restmp//indi, 'L', jindi2)
    do 20 ibid = 1, nbrefs
!       INDI entry
        zi(jindi+ibid-1) = zi(jindi2+ibid-1)
!       REFD entry
        call jeveuo(jexnum(restmp//refd, ibid), 'L', jrefe2)
        call jecroc(jexnum( resin//refd, ibid))
        call jeveuo(jexnum( resin//refd, ibid), 'E', jrefe)
        do 30 jbid = 1, 5
            zk24(jrefe+jbid-1) = zk24(jrefe2+jbid-1)
30      continue 
!
20  continue
!
!   Cleanup the temporary objects
    call jedetr(restmp//refd)
    call jedetr(restmp//indi)
!
!
    call jedema()
end subroutine
