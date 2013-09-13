subroutine x195cb(tychr, nomgd, chou)
    implicit none
!     -----------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
!     BUT : TRAITER :
!          - OPTION 'COMB' DE LA COMMANDE CREA_CHAMP
!     -----------------------------------------------------------------
!
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/idensd.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jerazo.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mess.h"
    character(len=8) :: chou
    character(len=4) :: tychr, tych1
    integer :: ib, nbocc, iocc, k, jvale1, jvale2, n1, n2
    real(kind=8) :: coefr
    character(len=8) :: nomgd, nomgd1, ma1, ma2
    character(len=3) :: tsca
    character(len=19) :: ch1, ch2, pfcn1, pfcn2
!
!     -----------------------------------------------------------------
!
    call jemarq()
    call getfac('COMB', nbocc)
    ASSERT(nbocc.ge.1)
    ASSERT(tychr.eq.'NOEU')
!
!
!     -- 1. CREATION DU CHAMP "OUT" SUR LE MODELE DU 1ER CHAMP "IN"
!     -------------------------------------------------------------
!     -- ATTENTION : LA COMMANDE PEUT ETRE REENTRANTE,
!        IL NE FAUT PAS DETRUIRE CHOU TOUT DE SUITE.
    call getvid('COMB', 'CHAM_GD', iocc=1, scal=ch1, nbret=ib)
    ch2='&&X195CB.CHOU'
    call copisd('CHAMP', 'V', ch1, ch2)
!     -- INITIALISATION A ZERO :
    call jeveuo(ch2//'.VALE', 'E', jvale2)
    call jelira(ch2//'.VALE', 'LONMAX', n1)
    call jerazo(ch2//'.VALE', n1, 1)
    call dismoi('F', 'NOM_MAILLA', ch2, 'CHAMP', ib,&
                ma2, ib)
    call dismoi('F', 'PROF_CHNO', ch2, 'CHAM_NO', ib,&
                pfcn2, ib)
!
!
!     -- 2. CALCUL DU CONTENU DE CH2 :
!     ---------------------------------
    do 10,iocc = 1,nbocc
    call getvid('COMB', 'CHAM_GD', iocc=iocc, scal=ch1, nbret=ib)
!
!       -- QUELQUES VERIFICATIONS DE COHERENCE :
    call dismoi('F', 'TYPE_CHAMP', ch1, 'CHAMP', ib,&
                tych1, ib)
    if (tych1 .ne. 'NOEU') call u2mess('F', 'MODELISA5_10')
!
    call dismoi('F', 'NOM_MAILLA', ch1, 'CHAMP', ib,&
                ma1, ib)
    if (ma1 .ne. ma2) call u2mess('F', 'MODELISA5_13')
!
    call dismoi('F', 'PROF_CHNO', ch1, 'CHAM_NO', ib,&
                pfcn1, ib)
    if (.not.idensd('PROF_CHNO',pfcn1,pfcn2)) call u2mess('F', 'MODELISA5_12')
!
    call jelira(ch1//'.VALE', 'LONMAX', n2)
    if (n2 .ne. n1) ASSERT(.false.)
!
    call dismoi('F', 'NOM_GD', ch1, 'CHAMP', ib,&
                nomgd1, ib)
    if (nomgd1 .ne. nomgd) call u2mess('F', 'MODELISA5_11')
!
    call dismoi('F', 'TYPE_SCA', nomgd1, 'GRANDEUR', ib,&
                tsca, ib)
    ASSERT(tsca.eq.'R'.or.tsca.eq.'C')
!
!       -- CUMUL DES VALEURS :
    call jeveuo(ch1//'.VALE', 'L', jvale1)
    call getvr8('COMB', 'COEF_R', iocc=iocc, scal=coefr, nbret=ib)
    ASSERT(ib.eq.1)
    if (tsca .eq. 'R') then
        do 11, k=1,n1
        zr(jvale2-1+k)=zr(jvale2-1+k)+coefr*zr(jvale1-1+k)
11      continue
    else if (tsca.eq.'C') then
        do 12, k=1,n1
        zc(jvale2-1+k)=zc(jvale2-1+k)+coefr*zc(jvale1-1+k)
12      continue
    endif
    10 end do
!
!
!     -- RECOPIE DE CH2 DANS CHOU :
!     -------------------------------
    call copisd('CHAMP', 'G', ch2, chou)
    call detrsd('CHAMP', ch2)
!
    call jedema()
end subroutine
