subroutine op0012()
!======================================================================
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
!                       OPERATEUR ASSE_MATRICE
!----------------------------------------------------------------------
!     VARIABLES LOCALES
!----------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/asmatr.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/sdmpic.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: nu, matas, syme, sym2, kmpic, modele
    character(len=16) :: typm, oper
    character(len=19) :: solveu
    character(len=24) :: lchci, lmatel, partit, partit2, ligrel
    integer :: itysca, nbchc, nbmat, jlimat, jlchci, ibid, k
    integer :: ico
    character(len=24), pointer :: slvk(:) => null()
    character(len=24), pointer :: lime(:) => null()
!----------------------------------------------------------------------
    call jemarq()
!
!
!
!---- ARGUMENT IMPR
    call infmaj()
!
!---- RECUPERATION DES ARGUMENTS ET DU CONCEPT
    call getres(matas, typm, oper)
    if (typm(16:16) .eq. 'R') itysca = 1
    if (typm(16:16) .eq. 'C') itysca = 2
!
!
!---- RECUPERATION DES MATRICES ELEMENTAIRES ---
    call getvid(' ', 'MATR_ELEM', nbval=0, nbret=nbmat)
    nbmat = -nbmat
    lmatel='&&OP0012.LMATEL'
    call wkvect(lmatel, 'V V K24', nbmat, jlimat)
    call getvid(' ', 'MATR_ELEM', nbval=nbmat, vect=zk24(jlimat), nbret=ibid)
!
!
!---- RECUPERATION DES CHARGES CINEMATIQUES ---
    lchci='&&OP0012.LCHARCINE'
    call getvid(' ', 'CHAR_CINE', nbval=0, nbret=nbchc)
    nbchc = -nbchc

    if (nbchc .gt. 0) then
        call wkvect(lchci, 'V V K24', nbchc, jlchci)
        call getvid(' ', 'CHAR_CINE', nbval=nbchc, vect=zk24(jlchci), nbret=ico)
    endif
!
!
!---- MOT CLE : NUME_DDL
    call getvid(' ', 'NUME_DDL', scal=nu, nbret=ibid)
    call dismoi('NOM_MODELE', nu, 'NUME_DDL', repk = modele)
    call dismoi('NOM_LIGREL', modele, 'MODELE', repk = ligrel)
    call dismoi('PARTITION', ligrel, 'LIGREL', repk = partit)
    if( partit.ne. ' ' ) then
        call utmess('F', 'ASSEMBLA_2')
    endif
!---- POUR ASSE_MATRICE, ASSMAM INTERDIT LA DISTRIBUTION DES RESU_ELEM
    do k = 0, nbmat-1
        call dismoi('PARTITION', zk24(jlimat+k), 'MATR_ELEM', repk = partit2)
        if( partit2.ne. ' ' ) then
            call utmess('F', 'ASSEMBLA_2')
        endif
    enddo
!
!---- ASSEMBLAGE PROPREMENT DIT
    syme = ' '
    call getvtx(' ', 'SYME', scal=syme, nbret=ibid)
    if (syme .eq. 'OUI') then
        call dismoi('SOLVEUR', nu, 'NUME_DDL', repk=solveu)
        call jeveuo(solveu(1:19)//'.SLVK', 'E', vk24=slvk)
        sym2 = slvk(5)(1:8)
        slvk(5)='OUI'
        call asmatr(nbmat, zk24(jlimat), ' ', nu, solveu,&
                    lchci, 'ZERO', 'G', itysca, matas)
        slvk(5)=sym2(1:3)
        call jeveuo(matas//'           .LIME', 'E', vk24=lime)
        do k = 1, nbmat
            lime(k)=zk24(jlimat-1+k)
        end do
    else
        call asmatr(nbmat, zk24(jlimat), ' ', nu, ' ',&
                    lchci, 'ZERO', 'G', itysca, matas)
!
    endif
!
!
!     -- SI MATAS N'EST PAS MPI_COMPLET, ON LA COMPLETE :
    call dismoi('MPI_COMPLET', matas, 'MATR_ASSE', repk=kmpic)
    ASSERT((kmpic.eq.'OUI').or.(kmpic.eq.'NON'))
    if (kmpic .eq. 'NON') call sdmpic('MATR_ASSE', matas)
!
!
!
!
!     -- MENAGE :
    call jedetr(lchci)
    call jedetr(lmatel)
!
    call jedema()
end subroutine
