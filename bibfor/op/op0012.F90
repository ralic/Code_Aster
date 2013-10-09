subroutine op0012()
!======================================================================
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
!
!                       OPERATEUR ASSE_MATRICE
!======================================================================
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
#include "asterfort/wkvect.h"
!
    character(len=8) :: nu, matas, charge, syme, sym2, kmpic
    character(len=16) :: typm, oper
    character(len=19) :: matel, solveu
    character(len=24) :: lchci, lmatel
    integer :: itysca, nbchc, nbmat, jlimat, jlchci, ibid, k, j, nbchar
    integer :: jrecc, ico, iexi, islvk, ilimat
!-----------------------------------------------------------------------
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
!     -- LES SD_CHAR_XXX PEUVENT CONTENIR UNE SD_CHAR_CINE :
    do k = 1, nbmat
        matel=zk24(jlimat-1+k)
        ASSERT(zk24(jlimat-1+k)(9:24).eq.' ')
        call jeexin(matel//'.RECC', iexi)
        if (iexi .gt. 0) then
            call jeveuo(matel//'.RECC', 'L', jrecc)
            call jelira(matel//'.RECC', 'LONMAX', nbchar)
            do j = 1, nbchar
                charge=zk8(jrecc-1+j)
                call jeexin(charge//'.ELIM      .AFCK', iexi)
                if (iexi .gt. 0) nbchc=nbchc+1
            end do
        endif
    end do
!
    if (nbchc .gt. 0) then
        call wkvect(lchci, 'V V K24', nbchc, jlchci)
        call getvid(' ', 'CHAR_CINE', nbval=nbchc, vect=zk24(jlchci), nbret=ico)
        do k = 1, nbmat
            matel=zk24(jlimat-1+k)
            call jeexin(matel//'.RECC', iexi)
            if (iexi .gt. 0) then
                call jeveuo(matel//'.RECC', 'L', jrecc)
                call jelira(matel//'.RECC', 'LONMAX', nbchar)
                do j = 1, nbchar
                    charge=zk8(jrecc-1+j)
                    call jeexin(charge//'.ELIM      .AFCK', iexi)
                    if (iexi .gt. 0) then
                        ico=ico+1
                        zk24(jlchci-1+ico)=charge//'.ELIM'
                    endif
                end do
            endif
        end do
    endif
!
!
!---- MOT CLE : NUME_DDL
    call getvid(' ', 'NUME_DDL', scal=nu, nbret=ibid)
!
!---- ASSEMBLAGE PROPREMENT DIT
    syme = ' '
    call getvtx(' ', 'SYME', scal=syme, nbret=ibid)
    if (syme .eq. 'OUI') then
        call dismoi('SOLVEUR', nu, 'NUME_DDL', repk=solveu)
        call jeveuo(solveu(1:19)//'.SLVK', 'E', islvk)
        sym2 = zk24(islvk+5-1)(1:8)
        zk24(islvk+5-1)='OUI'
        call asmatr(nbmat, zk24(jlimat), ' ', nu, solveu,&
                    lchci, 'ZERO', 'G', itysca, matas)
        zk24(islvk+5-1)=sym2(1:3)
        call jeveuo(matas//'           .LIME', 'E', ilimat)
        do k = 1, nbmat
            zk24(ilimat-1+k)=zk24(jlimat-1+k)
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
