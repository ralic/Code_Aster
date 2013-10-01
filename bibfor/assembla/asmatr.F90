subroutine asmatr(nbmat, tlimat, licoef, nu, solveu,&
                  infcha, cumul, base, itysca, mataz)
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "jeveux.h"
#include "asterfort/ascima.h"
#include "asterfort/assert.h"
#include "asterfort/assmam.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcncon.h"
#include "asterfort/jedbg2.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/masyns.h"
#include "asterfort/resyme.h"
#include "asterfort/typmat.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: base, mataz, tlimat(*), licoef, nu
    integer :: nbmat, itysca
    character(len=*) :: solveu, infcha
    character(len=4) :: cumul
!-----------------------------------------------------------------------
! IN  I   NBMAT  : NOMBRE DE MATR_ELEM DE LA LISTE TLIMAT
! IN  K19 TLIMAT : LISTE DES MATR_ELEM
! IN  K24 LICOEF : NOM DU VECTEUR CONTENANT LES COEF. MULT.
!                  DES MATR_ELEM
!                  SI LICOEF=' ' ON PREND 1.D0 COMME COEF.
! IN  K14 NU     : NOM DU NUME_DDL
! IN  K19 SOLVEU : NOM DU SOLVEUR (OU ' ')
! IN  K19 INFCHA : POUR LES CHARGES CINEMATIQUES :
!                  / SD_INFCHA (K19)
!                  / NOM D'UN OBJET JEVEUX (K24) CONTENANT
!                    LES NOMS DES CHARGES CINEMATIQUES (K24)
! IN  K4 CUMUL : 'ZERO' OU 'CUMU'
!                 'ZERO':SI UN OBJET DE NOM MATAS ET DE TYPE
!                        MATR_ASSE EXISTE ON ECRASE SON CONTENU.
!                 'CUMU':SI UN OBJET DE NOM MATAS ET DE TYPE
!                        MATR_ASSE EXISTE ON CUMULE DANS .VALM
! IN  K1  BASE   : BASE SUR LAQUELLE ON CREE L'OBJET MATAZ
! IN  I   ITYSCA  : TYPE DES MATRICES ELEMENTAIRES A ASSEMBLER
!                          1 --> REELLES
!                          2 --> COMPLEXES
! IN/OUT K19 MATAZ : L'OBJET MATAZ DE TYPE MATR_ASSE EST CREE ET REMPLI
!-----------------------------------------------------------------------
!
    character(len=1) :: matsym
    character(len=3) :: syme
    character(len=7) :: symel
    character(len=24) :: metres, licoe2
    integer :: k
    character(len=8) :: matk8
    character(len=19) :: tlima2(150), solve2, matas, matel, infc19
    integer :: ilicoe, i, jslvk, iret, ibid, idbgav, ilimat, ier
    integer :: jrefa
!DEB-------------------------------------------------------------------
    call jemarq()
    call jedbg2(idbgav, 0)
!
    matas = mataz
    licoe2 = licoef
    infc19 = infcha
    solve2 = solveu
    if (solve2 .eq. ' ') then
        call dismoi('F', 'SOLVEUR', nu, 'NUME_DDL', ibid,&
                    solve2, ier)
    endif
!
    ASSERT(cumul.eq.'ZERO'.or.cumul.eq.'CUMU')
    if (cumul .eq. 'ZERO') call detrsd('MATR_ASSE', matas)
    if (nbmat .gt. 150) ASSERT(.false.)
    do k = 1, nbmat
        tlima2(k) = tlimat(k)
    end do
!
!
!     -- TRAITEMENT DE LA LISTE DES COEF. MULTIPLICATEURS :
!     ---------------------------------------------------------------
    if (licoe2 .eq. ' ') then
        call wkvect('&&ASMATR.LICOEF', 'V V R', nbmat, ilicoe)
        do i = 1, nbmat
            zr(ilicoe+i-1) = 1.d0
        end do
    else
        call jeveuo(licoe2, 'L', ilicoe)
    endif
!
!
!
!     -- PREPARATION DE LA LISTE DE MATR_ELEM POUR QU'ILS SOIENT
!        DU MEME TYPE (SYMETRIQUE OU NON) QUE LA MATR_ASSE :
!     ---------------------------------------------------------------
    call wkvect('&&ASMATR.LMATEL', 'V V K24', nbmat, ilimat)
    matsym = typmat(nbmat,tlima2)
!
    call jeveuo(solve2//'.SLVK', 'L', jslvk)
    syme = zk24(jslvk+5-1)(1:3)
    if (syme .eq. 'OUI') then
        do i = 1, nbmat
            call dismoi('F', 'TYPE_MATRICE', tlima2(i), 'MATR_ELEM', ibid,&
                        symel, ier)
            if (symel .eq. 'NON_SYM') then
                call gcncon('.', matk8)
                matel=matk8
                zk24(ilimat+i-1) = matel
                call resyme(tlima2(i), 'V', matel)
            else
                zk24(ilimat+i-1) = tlima2(i)
            endif
        end do
        matsym = 'S'
    else
        do i = 1, nbmat
            zk24(ilimat+i-1) = tlima2(i)
        end do
    endif
!
!
!
!     -- VERIFICATIONS :
!     ------------------
    metres = zk24(jslvk)
    if ((metres.eq.'GCPC') .and. (matsym.eq.'N')) then
        call utmess('F', 'ASSEMBLA_1', sk=matsym)
    endif
!
!
!
!     -- SI MATRICE EXISTE DEJA ET QU'ELLE DOIT ETRE NON-SYMETRIQUE,
!        ON LA DE-SYMETRISE :
!     ---------------------------------------------------------------
    if (cumul .eq. 'CUMU') then
        call jeexin(matas//'.REFA', iret)
        ASSERT(iret.gt.0)
        call jeveuo(matas//'.REFA', 'L', jrefa)
        if (matsym .eq. 'N' .and. zk24(jrefa-1+9) .eq. 'MS') call masyns(matas)
    endif
!
!
!     -- ASSEMBLAGE PROPREMENT DIT :
!     -------------------------------
    call assmam(base, matas, nbmat, zk24(ilimat), zr(ilicoe),&
                nu, cumul, itysca)
!
!
!     -- TRAITEMENT DES CHARGES CINEMATIQUES :
!     ----------------------------------------
    call jeveuo(matas//'.REFA', 'L', jrefa)
    ASSERT(zk24(jrefa-1+3).ne.'ELIMF')
    call ascima(infc19, nu, matas, cumul)
!
!
!     -- MENAGE :
!     -----------
    call jedetr('&&ASMATR.LICOEF')
    if (syme .eq. 'OUI') then
        do i = 1, nbmat
            call dismoi('F', 'TYPE_MATRICE', tlima2(i), 'MATR_ELEM', ibid,&
                        symel, ier)
            if (symel .eq. 'SYMETRI') goto 60
            call detrsd('MATR_ELEM', zk24(ilimat+i-1)(1:19))
 60         continue
        end do
    endif
!
!
!
    call jedetr('&&ASMATR.LMATEL')
    call jedbg2(ibid, idbgav)
    call jedema()
end subroutine
