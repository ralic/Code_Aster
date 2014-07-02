subroutine asasve(vechar, numedd, typres, vachar)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/assvec.h"
#include "asterfort/corich.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcnco2.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcopy.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: numedd, typres, vechar
    character(len=24) :: vachar
! ----------------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
!
! BUT : ASSEMBLER UN VECT_ELEM RESPECTANT CERTAINES CONVENTIONS
!  =============================================================
!
! IN/JXVAR  VECHAR  (K19) : VECT_ELEM A ASSEMBLER.
! IN/JXIN   NUMEDD  (K14): NUME_DDL DU SYSTEME ASSEMBLE
! IN        TYPRES  (K1) : 'R' OU 'C' (REELS OU COMPLEXES)
! OUT/JXOU VACHAR  (K24): OBJET JEVEUX CONTENANT LA LISTE DES CHAM_NO
!                    RESULTAT DE L'ASSEMBLAGE DES DIFFERENTS RESUELEM
!                    DU VECT_ELEM (VECHAR).
!
!  REMARQUES IMPORTANTES :
!  ======================
!
!   - LE NOM DU VACHAR EST OBTENU EN PRENANT LE NOM DU VECT_ELEM
!     ET EN METTANT UN "A" EN 4EME POSITION
!
!   - POUR CHAQUE RESUELEM DU VECT_ELEM (VECHAR), ON CREE UN CHAM_NO
!     PAR 1 APPEL A ASSVEC.
!
!   - SI LE VECT_ELEM EST TRUANDE (PAR EXEMPLE S'IL VIENT DE VECHME)
!     CERTAINS DES RESUELEM N'EN SONT PAS : CE SONT DEJA DES CHAM_NO
!     DANS CE CAS, ON NE L'ASSEMBLE PAS, MAIS ON LE RECOPIE.
!
!   - SI LE VECT_ELEM EST BIDON ON REND UN VACHAR BIDON CONTENANT
!     1 SEUL CHAM_NO NUL.
!     1 VECT_ELEM EST BIDON SI IL NE CONTIENT AUCUN CHAMP (LONUTI=0)
!
!   - ATTENTION : LE VECT_ELEM EST DETRUIT A LA FIN DE LA ROUTINE
!
!
!
!
!
    integer :: nbvec, ityp, neq, jass, i, ibid, iret, icha
    integer :: n1, jvacha
    aster_logical :: bidon
    character(len=4) :: tych
    character(len=8) :: modele, newnom, vacha8
    character(len=19) :: chamno, resuel, vecele
    character(len=24), pointer :: relr(:) => null()
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
    vecele = vechar
    vacha8 = vecele(1:3)//'A'//vecele(5:8)
    chamno = vacha8//'.???????'
    newnom='.0000000'
!
!
!     1. SI LE VECT_ELEM N'EXISTE PAS : ERREUR FATALE
!     --------------------------------------------------------
    call jeexin(vecele//'.RELR', iret)
    if (iret .eq. 0) then
        call utmess('F', 'ALGORITH_13', sk=vecele)
    endif
    call jelira(vecele//'.RELR', 'LONUTI', nbvec)
    call jeveuo(vecele//'.RELR', 'E', vk24=relr)
!
!
!     2. DESTRUCTION ET RE-ALLOCATION DE VACHAR :
!     --------------------------------------------------------
    call jeexin(vacha8, iret)
    if (iret .gt. 0) then
        call jeveuo(vacha8, 'L', jvacha)
        call jelira(vacha8, 'LONMAX', n1)
        do i = 1, n1
            call detrsd('CHAMP_GD', zk24(jvacha-1+i) (1:19))
        end do
        call jedetr(vacha8)
    endif
    call wkvect(vacha8, 'V V K24', max(nbvec, 1), jass)
!
!
!     2. SI IL N'Y A RIEN A FAIRE (VECT_ELEM BIDON):
!     --------------------------------------------------------
    bidon = .false.
    if (nbvec .eq. 0) bidon = .true.
!
    if (bidon) then
        call gcnco2(newnom)
        chamno(10:16) = newnom(2:8)
        call corich('E', chamno, -2, ibid)
        call vtcreb(chamno, numedd, 'V', typres, neq)
        zk24(jass-1+1) = chamno
        goto 30
    endif
!
!
!     3. SI IL FAUT FAIRE QUELQUE CHOSE :
!     --------------------------------------------------------
    call dismoi('NOM_MODELE', numedd, 'NUME_DDL', repk=modele)
    call memare('V', '&&ASASVE', modele, ' ', ' ',&
                'CHAR_MECA')
    call reajre('&&ASASVE', ' ', 'V')
!
    ityp = 1
    if (typres .eq. 'C') ityp = 2
!
    do i = 1, nbvec
        resuel = relr(i)(1:19)
!       CALL UTIMS2('ASASVE 1',I,RESUEL,1,' ')
!
        call corich('L', resuel, ibid, icha)
        ASSERT((icha.ne.0).and.(icha.ge.-2))
!
        call gcnco2(newnom)
        chamno(10:16) = newnom(2:8)
        call corich('E', chamno, icha, ibid)
        zk24(jass+i-1) = chamno
!
!       -- SI LE RESUELEM EST UN RESUELEM !
        call dismoi('TYPE_CHAMP', resuel, 'CHAMP', repk=tych)
        if (tych .eq. 'RESL') then
            call jedetr('&&ASASVE           .RELR')
            call reajre('&&ASASVE', resuel, 'V')
            call assvec('V', chamno, 1, '&&ASASVE           .RELR', [1.d0],&
                        numedd, ' ', 'ZERO', ityp)
!
!
!       -- SI LE RESUELEM N'EST PAS UN RESUELEM !(CHAM_NO)
        else if (tych.eq.'NOEU') then
            call vtcreb(chamno, numedd, 'V', typres, neq)
            call vtcopy(resuel, chamno, ' ', iret)
!
        else
            ASSERT(.false.)
        endif
!
    end do
    call jedetr('&&ASASVE           .RELR')
    call jedetr('&&ASASVE           .RERR')
!
!
 30 continue
!
!
!
!     DESTRUCTION DU VECT_ELEM :
!     -----------------------------------
    do i = 1, nbvec
        call corich('S', relr(i) (1:19), ibid, ibid)
        call detrsd('CHAMP_GD', relr(i))
    end do
    call jedetr(vecele//'.RELR')
    call jedetr(vecele//'.RERR')
!
    vachar=vacha8
!
!
    call jedema()
end subroutine
