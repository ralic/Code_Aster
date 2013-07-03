subroutine asvepr(lischa, vecelz, typres, numedd)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit      none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/assvec.h"
#include "asterfort/chor2c.h"
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
#include "asterfort/lisltc.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
#include "asterfort/vtcopy.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
    character(len=19) :: lischa
    character(len=*) :: vecelz, numedd
    character(len=1) :: typres
!
! ----------------------------------------------------------------------
!
! PREPARATION DE L'ASSEMBLAGE D'UN VECT_ELEM
!
! ----------------------------------------------------------------------
!
!
! IN  LISCHA : SD LISTE DES CHARGES
! IN  VECELE : NOM DU VECT_ELEM
! IN  TYPRES : TYPE DU RESULTAT 'R' OU 'C' (REELS OU COMPLEXES)
! IN  NUMEDD : NUME_DDL DU SYSTEME ASSEMBLE
!
! LE VACHAR CONTIENT LA LISTE DES CHAM_NO RESULTAT DE L'ASSEMBLAGE DES
! DIFFERENTS RESU_ELEM DU VECT_ELEM
!
! * POUR CHAQUE RESU_ELEM DU VECT_ELEM, ON CREE UN CHAM_NO PAR UN APPEL
!   A ASSVEC
! * SI LE VECT_ELEM EST TRUANDE (VOIR VECHME), CERTAINS DES RESU_ELEM
!   N'EN SONT PAS : CE SONT DEJA DES CHAM_NO (CHARGEMENT VECT_ASSE DANS
!   AFFE_CHAR_MECA). DANS CE CAS, ON NE L'ASSEMBLE PAS, MAIS ON LE
!   RECOPIE.
! * SI LE VECT_ELEM EST BIDON ON REND UN VACHAR BIDON CONTENANT
!     1 SEUL CHAM_NO NUL.
!     1 VECT_ELEM EST BIDON SI IL NE CONTIENT AUCUN CHAMP (LONUTI=0)
!
! ATTENTION : LE VECT_ELEM EST DETRUIT A LA FIN DE LA ROUTINE
!
!
!
!
    character(len=19) :: vecele, chamno
    integer :: jvec
    character(len=24) :: vachar
    integer :: jvacha
    character(len=24) :: resuel
    character(len=8) :: k8bid, newnom, modele, typech, typsca
    integer :: ivach, nbvach
    integer :: neq, nbvec
    integer :: iret, ibid, ivec, ichar, ityprs
    character(len=4) :: tyresl
    character(len=1) :: typchn
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    newnom = '.0000000'
    vecele = vecelz
    call assert(typres.eq.'R'.or.typres.eq.'C')
!
! --- LE VECT_ELEM EXISTE-IL ?
!
    call jeexin(vecele//'.RELR', iret)
    if (iret .eq. 0) then
        nbvec = 0
    else
        call jelira(vecele//'.RELR', 'LONUTI', nbvec, k8bid)
    endif
!
! --- NOM DU CHAMNO
!
    chamno = vecele(1:8)//'.???????'
!
! --- NOM DU VACHAR
!
    vachar = vecele(1:19)//'.CHNO'
!
! --- DESTRUCTION DU VACHAR
!
    call jeexin(vachar, iret)
    if (iret .gt. 0) then
        call jeveuo(vachar, 'L', jvacha)
        call jelira(vachar, 'LONMAX', nbvach, k8bid)
        do 10 ivach = 1, nbvach
            call detrsd('CHAMP_GD', zk24(jvacha-1+ivach)(1:19))
10      continue
        call jedetr(vachar)
    endif
!
! --- CREATION DU VACHAR
!
    call wkvect(vachar, 'V V K24', max(nbvec, 1), jvacha)
!
! --- SI IL N'Y A RIEN A FAIRE, ON CREE UN CHAM_NO BIDON
!
    if (nbvec .eq. 0) then
        call gcnco2(newnom)
        chamno(10:16) = newnom(2:8)
        call corich('E', chamno, -2, ibid)
        call vtcreb(chamno, numedd, 'V', typres, neq)
        zk24(jvacha-1+1) = chamno
        goto 99
    endif
!
! --- CREER L'OBJET .RERR DU VECT_ELEM
!
    call dismoi('F', 'NOM_MODELE', numedd, 'NUME_DDL', ibid,&
                modele, iret)
    call memare('V', '&&ASVEPR', modele, ' ', ' ',&
                'CHAR_MECA')
!
! --- INITIALISER L'OBJET .RELR DU VECT_ELEM
!
    call reajre('&&ASVEPR', ' ', 'V')
    call jeveuo(vecele//'.RELR', 'E', jvec)
!
! --- ASSEMBLAGE DES VECT_ELEM
!
    do 20 ivec = 1, nbvec
!
! ----- NOM DU RESU_ELEM
!
        resuel = zk24(jvec-1+ivec)
!
! ----- PREPARATION DU NOM DU CHAM_NO
!
        call gcnco2(newnom)
        chamno(10:16) = newnom(2:8)
        zk24(jvacha-1+ivec) = chamno
!
! ----- ENREGISTREMENT DU NUMERO DE LA CHARGE DANS LE CHAM_NO
!
        call corich('L', resuel, ibid, ichar)
        call corich('E', chamno, ichar, ibid)
!
! ----- TYPE DU RESU_ELEM
!
        call dismoi('F', 'TYPE_CHAMP', resuel, 'CHAMP', ibid,&
                    tyresl, ibid)
        call assert(tyresl.eq.'RESL'.or.tyresl.eq.'NOEU')
!
! ----- SI LE RESU_ELEM EST UN VRAI RESU_ELEM (ISSU DE CALCUL)
!
        if (tyresl .eq. 'RESL') then
            call jedetr('&&ASVEPR           .RELR')
            call reajre('&&ASVEPR', resuel, 'V')
            call dismoi('F', 'TYPE_SCA', resuel, 'RESUELEM', ibid,&
                        typsca, ibid)
            if (typsca .eq. 'R') then
                ityprs = 1
            else if (typsca.eq.'C') then
                ityprs = 2
            else
                call assert(.false.)
            endif
            call assvec('V', chamno, 1, '&&ASVEPR           .RELR', 1.d0,&
                        numedd, ' ', 'ZERO', ityprs)
        endif
!
! ----- SI LE RESU_ELEM EST UN FAUX RESU_ELEM (CHAM_NO)
!
        if (tyresl .eq. 'NOEU') then
            call lisltc(lischa, ichar, typech)
            typchn = 'R'
            if (typech .eq. 'COMP') typchn = 'C'
            call vtcreb(chamno, numedd, 'V', typchn, neq)
            call vtcopy(resuel, chamno, 'F', ibid)
        endif
!
20  end do
    call jedetr('&&ASVEPR           .RELR')
    call jedetr('&&ASVEPR           .RERR')
!
! --- TRANSFORMATION EN COMPLEXE SI NECESSAIRE
!
    if (typres .eq. 'C') then
        call chor2c(lischa, vecele)
    endif
!
! --- DESTRUCTION DU VECT_ELEM
!
    call jeexin(vecele//'.RELR', iret)
    do 40 ivec = 1, nbvec
        resuel = zk24(jvec-1+ivec)
        call corich('S', resuel, ibid, ibid)
        call detrsd('CHAMP_GD', resuel)
40  end do
    call jedetr(vecele//'.RELR')
    call jedetr(vecele//'.RERR')
!
99  continue
!
    call jedema()
end subroutine
