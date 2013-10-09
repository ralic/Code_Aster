subroutine op0156()
    implicit none
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!
!     OPERATEUR :   PROD_MATR_CHAM
!
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/chpver.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/idensd.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mcmult.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcopy.h"
#include "asterfort/vtcreb.h"
    integer ::  n1, iret, neq, ier
    integer :: lmat, jchin, jchout
    character(len=1) :: typmat, typres
    character(len=24) :: valk(2)
    character(len=14) :: numem
    character(len=19) :: pfchn1, pfchn2
    character(len=16) :: type, nomcmd
    character(len=19) :: masse, resu, chamno, chamn2
!     ------------------------------------------------------------------
    call jemarq()
    call infmaj()
!
!     --- RECUPERATION DES ARGUMENTS DE LA COMMANDE ---
    resu=' '
    call getres(resu, type, nomcmd)
!
!     0. MATRICE :
!     -------------
    call getvid(' ', 'MATR_ASSE', scal=masse, nbret=n1)
    call mtdscr(masse)
    call jeveuo(masse(1:19)//'.&INT', 'E', lmat)
    if (zi(lmat+3) .eq. 1) then
        typmat='R'
    else if (zi(lmat+3).eq.2) then
        typmat='C'
    else
        call utmess('F', 'ALGELINE2_86')
    endif
    call dismoi('NOM_NUME_DDL', masse, 'MATR_ASSE', repk=numem)
    call dismoi('PROF_CHNO', masse, 'MATR_ASSE', repk=pfchn1)
!
!
!     1. CHAM_NO  :
!     -------------
    call getvid(' ', 'CHAM_NO', scal=chamno, nbret=n1)
    call chpver('F', chamno, 'NOEU', '*', ier)
    call jelira(chamno//'.VALE', 'TYPE', cval=typres)
    if (typmat .ne. typres) then
        valk(1)=typmat
        valk(2)=typres
        call utmess('F', 'ALGELINE4_42', nk=2, valk=valk)
    endif
!
    call dismoi('PROF_CHNO', chamno, 'CHAM_NO', repk=pfchn2)
!     -- SI LA NUMEROTATION DE CHAM_NO N'EST PAS LA MEME QUE CELLE DE
!        LA MATRICE, ON CHANGE LA NUMEROTATION DE CHAM_NO.
!        EN APPELANT VTCOPY, ON PERD LA VALEUR DES LAGRANGES
    if (.not.idensd('PROF_CHNO',pfchn1,pfchn2)) then
        valk(1)=pfchn1
        valk(2)=pfchn2
        call utmess('A', 'CALCULEL3_46', nk=2, valk=valk)
        chamn2='&&OP0156.CHAM_NO'
        call vtcreb(chamn2, numem, 'V', typres, neq)
        call vtcopy(chamno, chamn2, 'F', ier)
        chamno=chamn2
    endif
    call jeveuo(chamno//'.VALE', 'L', jchin)
!
!
!     3. ALLOCATION DU CHAM_NO RESULTAT :
!     ----------------------------------
    call jeexin(resu//'.VALE', iret)
    if (iret .ne. 0) then
        call utmess('F', 'ALGELINE2_87', sk=resu(1:8))
    endif
    call vtcreb(resu, numem, 'G', typres, neq)
    call jeveuo(resu//'.VALE', 'E', jchout)
!
!
!     4. PRODUIT MATRICE X VECTEUR :
!     ----------------------------------
    if (typres .eq. 'R') then
        call mrmult('ZERO', lmat, zr(jchin), zr(jchout), 1,&
                    .true.)
    else if (typres.eq.'C') then
        call mcmult('ZERO', lmat, zc(jchin), zc(jchout), 1,&
                    .true.)
    endif
!
!
    call titre()
!
    call jedema()
end subroutine
