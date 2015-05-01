subroutine foattr(motcle, iocc, nomfon)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
    integer :: iocc
    character(len=*) :: motcle, nomfon
!     ----------------------------------------------------------------
! ======================================================================
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
!     SURCHARGE LES ATTRIBUTS D'UN CONCEPT DE TYPE "FONCTION"
!     ----------------------------------------------------------------
    character(len=4) :: interp(2)
    character(len=8) :: prolg, prold
    character(len=16) :: npara, nresu
    character(len=19) :: temp
    character(len=24) :: prol
!     ----------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: if, l1, l2, l3, l4, l5, l6
    integer :: l7, l8, l9, lpro, nbfonc, nbprol
!
!-----------------------------------------------------------------------
    call jemarq()
    temp = nomfon
    ASSERT(lxlgut(temp).le.24)
    prol = temp//'.PROL'
    call jeveuo(prol, 'E', lpro)
    call jelira(prol, 'LONUTI', nbprol)
!
    if (zk24(lpro) .eq. 'NAPPE   ') then
        nbfonc = ( nbprol - 7 ) / 2
!
        call getvtx(motcle, 'INTERPOL', iocc=iocc, nbval=2, vect=interp,&
                    nbret=l1)
        if (l1 .eq. 1) zk24(lpro+1) = interp(1)//interp(1)
        if (l1 .eq. 2) zk24(lpro+1) = interp(1)//interp(2)
!
        call getvtx(motcle, 'NOM_PARA', iocc=iocc, scal=npara, nbret=l2)
        if (l2 .ne. 0) zk24(lpro+2) = npara
!
        call getvtx(motcle, 'NOM_RESU', iocc=iocc, scal=nresu, nbret=l3)
        if (l3 .ne. 0) zk24(lpro+3) = nresu
!
        call getvtx(motcle, 'PROL_GAUCHE', iocc=iocc, scal=prolg, nbret=l4)
        if (l4 .ne. 0) zk24(lpro+4)(1:1) = prolg(1:1)
!
        call getvtx(motcle, 'PROL_DROITE', iocc=iocc, scal=prold, nbret=l5)
        if (l5 .ne. 0) zk24(lpro+4)(2:2) = prold(1:1)
!
        zk24(lpro+5) = temp
!
        call getvtx(motcle, 'NOM_PARA_FONC', iocc=iocc, scal=npara, nbret=l6)
        if (l6 .ne. 0) zk24(lpro+6) = npara
!
        call getvtx(motcle, 'INTERPOL_FONC', iocc=iocc, nbval=2, vect=interp,&
                    nbret=l7)
        if (l7 .ne. 0) then
            do 10 if = 1, nbfonc
                if (l7 .eq. 1) zk24(lpro+7+2*(if-1)) = interp(1)// interp(1)
                if (l7 .eq. 2) zk24(lpro+7+2*(if-1)) = interp(1)// interp(2)
10          continue
        endif
!
        call getvtx(motcle, 'PROL_GAUCHE_FONC', iocc=iocc, scal=prolg, nbret=l8)
        if (l8 .ne. 0) then
            do 12 if = 1, nbfonc
                zk24(lpro+8+2*(if-1))(1:1) = prolg(1:1)
12          continue
        endif
!
        call getvtx(motcle, 'PROL_DROITE_FONC', iocc=iocc, scal=prold, nbret=l9)
        if (l9 .ne. 0) then
            do 14 if = 1, nbfonc
                zk24(lpro+8+2*(if-1))(2:2) = prold(1:1)
14          continue
        endif
!
!
    else
!
        call getvtx(motcle, 'INTERPOL', iocc=iocc, nbval=2, vect=interp,&
                    nbret=l1)
        if (l1 .ne. 0) then
            if (l1 .eq. 1) zk24(lpro+1) = interp(1)//interp(1)
            if (l1 .eq. 2) zk24(lpro+1) = interp(1)//interp(2)
        endif
!
        call getvtx(motcle, 'NOM_PARA', iocc=iocc, scal=npara, nbret=l2)
        if (l2 .ne. 0) zk24(lpro+2) = npara
!
        call getvtx(motcle, 'NOM_RESU', iocc=iocc, scal=nresu, nbret=l3)
        if (l3 .ne. 0) zk24(lpro+3) = nresu
!
        call getvtx(motcle, 'PROL_GAUCHE', iocc=iocc, scal=prolg, nbret=l4)
        if (l4 .ne. 0) zk24(lpro+4)(1:1) = prolg(1:1)
!
        call getvtx(motcle, 'PROL_DROITE', iocc=iocc, scal=prold, nbret=l5)
        if (l5 .ne. 0) zk24(lpro+4)(2:2) = prold(1:1)
!
        zk24(lpro+5) = temp
!
    endif
!
    call jedema()
end subroutine
