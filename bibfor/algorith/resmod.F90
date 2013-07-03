subroutine resmod(bmodal, nbmode, neq, numgen, mdgene,&
                  noecho, modsst)
    implicit none
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
!***********************************************************************
!  C. VARE     DATE 16/10/95
!-----------------------------------------------------------------------
!  BUT : < CALCUL DEPLACEMENT MODAL >
!  CALCULER LES DEPLACEMENTS MODAUX D'UN NOEUD D'UNE SOUS-STRUCTURE
!-----------------------------------------------------------------------
!
! BMODAL /I/ : BASE MODALE DE LA STRUCTURE COMPLETE
! NBMODE /I/ : NOMBRE DE MODES DE LA STRUCTURE COMPLETE
! NEQ    /I/ : NOMBRE D'EQUATIONS DU MODELE GENERALISE
! NUMGEN /I/ : NUMEROTATION DU PROBLEME GENERALISE
! MDGENE /I/ : MODELE GENERALISE
! NOECHO /I/ : NOEUD A RESTITUER : NOECHO(1) = NOEUD_1
!                                  NOECHO(2) = SOUS_STRUC_1
!                                  NOECHO(3) = NUME_1
! MODSST /O/ : DEPL PHYSIQUES DES MODES AUX NOEUDS DE CHOC
!
!
!
#include "jeveux.h"
!
#include "asterfort/dcapno.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mgutdm.h"
#include "asterfort/orient.h"
#include "asterfort/posddl.h"
#include "asterfort/wkvect.h"
!
!
    integer :: nbmode, ddl(6), neq
    character(len=8) :: basmod, nomsst, soutr, kb, numddl, noeud, noecho(3)
    character(len=16) :: depl
    character(len=24) :: chamba, mdgene, numgen
    real(kind=8) :: bmodal(neq, *), coord(3), modsst(nbmode, 6)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ibid, ieq, j, jcoord, lchab, llors
    integer :: llprs, neqgen, nsst, nunoe, nusst, nutars
!-----------------------------------------------------------------------
    data depl   /'DEPL            '/
    data soutr  /'&SOUSSTR'/
!-----------------------------------------------------------------------
!
    call jemarq()
    noeud = noecho(1)
    nomsst = noecho(2)
    numddl = noecho(3)
    numgen = numgen(1:14)//'.NUME'
!
! --- NUMERO DE SOUS-STRUCTURE ET DU NOEUD TARDIF CORRESPONDANT
!
    call jenonu(jexnom(mdgene(1:14)//'.MODG.SSNO', nomsst), nusst)
    call jenonu(jexnom(numgen(1:19)//'.LILI', soutr), ibid)
    call jeveuo(jexnum(numgen(1:19)//'.ORIG', ibid), 'L', llors)
    call jelira(jexnum(numgen(1:19)//'.ORIG', ibid), 'LONMAX', nsst, kb)
    do 10 i = 1, nsst
        if (zi(llors+i-1) .eq. nusst) nutars=i
10  end do
!
    call jenonu(jexnom(numgen(1:19)//'.LILI', soutr), ibid)
    call jeveuo(jexnum(numgen(1:19)//'.PRNO', ibid), 'L', llprs)
    neqgen=zi(llprs+(nutars-1)*2+1)
    ieq=zi(llprs+(nutars-1)*2)
!
! --- BASE MODALE ET DU NBRE D'EQUATIONS DE LA SOUS-STRUCTURE
!
    call mgutdm(mdgene, nomsst, ibid, 'NOM_BASE_MODALE', ibid,&
                basmod)
!
! --- RESTITUTION PROPREMENT DITE
!
    call posddl('NUME_DDL', numddl, noeud, 'DX', nunoe,&
                ddl(1))
    call posddl('NUME_DDL', numddl, noeud, 'DY', nunoe,&
                ddl(2))
    call posddl('NUME_DDL', numddl, noeud, 'DZ', nunoe,&
                ddl(3))
    call posddl('NUME_DDL', numddl, noeud, 'DRX', nunoe,&
                ddl(4))
    call posddl('NUME_DDL', numddl, noeud, 'DRY', nunoe,&
                ddl(5))
    call posddl('NUME_DDL', numddl, noeud, 'DRZ', nunoe,&
                ddl(6))
!
    call wkvect('&&RESMOD.COORDO', 'V V R', 3, jcoord)
    do 20 i = 1, nbmode
        modsst(i,1)=0.d0
        modsst(i,2)=0.d0
        modsst(i,3)=0.d0
        modsst(i,4)=0.d0
        modsst(i,5)=0.d0
        modsst(i,6)=0.d0
        zr(jcoord) = 0.d0
        zr(jcoord+1) = 0.d0
        zr(jcoord+2) = 0.d0
        do 30 j = 1, neqgen
            call dcapno(basmod, depl, j, chamba)
            call jeveuo(chamba, 'E', lchab)
            modsst(i,1)=modsst(i,1)+bmodal(ieq+j-1,i)*zr(lchab+ddl(1)-&
            1)
            modsst(i,2)=modsst(i,2)+bmodal(ieq+j-1,i)*zr(lchab+ddl(2)-&
            1)
            modsst(i,3)=modsst(i,3)+bmodal(ieq+j-1,i)*zr(lchab+ddl(3)-&
            1)
            if (ddl(4) .ne. 0 .and. ddl(5) .ne. 0 .and. ddl(6) .ne. 0) then
                modsst(i,4)=modsst(i,4)+bmodal(ieq+j-1,i)*zr(lchab+&
                ddl(4)-1)
                modsst(i,5)=modsst(i,5)+bmodal(ieq+j-1,i)*zr(lchab+&
                ddl(5)-1)
                modsst(i,6)=modsst(i,6)+bmodal(ieq+j-1,i)*zr(lchab+&
                ddl(6)-1)
            endif
30      continue
        zr(jcoord) = modsst(i,1)
        zr(jcoord+1) = modsst(i,2)
        zr(jcoord+2) = modsst(i,3)
        call orient(mdgene, nomsst, jcoord, 1, coord,&
                    0)
        modsst(i,1) = coord(1)
        modsst(i,2) = coord(2)
        modsst(i,3) = coord(3)
        zr(jcoord) = modsst(i,4)
        zr(jcoord+1) = modsst(i,5)
        zr(jcoord+2) = modsst(i,6)
        call orient(mdgene, nomsst, jcoord, 1, coord,&
                    0)
        modsst(i,4) = coord(1)
        modsst(i,5) = coord(2)
        modsst(i,6) = coord(3)
20  end do
    call jedetr('&&RESMOD.COORDO')
!
    call jedema()
end subroutine
