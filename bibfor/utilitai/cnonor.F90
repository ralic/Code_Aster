subroutine cnonor(nomo, gran, base, cno)
    implicit none
#include "jeveux.h"
#include "asterfort/afchno.h"
#include "asterfort/affeno.h"
#include "asterfort/canort.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbnlma.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/vericp.h"
#include "asterfort/wkvect.h"
!
    character(len=1) :: base
    character(len=8) :: nomo, gran, cno
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! BUT :     COMMANDE : CREA_CHAMP/OPERATION:'NORMALE'
! ----------------------------------------------------------------------
    integer :: nec, iacmp, iav, i, iret, ii, ino, jj, ncmpmx, numgd
    integer :: ndim, nbno, nbnoeu, idim, nn, nbma, nbcomp, nbtyp, lonval, icomp
    integer :: ic, iec, iand, jlma,   jnno, jval, jnbca, jdesc
    real(kind=8) :: valr(3)
    character(len=2) :: typval
    character(len=8) :: k8b, resu, noma, typmcl(4), nocmp(3), listyp(10)
    character(len=16) :: motclf, motcle(2)
    character(len=24) :: nomnoe, mesmai
    character(len=24) :: valk(2)
    real(kind=8), pointer :: normale(:) => null()
    integer, pointer :: ln(:) => null()
! ----------------------------------------------------------------------
    call jemarq()
!
    resu = cno
!
    call jenonu(jexnom('&CATA.GD.NOMGD', gran), numgd)
    if (numgd .eq. 0) then
        valk (1) = gran
        call utmess('F', 'UTILITAI6_1', sk=valk(1))
    endif
    call dismoi('NB_EC', gran, 'GRANDEUR', repi=nec)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', gran), 'L', iacmp)
    call jeveuo(jexatr('&CATA.GD.NOMCMP', 'LONCUM'), 'L', iav)
    ncmpmx = zi(iav+numgd) - zi(iav+numgd-1)
!
    call dismoi('NOM_MAILLA', nomo, 'MODELE', repk=noma)
!
! --- DIMENSION DU PROBLEME
!
    call dismoi('DIM_GEOM', noma, 'MAILLAGE', repi=ndim)
!
! --- DEFINITION DES COMPOSANTES ET DES TYPES DE MAILLE A TRAITER
!
    if (ndim .eq. 2) then
        nbcomp = 2
        nocmp(1) = 'X'
        nocmp(2) = 'Y'
        nbtyp = 3
        listyp(1) = 'SEG2'
        listyp(2) = 'SEG3'
        listyp(3) = 'SEG4'
    else
        nbcomp = 3
        nocmp(1) = 'X'
        nocmp(2) = 'Y'
        nocmp(3) = 'Z'
        nbtyp = 10
        listyp(1) = 'TRIA3'
        listyp(2) = 'TRIA6'
        listyp(3) = 'TRIA9'
        listyp(4) = 'QUAD4'
        listyp(5) = 'QUAD8'
        listyp(6) = 'QUAD9'
        listyp(7) = 'QUAD12'
        listyp(8) = 'SEG2'
        listyp(9) = 'SEG3'
        listyp(10) = 'SEG4'
    endif
!
! --- VERIFICATION QUE LES COMPOSANTES APPARTIENNENT A LA GRANDEUR
!
    do i = 1, nbcomp
        call vericp(zk8(iacmp), nocmp(i), ncmpmx, iret)
        if (iret .ne. 0) then
            valk (1) = gran
            valk (2) = nocmp(i)
            call utmess('F', 'UTILITAI6_11', nk=2, valk=valk)
        endif
    end do
!
! --- LISTE DES MAILLES A TRAITER
!
    mesmai = '&&CNONOR.MES_MAILLES'
    motclf = ' '
    motcle(1) = 'MAILLE'
    motcle(2) = 'GROUP_MA'
    typmcl(1) = 'MAILLE'
    typmcl(2) = 'GROUP_MA'
!
    call reliem(' ', noma, 'NU_MAILLE', motclf, 1,&
                2, motcle, typmcl, mesmai, nbma)
    call jeveuo(mesmai, 'L', jlma)
!
    call nbnlma(noma, nbma, zi(jlma), nbtyp, listyp,&
                nbno)
    call jeveuo('&&NBNLMA.LN', 'L', vi=ln)
!
! --- DETERMINATION DES NORMALES
!
    call canort(noma, nbma, zi(jlma), ndim, nbno,&
                ln, 1)
!
    call jeveuo('&&CANORT.NORMALE', 'L', vr=normale)
!
!-----------------------------------------------------------------------
!
    nomnoe = noma//'.NOMNOE'
    call jelira(nomnoe, 'NOMMAX', nbnoeu)
    typval = 'R'
!
!     AFFE DU CHAMP AUX NOEUDS
!     ------------------------
! --- ALLOCATION DE 4 OBJETS INTERMEDIAIRES SERVANT AUX CALCULS
!     DE .PRNO ET .VALE
!
    call wkvect('&&CNONOR.NOMS_NOEUDS', 'V V K8', nbnoeu, jnno)
    call wkvect('&&CNONOR.VALCOMPNO', 'V V R', nbnoeu*ncmpmx, jval)
    call wkvect('&&CNONOR.NCMPMX_AFFE', 'V V I ', nbnoeu, jnbca)
    call wkvect('&&CNONOR.DESC_NOEUD', 'V V I', nec*nbnoeu, jdesc)
!
    do ii = 1, nbno
        ino = ln(ii)
        call jenuno(jexnum(nomnoe, ino ), zk8(jnno+ino-1))
!
        do idim = 1, ndim
            valr(idim) = normale(ndim*(ii-1)+idim)
        enddo
!
        call affeno(1, ino, nocmp, nbcomp, zk8(iacmp),&
                    ncmpmx, valr, k8b, zi(jdesc), zr(jval),&
                    k8b, typval, nec)
!
    end do
!
! --- CALCUL DU NOMBRE TOTAL DE CMP AFFECTEES (SOMMEES SUR LES NOEUDS)
!
    lonval = 0
    do ino = 1, nbnoeu
        icomp = 0
        do ic = 1, ncmpmx
            iec = (ic-1)/30 + 1
            jj = ic - 30* (iec-1)
            ii = 2**jj
            nn = iand(zi(jdesc+ (ino-1)*nec+iec-1),ii)
            if (nn .gt. 0) then
                icomp = icomp + 1
            endif
        end do
        zi(jnbca-1+ino) = icomp
        lonval = lonval + icomp
    end do
!
    call afchno(resu, base, gran, noma, nbnoeu,&
                zi(jnbca), zi(jdesc), lonval, typval, zr(jval),&
                zc(jval), k8b)
!
    call jedema()
end subroutine
