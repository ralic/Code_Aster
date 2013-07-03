subroutine rcmate(chmat, nomail, nomode)
    implicit   none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getvid.h"
#include "asterc/getvtx.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
    character(len=8) :: chmat, nomail, nomode
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
!
!  IN : CHMAT  : CHAMP MATERIAU PRODUIT
!  IN : NOMAIL : NOM DU MAILLAGE
! ----------------------------------------------------------------------
!
    integer :: ibid, nocc, i, nm, nt, jncmp, jvalv, nbma, jmail, ier, nbcmp
    integer :: iarg, jad
    character(len=4) :: oui
    character(len=8) :: k8b, nommat, typmcl(2)
    character(len=16) :: motcle(2)
    character(len=24) :: chamat, mesmai
! ----------------------------------------------------------------------
!
    call jemarq()
    chamat = chmat//'.CHAMP_MAT'
!
    call alcart('G', chamat, nomail, 'NOMMATER')
    call jeveuo(chamat(1:19)//'.NCMP', 'E', jncmp)
    call jeveuo(chamat(1:19)//'.VALV', 'E', jvalv)
!
    call dismoi('F', 'NB_CMP_MAX', 'NOMMATER', 'GRANDEUR', nbcmp,&
                k8b, ier)
    call assert(nbcmp.eq.30)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', 'NOMMATER'), 'L', jad)
    do 5 i = 1, nbcmp
        zk8(jncmp-1+i) = zk8(jad-1+i)
 5  end do
!
    call getfac('AFFE', nocc)
!
    motcle(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
    mesmai = '&&RCMATE.MES_MAILLES'
!
    do 10 i = 1, nocc
        call getvid('AFFE', 'MATER', i, iarg, 1,&
                    nommat, nm)
        if (nm .lt. -1) nm = -nm
        call assert(nm.le.nbcmp)
        call getvid('AFFE', 'MATER', i, iarg, nm,&
                    zk8(jvalv), nm)
        call getvtx('AFFE', 'TOUT', i, iarg, 1,&
                    oui, nt)
        if (nt .ne. 0) then
            call nocart(chamat, 1, k8b, k8b, 0,&
                        k8b, ibid, ' ', nm)
        else
            call reliem(nomode, nomail, 'NU_MAILLE', 'AFFE', i,&
                        2, motcle(1), typmcl(1), mesmai, nbma)
            if (nbma .ne. 0) then
                call jeveuo(mesmai, 'L', jmail)
                call nocart(chamat, 3, k8b, 'NUM', nbma,&
                            k8b, zi(jmail), ' ', nm)
                call jedetr(mesmai)
            endif
        endif
10  end do
!
    call jedetr(chamat(1:19)//'.VALV')
    call jedetr(chamat(1:19)//'.NCMP')
!
    call jedema()
end subroutine
