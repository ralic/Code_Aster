subroutine ualfcr(mataz, basz)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jevtbl.h"
#include "asterfort/jexnum.h"
#include "asterfort/smosli.h"
#include "asterfort/utmess.h"
!
    character(len=*) :: mataz, basz
!     ------------------------------------------------------------------
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
!     CREATION DE L'OBJET MATAZ.UALF POUR CONTENIR LA FACTORISEE LDLT
!     DE LA MATRICE MATAZ
!     RQ : CETTE ROUTINE CREE (SI NECESSAIRE) LE STOCKAGE MORSE DE MATAZ
!     ------------------------------------------------------------------
! IN  JXVAR K19 MATAZ     : NOM D'UNE S.D. MATR_ASSE
! IN        K1  BASZ      : BASE DE CREATION POUR .UALF
!                  SI BASZ=' ' ON PREND LA MEME BASE QUE CELLE DE .VALM
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!     VARIABLES LOCALES
    character(len=1) :: base, tyrc, basto
    character(len=4) :: kmpic
    character(len=14) :: nu
    character(len=19) :: stomor, stolci, matas
    integer :: jscde, neq, nbloc, nblocm
    integer :: jsmhc, jsmdi, jscdi, jschc
    integer :: itbloc, ieq, ibloc, jualf, jvalm, kterm, nbterm, ilig
    integer :: ismdi, ismdi0, ibloav, iscdi, jrefa, jscib, kblocm, iret
    real(kind=8) :: rtbloc
!     ------------------------------------------------------------------
!
!
!
    call jemarq()
    matas=mataz
    base=basz
    call dismoi('MPI_COMPLET', matas, 'MATR_ASSE', repk=kmpic)
    if (kmpic .ne. 'OUI') then
        call utmess('F', 'CALCULEL6_54')
    endif
    if (base .eq. ' ') call jelira(matas//'.VALM', 'CLAS', cval=base)
!
!     -- ON DETRUIT .UALF S'IL EXISTE DEJA :
    call jedetr(matas//'.UALF')
!
    call jeveuo(matas//'.REFA', 'L', jrefa)
    nu=zk24(jrefa-1+2)(1:14)
    stomor=nu//'.SMOS'
    stolci=nu//'.SLCS'
!
!     -- SI LE STOCKAGE STOLCI N'EST PAS ENCORE CREE, ON LE FAIT :
    call jeexin(stolci//'.SCDE', iret)
    if (iret .eq. 0) then
        call jelira(stomor//'.SMDI', 'CLAS', cval=basto)
        rtbloc=jevtbl('TAILLE_BLOC')
        call smosli(stomor, stolci, basto, rtbloc)
    endif
!
    call jeveuo(stomor//'.SMDI', 'L', jsmdi)
    call jeveuo(stomor//'.SMHC', 'L', jsmhc)
!
    call jeveuo(stolci//'.SCDE', 'L', jscde)
    call jeveuo(stolci//'.SCDI', 'L', jscdi)
    call jeveuo(stolci//'.SCHC', 'L', jschc)
    call jeveuo(stolci//'.SCIB', 'L', jscib)
    neq=zi(jscde-1+1)
    itbloc= zi(jscde-1+2)
    nbloc= zi(jscde-1+3)
!
    call jelira(matas//'.VALM', 'NMAXOC', nblocm)
    ASSERT(nblocm.eq.1 .or. nblocm.eq.2)
!
!     -- REEL OU COMPLEXE ?
    call jelira(matas//'.VALM', 'TYPE', cval=tyrc)
    ASSERT(tyrc.eq.'R' .or. tyrc.eq.'C')
!
!
!     1. ALLOCATION DE .UALF :
!     ----------------------------------------
    call jecrec(matas//'.UALF', base//' V '//tyrc, 'NU', 'DISPERSE', 'CONSTANT',&
                nblocm*nbloc)
    call jeecra(matas//'.UALF', 'LONMAX', itbloc)
    do ibloc = 1, nblocm*nbloc
        call jecroc(jexnum(matas//'.UALF', ibloc))
    end do
!
!
!
!     2. REMPLISSAGE DE .UALF :
!     ----------------------------------------
    do kblocm = 1, nblocm
        call jeveuo(jexnum(matas//'.VALM', kblocm), 'L', jvalm)
        ibloav=0+nbloc*(kblocm-1)
        ismdi0=0
        do ieq = 1, neq
            iscdi=zi(jscdi-1+ieq)
            ibloc=zi(jscib-1+ieq)+nbloc*(kblocm-1)
!
!          -- ON RAMENE LE BLOC EN MEMOIRE SI NECESSAIRE:
            if (ibloc .ne. ibloav) then
                call jeveuo(jexnum(matas//'.UALF', ibloc), 'E', jualf)
                if (ibloav .ne. 0) then
                    call jelibe(jexnum(matas//'.UALF', ibloav))
                endif
                ibloav=ibloc
            endif
!
            ismdi=zi(jsmdi-1+ieq)
            nbterm=ismdi-ismdi0
!
            do kterm = 1, nbterm
                ilig=zi4(jsmhc-1+ismdi0+kterm)
                if (tyrc .eq. 'R') then
                    zr(jualf-1+ iscdi +ilig-ieq)=zr(jvalm-1+ismdi0+&
                    kterm)
                else
                    zc(jualf-1+ iscdi +ilig-ieq)=zc(jvalm-1+ismdi0+&
                    kterm)
                endif
            end do
            ASSERT(ilig.eq.ieq)
!
            ismdi0=ismdi
        end do
    end do
!
!
    call jedema()
end subroutine
