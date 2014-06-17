subroutine ualfva(mataz, basz)
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/crsmos.h"
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
#include "asterfort/jexnum.h"
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
!     CREATION DE L'OBJET MATAZ.VALM A PARTIR DE L'OBJET MATAZ.UALF
!     L'OBJET .UALF DOIT CONTENIR LA MATRICE INITIALE NON FACTORISEE :
!       - ON CREE L'OBJET.VALM
!       - ON DETRUIT .UALF (A LA FIN DE LA ROUTINE)
!       - ON CREE LE STOCKAGE MORSE DANS LE NUME_DDL S'IL N'EXISTE PAS.
!
!     CETTE ROUTINE NE DEVRAIT ETRE UTILISEE QUE RAREMENT :
!        LORSQUE LA MATR_ASSE A ETE CREE SOUS LA FORME .UALF POUR DES
!        RAISONS HISTORIQUES.
!
!     ------------------------------------------------------------------
! IN  JXVAR K19 MATAZ     : NOM D'UNE S.D. MATR_ASSE
! IN        K1  BASZ      : BASE DE CREATION POUR .VALM
!                  SI BASZ=' ' ON PREND LA MEME BASE QUE CELLE DE .UALF
!     REMARQUE : ON DETRUIT L'OBJET .UALF
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!     VARIABLES LOCALES
    character(len=1) :: base, tyrc
    character(len=14) :: nu
    character(len=19) :: stomor, stolci, matas
    logical :: ldiag, lplein
    integer ::  neq, nbloc, nblocm, iret
    integer :: jsmhc
    integer :: itbloc, ieq, ibloc, jualf, jvale, kterm, nbterm, ilig
    integer :: ismdi, ismdi0, ibloav, iscdi,   kblocm, nblocl
    integer, pointer :: scdi(:) => null()
    integer, pointer :: smde(:) => null()
    integer, pointer :: scib(:) => null()
    integer, pointer :: schc(:) => null()
    integer, pointer :: smdi(:) => null()
    character(len=24), pointer :: refa(:) => null()
    integer, pointer :: scde(:) => null()
!     ------------------------------------------------------------------
!
!
!
    call jemarq()
    matas=mataz
    base=basz
    if (base .eq. ' ') call jelira(matas//'.UALF', 'CLAS', cval=base)
!
!     -- .VALM NE DOIT PAS EXISTER :
    call jeexin(matas//'.VALM', iret)
    ASSERT(iret.eq.0)
!
    call jeveuo(matas//'.REFA', 'L', vk24=refa)
    nu=refa(2)(1:14)
    stomor=nu//'.SMOS'
    stolci=nu//'.SLCS'
!
    call jeveuo(stolci//'.SCDE', 'L', vi=scde)
    call jeveuo(stolci//'.SCDI', 'L', vi=scdi)
    call jeveuo(stolci//'.SCHC', 'L', vi=schc)
    call jeveuo(stolci//'.SCIB', 'L', vi=scib)
    neq=scde(1)
    nbloc= scde(3)
!
!     -- SI STOMOR N'EXISTE PAS, ON LE CREE :
    call jeexin(stomor//'.SMDI', iret)
    if (iret .eq. 0) then
!        -- ON NE SAIT TRAITER QUE LES MATRICES DIAGONALES OU PLEINES :
        ldiag=.true.
        lplein=.true.
        do 5,ieq=1,neq
        if (schc(ieq) .ne. 1) ldiag=.false.
        if (schc(ieq) .ne. ieq) lplein=.false.
 5      continue
        if (ldiag) then
            call crsmos(stomor, 'DIAG', neq)
        else
            if (lplein) then
                call crsmos(stomor, 'PLEIN', neq)
            else
                ASSERT(.false.)
            endif
        endif
    endif
!
    call jeveuo(stomor//'.SMDI', 'L', vi=smdi)
    call jeveuo(stomor//'.SMHC', 'L', jsmhc)
    call jeveuo(stomor//'.SMDE', 'L', vi=smde)
    itbloc= smde(2)
!
    call jelira(matas//'.UALF', 'NMAXOC', nblocl)
    ASSERT(nblocl.eq.nbloc .or. nblocl.eq.2*nbloc)
    nblocm=1
    if (nblocl .eq. 2*nbloc) nblocm=2
!
!     -- REEL OU COMPLEXE ?
    call jelira(matas//'.UALF', 'TYPE', cval=tyrc)
    ASSERT(tyrc.eq.'R' .or. tyrc.eq.'C')
!
!
!     1. ALLOCATION DE .VALM :
!     ----------------------------------------
    call jecrec(matas//'.VALM', base//' V '//tyrc, 'NU', 'DISPERSE', 'CONSTANT',&
                nblocm)
    call jeecra(matas//'.VALM', 'LONMAX', itbloc)
    do 3,kblocm=1,nblocm
    call jecroc(jexnum(matas//'.VALM', kblocm))
    3 end do
!
!
!     2. REMPLISSAGE DE .VALM :
!     ----------------------------------------
    do 10, kblocm=1,nblocm
    call jeveuo(jexnum(matas//'.VALM', kblocm), 'E', jvale)
    ibloav=0+nbloc*(kblocm-1)
    ismdi0=0
    do 1, ieq=1,neq
    iscdi=scdi(ieq)
    ibloc=scib(ieq)+nbloc*(kblocm-1)
!
!          -- ON RAMENE LE BLOC EN MEMOIRE SI NECESSAIRE:
    if (ibloc .ne. ibloav) then
        call jeveuo(jexnum(matas//'.UALF', ibloc), 'L', jualf)
        if (ibloav .ne. 0) then
            call jelibe(jexnum(matas//'.UALF', ibloav))
        endif
        ibloav=ibloc
    endif
!
    ismdi=smdi(ieq)
    nbterm=ismdi-ismdi0
!
    do 2, kterm=1,nbterm
    ilig=zi4(jsmhc-1+ismdi0+kterm)
    if (tyrc .eq. 'R') then
        zr(jvale-1+ismdi0+kterm)=zr(jualf-1+ iscdi +ilig-&
                    ieq)
    else
        zc(jvale-1+ismdi0+kterm)=zc(jualf-1+ iscdi +ilig-&
                    ieq)
    endif
 2  continue
    ASSERT(ilig.eq.ieq)
!
    ismdi0=ismdi
 1  continue
    10 end do
!
!
!
    call jedetr(matas//'.UALF')
!
    call jedema()
!     CALL CHEKSD('sd_matr_asse',MATAS,IRET)
end subroutine
