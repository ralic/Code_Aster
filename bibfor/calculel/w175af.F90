subroutine w175af(modele, chfer1)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
    character(len=8) :: modele
    character(len=19) :: chfer1
!-----------------------------------------------------------------------
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
! ======================================================================
! BUT : CREER LE CHAMP DE DONNEES POUR CALC_FERRAILLAGE
!
!-----------------------------------------------------------------------
    integer :: gd, ibid, nocc, ncmpmx, nbtou, iret
    integer :: n1, n2, n3, n4, n5, n6, n7
    integer :: jncmp, jvalv, jmail, iocc, nbmail
    real(kind=8) :: valr
    character(len=8) :: k8b, typmcl(2), noma, typcb
    character(len=16) :: motcls(2)
    character(len=24) :: mesmai
!     ------------------------------------------------------------------
    call jemarq()
!
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                noma, iret)
    ASSERT(noma.ne.' ')
!
    call getfac('AFFE', nocc)
!
    mesmai = '&&W175AF.MES_MAILLES'
    motcls(1) = 'GROUP_MA'
    motcls(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
!     1- ALLOCATION DU CHAMP CHFER1 (CARTE)
!     --------------------------------------------
    call alcart('V', chfer1, noma, 'FER1_R')
    call jeveuo(chfer1//'.NCMP', 'E', jncmp)
    call jeveuo(chfer1//'.VALV', 'E', jvalv)
!
    call jenonu(jexnom('&CATA.GD.NOMGD', 'FER1_R'), gd)
    call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx)
!
    ASSERT(ncmpmx.eq.8)
    zk8(jncmp-1+1)='TYPCOMB'
    zk8(jncmp-1+2)='ENROBG'
    zk8(jncmp-1+3)='CEQUI'
    zk8(jncmp-1+4)='SIGACI'
    zk8(jncmp-1+5)='SIGBET'
    zk8(jncmp-1+6)='PIVA'
    zk8(jncmp-1+7)='PIVB'
    zk8(jncmp-1+8)='ES'
!
!     2. MOTS CLES GLOBAUX :
!     ----------------------
!     2.1 TYPE_COMB :
    call getvtx(' ', 'TYPE_COMB', scal=typcb, nbret=n1)
    ASSERT(typcb.eq.'ELU'.or.typcb.eq.'ELS')
    if (typcb .eq. 'ELU') valr=0.d0
    if (typcb .eq. 'ELS') valr=1.d0
    zr(jvalv-1+1)=valr
!
!
!     3- BOUCLE SUR LES OCCURENCES DU MOT CLE AFFE
!     --------------------------------------------
    do 30 iocc = 1, nocc
!
        call getvr8('AFFE', 'ENROBG', iocc=iocc, scal=zr(jvalv-1+2), nbret=n1)
        call getvr8('AFFE', 'CEQUI', iocc=iocc, scal=zr(jvalv-1+3), nbret=n2)
        call getvr8('AFFE', 'SIGM_ACIER', iocc=iocc, scal=zr(jvalv-1+4), nbret=n3)
        call getvr8('AFFE', 'SIGM_BETON', iocc=iocc, scal=zr(jvalv-1+5), nbret=n4)
        call getvr8('AFFE', 'PIVA', iocc=iocc, scal=zr(jvalv-1+6), nbret=n5)
        call getvr8('AFFE', 'PIVB', iocc=iocc, scal=zr(jvalv-1+7), nbret=n6)
        call getvr8('AFFE', 'ES', iocc=iocc, scal=zr(jvalv-1+8), nbret=n7)
!
        if (typcb .eq. 'ELU') then
            if (n5 .eq. 0 .or. n6 .eq. 0) then
                call utmess('F', 'CALCULEL_73')
            endif
            if (n7 .eq. 0) then
                call utmess('F', 'CALCULEL_73')
            endif
            if (zr(jvalv-1+8) .le. 0) then
                call utmess('F', 'CALCULEL_74', sr=zr(jvalv-1+8))
            endif
        else
            if (n2 .eq. 0) then
                call utmess('F', 'CALCULEL_73')
            endif
        endif
!
        call getvtx('AFFE', 'TOUT', iocc=iocc, scal=k8b, nbret=nbtou)
        if (nbtou .ne. 0) then
            call nocart(chfer1, 1, ncmpmx)
!
        else
            call reliem(' ', noma, 'NU_MAILLE', 'AFFE', iocc,&
                        2, motcls, typmcl, mesmai, nbmail)
            call jeveuo(mesmai, 'L', jmail)
            call nocart(chfer1, 3, ncmpmx, mode='NUM', nma=nbmail,&
                        limanu=zi(jmail))
            call jedetr(mesmai)
        endif
30  continue
!
    call jedetr(chfer1//'.NCMP')
    call jedetr(chfer1//'.VALV')
!
    call jedema()
end subroutine
