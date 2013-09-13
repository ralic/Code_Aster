subroutine caraff(noma, gran, base, cartz)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/r8vide.h"
#include "asterfort/alcart.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvc8.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
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
#include "asterfort/tecart.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
!
    character(len=1) :: base
    character(len=8) :: noma, gran
    character(len=*) :: cartz
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! BUT :
!  - TRAITER L'OPTION 'AFFE' DE LA COMMANDE CREA_CHAMP
!    POUR LES CARTES ET LES CHAM_ELEM (SAUF POUR VARI_R)
!  - CREER LA CARTE  (CARTZ)
!-----------------------------------------------------------------------
    integer :: gd, ibid, ied, nocc, ncmpmx, nbtou, n1, vali(2)
    integer :: iad, jncmp, jvalv, jmail, nbcmp, k, iocc, nbmail, nbvar
    real(kind=8) ::  rvid
    character(len=8) :: k8b, tsca, typmcl(2)
    character(len=16) :: motclf, motcls(2)
    character(len=19) :: carte
    character(len=24) :: mesmai
!     ------------------------------------------------------------------
    call jemarq()
!
    if (noma .eq. ' ') call u2mess('F', 'UTILITAI_10')
!
    if (gran .eq. 'VARI_R') call u2mess('F', 'UTILITAI_11')
!
    call dismoi('F', 'TYPE_SCA', gran, 'GRANDEUR', ibid,&
                tsca, ied)
!
    motclf = 'AFFE'
    call getfac(motclf, nocc)
!
    mesmai = '&&CARAFF.MES_MAILLES'
    motcls(1) = 'GROUP_MA'
    motcls(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
!     1- ALLOCATION DE LA CARTE
!     --------------------------------------------
    carte = cartz
    call alcart(base, carte, noma, gran)
    call jeveuo(carte//'.NCMP', 'E', jncmp)
    call jeveuo(carte//'.VALV', 'E', jvalv)
!
    call jenonu(jexnom('&CATA.GD.NOMGD', gran), gd)
    call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', iad)
!
    if (gran .eq. 'VAR2_R') then
        rvid=r8vide()
        nbcmp = ncmpmx
        do 10,k = 1,ncmpmx
        zk8(jncmp-1+k) = zk8(iad-1+k)
        zr(jvalv-1+k) = rvid
10      continue
        call nocart(carte, 1, ' ', 'NOM', 0,&
                    ' ', 0, ' ', nbcmp)
    endif
!
!     2- BOUCLE SUR LES OCCURENCES DU MOT CLE AFFE
!     --------------------------------------------
    do 30 iocc = 1, nocc
!
        call getvtx(motclf, 'NOEUD', iocc=iocc, nbval=0, nbret=n1)
        if (n1 .ne. 0) call u2mess('F', 'UTILITAI_12')
!
        call getvtx(motclf, 'GROUP_NO', iocc=iocc, nbval=0, nbret=n1)
        if (n1 .ne. 0) call u2mess('F', 'UTILITAI_13')
!
        call getvtx(motclf, 'NOM_CMP', iocc=iocc, nbval=0, nbret=nbcmp)
!
        if (tsca .eq. 'R') then
            call getvr8(motclf, 'VALE', iocc=iocc, nbval=0, nbret=nbvar)
        else if (tsca.eq.'I') then
            call getvis(motclf, 'VALE_I', iocc=iocc, nbval=0, nbret=nbvar)
        else if (tsca.eq.'C') then
            call getvc8(motclf, 'VALE_C', iocc=iocc, nbval=0, nbret=nbvar)
        else if (tsca.eq.'K8') then
            call getvid(motclf, 'VALE_F', iocc=iocc, nbval=0, nbret=nbvar)
        else
            call u2mesk('F', 'UTILITAI_14', 1, tsca)
        endif
!
!       TEST SUR LES DONNEES INTRODUITES
        if (nbvar .ne. nbcmp) then
            call u2mess('F', 'UTILITAI_15')
        else if (-nbvar.gt.ncmpmx) then
            vali(1)=-nbvar
            vali(2)=ncmpmx
            call u2mesi('F', 'UTILITAI_8', 2, vali)
        else
            nbcmp = -nbcmp
            nbvar = -nbvar
            call getvtx(motclf, 'NOM_CMP', iocc=iocc, nbval=nbcmp, vect=zk8(jncmp))
            if (tsca .eq. 'R') then
                call getvr8(motclf, 'VALE', iocc=iocc, nbval=nbvar, vect=zr(jvalv))
            else if (tsca.eq.'I') then
                call getvis(motclf, 'VALE_I', iocc=iocc, nbval=nbvar, vect=zi(jvalv))
            else if (tsca.eq.'C') then
                call getvc8(motclf, 'VALE_C', iocc=iocc, nbval=nbvar, vect=zc(jvalv))
            else if (tsca.eq.'K8') then
                call getvid(motclf, 'VALE_F', iocc=iocc, nbval=nbvar, vect=zk8(jvalv))
            endif
        endif
!
        call getvtx(motclf, 'TOUT', iocc=iocc, scal=k8b, nbret=nbtou)
        if (nbtou .ne. 0) then
            call nocart(carte, 1, ' ', 'NOM', 0,&
                        ' ', 0, ' ', nbcmp)
!
        else
            call reliem(' ', noma, 'NU_MAILLE', motclf, iocc,&
                        2, motcls, typmcl, mesmai, nbmail)
            if (nbmail .eq. 0) goto 30
            call jeveuo(mesmai, 'L', jmail)
            call nocart(carte, 3, ' ', 'NUM', nbmail,&
                        k8b, zi(jmail), ' ', nbcmp)
            call jedetr(mesmai)
        endif
30  end do
!
    call tecart(carte)
    call jedetr(carte//'.NCMP')
    call jedetr(carte//'.VALV')
!
    call jedema()
end subroutine
