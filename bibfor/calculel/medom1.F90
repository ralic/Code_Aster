subroutine medom1(modele, mate, cara, kcha, ncha,&
                  ctyp, result, nuord)
    implicit none
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rcmfmc.h"
#include "asterfort/rslesd.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: ncha, nuord
    character(len=4) :: ctyp
    character(len=8) :: modele, cara, result
    character(len=24) :: mate
    character(len=19) :: kcha
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     SAISIE ET VERIFICATION DE LA COHERENCE DES DONNEES MECANIQUES
!     DU PROBLEME
!
! ----------------------------------------------------------------------
! OUT : MODELE : NOM DU MODELE
! OUT : MATE   : CHAMP MATERIAU
! OUT : CARA   : NOM DU CHAMP DE CARACTERISTIQUES
! IN  : KCHA   : NOM JEVEUX POUR STOCKER LES CHARGES
! OUT : NCHA   : NOMBRE DE CHARGES
! OUT : CTYP   : TYPE DE CHARGE
! IN  : RESULT : NOM DE LA SD RESULTAT
! IN  : NUORD  : NUMERO D'ORDRE
! ----------------------------------------------------------------------
    integer :: iret
    integer :: iexcit, ierd, i, ibid, icha, ie, ikf, in
    integer :: jfcha, jinfc, jlcha, n, n1, n2, n3, n5
!-----------------------------------------------------------------------
    character(len=8) :: k8b, nomo, materi
    character(len=8) :: blan8
    character(len=16) :: concep, nomcmd, phenom
    character(len=19) :: excit
    integer :: iarg
!
    call jemarq()
!              12345678
    blan8 = '        '
    ncha = 0
    ctyp = ' '
    modele = ' '
    cara = ' '
    materi = ' '
    nomo = blan8
    iexcit = 1
    n1=0
!
    call getres(k8b, concep, nomcmd)
!
    if ((nomcmd.eq.'CALC_CHAMP' ) .or. (nomcmd.eq.'CALC_ERREUR' ) .or.&
        (nomcmd.eq.'CALC_META  ' ) .or. (nomcmd.eq.'POST_ELEM' ) .or. (nomcmd.eq.'CALC_G' )) then
!
!        RECUPERATION DU MODELE, MATERIAU, CARA_ELEM et EXCIT
!        POUR LE NUMERO d'ORDRE NUORD
!
        call rslesd(result, nuord, modele, materi, cara,&
                    excit, iexcit)
        if (materi .ne. blan8) then
            call rcmfmc(materi, mate)
        else
            mate = ' '
        endif
    else
!
        call getvid(' ', 'MODELE', scal=modele, nbret=n1)
        call getvid(' ', 'CARA_ELEM', scal=cara, nbret=n2)
        call dismoi('F', 'EXI_RDM', modele, 'MODELE', ibid,&
                    k8b, ie)
        if ((n2.eq.0) .and. (k8b(1:3).eq.'OUI')) call u2mess('A', 'CALCULEL3_39')
!
        call getvid(' ', 'CHAM_MATER', scal=materi, nbret=n3)
        call dismoi('F', 'BESOIN_MATER', modele, 'MODELE', ibid,&
                    k8b, ie)
        if ((n3.eq.0) .and. (k8b(1:3).eq.'OUI')) call u2mess('A', 'CALCULEL3_40')
!
        if (n3 .ne. 0) then
            call rcmfmc(materi, mate)
        else
            mate = ' '
        endif
    endif
!
!     TRAITEMENT DU CHARGEMENT
!
!     SI IEXCIT=1 ON PREND LE CHARGEMENT DONNE PAR L'UTILISATEUR
    if (iexcit .eq. 1) then
        if (getexm('EXCIT',' ') .eq. 0) then
            n5 = 0
        else
            call getfac('EXCIT', n5)
        endif
!
        if (n5 .ne. 0) then
            ncha = n5
            call jeexin(kcha//'.LCHA', iret)
            if (iret .ne. 0) then
                call jedetr(kcha//'.LCHA')
                call jedetr(kcha//'.FCHA')
            endif
            call wkvect(kcha//'.LCHA', 'V V K8', n5, icha)
            call wkvect(kcha//'.FCHA', 'V V K8', n5, ikf)
            do 20 iexcit = 1, n5
                call getvid('EXCIT', 'CHARGE', iocc=iexcit, scal=zk8(icha+ iexcit-1), nbret=n)
                call getvid('EXCIT', 'FONC_MULT', iocc=iexcit, scal=k8b, nbret=n)
                if (n .ne. 0) then
                    zk8(ikf+iexcit-1) = k8b
                endif
20          continue
        else
            call jeexin(kcha//'.LCHA', iret)
            if (iret .ne. 0) then
                call jedetr(kcha//'.LCHA')
                call jedetr(kcha//'.FCHA')
            endif
            call wkvect(kcha//'.LCHA', 'V V K8', 1, icha)
            call wkvect(kcha//'.FCHA', 'V V K8', 1, ikf)
        endif
!
        if (ncha .gt. 0) then
!           VERIFICATION QUE LES CHARGES PORTENT SUR LE MEME MODELE.
            call dismoi('F', 'NOM_MODELE', zk8(icha), 'CHARGE', ibid,&
                        nomo, ie)
            do 30 i = 1, ncha
                call dismoi('F', 'NOM_MODELE', zk8(icha-1+i), 'CHARGE', ibid,&
                            k8b, ie)
                if (k8b .ne. nomo) then
                    call u2mess('F', 'CALCULEL3_41')
                endif
30          continue
!           VERIFICATION QUE LES CHARGES PORTENT SUR LE MODELE
            if (n1 .ne. 0 .and. modele .ne. nomo) then
                call u2mess('F', 'CALCULEL3_42')
            endif
        endif
!
!     SI IEXCIT=0 ON PREND LE CHARGEMENT PRESENT DANS LA SD
!
    else
!
        call jeveuo(excit//'.INFC', 'L', jinfc)
        call jeveuo(excit//'.LCHA', 'L', jlcha)
        call jeveuo(excit//'.FCHA', 'L', jfcha)
        ncha=zi(jinfc)
!
        call jeexin(kcha//'.LCHA', iret)
        if (iret .ne. 0) then
            call jedetr(kcha//'.LCHA')
            call jedetr(kcha//'.FCHA')
        endif
        call wkvect(kcha//'.LCHA', 'V V K8', ncha, icha)
        call wkvect(kcha//'.FCHA', 'V V K8', ncha, ikf)
        call dismoi('F', 'PHENOMENE', modele, 'MODELE', ibid,&
                    phenom, ierd)
        ctyp=phenom(1:4)
        in=0
        do 50 i = 1, ncha
!           ON STOCKE LES CHARGES DONT LE TYPE CORRESPOND A CTYP
            call dismoi('C', 'TYPE_CHARGE', zk24(jlcha+i-1), 'CHARGE', ibid,&
                        k8b, ie)
            if ((ie.eq.0) .and. (ctyp.eq.k8b(1:4))) then
                zk8(icha+in)= zk24(jlcha+i-1)(1:8)
                zk8(ikf+in) = zk24(jfcha+i-1)(1:8)
                in=in+1
            endif
50      continue
        ncha=in
!
    endif
!
    call jedema()
end subroutine
