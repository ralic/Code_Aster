subroutine op0005()
    implicit   none
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
! person_in_charge: j-pierre.lefebvre at edf.fr
!
!     LECTURE DE LA COMMANDE DEFI_MATERIAU
!     STOCKAGE DANS UN OBJET DE TYPE MATERIAU NOUVELLE FORMULE
!
!
! ----------------------------------------------------------------------
!
!
!
#include "jeveux.h"
#include "asterc/getmat.h"
#include "asterc/getmjm.h"
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterfort/aniver.h"
#include "asterfort/indk16.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupc.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/rcstoc.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    integer :: nmocl
    parameter         (nmocl=200)
    character(len=16) :: motcle(nmocl)
    integer :: nbobm, jnbobj, niv, n1
    integer :: jtypfo, irc, jvalrm, jvalcm, jvalkm, jnomrc
    integer :: ind, ifm, i, k, nbrcme
    integer :: nbr, nbc, nbk, nbk2
    integer :: nbmati, jnorci, krc
    character(len=8) :: k8b, matout, matin, schout
    character(len=16) :: nomrc, typmat, materi, k16bid
    character(len=19) :: noobrc
    character(len=24) :: valk(4)
    character(len=1) :: k1bid
    integer :: iarg
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call getres(matout, typmat, materi)
!
    nbrcme = 0
    call getmat(nbrcme, motcle)
!
    if (nbrcme .le. 0) then
        call u2mesk('F', 'MODELISA10_16', 0, valk)
    endif
!
    matin = ' '
    nbmati = 0
    call getvid(' ', 'MATER', 1, iarg, 1,&
                matin, n1)
    if (n1 .ne. 0) then
!
! ------ ON VERIFIE QUE L'ON A QUE DES NOUVEAUX MATERIAUX
!
        call jeveuo(matin//'.MATERIAU.NOMRC', 'L', jnorci)
        call jelira(matin//'.MATERIAU.NOMRC', 'LONMAX', nbmati, k8b)
        do 10 irc = 1, nbrcme
            nomrc = motcle(irc)
            ind = indk16 ( zk16(jnorci), nomrc, 1, nbmati )
            if (ind .ne. 0) then
                valk(1)=matin
                valk(2)=nomrc
                call u2mesk('F', 'MODELISA9_9', 2, valk)
            endif
10      continue
!
! ------ ON COPIE TOUT SUR LA VOLATILE
!
        schout = '&&OP0005'
        call jedupc('G', matin, 1, 'V', schout,&
                    .false.)
        call jeveuo(schout//'.MATERIAU.NOMRC', 'L', jnorci)
        if (matout .eq. matin) call jedetc('G', matin, 1)
    endif
!
! --- ON DUPLIQUE MATIN DANS MATOUT
!
    if (nbmati .ne. 0) then
        call jedupc('V', schout, 1, 'G', matout,&
                    .false.)
        call jedetr(matout//'.MATERIAU.NOMRC')
    endif
!
    call wkvect(matout//'.MATERIAU.NOMRC', 'G V K16', nbrcme+nbmati, jnomrc)
!
    do irc = 1, nbmati
        zk16(jnomrc+irc-1) = zk16(jnorci+irc-1)
    end do
!
    call wkvect('&&OP0005.NBOBJE', 'V V I', nbrcme, jnbobj)
    call wkvect('&&OP0005.TYPFON', 'V V L', nbrcme, jtypfo)
!
    krc = nbmati
    do irc = 1, nbrcme
        nomrc = motcle(irc)
        ind = index(nomrc,'_FO')
        if (ind .gt. 0) then
            nomrc(ind:ind+2) = '   '
            zl(jtypfo+irc-1) = .true.
        else
            zl(jtypfo+irc-1) = .false.
        endif
        krc = krc + 1
        zk16(jnomrc+krc-1) = nomrc
        noobrc=matout//'.'//nomrc(1:10)
!
        if (zl(jtypfo+irc-1)) then
            ind=lxlgut(nomrc)+1
            nomrc(ind:ind+2) = '_FO'
        endif
        call getmjm(nomrc, 1, 0, k16bid, k16bid,&
                    nbobm)
        nbobm = - nbobm
        if ( nbobm .eq. 0 ) then
          call u2mesk('F','MODELISA9_80',1,nomrc)
        endif
!
        if (nomrc .eq. 'THER_NL') nbobm = nbobm + 1
        call wkvect(noobrc//'.VALR', 'G V R', nbobm, jvalrm)
        call wkvect(noobrc//'.VALC', 'G V C', nbobm, jvalcm)
        call wkvect(noobrc//'.VALK', 'G V K8', 2*nbobm, jvalkm)
!
        call rcstoc(matout, nomrc, nbobm, zr(jvalrm), zc(jvalcm),&
                    zk8( jvalkm), nbr, nbc, nbk)
        call jeecra(noobrc//'.VALR', 'LONUTI', nbr, ' ')
        call jeecra(noobrc//'.VALC', 'LONUTI', nbc, ' ')
        call jeecra(noobrc//'.VALK', 'LONUTI', nbr+nbc+2*nbk, ' ')
    end do
!
    call infmaj()
    call infniv(ifm, niv)
!
    write (ifm,'(1X)')
    write (ifm,'(1X,2A)') 'MATERIAU : ', matout
    call jeveuo(matout//'.MATERIAU.NOMRC', 'L', jnomrc)
    write (ifm,'(1X,A,A16)') 'RELATION DE COMPORTEMENT: ',zk16(jnomrc)
    write (ifm,'(27X,A16)') (zk16(jnomrc+k-1),k=2,nbrcme)
    write (ifm,'(1X)')
!
    if (niv .eq. 2) then
        do 200 k = 1, nbrcme
            noobrc=matout//'.'//zk16(jnomrc+k-1)(1:10)
            call jeveuo(noobrc//'.VALR', 'L', jvalrm)
            call jeveuo(noobrc//'.VALC', 'L', jvalcm)
            call jeveuo(noobrc//'.VALK', 'L', jvalkm)
            call jelira(noobrc//'.VALR', 'LONUTI', nbr, k1bid)
            call jelira(noobrc//'.VALC', 'LONUTI', nbc, k1bid)
            call jelira(noobrc//'.VALK', 'LONUTI', nbk2, k1bid)
            nbk=(nbk2-nbr-nbc)/2
            write(ifm,'(1X,2A)') 'PARAMETRES DE LA RELATION : ',zk16(&
            jnomrc+k-1)
            write(ifm,'(5(3X,A8,5X))') (zk8(jvalkm-1+i),i=1,nbr)
            write(ifm,'(5(3X,G13.6))') (zr (jvalrm-1+i),i=1,nbr)
            write(ifm,'(5(3X,A8,5X))') (zk8(jvalkm-1+i),i=nbr+1,nbr+&
            nbc)
            write(ifm,'(5(3X,2G13.6))')(zc (jvalcm-1+i),i=1,nbc)
            write(ifm,'(5(3X,A8,5X))') (zk8(jvalkm-1+i), i = nbr+nbc+&
            1, nbr+nbc+nbk)
            write(ifm,'(5(3X,A8,5X))') (zk8(jvalkm-1+i), i = nbr+nbc+&
            nbk+1, nbr+nbc+2*nbk)
            write(ifm,'(1X)')
200      continue
    endif
!
    call aniver(matout)
!
    call jedema()
end subroutine
