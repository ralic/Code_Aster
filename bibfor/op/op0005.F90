subroutine op0005()
    implicit none
! ----------------------------------------------------------------------
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
#include "asterfort/aniver.h"
#include "asterfort/codent.h"
#include "asterfort/deprecated_material.h"
#include "asterfort/getvid.h"
#include "asterfort/indk32.h"
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
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nbobm, jnbobj, niv, n1
    integer :: jtypfo, irc, jvalrm, jvalcm, jvalkm, jnomrc
    integer :: ind, ifm, i, k, nbrcme
    integer :: nbr, nbc, nbk, nbk2
    integer :: nbmati, jnorci, krc
    character(len=6) :: nom
    character(len=8) :: matout, matin, schout
    character(len=16) :: typmat, materi, k16bid
    character(len=19) :: noobrc
    character(len=24) :: valk(4)
    character(len=32) :: nomrc
    character(len=32), allocatable :: motcle(:)
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call getres(matout, typmat, materi)
!
    nbrcme = -1
    call getmat(nbrcme, k16bid)
!
    if (nbrcme .eq. 0) then
        call utmess('F', 'MODELISA10_16')
    else
        allocate(motcle(nbrcme))
        call getmat(nbrcme, motcle)
    endif
!
    matin = ' '
    nbmati = 0
    call getvid(' ', 'MATER', scal=matin, nbret=n1)
    if (n1 .ne. 0) then
!
! ------ ON VERIFIE QUE L'ON A QUE DES NOUVEAUX MATERIAUX
!
        call jeveuo(matin//'.MATERIAU.NOMRC', 'L', jnorci)
        call jelira(matin//'.MATERIAU.NOMRC', 'LONMAX', nbmati)
        do 10 irc = 1, nbrcme
            nomrc = motcle(irc)
            ind = indk32 ( zk32(jnorci), nomrc, 1, nbmati )
            if (ind .ne. 0) then
                valk(1)=matin
                valk(2)=nomrc
                call utmess('F', 'MODELISA9_9', nk=2, valk=valk)
            endif
10      continue
!
! ------ ON COPIE TOUT SUR LA VOLATILE
!
        schout = '&&OP0005'
        call jedupc('G', matin, 1, 'V', schout,&
                    .false._1)
        call jeveuo(schout//'.MATERIAU.NOMRC', 'L', jnorci)
        if (matout .eq. matin) call jedetc('G', matin, 1)
    endif
!
! --- ON DUPLIQUE MATIN DANS MATOUT
!
    if (nbmati .ne. 0) then
        call jedupc('V', schout, 1, 'G', matout,&
                    .false._1)
        call jedetr(matout//'.MATERIAU.NOMRC')
    endif
!
    call wkvect(matout//'.MATERIAU.NOMRC', 'G V K32', nbrcme+nbmati, jnomrc)
!
    do irc = 1, nbmati
        zk32(jnomrc+irc-1) = zk32(jnorci+irc-1)
    end do
!
    call wkvect('&&OP0005.NBOBJE', 'V V I', nbrcme, jnbobj)
    call wkvect('&&OP0005.TYPFON', 'V V L', nbrcme, jtypfo)
!
    krc = nbmati
    do irc = 1, nbrcme
        nomrc = motcle(irc)
        call deprecated_material(nomrc)
        ind = index(nomrc,'_FO')
        if (ind .gt. 0) then
            nomrc(ind:ind+2) = '   '
            zl(jtypfo+irc-1) = .true.
        else
            zl(jtypfo+irc-1) = .false.
        endif
        krc = krc + 1
        zk32(jnomrc+krc-1) = nomrc
        call codent(krc, 'D0', nom)
        noobrc=matout//'.CPT.'//nom
!
        ind=lxlgut(nomrc)+1
        if (ind .gt. 32) then
            call utmess('F','MODELISA9_83', sk=nomrc)
        endif
        if (zl(jtypfo+irc-1)) then
            nomrc(ind:ind+2) = '_FO'
        endif
        call getmjm(nomrc, 1, 0, k16bid, k16bid, nbobm)
        nbobm = - nbobm
        if (nbobm .eq. 0) then
            call utmess('F', 'MODELISA9_80', sk=nomrc)
        endif
!
        if (nomrc .eq. 'THER_NL') nbobm = nbobm + 1

!       -- les objets .VALR, .VALC et .VALK sont sur-dimensionnes :
        call wkvect(noobrc//'.VALR', 'G V R', nbobm, jvalrm)
        call wkvect(noobrc//'.VALC', 'G V C', nbobm, jvalcm)
        call wkvect(noobrc//'.VALK', 'G V K16', 2*nbobm, jvalkm)
!
        call rcstoc(matout, nomrc, noobrc, nbobm, zr(jvalrm), zc(jvalcm),&
                    zk16(jvalkm), nbr, nbc, nbk)
        call jeecra(noobrc//'.VALR', 'LONUTI', nbr)
        call jeecra(noobrc//'.VALC', 'LONUTI', nbc)
        call jeecra(noobrc//'.VALK', 'LONUTI', nbr+nbc+2*nbk)
    end do
!
    call infmaj()
    call infniv(ifm, niv)
!
    write (ifm,'(1X)')
    write (ifm,'(1X,2A)') 'MATERIAU : ', matout
    call jeveuo(matout//'.MATERIAU.NOMRC', 'L', jnomrc)
    write (ifm,'(1X,A,A32)') 'RELATION DE COMPORTEMENT: ',zk32(jnomrc)
    write (ifm,'(27X,A32)') (zk32(jnomrc+k-1),k=2,nbrcme)
    write (ifm,'(1X)')
!   niv=2
    if (niv .eq. 2) then
        do 200 k = 1, nbrcme
            call codent(k, 'D0', nom)
            noobrc=matout//'.CPT.'//nom
            call jeveuo(noobrc//'.VALR', 'L', jvalrm)
            call jeveuo(noobrc//'.VALC', 'L', jvalcm)
            call jeveuo(noobrc//'.VALK', 'L', jvalkm)
            call jelira(noobrc//'.VALR', 'LONUTI', nbr)
            call jelira(noobrc//'.VALC', 'LONUTI', nbc)
            call jelira(noobrc//'.VALK', 'LONUTI', nbk2)
            nbk=(nbk2-nbr-nbc)/2
            write(ifm,'(1X,2A)') 'PARAMETRES DE LA RELATION : ',zk32(jnomrc+k-1)
            write(ifm,'(5(3X,A16,5X))') (zk16(jvalkm-1+i),i=1,nbr)
            write(ifm,'(5(3X,G13.6))')  (zr(jvalrm-1+i),i=1,nbr)
            write(ifm,'(5(3X,A16,5X))') (zk16(jvalkm-1+i),i=nbr+1,nbr+nbc)
            write(ifm,'(5(3X,2G13.6))') (zc(jvalcm-1+i),i=nbr+1,nbr+nbc)
            write(ifm,'(5(3X,A16,5X))') (zk16(jvalkm-1+i),i=nbr+nbc+1,nbr+nbc+nbk)
            write(ifm,'(5(3X,A16,5X))') (zk16(jvalkm-1+i),i=nbr+nbc+nbk+1,nbr+nbc+2*nbk)
            write(ifm,'(1X)')
200      continue
    endif
!
    call aniver(matout)
!
    deallocate(motcle)

    call jedema()
end subroutine
