subroutine giecas(nfic, ndim, nbobj)
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
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/giecma.h"
#include "asterfort/giinco.h"
#include "asterfort/jacopo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/uttrii.h"
#include "asterfort/wkvect.h"
    integer :: nfic, ndim, nbobj
! ----------------------------------------------------------------------
!     BUT: ECRIRE LE FICHIER DE MAILLAGE ASTER A PARTIR DES OBJETS
!          CREES PAR GILIRE ( '&&GILIRE.....')
!
!     IN : NFIC : UNITE D'ECRITURE
!          NDIM : DIMENSION DU PROBLEME (2D OU 3D)
!          NBOBJ: NOMBRE D'OBJETS (AU SENS GIBI)
!
! ----------------------------------------------------------------------
!
    integer :: vali
!     VARIABLES LOCALES:
    character(len=7) :: k7bid, k7nom(7)
    character(len=8) :: tymail, nomobj, nomno, k8nom(7), nomobg
    logical :: magoui, trouve, indir
!
    character(len=1) :: cbid
!-----------------------------------------------------------------------
    integer :: i, iacoor, iacuel, iadsob, ianema, ianoob, iaobno
    integer :: iaobnu, iaptin, iaptno, iaptnu, iasoob, iassob, icok
    integer :: icoma, iecrit, iecrma, ii, il, ima, imb
    integer :: ino, inu, inutri, iret, itot, itrnu, j
    integer :: jj, k, kk, l, nbelc, nbele, nbelim
    integer :: nbelt, nbfois, nbno, nbnono, nbnoto, nbobno, nbrest
    integer :: nbsoob, ncoo, nmelim, nono, numno
!-----------------------------------------------------------------------
    data cbid/' '/
!
!
!     -- ON INITIALISE LA COLLECTION QUI CONTIENT LES CORRESPONDANCES
!     -- ENTRE LES NUMEROTATIONS LOCALES DES NOEUDS GIBI ET ASTER:
    call jemarq()
    call giinco()
!
    call jeveuo('&&GILIRE.COORDO   ', 'L', iacoor)
    call jelira('&&GILIRE.COORDO   ', 'LONMAX', ncoo)
!
    call jeexin('&&GILIRE.NOMOBJ', iret)
    if (iret .eq. 0) then
        call utmess('F', 'PREPOST_46')
    endif
    call jeveuo('&&GILIRE.NOMOBJ', 'L', ianoob)
    call jeveuo('&&GILIRE.DESCOBJ', 'L', iadsob)
    call jeveuo('&&GILIRE.CUMUL_ELE', 'L', iacuel)
!
    call jeveuo('&&GILIRE.OBJET_NOM', 'L', iaobno)
    call jeveuo('&&GILIRE.OBJET_NUM', 'L', iaobnu)
!
    call jeveuo('&&GILIRE.NUMANEW', 'L', ianema)
!
!
!     -----------------------------------------------------------------
!     --ECRITURE DU TITRE:
!     -----------------------------------------------------------------
    write(nfic,*) 'TITRE'
    write(nfic,*) '%  GIBI FECIT'
    write(nfic,*) 'FINSF'
    write(nfic,*) '%'
!
!     -----------------------------------------------------------------
!     --ECRITURE DES NOEUDS:
!     -----------------------------------------------------------------
    if (ndim .eq. 3) then
        write(nfic,*) 'COOR_3D'
    else if (ndim.eq.2) then
        write(nfic,*) 'COOR_2D'
    else if (ndim.eq.1) then
        write(nfic,*) 'COOR_1D'
    else
        call utmess('F', 'PREPOST_53')
    endif
    indir =.false.
    call jeexin('&&GILIRE.INDIRECT', iret)
    if (iret .ne. 0) then
        indir =.true.
        call jelira('&&GILIRE.INDIRECT', 'LONMAX', nbnoto)
        call jeveuo('&&GILIRE.INDIRECT', 'L', iaptin)
        call wkvect('&&GILIRE.NOENOM', 'V V I', nbnoto, inutri)
        call jacopo(nbnoto, 'I', iaptin, inutri)
        call uttrii(zi(inutri), nbnoto)
        nbelim = (ncoo/ndim)-nbnoto
        if (nbelim .gt. 0) then
            vali = nbelim
            call utmess('I', 'PREPOST5_19', si=vali)
        endif
    else
        nbnoto=ncoo/ndim
    endif
!
    do 1, ino=1,nbnoto
    if (indir) then
        nono = zi(inutri-1+ino)
    else
        nono = ino
    endif
    call codent(nono, 'G', k7bid)
    write(nfic,1001) 'N'//k7bid, (zr(iacoor-1+ndim*(nono-1)+j),j=&
        1,ndim)
    1 end do
!
    write(nfic,*) 'FINSF'
    write(nfic,*) '%'
!
!     -----------------------------------------------------------------
!     --ECRITURE DES MAILLES:
!     -----------------------------------------------------------------
!
    call jelira('&&GILIRE.OBJET_NOM', 'LONMAX', nbobno)
    call wkvect('&&GILIRE.OBJTRI_NUM', 'V V I', nbobj, itrnu)
    call wkvect('&&GILIRE.ECRIGRM', 'V V L', nbobj, iecrit)
!
    call jeveuo('&&GILIRE.NUMANEW', 'L', ianema)
    call jelira('&&GILIRE.NUMANEW', 'LONUTI', itot)
!
!   CALCUL DU NB TOT D'ELEMENTS
    call wkvect('&&GILIRE.ECRMAIL', 'V V L', itot, iecrma)
!
!
    do 18 il = 1, nbobj
        zl(iecrit+il-1)=.false.
18  end do
    imb = 1
    do 14 ima = 1, nbobno
        ii = zi(iaobnu+ima-1)
        if (.not.(zl(iecrit+ii-1))) then
            zi(itrnu+imb-1) = ii
            imb =imb + 1
            zl(iecrit+ii-1)=.true.
        endif
14  end do
!
    do 15 i = 1, nbobno
        ii = zi(iaobnu+i-1)
        nbsoob = zi(iadsob-1+4*(ii-1)+1)
        nomobj=zk8(ianoob-1+2*(ii-1)+1)
        if (nbsoob .ne. 0) then
            call jeveuo('&&GILIRE'//nomobj//'.SOUSOB', 'L', iasoob)
            do 16 kk = 1, nbsoob
                jj = zi(iasoob+kk-1)
                if (.not.(zl(iecrit+jj-1))) then
                    zi(itrnu+imb-1)= jj
                    zl(iecrit+jj-1)=.true.
                    imb =imb + 1
                endif
16          continue
        endif
15  continue
!
! ON SUPPRIME UN IMB CAR ON EN COMPTE UN DE PLUS DANS LA FIN DE BOUCLE
!
    imb =imb-1
!
! ON TRIE LA TABLE
!
    if (imb .gt. 1) then
        call uttrii(zi(itrnu), imb)
    endif
!
    icoma = 0
    nbelt = 0
    nbelc = 0
!
    do 2, i=1,nbobj
    trouve =.false.
    do 12 jj = 1, imb
        ii = zi(itrnu+jj-1)
        if (i .eq. ii) then
            trouve = .true.
            goto 13
        endif
12  continue
13  continue
!
    nbno =zi(iadsob-1+4*(i-1)+3)
    nbele =zi(iadsob-1+4*(i-1)+4)
    nomobj =zk8(ianoob-1+2*(i-1)+1)
    tymail =zk8(ianoob-1+2*(i-1)+2)
    nbelt = nbelt+nbele
    if (trouve) nbelc = nbelc +nbele
!
!        -- SI L'OBJET EST 1 OBJET SIMPLE , ON ECRIT SES MAILLES:
    if (nbele .gt. 0) then
        call giecma(nfic, trouve, nbele, nomobj, tymail,&
                    nbno, zl( iecrma), icoma)
    endif
    2 end do
    if (nbelc .gt. 9999999) then
        vali = nbelc
        call utmess('F', 'PREPOST6_2', si=vali)
    endif
    nmelim = nbelt - nbelc
    if (nmelim .gt. 0) then
        vali = nmelim
        call utmess('I', 'PREPOST5_20', si=vali)
    endif
!
!     -----------------------------------------------------------------
!     --ECRITURE DES GROUP_NO:
!     -----------------------------------------------------------------
!
    call jeexin('&&GILIRE.POINT_NOM', iret)
    if (iret .gt. 0) then
        call jeveuo('&&GILIRE.POINT_NOM', 'L', iaptno)
        call jeveuo('&&GILIRE.POINT_NUM', 'L', iaptnu)
        call jelira('&&GILIRE.POINT_NOM', 'LONMAX', nbnono)
    else
        nbnono=0
    endif
!
    do 3, i=1,nbnono
    nomno =zk8(iaptno-1+i)
    if (nomno(1:1) .eq. '#') goto 3
    numno =zi (iaptnu-1+i)
    call codent(numno, 'G', k7bid)
    write(nfic,*) 'GROUP_NO'
    write(nfic,1002) nomno,'N'//k7bid
    write(nfic,*) 'FINSF'
    write(nfic,*) '%'
    3 end do
!
!     -----------------------------------------------------------------
!     --ECRITURE DES GROUP_MA:
!     -----------------------------------------------------------------
!
    call jelira('&&GILIRE.OBJET_NOM', 'LONMAX', nbobno)
    do 4, ii=1,nbobj
    trouve =.false.
    do 21 inu = 1, nbobno
        if (zi(iaobnu-1+inu) .eq. ii) then
            trouve = .true.
            nomobg=zk8(iaobno-1+inu)
            if (nomobg(1:1) .eq. '#') goto 21
            write(nfic,*) 'GROUP_MA'
            write(nfic,*) '  ',nomobg
            nbsoob =zi(iadsob-1+4*(ii-1)+1)
            if (nbsoob .eq. 0) then
!
!           -- ON FAIT COMME SI L'OBJET SE CONTENAIT LUI-MEME:
                nbsoob=1
                magoui=.true.
            else
                magoui=.false.
                nomobj=zk8(ianoob-1+2*(ii-1)+1)
                call jeveuo('&&GILIRE'//nomobj//'.SOUSOB', 'L', iassob)
            endif
            do 5,j=1,nbsoob
!
!        -- L'OBJET EST 1 OBJET COMPOSE, ON ECRIT SES MAILLES:
            if (magoui) then
                jj= ii
            else
                jj= zi(iassob-1+j)
            endif
            nomobj=zk8(ianoob-1+2*(jj-1)+1)
            nbno =zi(iadsob-1+4*(jj-1)+3)
            nbele =zi(iadsob-1+4*(jj-1)+4)
            nbfois = nbele/7
            nbrest= nbele-7*nbfois
            icok= zi(iacuel-1+jj)
!
            do 6, k=1,nbfois
            do 7, kk=1,7
            icok=icok+1
            call codent(zi(ianema-1+icok), 'G', k7nom( kk))
            k8nom(kk)='M'//k7nom(kk)
 7          continue
            write(nfic,1003) (k8nom(l),l=1,7)
 6          continue
!
            do 8, kk=1,nbrest
            icok=icok+1
            call codent(zi(ianema-1+icok), 'G', k7nom(kk))
            k8nom(kk)='M'//k7nom(kk)
 8          continue
            write(nfic,1003) (k8nom(l),l=1,nbrest)
!
 5          continue
            write(nfic,*) 'FINSF'
            write(nfic,*) '%'
        endif
21  continue
!
    4 end do
!
!     -- ON ECRIT LE "FIN" FINAL ET ON REMBOBINE LE FICHIER:
!     ------------------------------------------------------
    write(nfic,*) 'FIN'
    rewind(nfic)
!
    call jedetc('V', '&&GILIRE', 1)
    1001 format(1x,a8,1x,1pd21.14,1x,1pd21.14,1x,1pd21.14)
    1002 format(1x,a8,1x,a8)
    1003 format(7(1x,a8))
!
    call jedema()
end subroutine
