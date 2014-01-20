subroutine irgags(ncmpmx, nomcmp, nomsym, nbchs, nomchs,&
                  nbcmps, nomgds, ipcmps)
! aslint: disable=W1501
    implicit none
!
#include "jeveux.h"
#include "asterfort/jedetr.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: ncmpmx, nbchs, nbcmps(*), ipcmps(*)
    character(len=*) :: nomcmp(*), nomsym, nomchs(*), nomgds(*)
!
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
!--------------------------------------------------------------------
!       RECHERCHE DES GRANDEURS SUPERTAB PRESENTENT DANS UNE GRANDEUR
!       ASTER
!      ENTREE:
!         NCMPMX: NOMBRE MAXI DE CMP DE LA GRANDEUR NOMGD
!         NOMCMP: NOMS DES CMP
!         NOMSYM: NOM SYMBOLIQUE
!      SORTIE:
!         NBCHS : NOMBRE DE GRANDEURS SUPERTAB
!         NOMCHS: NOM DE L'INFORMATION SUPERTAB
!         NBCMPS: NOMBRE DE COMPOSANTES DE CHAQUE GRANDEUR SUPERTAB
!         NOMGDS: NOM DES GRANDEURS SUPERTAB
!         IPCMPS: POSITION DES COMPOSANTES SUPERTAB DANS LA COMPOSANTE
!                 ASTER
!
!     ------------------------------------------------------------------
    integer :: nbdepl, nbtemp, nbtemi, nbtems, nbvari
    integer :: nbsigm, nbsigi, nbsigs, nbpres, nbepsm, nbepsi, nbepss
    integer :: nbpre1, nbpre2
    integer :: nbflu, nbflui, nbflus
    integer :: nbfh11, nbfh12, nbfh21, nbfh22, nbfht, nbsip
!
!-----------------------------------------------------------------------
    integer :: i, icmas,  ient
    integer ::      iflu
    integer ::     ires
    integer ::      iva, ivar
    integer ::  j
    integer, pointer :: videpl(:) => null()
    integer, pointer :: viepsi(:) => null()
    integer, pointer :: viepsm(:) => null()
    integer, pointer :: viepss(:) => null()
    integer, pointer :: vifh11(:) => null()
    integer, pointer :: vifh12(:) => null()
    integer, pointer :: vifh21(:) => null()
    integer, pointer :: vifh22(:) => null()
    integer, pointer :: vifht(:) => null()
    integer, pointer :: viflui(:) => null()
    integer, pointer :: viflus(:) => null()
    integer, pointer :: vipre1(:) => null()
    integer, pointer :: vipre2(:) => null()
    integer, pointer :: vipres(:) => null()
    integer, pointer :: visigi(:) => null()
    integer, pointer :: visigm(:) => null()
    integer, pointer :: visigs(:) => null()
    integer, pointer :: visip(:) => null()
    integer, pointer :: vitemi(:) => null()
    integer, pointer :: vitemp(:) => null()
    integer, pointer :: vitems(:) => null()
    integer, pointer :: vivari(:) => null()
!-----------------------------------------------------------------------
    AS_ALLOCATE(vi=videpl, size=6)
    AS_ALLOCATE(vi=vitemp, size=1)
    AS_ALLOCATE(vi=vitemi, size=1)
    AS_ALLOCATE(vi=vitems, size=1)
    AS_ALLOCATE(vi=vipres, size=1)
    AS_ALLOCATE(vi=vipre1, size=1)
    AS_ALLOCATE(vi=vipre2, size=1)
    AS_ALLOCATE(vi=visip, size=1)
    AS_ALLOCATE(vi=visigm, size=6)
    AS_ALLOCATE(vi=visigs, size=6)
    AS_ALLOCATE(vi=visigi, size=6)
    AS_ALLOCATE(vi=viepsi, size=6)
    AS_ALLOCATE(vi=viepsm, size=6)
    AS_ALLOCATE(vi=viepss, size=6)
    call wkvect('&&IRGAGS.IFLU', 'V V I', 3, iflu)
    AS_ALLOCATE(vi=viflui, size=3)
    AS_ALLOCATE(vi=viflus, size=3)
    AS_ALLOCATE(vi=vifh11, size=3)
    AS_ALLOCATE(vi=vifh12, size=3)
    AS_ALLOCATE(vi=vifh21, size=3)
    AS_ALLOCATE(vi=vifh22, size=3)
    AS_ALLOCATE(vi=vifht, size=3)
    AS_ALLOCATE(vi=vivari, size=ncmpmx)
!
!  --- INITIALISATIONS ----
!
    nbdepl = 0
    nbflu = 0
    nbflui = 0
    nbflus = 0
    nbfh11 = 0
    nbfh12 = 0
    nbfh21 = 0
    nbfh22 = 0
    nbfht = 0
    nbtemp = 0
    nbtemi = 0
    nbtems = 0
    nbpres = 0
    nbpre1 = 0
    nbpre2 = 0
    nbsip = 0
    nbsigm = 0
    nbsigi = 0
    nbsigs = 0
    nbepsm = 0
    nbepsi = 0
    nbepss = 0
    nbvari = 0
    iva = 0
    nbchs = 0
!
!  --- RECHERCHE DES GRANDEURS SUPERTAB ASSOCIEES A LA GRANDEUR ASTER---
!
    do 1 icmas = 1, ncmpmx
        if (nomcmp(icmas) .eq. 'DX') then
            nbdepl= nbdepl+1
            videpl(1)=icmas
        else if (nomcmp(icmas).eq.'DY') then
            nbdepl= nbdepl+1
            videpl(2)=icmas
        else if (nomcmp(icmas).eq.'DZ') then
            nbdepl= nbdepl+1
            videpl(3)=icmas
        else if (nomcmp(icmas).eq.'DRX') then
            nbdepl= nbdepl+1
            videpl(4)=icmas
        else if (nomcmp(icmas).eq.'DRY') then
            nbdepl= nbdepl+1
            videpl(5)=icmas
        else if (nomcmp(icmas).eq.'DRZ') then
            nbdepl= nbdepl+1
            videpl(6)=icmas
        else if (nomcmp(icmas).eq.'FLUXI') then
            nbflui= nbflui+1
            viflui(1)=icmas
        else if (nomcmp(icmas).eq.'FLUYI') then
            nbflui= nbflui+1
            viflui(2)=icmas
        else if (nomcmp(icmas).eq.'FLUZI') then
            nbflui= nbflui+1
            viflui(3)=icmas
        else if (nomcmp(icmas).eq.'FLUXS') then
            nbflus= nbflus+1
            viflus(1)=icmas
        else if (nomcmp(icmas).eq.'FLUYS') then
            nbflus= nbflus+1
            viflus(2)=icmas
        else if (nomcmp(icmas).eq.'FLUZS') then
            nbflus= nbflus+1
            viflus(3)=icmas
        else if (nomcmp(icmas).eq.'FLUX') then
            nbflu= nbflu+1
            zi(iflu-1+1)=icmas
        else if (nomcmp(icmas).eq.'FLUY') then
            nbflu= nbflu+1
            zi(iflu-1+2)=icmas
        else if (nomcmp(icmas).eq.'FLUZ') then
            nbflu= nbflu+1
            zi(iflu-1+3)=icmas
        else if (nomcmp(icmas).eq.'FH11X') then
            nbfh11 = nbfh11+1
            vifh11(1)=icmas
        else if (nomcmp(icmas).eq.'FH11Y') then
            nbfh11= nbfh11+1
            vifh11(2)=icmas
        else if (nomcmp(icmas).eq.'FH11Z') then
            nbfh11= nbfh11+1
            vifh11(3)=icmas
        else if (nomcmp(icmas).eq.'FH12X') then
            nbfh12= nbfh12+1
            vifh12(1)=icmas
        else if (nomcmp(icmas).eq.'FH12Y') then
            nbfh12= nbfh12+1
            vifh12(2)=icmas
        else if (nomcmp(icmas).eq.'FH12Z') then
            nbfh12= nbfh12+1
            vifh12(3)=icmas
        else if (nomcmp(icmas).eq.'FH21X') then
            nbfh21= nbfh21+1
            vifh21(1)=icmas
        else if (nomcmp(icmas).eq.'FH21Y') then
            nbfh21= nbfh21+1
            vifh21(2)=icmas
        else if (nomcmp(icmas).eq.'FH21Z') then
            nbfh21= nbfh21+1
            vifh21(3)=icmas
        else if (nomcmp(icmas).eq.'FH22X') then
            nbfh22= nbfh22+1
            vifh22(1)=icmas
        else if (nomcmp(icmas).eq.'FH22Y') then
            nbfh22= nbfh22+1
            vifh22(2)=icmas
        else if (nomcmp(icmas).eq.'FH22Z') then
            nbfh22= nbfh22+1
            vifh22(3)=icmas
        else if (nomcmp(icmas).eq.'FHTX') then
            nbfht= nbfht+1
            vifht(1)=icmas
        else if (nomcmp(icmas).eq.'FHTY') then
            nbfht= nbfht+1
            vifht(2)=icmas
        else if (nomcmp(icmas).eq.'FHTZ') then
            nbfht= nbfht+1
            vifht(3)=icmas
        else if (nomcmp(icmas).eq.'TEMP_I') then
            nbtemi= nbtemi+1
            vitemi(1)=icmas
        else if (nomcmp(icmas).eq.'TEMP_S') then
            nbtems= nbtems+1
            vitems(1)=icmas
        else if (nomcmp(icmas).eq.'TEMP') then
            nbtemp= nbtemp+1
            vitemp(1)=icmas
        else if (nomcmp(icmas).eq.'PRES') then
            nbpres= nbpres+1
            vipres(1)=icmas
        else if (nomcmp(icmas).eq.'PRE1') then
            nbpre1= nbpre1+1
            vipre1(1)=icmas
        else if (nomcmp(icmas).eq.'PRE2') then
            nbpre2= nbpre2+1
            vipre2(1)=icmas
        else if (nomcmp(icmas).eq.'SIP') then
            nbsip= nbsip+1
            visip(1)=icmas
        else if (nomcmp(icmas).eq.'SIXX_I') then
            nbsigi= nbsigi+1
            visigi(1) = icmas
        else if (nomcmp(icmas).eq.'SIXY_I') then
            nbsigi= nbsigi+1
            visigi(2) = icmas
        else if (nomcmp(icmas).eq.'SIYY_I') then
            nbsigi= nbsigi+1
            visigi(3) = icmas
        else if (nomcmp(icmas).eq.'SIXZ_I') then
            nbsigi= nbsigi+1
            visigi(4) = icmas
        else if (nomcmp(icmas).eq.'SIYZ_I') then
            nbsigi= nbsigi+1
            visigi(5) = icmas
        else if (nomcmp(icmas).eq.'SIZZ_I') then
            nbsigi= nbsigi+1
            visigi(6) = icmas
        else if (nomcmp(icmas).eq.'SIXX_S') then
            nbsigs= nbsigs+1
            visigs(1) = icmas
        else if (nomcmp(icmas).eq.'SIXY_S') then
            nbsigs= nbsigs+1
            visigs(2) = icmas
        else if (nomcmp(icmas).eq.'SIYY_S') then
            nbsigs= nbsigs+1
            visigs(3) = icmas
        else if (nomcmp(icmas).eq.'SIXZ_S') then
            nbsigs= nbsigs+1
            visigs(4) = icmas
        else if (nomcmp(icmas).eq.'SIYZ_S') then
            nbsigs= nbsigs+1
            visigs(5) = icmas
        else if (nomcmp(icmas).eq.'SIZZ_S') then
            nbsigs= nbsigs+1
            visigs(6) = icmas
        else if (nomcmp(icmas).eq.'SIXX') then
            nbsigm= nbsigm+1
            visigm(1) = icmas
        else if (nomcmp(icmas).eq.'SIXY') then
            nbsigm= nbsigm+1
            visigm(2) = icmas
        else if (nomcmp(icmas).eq.'SIYY') then
            nbsigm= nbsigm+1
            visigm(3) = icmas
        else if (nomcmp(icmas).eq.'SIXZ') then
            nbsigm= nbsigm+1
            visigm(4) = icmas
        else if (nomcmp(icmas).eq.'SIYZ') then
            nbsigm= nbsigm+1
            visigm(5) = icmas
        else if (nomcmp(icmas).eq.'SIZZ') then
            nbsigm= nbsigm+1
            visigm(6) = icmas
        else if (nomcmp(icmas).eq.'EPXX_I') then
            nbepsi= nbepsi+1
            viepsi(1) = icmas
        else if (nomcmp(icmas).eq.'EPXY_I') then
            nbepsi= nbepsi+1
            viepsi(2) = icmas
        else if (nomcmp(icmas).eq.'EPYY_I') then
            nbepsi= nbepsi+1
            viepsi(3) = icmas
        else if (nomcmp(icmas).eq.'EPXZ_I') then
            nbepsi= nbepsi+1
            viepsi(4) = icmas
        else if (nomcmp(icmas).eq.'EPYZ_I') then
            nbepsi= nbepsi+1
            viepsi(5) = icmas
        else if (nomcmp(icmas).eq.'EPZZ_I') then
            nbepsi= nbepsi+1
            viepsi(6) = icmas
        else if (nomcmp(icmas).eq.'EPXX_S') then
            nbepss= nbepss+1
            viepss(1) = icmas
        else if (nomcmp(icmas).eq.'EPXY_S') then
            nbepss= nbepss+1
            viepss(2) = icmas
        else if (nomcmp(icmas).eq.'EPYY_S') then
            nbepss= nbepss+1
            viepss(3) = icmas
        else if (nomcmp(icmas).eq.'EPXZ_S') then
            nbepss= nbepss+1
            viepss(4) = icmas
        else if (nomcmp(icmas).eq.'EPYZ_S') then
            nbepss= nbepss+1
            viepss(5) = icmas
        else if (nomcmp(icmas).eq.'EPZZ_S') then
            nbepss= nbepss+1
            viepss(6) = icmas
        else if (nomcmp(icmas).eq.'EPXX') then
            nbepsm= nbepsm+1
            viepsm(1) = icmas
        else if (nomcmp(icmas).eq.'EPXY') then
            nbepsm= nbepsm+1
            viepsm(2) = icmas
        else if (nomcmp(icmas).eq.'EPYY') then
            nbepsm= nbepsm+1
            viepsm(3) = icmas
        else if (nomcmp(icmas).eq.'EPXZ') then
            nbepsm= nbepsm+1
            viepsm(4) = icmas
        else if (nomcmp(icmas).eq.'EPYZ') then
            nbepsm= nbepsm+1
            viepsm(5) = icmas
        else if (nomcmp(icmas).eq.'EPZZ') then
            nbepsm= nbepsm+1
            viepsm(6) = icmas
        else
            nbvari= nbvari+1
            vivari(nbvari)= icmas
        endif
 1  end do
!
!  --- RECHERCHE DES GRANDEURS SUPERTAB ASSOCIEES A LA GRANDEUR ASTER---
!
    if (nbdepl .ne. 0) then
        if (nomsym .eq. 'DEPL' .or. nomsym .eq. 'VITE' .or. nomsym .eq. 'ACCE') then
            nbchs=nbchs+1
            nomchs(nbchs) = 'DEPL'
            nbcmps(nbchs) = nbdepl
            nomgds(nbchs) = 'DEPL'
            if (nomsym .eq. 'DEPL') nomgds(nbchs) = 'DEPL'
            if (nomsym .eq. 'VITE') nomgds(nbchs) = 'VITE'
            if (nomsym .eq. 'ACCE') nomgds(nbchs) = 'ACCE'
        endif
    endif
    if (nbflu .ne. 0) then
        nbchs=nbchs+1
        nomchs(nbchs) = 'FLUX'
        nbcmps(nbchs) = nbflu
        nomgds(nbchs) = 'FLUX'
    endif
    if (nbflui .ne. 0) then
        nbchs=nbchs+1
        nomchs(nbchs) = 'FLUI'
        nbcmps(nbchs) = nbflui
        nomgds(nbchs) = 'FLUX'
    endif
    if (nbflus .ne. 0) then
        nbchs=nbchs+1
        nomchs(nbchs) = 'FLUS'
        nbcmps(nbchs) = nbflus
        nomgds(nbchs) = 'FLUX'
    endif
    if (nbfh11 .ne. 0) then
        nbchs=nbchs+1
        nomchs(nbchs) = 'FH11'
        nbcmps(nbchs) = nbfh11
        nomgds(nbchs) = 'FLUX'
    endif
    if (nbfh12 .ne. 0) then
        nbchs=nbchs+1
        nomchs(nbchs) = 'FH12'
        nbcmps(nbchs) = nbfh12
        nomgds(nbchs) = 'FLUX'
    endif
    if (nbfh21 .ne. 0) then
        nbchs=nbchs+1
        nomchs(nbchs) = 'FH21'
        nbcmps(nbchs) = nbfh21
        nomgds(nbchs) = 'FLUX'
    endif
    if (nbfh22 .ne. 0) then
        nbchs=nbchs+1
        nomchs(nbchs) = 'FH22'
        nbcmps(nbchs) = nbfh22
        nomgds(nbchs) = 'FLUX'
    endif
    if (nbfht .ne. 0) then
        nbchs=nbchs+1
        nomchs(nbchs) = 'FHT'
        nbcmps(nbchs) = nbfht
        nomgds(nbchs) = 'FLUX'
    endif
    if (nbtemp .ne. 0) then
        nbchs=nbchs+1
        nomchs(nbchs) = 'TEMP'
        nbcmps(nbchs) = nbtemp
        nomgds(nbchs) = 'TEMP'
    endif
    if (nbtemi .ne. 0) then
        nbchs=nbchs+1
        nomchs(nbchs) = 'TEMI'
        nbcmps(nbchs) = nbtemi
        nomgds(nbchs) = 'TEMP'
    endif
    if (nbtems .ne. 0) then
        nbchs=nbchs+1
        nomchs(nbchs) = 'TEMS'
        nbcmps(nbchs) = nbtems
        nomgds(nbchs) = 'TEMP'
    endif
    if (nbpres .ne. 0) then
        nbchs=nbchs+1
        nomchs(nbchs) = 'PRES'
        nbcmps(nbchs) = nbpres
        nomgds(nbchs) = 'PRES'
    endif
    if (nbpre1 .ne. 0) then
        nbchs=nbchs+1
        nomchs(nbchs) = 'PRE1'
        nbcmps(nbchs) = nbpre1
        nomgds(nbchs) = 'PRES'
    endif
    if (nbpre2 .ne. 0) then
        nbchs=nbchs+1
        nomchs(nbchs) = 'PRE2'
        nbcmps(nbchs) = nbpre2
        nomgds(nbchs) = 'PRES'
    endif
    if (nbsip .ne. 0) then
        nbchs=nbchs+1
        nomchs(nbchs) = 'SIP'
        nbcmps(nbchs) = nbsip
        nomgds(nbchs) = 'VARI'
    endif
    if (nbsigm .ne. 0) then
        nbchs=nbchs+1
        nomchs(nbchs) = 'SIGM'
        nbcmps(nbchs) = nbsigm
        nomgds(nbchs) = 'SIGM'
    endif
    if (nbsigs .ne. 0) then
        nbchs=nbchs+1
        nomchs(nbchs) = 'SIGS'
        nbcmps(nbchs) = nbsigs
        nomgds(nbchs) = 'SIGM'
    endif
    if (nbsigi .ne. 0) then
        nbchs=nbchs+1
        nomchs(nbchs) = 'SIGI'
        nbcmps(nbchs) = nbsigi
        nomgds(nbchs) = 'SIGM'
    endif
    if (nbepsm .ne. 0) then
        nbchs=nbchs+1
        nomchs(nbchs) = 'EPSM'
        nbcmps(nbchs) = nbepsm
        nomgds(nbchs) = 'EPSI'
    endif
    if (nbepss .ne. 0) then
        nbchs=nbchs+1
        nomchs(nbchs) = 'EPSS'
        nbcmps(nbchs) = nbepss
        nomgds(nbchs) = 'EPSI'
    endif
    if (nbepsi .ne. 0) then
        nbchs=nbchs+1
        nomchs(nbchs) = 'EPSI'
        nbcmps(nbchs) = nbepsi
        nomgds(nbchs) = 'EPSI'
    endif
    if (nbvari .ne. 0) then
        ient =nbvari/6
        ires =nbvari-(ient*6)
        if (ient .ne. 0) then
            do 50 ivar = 1, ient
                nomchs(nbchs+ivar)= 'VARI'
                nbcmps(nbchs+ivar)= 6
                nomgds(nbchs+ivar)= 'VARI'
50          continue
        endif
        if (ires .ne. 0) then
            nomchs(nbchs+ient+1)= 'VARI'
            nbcmps(nbchs+ient+1)= ires
            nomgds(nbchs+ient+1)= 'VARI'
            nbchs = nbchs+ient+1
        else
            nbchs = nbchs+ient
        endif
    endif
!
!  ---- POSITIONS DES COMPOSANTES SUPERTAB DANS LA GRANDEUR ASTER ----
!
    do 8 i = 1, nbchs
        if (nomchs(i) .eq. 'DEPL') then
            do 31 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = videpl(j)
31          continue
        else if (nomchs(i).eq.'FLUX') then
            do 32 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = zi(iflu-1+j)
32          continue
        else if (nomchs(i).eq.'FLUI') then
            do 33 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = viflui(j)
33          continue
        else if (nomchs(i).eq.'FLUS') then
            do 34 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = viflus(j)
34          continue
        else if (nomchs(i).eq.'FH11') then
            do 341 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = vifh11(j)
341          continue
        else if (nomchs(i).eq.'FH12') then
            do 342 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = vifh12(j)
342          continue
        else if (nomchs(i).eq.'FH21') then
            do 343 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = vifh21(j)
343          continue
        else if (nomchs(i).eq.'FH22') then
            do 344 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = vifh22(j)
344          continue
        else if (nomchs(i).eq.'FHT') then
            do 345 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = vifht(j)
345          continue
        else if (nomchs(i).eq.'TEMP') then
            do 35 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = vitemp(j)
35          continue
        else if (nomchs(i).eq.'TEMI') then
            do 36 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = vitemi(j)
36          continue
        else if (nomchs(i).eq.'TEMS') then
            do 37 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = vitems(j)
37          continue
        else if (nomchs(i).eq.'PRES') then
            do 38 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = vipres(j)
38          continue
        else if (nomchs(i).eq.'PRE1') then
            do 381 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = vipre1(j)
381          continue
        else if (nomchs(i).eq.'PRE2') then
            do 382 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = vipre2(j)
382          continue
        else if (nomchs(i).eq.'SIP') then
            do 383 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = visip(j)
383          continue
        else if (nomchs(i).eq.'SIGM') then
            do 39 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = visigm(j)
39          continue
        else if (nomchs(i).eq.'SIGI') then
            do 40 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = visigi(j)
40          continue
        else if (nomchs(i).eq.'SIGS') then
            do 41 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = visigs(j)
41          continue
        else if (nomchs(i).eq.'EPSM') then
            do 42 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = viepsm(j)
42          continue
        else if (nomchs(i).eq.'EPSS') then
            do 43 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = viepss(j)
43          continue
        else if (nomchs(i).eq.'EPSI') then
            do 44 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = viepsi(j)
44          continue
        else if (nomchs(i).eq.'VARI') then
            iva =iva + 1
            do 45 j = 1, nbcmps(i)
                ipcmps((i-1)*ncmpmx+j) = vivari((iva-1)*6+j)
45          continue
        endif
 8  end do
!
    AS_DEALLOCATE(vi=videpl)
    AS_DEALLOCATE(vi=vitemp)
    AS_DEALLOCATE(vi=vitemi)
    AS_DEALLOCATE(vi=vitems)
    AS_DEALLOCATE(vi=vipres)
    AS_DEALLOCATE(vi=vipre1)
    AS_DEALLOCATE(vi=vipre2)
    AS_DEALLOCATE(vi=visip)
    AS_DEALLOCATE(vi=visigm)
    AS_DEALLOCATE(vi=visigs)
    AS_DEALLOCATE(vi=visigi)
    AS_DEALLOCATE(vi=viepsi)
    AS_DEALLOCATE(vi=viepsm)
    AS_DEALLOCATE(vi=viepss)
    call jedetr('&&IRGAGS.IFLU')
    AS_DEALLOCATE(vi=viflui)
    AS_DEALLOCATE(vi=viflus)
    AS_DEALLOCATE(vi=vifh11)
    AS_DEALLOCATE(vi=vifh12)
    AS_DEALLOCATE(vi=vifh21)
    AS_DEALLOCATE(vi=vifh22)
    AS_DEALLOCATE(vi=vifht)
    AS_DEALLOCATE(vi=vivari)
end subroutine
