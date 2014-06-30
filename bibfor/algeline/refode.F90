subroutine refode(nbcmb, angle, nomch, nuharm, tyharm,&
                  coef, basz, chpres)
    implicit none
#include "jeveux.h"
#include "asterfort/celver.h"
#include "asterfort/digdel.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nbelem.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: nbcmb, nuharm(*)
    character(len=*) :: nomch(*), basz, tyharm(*), chpres
    real(kind=8) :: angle, coef(*)
!     ------------------------------------------------------------------
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
!     RECOMBINAISON DE FOURIER
!     -----------------------------------------------------------------
!     IN  NBCMB  : I   : NOMBRE DE CHAMPS A RECOMBINER
!     IN  ANGLE  : R8  : SECTION OU A LIEU LA RECOMBINAISON ( EN RD )
!     IN  NOMCH  : K8  : NOM DES CHAMPS A RECOMBINER
!     IN  NUHARM : I   : NUMERO DE L'HARMONIQUE
!     IN  TYHARM : K4  : TYPE DE L'HARMONIQUE (SYME OU ANTI)
!     IN  COEF   : R8  : COEF MULTIPLICATEUR ASSOCIE A L'HARMONIQUE
!     IN  CHPRES : K19 : NOM DU CHAMP RESULTAT
!     -----------------------------------------------------------------
    integer :: ibid, mode
    character(len=1) :: base
    character(len=4) :: docu
    character(len=5) :: refe, desc, vale
    character(len=8) :: noma, nomgd
    character(len=19) :: ch19, ligrel
    logical(kind=1) :: lmeca, lther
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, i1, ic, icoef, idecgr
    integer :: igrel, im, ino, ip, iret, ival
    integer ::   jdesc,  jrefe, jvale, k
    integer :: kdesc, krefe, kvale, lvale, nbdesc, nbec, nbelgr
    integer :: nbgr, nbnoeu, nbpt, nbrefe, nbscal, nbvale
    real(kind=8) :: ang
    integer, pointer :: celd(:) => null()
    real(kind=8), pointer :: celv(:) => null()
    character(len=24), pointer :: celk(:) => null()
    integer, pointer :: prno(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    base = basz
    ch19 = nomch(1)
!
    call jeexin(ch19//'.DESC', ibid)
    if (ibid .gt. 0) then
        call jelira(ch19//'.DESC', 'DOCU', cval=docu)
    else
        call jelira(ch19//'.CELD', 'DOCU', cval=docu)
    endif
!
    if (docu .eq. 'CHNO') then
        desc = '.DESC'
        refe = '.REFE'
        vale = '.VALE'
    else if (docu.eq.'CHML') then
        desc = '.CELD'
        refe = '.CELK'
        vale = '.CELV'
    else
        call utmess('F', 'UTILITAI_21')
    endif
!
    lmeca = .false.
    lther = .false.
    call dismoi('NOM_GD', ch19, 'CHAMP', repk=nomgd)
    if (nomgd .eq. 'DEPL_R' .or. nomgd .eq. 'SIEF_R' .or. nomgd .eq. 'EPSI_R') then
        lmeca = .true.
    else if (nomgd.eq.'TEMP_R' .or. nomgd.eq.'FLUX_R') then
        lther = .true.
    endif
    call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nbec)
!
!     --- CONSTRUCTION D'UN CHAMP RESULTAT SUR LE MODELE DE NOMCH(1)
!
    call jelira(ch19//desc, 'LONMAX', nbdesc)
    call jelira(ch19//vale, 'LONMAX', nbvale)
    call jelira(ch19//refe, 'LONMAX', nbrefe)
    call jeveuo(ch19//desc, 'L', jdesc)
    call jeveuo(ch19//refe, 'L', jrefe)
!
    ch19 = chpres
    call jeexin(ch19//vale, iret)
    if (iret .eq. 0) then
        call wkvect(ch19//desc, base//' V I', nbdesc, kdesc)
        call wkvect(ch19//vale, base//' V R', nbvale, kvale)
        call wkvect(ch19//refe, base//' V K24', nbrefe, krefe)
    else
        call jeveuo(ch19//desc, 'E', kdesc)
        call jeveuo(ch19//vale, 'E', kvale)
        call jeveuo(ch19//refe, 'E', krefe)
    endif
!
    call jeecra(ch19//desc, 'DOCU', cval=docu)
    do i = 0, nbdesc-1
        zi(kdesc+i) = zi(jdesc+i)
    end do
    call jelibe(ch19//desc)
!
!
    call wkvect('&&REFODE.VALE', 'V V R', nbvale, lvale)
!
    if (docu .eq. 'CHNO') then
        call jeveuo(zk24(jrefe+1)(1:19)//'.PRNO', 'L', vi=prno)
        do i = 0, nbrefe-1
            zk24(krefe+i) = zk24(jrefe+i)
        end do
        call jelibe(ch19//'.REFE')
        call dismoi('NOM_MAILLA', nomch(1), 'CHAMP', repk=noma)
        call jelira(noma//'.NOMNOE', 'NOMMAX', nbnoeu)
!
!        --- BOUCLE SUR LES CHAMPS A RECOMBINER ---
!
        do im = 1, nbcmb
            ang = angle * dble(nuharm(im))
            ch19 = nomch(im)
            call jeveuo(ch19//vale, 'L', jvale)
!
            if (lmeca) then
!
                if (tyharm(im)(1:4) .eq. 'SYME') then
!
                    do ino = 1, nbnoeu
                        i = prno(1+(ino-1)*(nbec+2))-2
                        if (i .ne. -2) then
                            zr(lvale+i+1) = zr(lvale+i+1) + coef(im)* cos(ang)* zr(jvale+i+1)
                            zr(lvale+i+2) = zr(lvale+i+2) + coef(im)* cos(ang)* zr(jvale+i+2)
                            zr(lvale+i+3) = zr(lvale+i+3) - coef(im)* sin(ang)* zr(jvale+i+3)
                        endif
                    end do
!
                else if (tyharm(im)(1:4).eq.'ANTI') then
!
                    do ino = 1, nbnoeu
                        i = prno(1+(ino-1)*(nbec+2))-2
                        if (i .ne. -2) then
                            zr(lvale+i+1) = zr(lvale+i+1) + coef(im)* sin(ang)* zr(jvale+i+1)
                            zr(lvale+i+2) = zr(lvale+i+2) + coef(im)* sin(ang)* zr(jvale+i+2)
                            zr(lvale+i+3) = zr(lvale+i+3) + coef(im)* cos(ang)* zr(jvale+i+3)
                        endif
                    end do
!
                else if (tyharm(im)(1:4).eq.'TOUS') then
!
                    do ino = 1, nbnoeu
                        i = prno(1+(ino-1)*(nbec+2))-2
                        if (i .ne. -2) then
                            zr(lvale+i+1) = zr(lvale+i+1) + coef(im)* sin(ang)*zr(jvale+i+1) + co&
                                            &ef(im)*cos(ang) *zr(jvale+i+1)
                            zr(lvale+i+2) = zr(lvale+i+2) + coef(im)* sin(ang)*zr(jvale+i+2) + co&
                                            &ef(im)*cos(ang) *zr(jvale+i+2)
                            zr(lvale+i+3) = zr(lvale+i+3) + coef(im)* cos(ang)*zr(jvale+i+3) - co&
                                            &ef(im)*sin(ang) *zr(jvale+i+3)
                        endif
                    end do
!
                endif
!
            else if (lther) then
!
                if (tyharm(im)(1:4) .eq. 'SYME') then
!
                    do ino = 1, nbnoeu
                        i = prno(1+(ino-1)*(nbec+2))-2
                        if (i .ne. -2) then
                            zr(lvale+i+1) = zr(lvale+i+1) + coef(im)* cos(ang)* zr(jvale+i+1)
                        endif
                    end do
!
                else if (tyharm(im)(1:4).eq.'ANTI') then
!
                    do ino = 1, nbnoeu
                        i = prno(1+(ino-1)*(nbec+2))-2
                        if (i .ne. -2) then
                            zr(lvale+i+1) = zr(lvale+i+1) + coef(im)* sin(ang)* zr(jvale+i+1)
                        endif
                    end do
!
                else if (tyharm(im)(1:4).eq.'TOUS') then
!
                    do ino = 1, nbnoeu
                        i = prno(1+(ino-1)*(nbec+2))-2
                        if (i .ne. -2) then
                            zr(lvale+i+1) = zr(lvale+i+1) + coef(im)* sin(ang)*zr(jvale+i+1) + co&
                                            &ef(im)*cos(ang) *zr(jvale+i+1)
                        endif
                    end do
!
                endif
            endif
        end do
!
    else if (docu.eq.'CHML') then
        do i = 0, nbrefe-1
            zk24(krefe+i) = zk24(jrefe+i)
        end do
        call jelibe(ch19//'.CELK')
!
        do im = 1, nbcmb
            i1 = -1
            ang = angle * dble(nuharm(im))
            ch19 = nomch(im)
!
!           -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
            call celver(ch19, 'NBVARI_CST', 'STOP', ibid)
            call celver(ch19, 'NBSPT_1', 'STOP', ibid)
!
            call jeveuo(ch19//'.CELD', 'L', vi=celd)
            call jeveuo(ch19//'.CELK', 'L', vk24=celk)
            call jeveuo(ch19//'.CELV', 'L', vr=celv)
            nbgr = celd(2)
            ligrel = celk(1)(1:19)
!
            do igrel = 1, nbgr
                mode=celd(celd(4+igrel) +2)
                if (mode .eq. 0) goto 210
                nbscal = digdel(mode)
                icoef=max(1,celd(4))
                if (icoef .ne. 1) then
                    call utmess('F', 'ALGELINE3_33')
                endif
                nbelgr = nbelem(ligrel,igrel)
                idecgr=celd(celd(4+igrel)+8)
!
                if (lmeca) then
!              --- ON EST EN AXIS, IL Y A 6 COMPOSANTES PAR POINT ---
                    nbpt = nbscal / 6
!
                    if (tyharm(im)(1:4) .eq. 'SYME') then
!
                        do k = 1, nbelgr
                            ic = -1
                            do ip = 1, nbpt
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *cos(ang)* celv(idec&
                                               &gr+(k-1)* nbscal+ic)
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *cos(ang)* celv(idec&
                                               &gr+(k-1)* nbscal+ic)
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *cos(ang)* celv(idec&
                                               &gr+(k-1)* nbscal+ic)
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *cos(ang)* celv(idec&
                                               &gr+(k-1)* nbscal+ic)
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) - coef(im) *sin(ang)* celv(idec&
                                               &gr+(k-1)* nbscal+ic)
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) - coef(im) *sin(ang)* celv(idec&
                                               &gr+(k-1)* nbscal+ic)
                            end do
                        end do
!
                    else if (tyharm(im)(1:4).eq.'ANTI') then
!
                        do k = 1, nbelgr
                            ic = -1
                            do ip = 1, nbpt
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *sin(ang)* celv(idec&
                                               &gr+(k-1)* nbscal+ic)
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *sin(ang)* celv(idec&
                                               &gr+(k-1)* nbscal+ic)
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *sin(ang)* celv(idec&
                                               &gr+(k-1)* nbscal+ic)
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *sin(ang)* celv(idec&
                                               &gr+(k-1)* nbscal+ic)
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *cos(ang)* celv(idec&
                                               &gr+(k-1)* nbscal+ic)
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *cos(ang)* celv(idec&
                                               &gr+(k-1)* nbscal+ic)
                            end do
                        end do
!
                    else if (tyharm(im)(1:4).eq.'TOUS') then
!
                        do k = 1, nbelgr
                            ic = -1
                            do ip = 1, nbpt
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *sin(ang)*celv(idecg&
                                               &r+(k-1)* nbscal+ic) + coef(im)*cos(ang)*celv(1&
                                               &-1+idecgr+(k-1)*nbscal+ic)
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *sin(ang)*celv(idecg&
                                               &r+(k-1)* nbscal+ic) + coef(im)*cos(ang)*celv(1&
                                               &-1+idecgr+(k-1)*nbscal+ic)
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *sin(ang)*celv(idecg&
                                               &r+(k-1)* nbscal+ic) + coef(im)*cos(ang)*celv(1&
                                               &-1+idecgr+(k-1)*nbscal+ic)
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *sin(ang)*celv(idecg&
                                               &r+(k-1)* nbscal+ic) + coef(im)*cos(ang)*celv(1&
                                               &-1+idecgr+(k-1)*nbscal+ic)
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *cos(ang)*celv(idecg&
                                               &r+(k-1)* nbscal+ic) - coef(im)*sin(ang)*celv(1&
                                               &-1+idecgr+(k-1)*nbscal+ic)
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *cos(ang)*celv(idecg&
                                               &r+(k-1)* nbscal+ic) - coef(im)*sin(ang)*celv(1&
                                               &-1+idecgr+(k-1)*nbscal+ic)
                            end do
                        end do
                    endif
!
                else if (lther) then
!              --- ON EST EN AXIS, IL Y A 3 COMPOSANTES PAR POINT ---
                    nbpt = nbscal / 3
!
                    if (tyharm(im)(1:4) .eq. 'SYME') then
!
                        do k = 1, nbelgr
                            ic = -1
                            do ip = 1, nbpt
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *cos(ang)* celv(idec&
                                               &gr+(k-1)* nbscal+ic)
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *cos(ang)* celv(idec&
                                               &gr+(k-1)* nbscal+ic)
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) - coef(im) *sin(ang)* celv(idec&
                                               &gr+(k-1)* nbscal+ic)
                            end do
                        end do
!
                    else if (tyharm(im)(1:4).eq.'ANTI') then
!
                        do k = 1, nbelgr
                            ic = -1
                            do ip = 1, nbpt
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *sin(ang)* celv(idec&
                                               &gr+(k-1)* nbscal+ic)
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *sin(ang)* celv(idec&
                                               &gr+(k-1)* nbscal+ic)
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *cos(ang)* celv(idec&
                                               &gr+(k-1)* nbscal+ic)
                            end do
                        end do
!
                    else if (tyharm(im)(1:4).eq.'TOUS') then
!
                        do k = 1, nbelgr
                            ic = -1
                            do ip = 1, nbpt
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *sin(ang)*celv(idecg&
                                               &r+(k-1)* nbscal+ic) + coef(im)*cos(ang)*celv(1&
                                               &-1+idecgr+(k-1)*nbscal+ic)
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *sin(ang)*celv(idecg&
                                               &r+(k-1)* nbscal+ic) + coef(im)*cos(ang)*celv(1&
                                               &-1+idecgr+(k-1)*nbscal+ic)
                                i1 = i1 + 1
                                ic = ic + 1
                                zr(lvale+i1) = zr(lvale+i1) + coef(im) *cos(ang)*celv(idecg&
                                               &r+(k-1)* nbscal+ic) - coef(im)*sin(ang)*celv(1&
                                               &-1+idecgr+(k-1)*nbscal+ic)
                            end do
                        end do
                    endif
                endif
210             continue
            end do
            call jelibe(ch19//desc)
            call jelibe(ch19//'.CELK')
            call jelibe(ch19//vale)
!
        end do
    endif
!
    do ival = 0, nbvale-1
        zr(kvale+ival) = zr(lvale+ival)
    end do
    ch19 = chpres
    call jelibe(ch19//vale)
    call jedetr('&&REFODE.VALE')
!
    call jedema()
end subroutine
