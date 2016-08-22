subroutine usupus(puusur, kforn, kvgli, nbpt)
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!     CALCULE LA PUISSANCE D'USURE AU SENS D'ARCHARD
!                    PU  =  FN * VT
!
! OUT : PUUSUR : PUISSANCE USURE
!-----------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/impus.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/nlget.h"
#include "asterfort/reliem.h"
#include "asterfort/statpu.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "blas/dcopy.h"

    character(len=8) :: noeu
    character(len=19) :: trange, kforn, kvgli
!
!-----------------------------------------------------------------------
    integer :: i, ichoc, idebut,  idwk4, ifin, ifires, nbvint, ifl, nbtot, nbflam, ic, dec, nbchoc
    integer :: impr, j, jfn, jnomno, nbno
    integer ::  jvg,  jwk1, jwk2, jwk3, lg
    integer :: n1, n2, n3, nbnoli, nbloc
    integer :: nbpas, nbpt, nbval, nt
    character(len=8) :: maillage , modele, base
    character(len=24) :: nomno
    character(len=16) :: motcle(2), typmcl(2)
    real(kind=8) :: puusur, tdebut, tfin, tmax, tmin
!-----------------------------------------------------------------------
    integer          , pointer :: desc  (:) => null()
    real(kind=8)     , pointer :: disc  (:) => null()
    integer          , pointer :: nltype(:) => null()
    integer          , pointer :: vindx (:) => null()
    character(len=24), pointer :: nlname(:) => null()
    real(kind=8)     , pointer :: vint  (:) => null()
!-----------------------------------------------------------------------
    integer, pointer :: chindx(:) => null()
    integer, pointer :: flindx(:) => null()    
    real(kind=8), pointer :: vcho(:) => null()
    real(kind=8), pointer :: fcho(:) => null()
    real(kind=8), pointer :: dloc(:) => null()
    character(len=8), pointer :: inti(:) => null()
    integer, pointer :: icho(:) => null()
    character(len=8), pointer :: ncho(:) => null()


!-----------------------------------------------------------------------
    data motcle  /'NOEUD','GROUP_NO'/
    data typmcl  /'NOEUD','GROUP_NO'/
!   ------------------------------------------------------------------
!
    call jemarq()
    nomno = '&&USUPUS.MES_NOEUDS'
    ifires = iunifi('RESULTAT')
    nbpt = 0
    impr = 2
!
    call getvr8(' ', 'PUIS_USURE', scal=puusur, nbret=n1)
    if (n1 .ne. 0) then
        call impus(ifires, 0, puusur)
        goto 999
    endif
!
    call getvid(' ', 'RESU_GENE', scal=trange, nbret=nt)
    if (nt .ne. 0) then
        call jeveuo(trange//'.DESC', 'L', vi=desc)
        if (desc(1) .eq. 2 .or. desc(1) .eq. 3) then
            nbnoli = desc(3)
            call getvis(' ', 'NB_BLOC', scal=nbloc, nbret=n1)
            if (n1 .eq. 0) nbloc = 1
            call getvr8(' ', 'INST_INIT', scal=tdebut, nbret=n2)
            call getvr8(' ', 'INST_FIN', scal=tfin, nbret=n3)
!
            call dismoi('BASE_MODALE', trange, 'RESU_DYNA', repk=base, arret='F')
            call dismoi('NOM_MODELE', base, 'RESULTAT', repk=modele)
            call dismoi('NOM_MAILLA', base, 'RESULTAT', repk=maillage)
!
            call reliem(modele, maillage, 'NO_NOEUD', ' ', 1,&
                        2, motcle, typmcl, nomno, nbno)
            call jeveuo(nomno, 'L', jnomno)
            noeu = zk8(jnomno)
!
            call jeveuo(trange(1:16)//'.NL.INTI', 'L', vk24=nlname)
!           --- RECHERCHE DU NOEUD DE CHOC ---
            do 10 ichoc = 1, nbnoli
                if (nlname((ichoc-1)*5+2)(1:8) .eq. noeu) goto 12
10          continue
            lg = max(1,lxlgut(noeu))
            call utmess('F', 'UTILITAI_87', sk=noeu(1:lg))
12          continue
!
            call jeveuo(trange//'.DISC', 'L', vr=disc)
            call jelira(trange//'.DISC', 'LONMAX', nbpt)
            tmax = disc(nbpt)
            tmin = disc(1)
            if (n2 .eq. 0) then
                tdebut = tmin
            else
                if (tdebut .lt. tmin) tdebut = tmin
            endif
            if (n3 .eq. 0) then
                tfin = tmax
            else
                if (tfin .gt. tmax) tfin = tmax
            endif
            if (tdebut .ge. tfin) then
                call utmess('F', 'PREPOST4_47')
            endif
            do 14 j = 1, nbpt
                if (disc(j) .ge. tdebut) then
                    idebut = j
                    goto 15
                endif
14          continue
15          continue
            do 16 j = 1, nbpt
                if (disc(j) .ge. tfin) then
                    ifin = j
                    goto 17
                endif
16          continue
17          continue
            nbpas = ifin - idebut + 1
            if (nbloc .eq. 0) nbloc = 1
            nbval = nbpas / nbloc
!
            call jeveuo(trange(1:16)//'.NL.TYPE', 'L', vi  =nltype)
            call jeveuo(trange(1:16)//'.NL.VIND', 'L', vi  =vindx)
            call jeveuo(trange(1:16)//'.NL.VINT', 'L', vr  =vint)
            nbvint = vindx(nbnoli+1)-1
!
            AS_ALLOCATE(vi=chindx, size=nbnoli)
            AS_ALLOCATE(vi=flindx, size=nbnoli)

            nbchoc = 0
            do i = 1, nbnoli
                if (nltype(i).eq.NL_CHOC) then
                    nbchoc = nbchoc + 1
                    chindx(nbchoc) = i
                end if
            end do

            nbflam = 0
            do i = 1, nbnoli
                if (nltype(i).eq.NL_BUCKLING) then
                    nbflam = nbflam + 1
                    flindx(nbflam) = i
                end if
            end do

            nbtot = nbchoc + nbflam

            AS_ALLOCATE(vr =fcho, size=  3*nbtot*nbpt)
            AS_ALLOCATE(vr =dloc, size=2*3*nbtot*nbpt)
            AS_ALLOCATE(vr =vcho, size=  3*nbtot*nbpt)
            AS_ALLOCATE(vi =icho, size=    nbtot*nbpt)
            AS_ALLOCATE(vk8=ncho, size=  2*nbtot)
            AS_ALLOCATE(vk8=inti, size=    nbtot)

            do ic = 1, nbchoc
                i = chindx(ic)
                do j = 1, nbpt
                    fcho((j-1)*3*nbtot+(ic-1)*3+1)     = vint((j-1)*nbvint+vindx(i)-1+1)
                    fcho((j-1)*3*nbtot+(ic-1)*3+2)     = vint((j-1)*nbvint+vindx(i)-1+2)
                    fcho((j-1)*3*nbtot+(ic-1)*3+3)     = vint((j-1)*nbvint+vindx(i)-1+3)

                    dloc((j-1)*3*nbtot+(ic-1)*3+1)     = vint((j-1)*nbvint+vindx(i)-1+4)
                    dloc((j-1)*3*nbtot+(ic-1)*3+2)     = vint((j-1)*nbvint+vindx(i)-1+5)
                    dloc((j-1)*3*nbtot+(ic-1)*3+3)     = vint((j-1)*nbvint+vindx(i)-1+6)
                    dec = 3*nbtot*nbpt
                    dloc(dec+(j-1)*3*nbtot+(ic-1)*3+1) = vint((j-1)*nbvint+vindx(i)-1+7)
                    dloc(dec+(j-1)*3*nbtot+(ic-1)*3+2) = vint((j-1)*nbvint+vindx(i)-1+8)
                    dloc(dec+(j-1)*3*nbtot+(ic-1)*3+3) = vint((j-1)*nbvint+vindx(i)-1+9)

                    vcho((j-1)*3*nbtot+(ic-1)*3+1)     = vint((j-1)*nbvint+vindx(i)-1+10)
                    vcho((j-1)*3*nbtot+(ic-1)*3+2)     = vint((j-1)*nbvint+vindx(i)-1+11)
                    vcho((j-1)*3*nbtot+(ic-1)*3+3)     = vint((j-1)*nbvint+vindx(i)-1+12)

                    icho((j-1)*nbtot+(ic-1)+1)  = nint(vint((j-1)*nbvint+vindx(i)-1+13))
                end do
                inti(ic)       = nlname((i-1)*5+1)(1:8)
                ncho(ic)       = nlname((i-1)*5+2)(1:8)
                ncho(nbtot+ic) = nlname((i-1)*5+3)(1:8)
            end do

            do ifl = 1, nbflam
                i = flindx(ifl)
                ic = nbchoc + ifl
                do j = 1, nbpt
                    fcho((j-1)*3*nbtot+(ic-1)*3+1)     = vint((j-1)*nbvint+vindx(i)-1+1)
                    fcho((j-1)*3*nbtot+(ic-1)*3+2)     = 0.d0
                    fcho((j-1)*3*nbtot+(ic-1)*3+3)     = 0.d0

                    dloc((j-1)*3*nbtot+(ic-1)*3+1)     = vint((j-1)*nbvint+vindx(i)-1+2)
                    dloc((j-1)*3*nbtot+(ic-1)*3+2)     = vint((j-1)*nbvint+vindx(i)-1+3)
                    dloc((j-1)*3*nbtot+(ic-1)*3+3)     = vint((j-1)*nbvint+vindx(i)-1+4)
                    dec = 3*nbtot*nbpt
                    dloc(dec+(j-1)*3*nbtot+(ic-1)*3+1) = vint((j-1)*nbvint+vindx(i)-1+5)
                    dloc(dec+(j-1)*3*nbtot+(ic-1)*3+2) = vint((j-1)*nbvint+vindx(i)-1+6)
                    dloc(dec+(j-1)*3*nbtot+(ic-1)*3+3) = vint((j-1)*nbvint+vindx(i)-1+7)

                    vcho((j-1)*3*nbtot+(ic-1)*3+1)     = vint((j-1)*nbvint+vindx(i)-1+8)
                    vcho((j-1)*3*nbtot+(ic-1)*3+2)     = 0.d0
                    vcho((j-1)*3*nbtot+(ic-1)*3+3)     = 0.d0

                    icho((j-1)*nbtot+(ic-1)+1)         = 0
                end do
                inti(ic)       = nlname((i-1)*5+1)(1:8)
                ncho(ic)       = nlname((i-1)*5+2)(1:8)
                ncho(nbtot+ic) = nlname((i-1)*5+3)(1:8)
            end do

            AS_DEALLOCATE(vi=chindx)
            AS_DEALLOCATE(vi=flindx)
            call jelibe(trange(1:16)//'.NL.TYPE')
            call jelibe(trange(1:16)//'.NL.VIND')
            call jelibe(trange(1:16)//'.NL.INTI')
            call jelibe(trange(1:16)//'.NL.VINT')
!
            call wkvect('&&USURPU.WK1', 'V V R', nbpt, jwk1)
            call wkvect('&&USURPU.WK2', 'V V R', nbpt, jwk2)
            call wkvect('&&USURPU.WK3', 'V V R', nbpt, jwk3)
            call wkvect('&&USURPU.IWK4', 'V V I', nbpt, idwk4)
!
            call statpu(nbnoli, nbpt, disc, fcho, vcho,&
                        icho, zr(jwk1), zr(jwk2), zr(jwk3), zi(idwk4),&
                        idebut, nbloc, nbval, ifires, ichoc,&
                        impr, puusur)
!
            call wkvect(kforn, 'V V R', nbpt, jfn)
            call wkvect(kvgli, 'V V R', nbpt, jvg)
            call dcopy(nbpt, zr(jwk1), 1, zr(jfn), 1)
            do 20 i = 0, nbpt-1
                zr(jvg+i) = sqrt( zr(jwk2+i)**2 + zr(jwk3+i)**2 )
20          continue
!
            call jedetr('&&USURPU.WK1')
            call jedetr('&&USURPU.WK2')
            call jedetr('&&USURPU.WK3')
            call jedetr('&&USURPU.IWK4')


            AS_DEALLOCATE(vr =dloc)
            AS_DEALLOCATE(vr =fcho)
            AS_DEALLOCATE(vr =vcho)
            AS_DEALLOCATE(vi =icho)
            AS_DEALLOCATE(vk8=ncho)
            AS_DEALLOCATE(vk8=inti)

        else
            call utmess('F', 'PREPOST4_84')
        endif
    endif
!
999  continue
    call jedetr(nomno)
    call jedema()
end subroutine
