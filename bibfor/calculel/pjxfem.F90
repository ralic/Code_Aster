subroutine pjxfem(correz, ch1z, ch2z, tychv, prfchz,&
                  prol0, ligrez, base, modz, inst, iret)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
    implicit none
!-------------------------------------------------------------------
!     BUT : PROJETER UN CHAMP "CH1" SUIVANT "CORRES"
!           POUR CREER "CH2" SUR LA BASE "BASE"
!-------------------------------------------------------------------
!  IRET (OUT)  : = 0    : OK
!                = 1    : PB : ON N' A PAS PU PROJETER LE CHAMP
!                = 10   : ON NE SAIT PAS ENCORE FAIRE
!-------------------------------------------------------------------
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscno.h"
#include "asterfort/cnsprm.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/jexnum.h"
#include "asterfort/jenuno.h"
#include "asterfort/jenonu.h"
#include "asterfort/elref2.h"
#include "asterfort/cnscre.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexatr.h"
#include "asterfort/cesexi.h"
#include "asterfort/celces.h"
#include "asterfort/getvid.h"
#include "asterfort/res2mat.h"
#include "asterfort/iselli.h"
#include "asterfort/xellin.h"
#include "asterfort/elrfvf.h"
#include "asterfort/rccome.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/fointe.h"
#include "asterfort/xcalfev_wrap.h"
!
    character(len=*) :: correz, ch1z, ch2z, prfchz, ligrez, modz
    integer :: iret
    real(kind=8) :: inst
!
    integer :: ddlmax, nbnomax
    parameter (ddlmax=27,nbnomax=27)
    character(len=3) :: tsca
    character(len=8) :: ma1, ma2, nomgd
    character(len=19) :: ch1, ch2, cns1, cns2, prfchn, ligrel
    character(len=16) :: corres, notype
    character(len=8) :: modx, lirefe(10), elrefp, resu, elrefp2
    character(len=4) :: tych, tychv
    character(len=1) :: base
    character(len=*) :: prol0
    character(len=8) :: chmat
    integer :: nbno1, ncmp, gd, nbno2
    integer :: idecal, jcns2c, jcns2v, jcns2l, jconx1, jconx2, jcns1l, jcns1v, jcns2k
    integer :: itypel, nbelr, ima, ino2, ndim, nfe, nfh, cmp(ddlmax), nnop2
    integer :: ipos, ino1, iad, i, alp, ig, nuno1, iacoo1, stano(nbnomax), nn, iad2
    aster_logical :: vide, lvarc, cplan, poiss, young
    real(kind=8) :: dx(3), h1(3), ff(nbnomax), geom(nbnomax*3), baslo(nbnomax*9)
    real(kind=8) :: lsn(nbnomax), lst(nbnomax)
    real(kind=8) :: fk_escl(27,3,3), fk_mait(27,3,3), ka, mu, ff2(8)
    integer, pointer :: pjef_nu(:) => null()
    character(len=8), pointer :: cns1k(:) => null()
    integer, pointer :: pjef_nb(:) => null()
    integer, pointer :: pjef_m1(:) => null()
    character(len=8), pointer :: cns1c(:) => null()
    character(len=24), pointer :: pjxx_k1(:) => null()
    real(kind=8), pointer :: pjef_cf(:) => null()
    real(kind=8), pointer :: pjef_co(:) => null()
    integer, pointer :: cns1d(:) => null()
    integer, pointer :: cns2d(:) => null()
    integer, pointer :: maille(:) => null()
    integer :: jcesd_stno, jcesv_stno, jcesl_stno, jcesd_bslo, jcesv_bslo, jcesl_bslo
    integer :: jcesd_varc, jcesv_varc, jcesl_varc, nbvarc, k, nbf, ik, nbr, nbc, nbk, ier
    character(len=8) :: nommat
    character(len=11) :: k11
    character(len=19) :: chs_stno, chs_bslo, varcns
    real(kind=8) :: varc(2), e, nu
    character(len=8), pointer :: cvrcvarc(:) => null()
    character(len=16), pointer :: valk(:) => null()
!
!
    corres=correz
    ch1=ch1z
    ch2=ch2z
    prfchn=prfchz
    ligrel=ligrez
    modx=modz
    ASSERT(tychv.eq.'NOEU')
!
    cns1 = '&&PJXFEM'//'.CH1S'
    cns2 = '&&PJXFEM'//'.CH2S'
    iret = 0
!
!------------------------------------------------------------------
!     0- CONVERSTION VERS LE CHAMP_NO_S DE TRAVAIL :
!     ----------------------------------------------------
    call dismoi('TYPE_CHAMP', ch1, 'CHAMP', repk=tych)
    ASSERT(tych.eq.'NOEU')
    call cnocns(ch1, 'V', cns1)
!
!------------------------------------------------------------------
!     1- RECUPERATION DES OBJETS ET INFORMATIONS DE CNS1 :
!     ----------------------------------------------------
!
    call jeveuo(cns1//'.CNSK', 'L', vk8=cns1k)
    call jeveuo(cns1//'.CNSD', 'L', vi=cns1d)
    call jeveuo(cns1//'.CNSC', 'L', vk8=cns1c)
    call jeveuo(cns1//'.CNSV', 'L', jcns1v)
    call jeveuo(cns1//'.CNSL', 'L', jcns1l)
!
    ma1 = cns1k(1)
    nomgd = cns1k(2)
    nbno1 = cns1d(1)
    ncmp = cns1d(2)
!
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
!
!
!------------------------------------------------------------------
!     2- RECUPERATION DES OBJETS ET INFORMATIONS DE CORRES :
!     ----------------------------------------------------
    call jeveuo(corres//'.PJXX_K1', 'L', vk24=pjxx_k1)
    call jeveuo(corres//'.PJEF_NB', 'L', vi=pjef_nb)
    call jeveuo(corres//'.PJEF_NU', 'L', vi=pjef_nu)
    call jeveuo(corres//'.PJEF_CF', 'L', vr=pjef_cf)
    call jeveuo(corres//'.PJEF_M1', 'L', vi=pjef_m1)
    call jeveuo(corres//'.PJEF_CO', 'L', vr=pjef_co)
!
    ma2 = pjxx_k1(2)(1:8)
!
!
!------------------------------------------------------------------
!     3- QUELQUES VERIFS :
!     ------------------------
    if (tsca .ne. 'R' .and. tsca .ne. 'C') then
!        -- ON NE TRAITE QUE LES CHAMPS R/C :
        iret = 1
        goto 99
!
    endif
!     TEST SUR IDENTITE DES 2 MAILLAGES
    ASSERT(pjxx_k1(1).eq.ma1)
!
    call jenonu(jexnom('&CATA.GD.NOMGD', nomgd), gd)
    if (gd .eq. 0) then
        call utmess('F', 'CALCULEL_67', sk=nomgd)
    endif
!
!
!------------------------------------------------------------------
!     4- ALLOCATION DE CNS2 :
!     ------------------------
    call detrsd('CHAM_NO_S', cns2)
    call cnscre(ma2, nomgd, ncmp, cns1c, base,&
                cns2)
    call jeveuo(cns2//'.CNSK', 'L', jcns2k)
    call jeveuo(cns2//'.CNSD', 'L', vi=cns2d)
    call jeveuo(cns2//'.CNSC', 'L', jcns2c)
    call jeveuo(cns2//'.CNSV', 'E', jcns2v)
    call jeveuo(cns2//'.CNSL', 'E', jcns2l)
!
    nbno2 = cns2d(1)
!
!------------------------------------------------------------------
!     5- EXTRACTION DES DONNEES DE CALCUL :
!     ------------------------
    call dismoi('DIM_GEOM', ma1, 'MAILLAGE', repi=ndim)
    call jeveuo(ma1//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(ma1//'.CONNEX', 'LONCUM'), 'L', jconx2)
    call jeveuo(modx//'.MAILLE', 'L', vi=maille)
    call jeveuo(ma1//'.COORDO    .VALE', 'L', iacoo1)
    chs_stno = '&&PJXFEM.STANO'
    call celces(modx//'.STNO', 'V', chs_stno)
    call jeveuo(chs_stno//'.CESD', 'L', jcesd_stno)
    call jeveuo(chs_stno//'.CESV', 'L', jcesv_stno)
    call jeveuo(chs_stno//'.CESL', 'L', jcesl_stno)
    chs_bslo = '&&PJXFEM.BASLOC'
    call celces(modx//'.BASLOC', 'V', chs_bslo)
    call jeveuo(chs_bslo//'.CESD', 'L', jcesd_bslo)
    call jeveuo(chs_bslo//'.CESV', 'L', jcesv_bslo)
    call jeveuo(chs_bslo//'.CESL', 'L', jcesl_bslo)
    call getvid(' ', 'RESULTAT', scal=resu, nbret=nn)
    ASSERT(nn.eq.1)
    varcns='&&PJXFEM.VARC'
    call res2mat(resu, inst, chmat, mu=mu, ka=ka,&
                 nommat=nommat, lvarc=lvarc, varcns=varcns,&
                 cplan=cplan)
    if (lvarc) then
      call jeveuo(varcns//'.CESD', 'L', jcesd_varc)
      call jeveuo(varcns//'.CESV', 'L', jcesv_varc)
      call jeveuo(varcns//'.CESL', 'L', jcesl_varc)
      call rccome(nommat, 'ELAS', ier, k11_ind_nomrc=k11)  
      call jeexin(nommat//k11//'.VALR', ier)
      call jelira(nommat//k11//'.VALR', 'LONUTI', nbr)
      call jelira(nommat//k11//'.VALC', 'LONUTI', nbc)
      call jeveuo(nommat//k11//'.VALK', 'L', vk16=valk)
      call jelira(nommat//k11//'.VALK', 'LONUTI', nbk)
      nbf = (nbk-nbr-nbc)/2
      call jelira(chmat//'.CVRCVARC', 'LONMAX', nbvarc)
      ASSERT(nbvarc.le.2)
      call jeveuo(chmat//'.CVRCVARC', 'L', vk8=cvrcvarc)
    endif

!
!------------------------------------------------------------------
!     6- CALCUL DES VALEURS DE CNS2 :
!     -------------------------------
    idecal = 0
    do ino2 = 1, nbno2
        nbno1 = pjef_nb(ino2)
!        print*,' ***** KOR *****'
!        print*,' - nbno1 =',nbno1
        if (nbno1 .eq. 0) goto 50
        ima = pjef_m1(ino2)
!       ELEMENT DE REFERENCE ASSOCIE A LA MAILLE
        itypel = maille(ima)
        call jenuno(jexnum('&CATA.TE.NOMTE', itypel), notype)
        call elref2(notype, 10, lirefe, nbelr)
        elrefp= lirefe(1)
!        print*,' - elref =',elrefp
!        print*,' - nbno  =',nbno1
!        print*,' - ndim  =',ndim
!        print*,' - ncmp  =',ncmp
        cmp(:)=0
        ipos=0
        nfh=0
        nfe=0
        do i=1,ncmp
        vide=.true.
        do ino1 = 1, nbno1
            nuno1 = pjef_nu(1+idecal-1+ino1)
            if (zl(jcns1l-1+(nuno1-1)*ncmp + i)) vide=.false.
        enddo
        if (cns1c(i)(1:1) .eq. 'D') then
            ipos = ipos +1
            cmp(ipos)=i
        endif
        if (cns1c(i)(1:1) .eq. 'H'.and.cns1c(i)(3:3).ne.'P') then
            ipos = ipos +1
            if (.not.vide) nfh = nfh +1
            cmp(ipos)=i
        endif
        if (cns1c(i)(1:2) .eq. 'K1' .or. cns1c(i)(1:2) .eq. 'K2' .or.&
            cns1c(i)(1:2) .eq. 'K3') then
            ipos = ipos +1
            if (.not.vide) nfe = nfe +1
            cmp(ipos)=i
        endif
        enddo
        nfh=nfh/ndim
        nfe=nfe/ndim
!       
!        print*,' - nfh   =',nfh
!        print*,' - nfe   =',nfe
!        print*,' - cmp  =',cmp
!
        dx(:)=0.
        h1(:)=0.
        do ino1 = 1, nbno1
            ff(ino1)=pjef_cf(1+idecal-1+ino1)
        enddo
!       CALCUL DES PARAMETRES MATERIAUX
!         INTERPOLATION DES VARC
        if (lvarc) then
          varc(:)=0.
          do ino1 = 1, nbno1
            do k = 1, nbvarc
              call cesexi('C', jcesd_varc, jcesl_varc, ima, ino1,&
                           1, k, iad)
              ASSERT(iad.gt.0)
              varc(k)=varc(k)+zr(jcesv_varc-1+iad)*ff(ino1)
            enddo
          enddo
!        print*,' - varc =',varc
          poiss=.false.
          young=.false.
          do ik = 1, nbf
            if (valk(nbr+nbc+ik).eq.'NU') then
              call fointe('C', valk(nbr+nbc+nbf+ik), nbvarc, cvrcvarc(1:nbvarc), varc(1:nbvarc),&
                          nu, ier)
              if (ier.eq.0) poiss=.true.
            endif
            if (valk(nbr+nbc+ik).eq.'E') then
              call fointe('C', valk(nbr+nbc+nbf+ik), nbvarc, cvrcvarc(1:nbvarc), varc(1:nbvarc),&
                           e, ier)
              if (ier.eq.0) young=.true.
            endif
          enddo
          if (poiss) then 
            ka = 3.d0-4.d0*nu
            if (cplan) ka = (3.d0-nu)/(1.d0+nu)
            if (young) mu = e/(2.d0*(1.d0+nu))
          endif
        endif
        if (nfe.gt.0) then
!         EXTRACTION DE LSN,LST
!           POUR LE MOENET CA NE SERT A RIEN DANS XCALFEV
          lsn(1:nbno1)=0.
          lst(1:nbno1)=0.
!         EXTRACTION DU STANO
          nn = zi(jcesd_stno-1+5+4*(ima-1)+3)
          if (nn .gt. 0) then
            do ino1 = 1, nbno1
              call cesexi('C', jcesd_stno, jcesl_stno, ima, ino1,&
                           1, 1, iad)
              ASSERT(iad.gt.0)
              stano(ino1)=zi(jcesv_stno-1+iad)
            enddo
          else
             stano(1:nbno1)=-2
          endif
!         EXTRACTION DU BASLO
          nn = zi(jcesd_bslo-1+5+4*(ima-1)+3)
          if (nn.gt.0) then
            do ino1 = 1, nbno1
              do i = 1, 3*ndim
                 call cesexi('C', jcesd_bslo, jcesl_bslo, ima, ino1,&
                              1, i, iad)
                 ASSERT(iad.gt.0)
                 baslo((ino1-1)*3*ndim+i)=zr(jcesv_bslo-1+iad)
              enddo
            enddo
          else
             baslo(1:(nbno1*3*ndim))=0.d0
          endif
!         EXTRACTION DU GEOM
          do ino1 = 1, nbno1
              nuno1 = pjef_nu(1+idecal-1+ino1)
              do i = 1, ndim
                  geom(ndim*(ino1-1)+i)=zr(iacoo1-1+3*(nuno1-1)+i)
              end do
          end do
!        print*,' - stano  =',stano(1:nbno1)
!        print*,' - baslo  =',baslo(1:6*nbno1)
!         print*,' - ka, mu=',ka,mu
!         CALCUL DES FF LINEAIRE AU CAS OU ...
          ff2(:)=0.
          if (.not.iselli(elrefp)) then
              call xellin(elrefp, nbno1, elrefp2, nnop2) 
              call elrfvf(elrefp2, pjef_co((3*(ino1-1)+1):(3*(ino1-1)+ndim)),&
                          8, ff2, nnop2)
          endif
!         APPEL A XCALFEV EN PLUS ET MOINS
          fk_escl(:,:,:)=0.
          fk_mait(:,:,:)=0.
          call xcalfev_wrap(ndim, nbno1, baslo, stano, -1.d0,&
                     lsn, lst, geom, ka, mu, ff, fk_escl, face='ESCL',&
                     elref=elrefp, nnop2=nnop2, ff2=ff2)
          call xcalfev_wrap(ndim, nbno1, baslo, stano, +1.d0,&
                     lsn, lst, geom, ka, mu, ff, fk_mait, face='MAIT',&
                     elref=elrefp, nnop2=nnop2, ff2=ff2)
        endif
!
        do ino1 = 1, nbno1
            nuno1 = pjef_nu(1+idecal-1+ino1)
            ipos  = 0
            iad   = jcns1v-1+ (nuno1-1)*ncmp
            iad2  = jcns1l-1+ (nuno1-1)*ncmp
!         DDLS CLASSIQUES
            do i = 1, ndim
                ipos=ipos+1
                dx(i) = dx(i) + ff(ino1) * zr(iad+cmp(ipos))
            enddo
!         DDLS HEAVISIDE
            do ig = 1, nfh
                do i = 1, ndim
                    ipos=ipos+1
                    if (.not.zl(iad2+cmp(ipos))) goto 21
                    h1(i) = h1(i) + ff(ino1) * zr(iad+cmp(ipos))
21                  continue
                enddo
            enddo
!         DDL ENRICHIS EN FOND DE FISSURE
            do ig = 1, nfe
                do alp = 1, ndim
                    ipos=ipos+1
                    if (.not.zl(iad2+cmp(ipos))) goto 22
                    do i = 1, ndim
                        dx(i) = dx(i) + (fk_mait(ino1,alp,i)+fk_escl(ino1,alp,i))/2.&
                                        *zr(iad+cmp(ipos))
                        h1(i) = h1(i) + (fk_mait(ino1,alp,i)-fk_escl(ino1,alp,i))/2.&
                                        *zr(iad+cmp(ipos))
                    enddo
22                  continue
                enddo
            enddo
         enddo
!        print*,' + dx  =',dx
!        print*,' + h1  =',h1
!        print*,' ***************'
!       COPIE DU RESULTAT DANS LES DDLS DX,DY,DZ ET H1X,H1Y,H1Z
        do i = 1, ndim
            zl(jcns2l-1+ (ino2-1)*ncmp+cmp(i)) = .true.
            zr(jcns2v-1+(ino2-1)*ncmp+cmp(i))=dx(i)
            zl(jcns2l-1+ (ino2-1)*ncmp+cmp(i+ndim)) = .true.
            zr(jcns2v-1+(ino2-1)*ncmp+cmp(i+ndim))=h1(i)
        enddo
        idecal = idecal + nbno1
 50     continue
    end do
!
!------------------------------------------------------------------
!     7- CONVERSTION VERS LE CHAMP_NO RESULTAT :
!     ----------------------------------------------------
     call cnscno(cns2, prfchn, prol0, base, ch2,&
                 'A', iret)
!
!------------------------------------------------------------------
!     8- NETTOYAGE DES OBJETS DE TRAVAIL :
!     -------------------------------
99  continue
!
    call detrsd('CHAM_NO_S', cns1)
    call detrsd('CHAM_NO_S', cns2)
    call detrsd('CHAM_ELEM_S', chs_stno)
    call detrsd('CHAM_ELEM_S', chs_bslo)
    if (lvarc) call detrsd('CHAM_NO_S', varcns)
!
end subroutine
