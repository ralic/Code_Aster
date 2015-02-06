subroutine xcelno(noma, modelx, cel_hno, opt, npa)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/dismoi.h"
#include "asterfort/detrsd.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cesexi.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeexin.h"
#include "asterfort/xcalc_code.h"
#include "asterfort/xtest_code.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
!
    character(len=8) :: noma, modelx
    character(len=19) :: cel_hno
    character(len=*) :: opt, npa
!
! ----------------------------------------------------------------------
! ROUTINE XFEM (METHODE XFEM - PREPARATION)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! BUT :: AJOUTER À LA SD FISS_XFEM LES IDENTIFIANTS DES DOMAINES VUS 
!           PAR CHAQUE NOEUD X-FEM
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  REMARQUES:
!  ** ON DEFINIT UNE CORRESPONDANCE ENTRE UNE FONCTION HEAVISIDE ET UN NUMERO DE DOMAINE
!  ** CETTE CORRESPONDANCE N A DE SENS QUE D UN POINT DE VUE ELEMENTAIRE
!     DONC ON NE CONSTRUIT PAS UNE IDENTIFICATION GLOBALE DES DOMAINES DE DISCONTINUITE
!     AU SENS D HANSBO SAUF BIEN ENTENDU QUAND NFISS=1
! ----------------------------------------------------------------------
!
    integer :: nbno, nbma, ibid, ino, nuno, numa, nusd
    integer :: nbpt, ncmp, deca, ilcnx1, ifh, nncp
    integer :: jcesd, jcesv, jcesl, iad
    integer :: jcesd_fno, jcesv_fno, jcesl_fno, iad_fno
    integer :: ier, nfh, cpt, ncompn, deca_fno, iad2, pos, iad5, id1, id2
    character(len=19) :: ces_hno, ces_fno
    aster_logical :: lfno, limpr
    aster_logical, pointer :: is_nfh_no(:) => null()
    integer, pointer :: list_sd_no(:) => null(), count_sd_no(:) => null(), connex(:) => null()
    integer, pointer :: tmp_fno(:) => null(), nfh_ref(:) => null()
    integer :: nfissmax
    parameter (nfissmax=4)
    integer :: fisno(nfissmax), ifiss
    integer :: nbsd, i, tmp_pos(nfissmax+1),tmp_id(nfissmax+1)
    aster_logical, pointer :: is_ma_xfem(:) => null(), is_no_mono(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    limpr=.false.
!
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbno)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
!
    AS_ALLOCATE(vl=is_ma_xfem,size=nbma) 
!
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
    call jeveuo(noma//'.CONNEX', 'L', vi=connex)
    ces_hno = '&&XCELNO.HNO'
    call celces(cel_hno, 'V', ces_hno)
    call jeveuo(ces_hno//'.CESD', 'L', jcesd)
    call jeveuo(ces_hno//'.CESL', 'L', jcesl)
    call jeveuo(ces_hno//'.CESV', 'L', jcesv)
!
    lfno=.false.
    call jeexin(modelx(1:8)//'.FISSNO    .CELD', ier)
    if (ier.ne.0) then
       lfno=.true.
       ces_fno='&&XCELNO.FISSNO'
       call celces(modelx(1:8)//'.FISSNO', 'V', ces_fno)
       call jeveuo(ces_fno//'.CESL', 'L', jcesl_fno)
       call jeveuo(ces_fno//'.CESD', 'L', jcesd_fno)
       call jeveuo(ces_fno//'.CESV', 'L', jcesv_fno)
    endif 
!
    id1=xcalc_code(1,he_inte=[-1])
    id2=xcalc_code(1,he_inte=[+1])
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   BOUCLE 1: * MARQUAGE DES MAILLES XFEM IMPACTEES PAR L OPTION TOPONO
!             * REPERAGE DES NOEUDS DE TRANSITION (EN MULTI-HEAVISIDE)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    AS_ALLOCATE(vl=is_nfh_no,size=nbno*nfissmax)
    deca_fno=nfissmax**2
    nfh=1
    AS_ALLOCATE(vl=is_no_mono,size=nbno)
    is_no_mono(1:nbno)=.true.
!
    do numa = 1, nbma
      nbpt = zi(jcesd-1+5+4* (numa-1)+1)
      ncmp  = zi(jcesd-1+5+4* (numa-1)+3)
      if (ncmp .eq. 0) goto 5
      is_ma_xfem(numa)=.true. 
      if (lfno) nfh = zi(jcesd_fno-1+5+4* (numa-1)+2)
      do ino =1, nbpt
        nuno = connex(1+zi(ilcnx1-1+numa)-2+ino)
        is_nfh_no(nfissmax*(nuno-1)+nfh)=.true.
      enddo
5   continue
    enddo
!
    AS_ALLOCATE(vi=nfh_ref,size=nbno) 
    do nuno=1,nbno
       do cpt=1,nfissmax
         if (is_nfh_no(nfissmax*(nuno-1)+cpt)) then
           nfh_ref(nuno)=cpt 
           goto 8
         endif
       enddo
8   continue
    if (nfh_ref(nuno).gt.1) is_no_mono(nuno)=.false.
    enddo
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   BOUCLE 2: ** REMPLISSAGE DES LISTES DE DOMAINES PAR NOEUD **
!    - EN DERNIERE POSITION ON STOCKE LE DOM. AUQUEL APPARTIENT LE NOEUD
!    - ENSUITE ON REMPLIT LES EMPLACEMENTS AVEC LES DOM. EN REPRENANT AU DEBUT DE LA LISTE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ncompn=nfissmax+1
    deca=ncompn
    AS_ALLOCATE(vi=list_sd_no,size=nbno*deca) 
    AS_ALLOCATE(vi=count_sd_no,size=nbno)
    deca_fno=ncompn*nfissmax
    AS_ALLOCATE(vi=tmp_fno,size=nbno*deca_fno) 
    fisno(1)=1
    fisno(2:nfissmax)=0
    nfh=1
!
    do numa = 1, nbma
      if (.not.is_ma_xfem(numa)) goto 10
      nbpt = zi(jcesd-1+5+4* (numa-1)+1)
      ncmp  = zi(jcesd-1+5+4* (numa-1)+3)
      if (lfno) nfh = zi(jcesd_fno-1+5+4* (numa-1)+2)
      do ino =1, nbpt
!
        nuno = connex(1+zi(ilcnx1-1+numa)-2+ino)
        if (count_sd_no(nuno).ge.nfh_ref(nuno)) goto 11
        if (lfno) then
          fisno(1:nfissmax)=0
          do cpt=1,nfh
            call cesexi('C', jcesd_fno, jcesl_fno, numa, ino,&
                         cpt, 1, iad_fno)
            if (iad_fno.le.0) then
               ASSERT(nfh.eq.1)
               fisno(1)=1
            else
               ifiss=zi(jcesv_fno-1+iad_fno)
               if (ifiss.le.0) then
                  ASSERT(nfh.eq.1)
                  fisno(1)=1
               else
                  fisno(cpt)=zi(jcesv_fno-1+iad_fno)
               endif
            endif
          enddo
        endif
! STOCKAGE DU DOMAINE AUQUEL APPARTIENT LE NOEUD
        call cesexi('C', jcesd, jcesl, numa, ino,&
                                1, ncompn, iad)
        nusd=zi(jcesv-1+iad)        
        if (nusd.le.0) goto 11
        if (list_sd_no(deca*(nuno-1)+ncompn).le.0) then
          list_sd_no(deca*(nuno-1)+ncompn)=nusd
          do cpt=1,nfh
            tmp_fno(deca_fno*(nuno-1)+nfissmax*(ncompn-1)+cpt)=fisno(cpt)
          enddo
        else
          if (xtest_code(nusd, list_sd_no(deca*(nuno-1)+ncompn), lfno, nfh_ref(nuno), nfissmax,&
                       fisno,&
                       tmp_fno((deca_fno*(nuno-1)+nfissmax*(ncompn-1)+1):&
                                  (deca_fno*(nuno-1)+nfissmax*(ncompn-1)+nfissmax)))) goto 12
          do cpt=1,count_sd_no(nuno)
            if (xtest_code(nusd, list_sd_no(deca*(nuno-1)+cpt), lfno, nfh_ref(nuno), nfissmax, &
                           fisno,&
                           tmp_fno((deca_fno*(nuno-1)+nfissmax*(cpt-1)+1):&
                                (deca_fno*(nuno-1)+nfissmax*(cpt-1)+nfissmax)))) goto 12
          enddo
          count_sd_no(nuno)=count_sd_no(nuno)+1
          list_sd_no(deca*(nuno-1)+count_sd_no(nuno))=nusd
          if (.not.lfno) goto 12
          do cpt=1,nfh
            tmp_fno(deca_fno*(nuno-1)+nfissmax*(count_sd_no(nuno)-1)+cpt)=fisno(cpt)
          enddo
        endif
12      continue
        if (count_sd_no(nuno).ge.nfh_ref(nuno)) goto 11
! STOCKAGE DES AUTRES DOMAINES
        do ifh=1,(ncmp-1)
          call cesexi('C', jcesd, jcesl, numa, ino,&
                                1, ifh, iad)
          nusd=zi(jcesv-1+iad)
          if (nusd.le.0) goto 11
          ASSERT(list_sd_no(deca*(nuno-1)+ncompn).gt.0)
          if (xtest_code(nusd, list_sd_no(deca*(nuno-1)+ncompn), lfno, nfh_ref(nuno), nfissmax,&
                       fisno,&
                       tmp_fno((deca_fno*(nuno-1)+nfissmax*(ncompn-1)+1):&
                                  (deca_fno*(nuno-1)+nfissmax*(ncompn-1)+nfissmax)))) goto 13
          do cpt=1,count_sd_no(nuno)
            if (xtest_code(nusd, list_sd_no(deca*(nuno-1)+cpt), lfno, nfh_ref(nuno), nfissmax,&
                           fisno,&
                           tmp_fno((deca_fno*(nuno-1)+nfissmax*(cpt-1)+1):&
                                (deca_fno*(nuno-1)+nfissmax*(cpt-1)+nfissmax)))) goto 13
          enddo
          count_sd_no(nuno)=count_sd_no(nuno)+1
          list_sd_no(deca*(nuno-1)+count_sd_no(nuno))=nusd
          if (.not.lfno) goto 13
          do cpt=1,nfh
            tmp_fno(deca_fno*(nuno-1)+nfissmax*(count_sd_no(nuno)-1)+cpt)=fisno(cpt)
          enddo
13        continue
          if (count_sd_no(nuno).ge.nfh_ref(nuno)) goto 11
        enddo
!
11    continue
!
      enddo
!
10  continue
!
    enddo
!
! AFFICHAGES: 
!   
    if (limpr) then
      write(6,*)'KORUPTION : LISTE DES NOEUDS ET DES DOMAINES'
      do nuno=1,nbno
      write(6,*)'nuno=',nuno
      write(6,*)'   - nfh_ref=',nfh_ref(nuno)
      write(6,*)'   - list_sd=', list_sd_no((deca*(nuno-1)+1):(deca*(nuno-1)+nfh_ref(nuno)))
      enddo
    endif
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   BOUCLE 3: MODIFICATION IN-SITU DU CHAM_ELNO_S AVEC LA NOUVELLE LISTE DES SOUS-D.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    nfh=1
    do numa = 1, nbma
      if ( .not. is_ma_xfem(numa)) goto 100
      nbpt = zi(jcesd-1+5+4* (numa-1)+1)
      ncmp = zi(jcesd-1+5+4* (numa-1)+3)
      if (lfno) nfh = zi(jcesd_fno-1+5+4* (numa-1)+2)
      do ino =1, nbpt
        nuno = connex(1+zi(ilcnx1-1+numa)-2+ino)
        nbsd=0
        if (lfno) then
          fisno(1:nfissmax)=0
          do cpt=1,nfh
            call cesexi('C', jcesd_fno, jcesl_fno, numa, ino,&
                         cpt, 1, iad_fno)
            if (iad_fno.le.0) then
               ASSERT(nfh.eq.1)
               fisno(1)=1               
            else
               ifiss=zi(jcesv_fno-1+iad_fno)
               if (ifiss.le.0) then
                  ASSERT(nfh.eq.1)
                  fisno(1)=1
               else
                  fisno(cpt)=zi(jcesv_fno-1+iad_fno)
               endif
            endif
          enddo
        endif
! REPOSITIONNEMENT DU DOMAINE AUQUEL APPARTIENT LE NOEUD
        call cesexi('C', jcesd, jcesl, numa, ino,&
                                1, ncompn, iad5)
        nusd=zi(jcesv-1+iad5) 
        pos=-1
        if (nusd.le.0) goto 101
        if (xtest_code(nusd, list_sd_no(deca*(nuno-1)+ncompn), lfno, nfh_ref(nuno), nfissmax,&
                       fisno,& 
                       tmp_fno((deca_fno*(nuno-1)+nfissmax*(ncompn-1)+1):&
                                (deca_fno*(nuno-1)+nfissmax*(ncompn-1)+nfissmax)))) then
           pos=ncompn
           goto 101
        endif
        do cpt=1,count_sd_no(nuno)
          if (xtest_code(nusd, list_sd_no(deca*(nuno-1)+cpt), lfno, nfh_ref(nuno), nfissmax,&
                         fisno,& 
                         tmp_fno((deca_fno*(nuno-1)+nfissmax*(cpt-1)+1):&
                                (deca_fno*(nuno-1)+nfissmax*(cpt-1)+nfissmax)))) then
            pos=cpt
            goto 101
          endif
        enddo
101     continue 
        if (pos.gt.0.and.pos.ne.ncompn) then
          nbsd=nbsd+1
          tmp_pos(nbsd)=pos
          zi(jcesv-1+iad5)=-1
          tmp_id(nbsd)=nusd
        endif
! REPOSITIONNEMENT DES AUTRES DOMAINES
        do ifh=1,(ncmp-1)
          call cesexi('C', jcesd, jcesl, numa, ino,&
                                1, ifh, iad)
          nusd=zi(jcesv-1+iad)
          pos=-1          
          if (nusd.le.0) goto 102
          if (xtest_code(nusd, list_sd_no(deca*(nuno-1)+ncompn), lfno, nfh_ref(nuno), nfissmax,&
                       fisno,& 
                       tmp_fno((deca_fno*(nuno-1)+nfissmax*(ncompn-1)+1):&
                                  (deca_fno*(nuno-1)+nfissmax*(ncompn-1)+nfissmax)))) then
             pos=ncompn
             goto 102
          endif
          do cpt=1,count_sd_no(nuno)
            if (xtest_code(nusd, list_sd_no(deca*(nuno-1)+cpt), lfno, nfh_ref(nuno), nfissmax,&
                           fisno,&
                           tmp_fno((deca_fno*(nuno-1)+nfissmax*(cpt-1)+1):&
                                (deca_fno*(nuno-1)+nfissmax*(cpt-1)+nfissmax)))) then
              pos=cpt
              goto 102
            endif
          enddo 
102       continue
          if (pos.gt.0.and.pos.ne.ifh) then
            nbsd=nbsd+1
            tmp_pos(nbsd)=pos
            tmp_id(nbsd)=nusd
            zi(jcesv-1+iad)=-1
          endif
        enddo
!  ECRITURE DES DOMAINES A LA POSITION CALCULEE
        do i=1,nbsd
          call cesexi('C', jcesd, jcesl, numa, ino,&
                       1, tmp_pos(i), iad2)
          zi(jcesv-1+iad2)=tmp_id(i)
        enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   BRICOLAGE POUR LE MONO-HEAVISIDE : 
!    ON RAJOUTE UN FLAG POUR OBTENIR POUR OBTENIR UN SAUT CONSTANT 
!    SI HE=-1  <=> NUSD=ID1 : ON IMPOSE XCALC_HEAV=+2 EN TRANSFORMANT NUSD=>-999
!    SI HE=+1  <=> NUSD=ID2 : ON IMPOSE XCALC_HEAV=-2 EN TRANSFORMANT NUSD=>+999
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        if (is_no_mono(nuno)) then
           if (xtest_code(id1, list_sd_no(deca*(nuno-1)+ncompn), lfno, 1, 4,&
                         [1,0,0,0],&
                         tmp_fno((deca_fno*(nuno-1)+nfissmax*(ncompn-1)+1):&
                                  (deca_fno*(nuno-1)+nfissmax*(ncompn-1)+4))))&
              zi(jcesv-1+iad5)=-999
           if (xtest_code(id2, list_sd_no(deca*(nuno-1)+ncompn), lfno, 1, 4,&
                         [1,0,0,0],&
                         tmp_fno((deca_fno*(nuno-1)+nfissmax*(ncompn-1)+1):&
                                  (deca_fno*(nuno-1)+nfissmax*(ncompn-1)+4))))&
              zi(jcesv-1+iad5)=999
        endif
!
      enddo      
!
100 continue
!
    enddo
!
    call detrsd('CHAM_ELEM',cel_hno)
    call cescel(ces_hno, modelx//'.MODELE', opt, npa, 'OUI',&
                nncp, 'G', cel_hno, 'F', ibid)
!
    AS_DEALLOCATE(vl=is_ma_xfem) 
    AS_DEALLOCATE(vl=is_nfh_no)
    AS_DEALLOCATE(vi=list_sd_no) 
    AS_DEALLOCATE(vi=count_sd_no)
    AS_DEALLOCATE(vi=tmp_fno)
    AS_DEALLOCATE(vi=nfh_ref)
    AS_DEALLOCATE(vl=is_no_mono)
!
    call jedema()
end subroutine
