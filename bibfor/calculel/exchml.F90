subroutine exchml(imodat, iparg)
    implicit none
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
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/chloet.h"
#include "asterfort/exisdg.h"
#include "asterfort/jacopo.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
!
    integer :: iparg, imodat
! ----------------------------------------------------------------------
!     ENTREES:
!        IMODAT : MODE LOCAL ATTENDU
!        IPARG  : NUMERO DU PARAMETRE DANS L'OPTION
!
! ----------------------------------------------------------------------
    integer :: igd, nec, ncmpmx, iachin, iachlo, iichin, ianueq, lprno
    integer :: ilchlo, itypgd
    common /caii01/igd,nec,ncmpmx,iachin,iachlo,iichin,ianueq,lprno,&
     &       ilchlo,itypgd
    common /cakk02/typegd
    character(len=8) :: typegd
    common /caii04/iachii,iachik,iachix
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
!
!
!     FONCTIONS EXTERNES:
!     ------------------
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: jceld, mode, debgr2, lggre2, iaux1
    integer :: itypl1, modlo1, nbpoi1, lgcata
    integer :: itypl2, modlo2, nbpoi2
    integer :: ilopmo, iaopmo, ilopno, iaopds, iaoppa, npario, nparin, iamloc
    integer :: ilmloc, iadsgd, iel
    integer :: iachii
    integer :: ncmp1, ncmp2
    integer :: iachik, iachix, iaoptt, lgco, iaopno, iret, debugr, lggrel
    integer :: jec, ncmp, jad1, jad2, jel, ipt2, k, ipt1, jparal
    integer :: nbpoi, icmp1, icmp2, kcmp, ipt
    logical :: etendu, lparal, lverec
    character(len=8) :: tych, cas
! DEB-------------------------------------------------------------------
!
!     PARALLELE OR NOT ?
!     -------------------------
    call jeexin('&CALCUL.PARALLELE', iret)
    if (iret .ne. 0) then
        lparal=.true.
        call jeveuo('&CALCUL.PARALLELE', 'L', jparal)
    else
        lparal=.false.
    endif
!
    tych=zk8(iachik-1+2*(iichin-1)+1)
    ASSERT(tych(1:4).eq.'CHML')
!
    jceld=zi(iachii-1+11*(iichin-1)+4)
    lggre2=zi(jceld-1+zi(jceld-1+4+igr)+4)
    debgr2=zi(jceld-1+zi(jceld-1+4+igr)+8)
!
    mode=zi(jceld-1+zi(jceld-1+4+igr)+2)
!
    lgcata=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+2)
    lggrel=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+4)
    debugr=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+5)
!
!
!     -- SI MODE=0 : IL FAUT METTRE CHAMP_LOC.EXIS A .FALSE.
    if (mode .eq. 0) then
        do 30,k=1,lggrel
        zl(ilchlo-1+debugr-1+k)=.false.
30      continue
        goto 170
    endif
!
!
!     -- SI LE CHAMP A LE MODE ATTENDU : ON RECOPIE
!     ----------------------------------------------------
    if (mode .eq. imodat) then
        call jacopo(lggrel, typegd, iachin-1+debgr2, iachlo+debugr-1)
        goto 9998
    endif
!
!
!     -- SI LE CHAMP N'A PAS LE MODE ATTENDU ...
!     ----------------------------------------------------
    call chloet(iparg, etendu, jceld)
    if (etendu) then
        call utmess('F', 'CALCULEL2_51')
    endif
!
!
    modlo1=iamloc-1+zi(ilmloc-1+mode)
    modlo2=iamloc-1+zi(ilmloc-1+imodat)
    itypl1=zi(modlo1-1+1)
    itypl2=zi(modlo2-1+1)
    ASSERT(itypl1.le.3)
    ASSERT(itypl2.le.3)
    nbpoi1=zi(modlo1-1+4)
    nbpoi2=zi(modlo2-1+4)
!
    ncmp1=lggre2/(nbpoi1*nbelgr)
    ncmp2=lgcata/nbpoi2
!
!     -- ON VERIFIE QUE LES POINTS NE SONT PAS "DIFF__" :
    ASSERT(nbpoi1.lt.10000)
    ASSERT(nbpoi2.lt.10000)
!
!
!
!
!     -- DANS QUEL CAS DE FIGURE SE TROUVE-T-ON ?
!     --------------------------------------------
    lverec=.true.
    if (nbpoi1 .eq. nbpoi2) then
        if (ncmp1 .eq. ncmp2) then
!         -- LE CAS "COPIE" EST BIZARRE : IL S'AGIT DE 2 MODES LOCAUX
!            DE MEME CONTENU MAIS DE NOMS DIFFERENTS.
!            FAUT-IL L'INTERDIRE ?
            cas='COPIE'
            ncmp=ncmp1
!         -- QUELQUES VERIFICATIONS :
            ASSERT(itypl1.eq.itypl2)
!         -- POUR LES CHAMPS ELGA, ON VERIFIE QUE C'EST LA MEME FAMILLE
            if (itypl1 .eq. 3) then
                ASSERT(zi(modlo1+4+nec).eq.zi(modlo2+4+nec))
            endif
        else
            cas='TRICMP'
            lverec=.false.
        endif
    else
        ASSERT(ncmp1.eq.ncmp2)
        ncmp=ncmp1
!
        if (nbpoi1 .eq. 1) then
            cas='EXPAND'
        else if (nbpoi2.eq.1) then
            cas='MOYENN'
        else
            ASSERT(.false.)
        endif
    endif
!
    if (lverec) then
!       -- ON VERIFIE QUE LES CMPS SONT LES MEMES:
!          (SINON IL FAUDRAIT TRIER ... => A FAIRE (TRIGD) )
        do 40,jec=1,nec
        ASSERT(zi(modlo1-1+4+jec).eq.zi(modlo2-1+4+jec))
40      continue
    endif
!
!
!
!     -- CAS "EXPAND" OU "COPIE":
!     ---------------------------
    if (cas .eq. 'EXPAND' .or. cas .eq. 'COPIE') then
        do 60,jel=1,nbelgr
        if (lparal) then
            if (.not.zl(jparal-1+jel)) goto 60
        endif
        if (cas .eq. 'EXPAND') then
            jad1=iachin-1+debgr2+(jel-1)*ncmp
            do 50,ipt2=1,nbpoi2
            jad2=iachlo+debugr-1+((jel-1)*nbpoi2+ipt2-1)*ncmp
            call jacopo(ncmp, typegd, jad1, jad2)
50          continue
        else if (cas.eq.'COPIE') then
            ASSERT(nbpoi1.eq.nbpoi2)
            jad1=iachin-1+debgr2+(jel-1)*ncmp*nbpoi1
            jad2=iachlo-1+debugr+(jel-1)*ncmp*nbpoi1
            call jacopo(ncmp*nbpoi1, typegd, jad1, jad2)
        endif
60      continue
!
!
!     -- CAS "TRICMP":
!     ---------------------------
    else if (cas.eq.'TRICMP') then
        nbpoi=nbpoi1
        icmp1=0
        icmp2=0
        do 52,kcmp=1,ncmpmx
        if (exisdg(zi(modlo2-1+5),kcmp)) then
            icmp2=icmp2+1
            if (exisdg(zi(modlo1-1+5),kcmp)) then
                icmp1=icmp1+1
            else
!             -- A FAIRE ... (GESTION DE ZL)
                ASSERT(.false.)
            endif
        else
            goto 52
        endif
        ASSERT(icmp1.ge.1 .and. icmp1.le.ncmp1)
        ASSERT(icmp2.ge.1 .and. icmp2.le.ncmp2)
        do 61,jel=1,nbelgr
        if (lparal) then
            if (.not.zl(jparal-1+jel)) goto 61
        endif
!
        do 51,ipt=1,nbpoi
        jad1=iachin+debgr2-1+((jel-1)*nbpoi+ipt-1)*ncmp1
        jad2=iachlo+debugr-1+((jel-1)*nbpoi+ipt-1)*ncmp2
        jad1=jad1-1+icmp1
        jad2=jad2-1+icmp2
        call jacopo(1, typegd, jad1, jad2)
51      continue
61      continue
52      continue
!
!
!     -- CAS "MOYENN" :
!     ------------------------
    else if (nbpoi2.eq.1) then
!
        if (typegd .eq. 'R') then
            if (lparal) then
                do 80 iel = 1, nbelgr
                    if (zl(jparal-1+iel)) then
                        iaux1=iachlo+debugr-1+(iel-1)*ncmp
                        do 70 k = 1, ncmp
                            zr(iaux1-1+k)=0.d0
70                      continue
                    endif
80              continue
            else
                do 90,k=1,nbelgr*ncmp
                zr(iachlo+debugr-1-1+k)=0.d0
90              continue
            endif
        else if (typegd.eq.'C') then
            if (lparal) then
                do 110 iel = 1, nbelgr
                    if (zl(jparal-1+iel)) then
                        iaux1=iachlo+debugr-1+(iel-1)*ncmp
                        do 100 k = 1, ncmp
                            zc(iaux1-1+k)=(0.d0,0.d0)
100                      continue
                    endif
110              continue
            else
                do 120,k=1,nbelgr*ncmp
                zc(iachlo+debugr-1-1+k)=(0.d0,0.d0)
120              continue
            endif
        else
            ASSERT(.false.)
        endif
!
        do 150,jel=1,nbelgr
        if (lparal) then
            if (.not.zl(jparal-1+jel)) goto 150
        endif
        jad2=iachlo+debugr-1+(jel-1)*ncmp
        do 140,ipt1=1,nbpoi1
        jad1=iachin-1+debgr2+((jel-1)*nbpoi1+ipt1-1)*ncmp
        do 130,k=0,ncmp-1
        if (typegd .eq. 'R') then
            zr(jad2+k)=zr(jad2+k)+zr(jad1+k)/dble(nbpoi1)
        else if (typegd.eq.'C') then
            zc(jad2+k)=zc(jad2+k)+zc(jad1+k)/dble(nbpoi1)
        endif
130      continue
140      continue
150      continue
!
!
!     -- AUTRES CAS PAS ENCORE PROGRAMMES :
    else
        ASSERT(.false.)
    endif
!
!
9998  continue
    do 160,k=1,lggrel
    zl(ilchlo-1+debugr-1+k)=.true.
    160 end do
!
170  continue
end subroutine
