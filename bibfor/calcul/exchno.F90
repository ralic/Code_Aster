subroutine exchno(imodat, iparg)

use calcul_module, only : ca_iachii_, ca_iachlo_, ca_ialiel_, ca_iamaco_,&
     ca_iamloc_, ca_iamsco_, ca_iawlo2_, ca_igr_,&
     ca_iichin_, ca_illiel_, ca_ilmaco_, ca_ilmloc_, ca_ilmsco_, &
     ca_nbelgr_, ca_nbgr_, ca_nec_, ca_typegd_, ca_lparal_, ca_paral_, ca_iel_

implicit none

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
! person_in_charge: jacques.pellet at edf.fr

#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/trigd.h"
#include "asterfort/utmess.h"

    integer :: imodat, iparg
!----------------------------------------------------------------------
!     entrees:
!        imodat  : indice dans la collection modeloc
!        igr    : numero du grel a traiter.
!     sorties:
!        ecriture dans le champ local
!----------------------------------------------------------------------
    integer :: ima, ino, nno, long, nugl, num
    integer :: desc, prno1, prno2, modloc, ityplo
    integer :: deb1, deb2, idg1, idg2, nbpt, nbpt2, lgcata, ncmp
    integer :: iaux1, k, iec, debugr
    aster_logical :: diff, moyenn

!     -- fonctions formules :
!     numail(igr,iel)=numero de la maille associee a l'element iel
#define numail(ca_igr_,ca_iel_) zi(ca_ialiel_-1+zi(ca_illiel_+ca_igr_-1)+ca_iel_-1)
!     numglm(ima,ino)=numero global du noeud ino de la maille ima
!                     ima etant une maille du maillage.
#define numglm(ima,ino) zi(ca_iamaco_-1+zi(ca_ilmaco_+ima-1)+ino-1)
!     numgls(ima,ino)=numero global du noeud ino de la maille ima
!                     ima etant une maille supplementaire du ligrel
#define numgls(ima,ino) zi(ca_iamsco_-1+zi(ca_ilmsco_+ima-1)+ino-1)
!-------------------------------------------------------------------

    desc=zi(ca_iachii_-1+11*(ca_iichin_-1)+4)
    num=zi(desc-1+2)
    modloc=ca_iamloc_-1+zi(ca_ilmloc_-1+imodat)
    ityplo=zi(modloc-1+1)
    debugr=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+5)
    lgcata=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+2)

    ASSERT(ityplo.lt.4)


!   1-  cas: chno -> elga :
!   -----------------------
!   le cas ityplo=3 n est pas prevu : developpement a faire ...
    ASSERT(ityplo.ne.3)


!   2-  cas: chno -> elno :
!       cas: chno -> elem (moyenne)
!   --------------------------------
    if ((ityplo.eq.2) .or. (ityplo.eq.1)) then
        if (ityplo .eq. 2) then
            moyenn=.false.
        else
            moyenn=.true.
        endif


!       2.1 on cherche nno sur le 1er element :
!       ---------------------------------------
        ima=numail(ca_igr_,1)
        ASSERT(ima.ne.0)
        if (ima .gt. 0) then
            nno=zi(ca_ilmaco_-1+ima+1)-zi(ca_ilmaco_-1+ima)
        else
            nno=zi(ca_ilmsco_-1-ima+1)-zi(ca_ilmsco_-1-ima)-1
        endif


!       2.2 on recupere le debut du descripteur grandeur :
!       --------------------------------------------------
        nbpt=zi(modloc-1+4)
        nbpt2=mod(nbpt,10000)
        if (nbpt .ne. nbpt2) then
            diff=.true.
        else
            diff=.false.
            idg2=5
        endif

!       moyenn => (nbpt2=1)
        ASSERT((.not.moyenn) .or. (nbpt2.eq.1))

!       .not.moyenn => (nbpt2=nno)
        ASSERT(moyenn .or. (nbpt2.eq.nno))


!       2.3 si moyenn, il faut mettre a zero le champ local
!           (pour pouvoir cumuler)
!       --------------------------------------------------
        if (moyenn) then
            ncmp=lgcata
            if (ca_typegd_ .eq. 'R') then
                if (ca_lparal_) then
                    do 20 ca_iel_ = 1, ca_nbelgr_
                        if (ca_paral_(ca_iel_)) then
                            iaux1=ca_iachlo_+debugr-1+(ca_iel_-1)*ncmp
                            do 10 k = 1, ncmp
                                zr(iaux1-1+k)=0.d0
 10                         continue
                        endif
 20                 continue
                else
                    do 30 k = 1, ca_nbelgr_*ncmp
                        zr(ca_iachlo_+debugr-1-1+k)=0.d0
 30                 continue
                endif
            else if (ca_typegd_.eq.'C') then
                if (ca_lparal_) then
                    do 50 ca_iel_ = 1, ca_nbelgr_
                        if (ca_paral_(ca_iel_)) then
                            iaux1=ca_iachlo_+debugr-1+(ca_iel_-1)*ncmp
                            do 40 k = 1, ncmp
                                zc(iaux1-1+k)=(0.d0,0.d0)
 40                         continue
                        endif
 50                 continue
                else
                    do 60 k = 1, ca_nbelgr_*ncmp
                        zc(ca_iachlo_+debugr-1-1+k)=(0.d0,0.d0)
 60                 continue
                endif
            else
                ASSERT(.false.)
            endif
        endif



!       -- si c'est 1 champ a representation constante (num<0):
!       -------------------------------------------------------
        if (num .lt. 0) then
            long=-num
            deb2=debugr
            do 90 ca_iel_ = 1, ca_nbelgr_
                if (ca_lparal_) then
                    if (.not.ca_paral_(ca_iel_)) then
                        deb2=deb2+lgcata
                        goto 90
                    endif
                endif
                ima=numail(ca_igr_,ca_iel_)
                ASSERT(ima.ne.0)
                do 80 ino = 1, nno
                    if (diff) idg2=5+ca_nec_*(ino-1)
                    if (ima .gt. 0) then
                        nugl=numglm(ima,ino)
                    else
                        nugl=numgls((-ima),ino)
                    endif
                    deb1=(nugl-1)*long+1

                    if (nugl .gt. 0) then
                        call trigd(zi(desc-1+3), deb1, zi(modloc-1+idg2), deb2, moyenn,&
                                   ino, nno)
                    else
!                 on verifie que le modloc affirme ncmp=0:
                        do 70 iec = 1, ca_nec_
                            if (zi(modloc-1+idg2-1+iec) .ne. 0) then
                                call utmess('F', 'CALCUL_9')
                            endif
 70                     continue
                    endif
 80             continue
 90         continue
        else

!           -- c'est 1 champ avec profil_noeud:
!           ------------------------------------
            prno1=zi(ca_iachii_-1+11*(ca_iichin_-1)+8)
            prno2=zi(ca_iachii_-1+11*(ca_iichin_-1)+9)
            deb2=debugr
            do 110 ca_iel_ = 1, ca_nbelgr_
                if (ca_lparal_) then
                    if (.not.ca_paral_(ca_iel_)) then
                        deb2=deb2+lgcata
                        goto 110
                    endif
                endif
                ima=numail(ca_igr_,ca_iel_)
                ASSERT(ima.ne.0)
                do 100 ino = 1, nno
                    if (diff) idg2=5+ca_nec_*(ino-1)
                    if (ima .gt. 0) then
                        nugl=numglm(ima,ino)
                    else
                        nugl=numgls((-ima),ino)
                    endif
                    deb1=(abs(nugl)-1)*(ca_nec_+2)+1
                    idg1=(abs(nugl)-1)*(ca_nec_+2)+3

                    if (nugl .gt. 0) then
                        call trigd(zi(prno1-1+idg1), zi(prno1-1+deb1), zi(modloc-1+idg2), deb2,&
                                   moyenn, ino, nno)
                    else
                        call trigd(zi(prno2-1+idg1), zi(prno2-1+deb1), zi(modloc-1+idg2), deb2,&
                                   moyenn, ino, nno)
                    endif
100             continue

110         continue
        endif


        if (moyenn) then
            ncmp=lgcata
            if (ca_typegd_ .eq. 'R') then
                if (ca_lparal_) then
                    do 130 ca_iel_ = 1, ca_nbelgr_
                        if (ca_paral_(ca_iel_)) then
                            iaux1=ca_iachlo_+debugr-1+(ca_iel_-1)*ncmp
                            do 120 k = 1, ncmp
                                zr(iaux1-1+k)=zr(iaux1-1+k)/dble(nno)
120                         continue
                        endif
130                 continue
                else
                    do 140 k = 1, ca_nbelgr_*ncmp
                        zr(ca_iachlo_-1+k)=zr(ca_iachlo_+debugr-1-1+k)/dble(&
                        nno)
140                 continue
                endif
            else if (ca_typegd_.eq.'C') then
                if (ca_lparal_) then
                    do 160 ca_iel_ = 1, ca_nbelgr_
                        if (ca_paral_(ca_iel_)) then
                            iaux1=ca_iachlo_+debugr-1+(ca_iel_-1)*ncmp
                            do 150 k = 1, ncmp
                                zc(iaux1-1+k)=zc(iaux1-1+k)/dble(nno)
150                         continue
                        endif
160                 continue
                else
                    do 170 k = 1, ca_nbelgr_*ncmp
                        zc(ca_iachlo_-1+k)=zc(ca_iachlo_+debugr-1-1+k)/dble(&
                        nno)
170                 continue
                endif
            else
                ASSERT(.false.)
            endif
        endif

    endif


end subroutine
