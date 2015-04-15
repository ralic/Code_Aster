subroutine exchno(imodat, iparg)
use module_calcul, only : ca_iachii_, ca_iachlo_, ca_ialiel_, ca_iamaco_,&
     ca_iamloc_, ca_iamsco_, ca_iawlo2_, ca_igr_,&
     ca_iichin_, ca_illiel_, ca_ilmaco_, ca_ilmloc_, ca_ilmsco_, &
     ca_nbelgr_, ca_nbgr_, ca_nec_, ca_typegd_
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/trigd.h"
#include "asterfort/utmess.h"
    integer :: imodat, iparg
! ----------------------------------------------------------------------
!     ENTREES:
!        IMODAT  : INDICE DANS LA COLLECTION MODELOC
!        IGR    : NUMERO DU GREL A TRAITER.
!     SORTIES:
!       ECRITURE DANS LE CHAMP LOCAL
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: ima, ino, nno, long, nugl, num, jparal, iret, iel
    integer :: desc, prno1, prno2, modloc, ityplo
    integer :: deb1, deb2, idg1, idg2, nbpt, nbpt2, lgcata, ncmp
    integer :: iaux1, k, iec, debugr
    aster_logical :: lparal
!
    aster_logical :: diff, moyenn
!
!     -- FONCTIONS FORMULES :
!     NUMAIL(IGR,IEL)=NUMERO DE LA MAILLE ASSOCIEE A L'ELEMENT IEL
#define numail(ca_igr_,iel) zi(ca_ialiel_-1+zi(ca_illiel_+ca_igr_-1)+iel-1)
!     NUMGLM(IMA,INO)=NUMERO GLOBAL DU NOEUD INO DE LA MAILLE IMA
!                     IMA ETANT UNE MAILLE DU MAILLAGE.
#define numglm(ima,ino) zi(ca_iamaco_-1+zi(ca_ilmaco_+ima-1)+ino-1)
!     NUMGLS(IMA,INO)=NUMERO GLOBAL DU NOEUD INO DE LA MAILLE IMA
!                     IMA ETANT UNE MAILLE SUPPLEMENTAIRE DU LIGREL
#define numgls(ima,ino) zi(ca_iamsco_-1+zi(ca_ilmsco_+ima-1)+ino-1)
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
    desc=zi(ca_iachii_-1+11*(ca_iichin_-1)+4)
    num=zi(desc-1+2)
    modloc=ca_iamloc_-1+zi(ca_ilmloc_-1+imodat)
    ityplo=zi(modloc-1+1)
    debugr=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+5)
    lgcata=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+2)
!
    ASSERT(ityplo.lt.4)
!
!     1-  CAS: CHNO -> ELGA :
!     -----------------------
!     LE CAS ITYPLO=3 N EST PAS PREVU : DEVELOPPEMENT A FAIRE ...
    ASSERT(ityplo.ne.3)
!
!     2-  CAS: CHNO -> ELNO :
!         CAS: CHNO -> ELEM (MOYENNE)
!     --------------------------------
    if ((ityplo.eq.2) .or. (ityplo.eq.1)) then
        if (ityplo .eq. 2) then
            moyenn=.false.
        else
            moyenn=.true.
        endif
!
!
!       2.1 ON CHERCHE NNO SUR LE 1ER ELEMENT :
!       ---------------------------------------
        ima=numail(ca_igr_,1)
        ASSERT(ima.ne.0)
        if (ima .gt. 0) then
            nno=zi(ca_ilmaco_-1+ima+1)-zi(ca_ilmaco_-1+ima)
        else
            nno=zi(ca_ilmsco_-1-ima+1)-zi(ca_ilmsco_-1-ima)-1
        endif
!
!
!       2.2 ON RECUPERE LE DEBUT DU DESCRIPTEUR GRANDEUR :
!       --------------------------------------------------
        nbpt=zi(modloc-1+4)
        nbpt2=mod(nbpt,10000)
        if (nbpt .ne. nbpt2) then
            diff=.true.
        else
            diff=.false.
            idg2=5
        endif
!
!       MOYENN => (NBPT2=1)
        ASSERT((.not.moyenn) .or. (nbpt2.eq.1))
!
!       .NOT.MOYENN => (NBPT2=NNO)
        ASSERT(moyenn .or. (nbpt2.eq.nno))
!
!
!       2.3 SI MOYENN, IL FAUT METTRE A ZERO LE CHAMP LOCAL
!           (POUR POUVOIR CUMULER)
!       --------------------------------------------------
        if (moyenn) then
            ncmp=lgcata
            if (ca_typegd_ .eq. 'R') then
                if (lparal) then
                    do 20 iel = 1, ca_nbelgr_
                        if (zl(jparal-1+iel)) then
                            iaux1=ca_iachlo_+debugr-1+(iel-1)*ncmp
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
                if (lparal) then
                    do 50 iel = 1, ca_nbelgr_
                        if (zl(jparal-1+iel)) then
                            iaux1=ca_iachlo_+debugr-1+(iel-1)*ncmp
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
!
!
!
!        ---SI C'EST 1 CHAMP A REPRESENTATION CONSTANTE (NUM<0):
!        -------------------------------------------------------
        if (num .lt. 0) then
            long=-num
            deb2=debugr
            do 90 iel = 1, ca_nbelgr_
                if (lparal) then
                    if (.not.zl(jparal-1+iel)) then
                        deb2=deb2+lgcata
                        goto 90
                    endif
                endif
                ima=numail(ca_igr_,iel)
                ASSERT(ima.ne.0)
                do 80 ino = 1, nno
                    if (diff) idg2=5+ca_nec_*(ino-1)
                    if (ima .gt. 0) then
                        nugl=numglm(ima,ino)
                    else
                        nugl=numgls((-ima),ino)
                    endif
                    deb1=(nugl-1)*long+1
!
                    if (nugl .gt. 0) then
                        call trigd(zi(desc-1+3), deb1, zi(modloc-1+idg2), deb2, moyenn,&
                                   ino, nno)
                    else
!                 ON VERIFIE QUE LE MODLOC AFFIRME NCMP=0:
                        do 70 iec = 1, ca_nec_
                            if (zi(modloc-1+idg2-1+iec) .ne. 0) then
                                call utmess('F', 'CALCUL_9')
                            endif
 70                     continue
                    endif
 80             continue
 90         continue
        else
!
!        --- C'EST 1 CHAMP AVEC PROFIL_NOEUD:
!        ------------------------------------
            prno1=zi(ca_iachii_-1+11*(ca_iichin_-1)+8)
            prno2=zi(ca_iachii_-1+11*(ca_iichin_-1)+9)
            deb2=debugr
            do 110 iel = 1, ca_nbelgr_
                if (lparal) then
                    if (.not.zl(jparal-1+iel)) then
                        deb2=deb2+lgcata
                        goto 110
                    endif
                endif
                ima=numail(ca_igr_,iel)
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
!
                    if (nugl .gt. 0) then
                        call trigd(zi(prno1-1+idg1), zi(prno1-1+deb1), zi(modloc-1+idg2), deb2,&
                                   moyenn, ino, nno)
                    else
                        call trigd(zi(prno2-1+idg1), zi(prno2-1+deb1), zi(modloc-1+idg2), deb2,&
                                   moyenn, ino, nno)
                    endif
100             continue
!
110         continue
        endif
!
!
        if (moyenn) then
            ncmp=lgcata
            if (ca_typegd_ .eq. 'R') then
                if (lparal) then
                    do 130 iel = 1, ca_nbelgr_
                        if (zl(jparal-1+iel)) then
                            iaux1=ca_iachlo_+debugr-1+(iel-1)*ncmp
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
                if (lparal) then
                    do 160 iel = 1, ca_nbelgr_
                        if (zl(jparal-1+iel)) then
                            iaux1=ca_iachlo_+debugr-1+(iel-1)*ncmp
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
!
    endif
!
!
end subroutine
