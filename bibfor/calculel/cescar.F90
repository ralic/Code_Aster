subroutine cescar(cesz, cartz, basz)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "jeveux.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: cartz, cesz, basz
! ------------------------------------------------------------------
! BUT: TRANSFORMER UN CHAM_ELEM_S (DE TYPE ELEM)  EN CARTE
! ATTENTION : CETTE ROUTINE EST COUTEUSE POUR LES GROS MAILLAGES
!             JACQUES DEVRA L'AMELIORER PLUTARD
! ------------------------------------------------------------------
!     ARGUMENTS:
! CESZ   IN/JXOUT K19 : SD CHAM_ELEM_S A TRANSFORMER
! CARTZ  IN/JXIN  K19 : SD CARTE A CREER
! BASZ   IN       K1  : BASE DE CREATION POUR CARTZ : G/V
!-----------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: jce1k, jce1d, jce1c, jce1l, jce1v, nbmam, ncmp, ncmpmx
    integer :: jncmp, jvalv, iad1, kcmp, ncmpma, nbpt, nbsp, ima
    integer :: jlima, k, jvals, nbpaqu, nbcmps, jnoms, vali(3)
    logical :: idprec, premie
    character(len=1) :: base
    character(len=8) :: ma, nomgd
    character(len=3) :: tsca
    character(len=19) :: cart, ces1
    character(len=24) :: valk(3)
!     ------------------------------------------------------------------
    call jemarq()
!      CALL IMPRSD('CHAMP',CESZ,6,'AJOCOT CESCAR IN')
!
    ces1=cesz
    cart=cartz
    base=basz
!
!
!
!     1- RECUPERATION D'INFORMATIONS DANS CES1 :
!     ------------------------------------------
    call jeveuo(ces1//'.CESK', 'L', jce1k)
    call jeveuo(ces1//'.CESD', 'L', jce1d)
    call jeveuo(ces1//'.CESC', 'L', jce1c)
    call jeveuo(ces1//'.CESV', 'L', jce1v)
    call jeveuo(ces1//'.CESL', 'L', jce1l)
!
    ma=zk8(jce1k-1+1)
    nomgd=zk8(jce1k-1+2)
    nbmam=zi(jce1d-1+1)
!
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
    call dismoi('NB_CMP_MAX', nomgd, 'GRANDEUR', repi=ncmpmx)
!
!
    call alcart(base, cart, ma, nomgd)
    call jeveuo(cart//'.NCMP', 'E', jncmp)
    call jeveuo(cart//'.VALV', 'E', jvalv)
!
    call wkvect('&&CESCAR.LIMA', 'V V I', nbmam, jlima)
    call wkvect('&&CESCAR.NOMS', 'V V K8', ncmpmx, jnoms)
    call wkvect('&&CESCAR.VALS', 'V V '//tsca, ncmpmx, jvals)
!
!     -- POUR ECONOMISER L'ESPACE ET LE TEMPS, ON REGROUPE
!        LES MAILLES SUCCESSIVES QUI PORTENT LES MEMES VALEURS :
!
!     -- IDPREC : .TRUE. -> LA MAILLE EST IDENTIQUE A LA PRECEDENTE
    idprec=.false.
!     -- NBPAQU : NOMBRE DE MAILLES DU "PAQUET" DE MAILLES IDENTIQUES
    nbpaqu=0
!     -- NBCMPS : NOMBRE DE CMPS DU PAQUET
    nbcmps=0
!
    do ima = 1, nbmam
        nbpt=zi(jce1d-1+5+4*(ima-1)+1)
        nbsp=zi(jce1d-1+5+4*(ima-1)+2)
        ncmp=zi(jce1d-1+5+4*(ima-1)+3)
        if ((nbpt.gt.1) .or. (nbsp.gt.1)) then
            valk(1)=cesz
            valk(2)=cartz
            vali(1)=nbpt
            vali(2)=nbsp
            vali(3)=ima
            call utmess('F', 'MODELISA9_8', nk=2, valk=valk, ni=3,&
                        vali=vali)
        endif
        if (nbpt*nbsp .eq. 0) goto 80
!
!       -- NCMPMA : NBRE DE CMPS SUR LA MAILLE :
        ncmpma=0
        do kcmp = 1, ncmp
            call cesexi('C', jce1d, jce1l, ima, 1,&
                        1, kcmp, iad1)
            ASSERT(iad1.ne.0)
            if (iad1 .gt. 0) then
                ncmpma=ncmpma+1
                zk8(jncmp-1+ncmpma)=zk8(jce1c-1+kcmp)
!
                if (tsca .eq. 'R') then
                    zr(jvalv-1+ncmpma)=zr(jce1v-1+iad1)
                else if (tsca.eq.'C') then
                    zc(jvalv-1+ncmpma)=zc(jce1v-1+iad1)
                else if (tsca.eq.'I') then
                    zi(jvalv-1+ncmpma)=zi(jce1v-1+iad1)
                else if (tsca.eq.'K8') then
                    zk8(jvalv-1+ncmpma)=zk8(jce1v-1+iad1)
                else if (tsca.eq.'K16') then
                    zk16(jvalv-1+ncmpma)=zk16(jce1v-1+iad1)
                else if (tsca.eq.'K24') then
                    zk24(jvalv-1+ncmpma)=zk24(jce1v-1+iad1)
                else if (tsca.eq.'K32') then
                    zk32(jvalv-1+ncmpma)=zk32(jce1v-1+iad1)
                else if (tsca.eq.'K80') then
                    zk80(jvalv-1+ncmpma)=zk80(jce1v-1+iad1)
                else
                    ASSERT(.false.)
                endif
            endif
        end do
        if (ncmpma .eq. 0) goto 80
!
!
        if (nbcmps .eq. 0) then
!         -- C'EST LE 1ER PAQUET QUI COMMENCE
            ASSERT(.not.idprec)
            nbcmps=ncmpma
            premie=.true.
!
        else
!         -- LA MAILLE EST-ELLE COMME LA MAILLE SAUVEGARDEE ?
            if (ncmpma .ne. nbcmps) goto 30
            do k = 1, nbcmps
                if (zk8(jnoms-1+k) .ne. zk8(jncmp-1+k)) goto 30
                if (tsca .eq. 'R') then
                    if (zr(jvals-1+k) .ne. zr(jvalv-1+k)) goto 30
                else if (tsca.eq.'C') then
                    if (zc(jvals-1+k) .ne. zc(jvalv-1+k)) goto 30
                else if (tsca.eq.'I') then
                    if (zi(jvals-1+k) .ne. zi(jvalv-1+k)) goto 30
                else if (tsca.eq.'K8') then
                    if (zk8(jvals-1+k) .ne. zk8(jvalv-1+k)) goto 30
                else if (tsca.eq.'K16') then
                    if (zk16(jvals-1+k) .ne. zk16(jvalv-1+k)) goto 30
                else if (tsca.eq.'K24') then
                    if (zk24(jvals-1+k) .ne. zk24(jvalv-1+k)) goto 30
                else if (tsca.eq.'K32') then
                    if (zk32(jvals-1+k) .ne. zk32(jvalv-1+k)) goto 30
                else if (tsca.eq.'K80') then
                    if (zk80(jvals-1+k) .ne. zk80(jvalv-1+k)) goto 30
                endif
            end do
            idprec=.true.
            goto 40
!
 30         continue
            idprec=.false.
 40         continue
        endif
!
!
        if (.not.idprec) then
!          -- SI LA MAILLE EST DIFFERENTE :
!            - IL FAUT STOCKER LE PAQUET PRECEDENT
!            - PUIS IL FAUT SAUVEGARDER LA NOUVELLE MAILLE
!          -----------------------------------------------------
            if (.not.premie) then
                do k = 1, nbcmps
                    zk8(jncmp-1+k)=zk8(jnoms-1+k)
                    if (tsca .eq. 'R') then
                        zr(jvalv-1+k)=zr(jvals-1+k)
                    else if (tsca.eq.'C') then
                        zc(jvalv-1+k)=zc(jvals-1+k)
                    else if (tsca.eq.'I') then
                        zi(jvalv-1+k)=zi(jvals-1+k)
                    else if (tsca.eq.'K8') then
                        zk8(jvalv-1+k)=zk8(jvals-1+k)
                    else if (tsca.eq.'K16') then
                        zk16(jvalv-1+k)=zk16(jvals-1+k)
                    else if (tsca.eq.'K24') then
                        zk24(jvalv-1+k)=zk24(jvals-1+k)
                    else if (tsca.eq.'K32') then
                        zk32(jvalv-1+k)=zk32(jvals-1+k)
                    else if (tsca.eq.'K80') then
                        zk80(jvalv-1+k)=zk80(jvals-1+k)
                    endif
                end do
                call nocart(cart, 3, nbcmps, mode='NUM', nma=nbpaqu,&
                            limanu=zi(jlima))
!
!           -- POUR FAIRE LE NOCART, ON A DU ECRASER JVALV.
!           -- IL FAUT LE RETABLIR :
                ncmpma=0
                do kcmp = 1, ncmp
                    call cesexi('C', jce1d, jce1l, ima, 1,&
                                1, kcmp, iad1)
                    ASSERT(iad1.ne.0)
                    if (iad1 .gt. 0) then
                        ncmpma=ncmpma+1
                        zk8(jncmp-1+ncmpma)=zk8(jce1c-1+kcmp)
!
                        if (tsca .eq. 'R') then
                            zr(jvalv-1+ncmpma)=zr(jce1v-1+iad1)
                        else if (tsca.eq.'C') then
                            zc(jvalv-1+ncmpma)=zc(jce1v-1+iad1)
                        else if (tsca.eq.'I') then
                            zi(jvalv-1+ncmpma)=zi(jce1v-1+iad1)
                        else if (tsca.eq.'K8') then
                            zk8(jvalv-1+ncmpma)=zk8(jce1v-1+iad1)
                        else if (tsca.eq.'K16') then
                            zk16(jvalv-1+ncmpma)=zk16(jce1v-1+iad1)
                        else if (tsca.eq.'K24') then
                            zk24(jvalv-1+ncmpma)=zk24(jce1v-1+iad1)
                        else if (tsca.eq.'K32') then
                            zk32(jvalv-1+ncmpma)=zk32(jce1v-1+iad1)
                        else if (tsca.eq.'K80') then
                            zk80(jvalv-1+ncmpma)=zk80(jce1v-1+iad1)
                        endif
                    endif
                end do
            endif
!
            premie=.false.
            nbcmps=ncmpma
            do k = 1, nbcmps
                zk8(jnoms-1+k)=zk8(jncmp-1+k)
                if (tsca .eq. 'R') then
                    zr(jvals-1+k)=zr(jvalv-1+k)
                else if (tsca.eq.'C') then
                    zc(jvals-1+k)=zc(jvalv-1+k)
                else if (tsca.eq.'I') then
                    zi(jvals-1+k)=zi(jvalv-1+k)
                else if (tsca.eq.'K8') then
                    zk8(jvals-1+k)=zk8(jvalv-1+k)
                else if (tsca.eq.'K16') then
                    zk16(jvals-1+k)=zk16(jvalv-1+k)
                else if (tsca.eq.'K24') then
                    zk24(jvals-1+k)=zk24(jvalv-1+k)
                else if (tsca.eq.'K32') then
                    zk32(jvals-1+k)=zk32(jvalv-1+k)
                else if (tsca.eq.'K80') then
                    zk80(jvals-1+k)=zk80(jvalv-1+k)
                endif
            end do
            nbpaqu=1
            zi(jlima-1+nbpaqu)=ima
!
!
        else
!         -- SI LA MAILLE EST IDENTIQUE :
!         --------------------------------
            nbpaqu=nbpaqu+1
            zi(jlima-1+nbpaqu)=ima
        endif
!
 80     continue
    end do
!
!     -- IL NE FAUT PAS OUBLIER LE DERNIER PAQUET :
    do k = 1, nbcmps
        zk8(jncmp-1+k)=zk8(jnoms-1+k)
        if (tsca .eq. 'R') then
            zr(jvalv-1+k)=zr(jvals-1+k)
        else if (tsca.eq.'C') then
            zc(jvalv-1+k)=zc(jvals-1+k)
        else if (tsca.eq.'I') then
            zi(jvalv-1+k)=zi(jvals-1+k)
        else if (tsca.eq.'K8') then
            zk8(jvalv-1+k)=zk8(jvals-1+k)
        else if (tsca.eq.'K16') then
            zk16(jvalv-1+k)=zk16(jvals-1+k)
        else if (tsca.eq.'K24') then
            zk24(jvalv-1+k)=zk24(jvals-1+k)
        else if (tsca.eq.'K32') then
            zk32(jvalv-1+k)=zk32(jvals-1+k)
        else if (tsca.eq.'K80') then
            zk80(jvalv-1+k)=zk80(jvals-1+k)
        endif
    end do
    call nocart(cart, 3, nbcmps, mode='NUM', nma=nbpaqu,&
                limanu=zi(jlima))
!
!
    call jedetr('&&CESCAR.LIMA')
    call jedetr('&&CESCAR.NOMS')
    call jedetr('&&CESCAR.VALS')
!
    call jedema()
end subroutine
