subroutine cnscno(cnsz, prchnz, prol0, basez, cnoz,&
                  kstop, iret)
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! aslint: disable=
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/cmpcha.h"
#include "asterfort/codent.h"
#include "asterfort/profchno_crsd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcncon.h"
#include "asterfort/gnomsd.h"
#include "asterfort/idensd.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/pteequ.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=*) :: cnsz, cnoz, basez, prchnz, prol0
    character(len=1) :: kstop
! ------------------------------------------------------------------
! BUT : TRANSFORMER UN CHAM_NO_S (CNSZ) EN CHAM_NO (CNOZ)
! ------------------------------------------------------------------
!     ARGUMENTS:
! CNSZ    IN/JXIN  K19 : SD CHAM_NO_S A TRANSFORMER
! PRCHNZ  IN/JXVAR K19 : SD PROF_CHNO  (OU ' ')
!          SI PRCHNZ EXISTE ON CREE CNOZ CONFORMEMENT A PRCHNZ :
!             => SI CNSZ CONTIENT DES VALEURS QUE L'ON NE SAIT PAS
!                STOCKER DANS PRCHNZ, ON LES "OUBLIE"
!             => SI PRCHNZ EXIGE DES VALEURS QUE L'ON NE TROUVE PAS
!                DANS CNSZ :
!                  - SI PROL0='OUI' : ON PRENDS LA VALEUR "ZERO"
!                  - SI PROL0='NON' : ERREUR <F>
!
!          SI PRCHNZ N'EXISTE PAS ON CREE CNOZ EN FONCTION
!             DU CONTENU DE CNSZ
!             SI PRCHNZ  = ' ' ON CREE UN PROF_CHNO "SOUS-TERRAIN"
!             SI PRCHNZ /= ' ' ON CREE UN PROF_CHNO DE NOM PRCHNZ
! PROL0   IN   K3  :  POUR PROLONGER (OU NON) LE CHAMP PAR "ZERO"
!        /OUI /NON  ( CET ARGUMENT N'EST UTILISE QUE SI PRCHNZ /= ' ')
!        "ZERO" : / 0       POUR LES CHAMPS NUMERIQUES (R/C/I)
!                 / ' '     POUR LES CHAMPS "KN"
!                 / .FALSE. POUR LES CHAMPS DE "L"
!
! BASEZ   IN       K1  : BASE DE CREATION POUR CNOZ : G/V/L
! CNOZ    IN/JXOUT K19 : SD CHAM_NO A CREER
! KSTOP   IN       K1  : COMPORTEMENT EN CAS DE PROBLEME :
!              / 'A' : ON EMET UNE ALARME ET ON REND IRET > 0
!              / 'F' : ON EMET UNE ERREUR FATALE
!              / ' ' : ON N'EMET PAS DE MESSAGE
! IRET    OUT       I  : CODE DE RETOUR :
!              / 0 : OK
!              / 1 : LE CHAM_NO N'A PAS PU ETRE CREE
!----------------------------------------------------------------------
!
    character(len=24) :: noojb
    character(len=24) :: valk(3)
!     -----------------------------------------------------------------
    integer :: icmp, nec,   jcnsv, jcnsl, gd, iexi, ncmp
    integer :: reste, iec, code, nbno
    integer :: ncmpmx, jrefe, ncmp1, nb_equa, jcmpgd, icmp1, k, ieq2, iexi2, nbec
    integer :: jprn2, ino, idg2, ico, jvale, iret, prno_length
    integer :: lshift, nuprf
    character(len=1) :: base
    character(len=8) :: ma, nomgd, nomno, nomcmp
    aster_logical :: l_crea_prchno, l_chck_prchno
    character(len=3) :: tsca
    character(len=19) :: cns, cno, prchno, messag, prnoav, nume_equa
    integer, pointer :: deeq(:) => null()
    integer, pointer :: cnsd(:) => null()
    character(len=8), pointer :: cnsc(:) => null()
    character(len=24), pointer :: refn(:) => null()
    character(len=8), pointer :: cnsk(:) => null()
    integer, pointer :: tmp_nucm1(:) => null()
    integer, pointer :: tmp_nucmp(:) => null()
    integer, pointer :: cata_to_field(:) => null()
    integer, pointer :: field_to_cata(:) => null()
    character(len=8), pointer :: cmp_name(:) => null()
    integer, pointer :: v_nequ(:) => null()
!     -----------------------------------------------------------------
    call jemarq()
!
!
    base=basez
    ASSERT((base.eq.'G') .or. (base.eq.'V'))
    cns=cnsz
    cno=cnoz
!
    l_chck_prchno = .false.
    l_crea_prchno = .false.
!     CALL UTIMSD(6,2,.TRUE.,.TRUE.,CNS,1,' ')
!
    call jeveuo(cns//'.CNSK', 'L', vk8=cnsk)
    call jeveuo(cns//'.CNSD', 'L', vi=cnsd)
    call jeveuo(cns//'.CNSC', 'L', vk8=cnsc)
    call jeveuo(cns//'.CNSV', 'L', jcnsv)
    call jeveuo(cns//'.CNSL', 'L', jcnsl)
!
    ma=cnsk(1)
    nomgd=cnsk(2)
    nbno=cnsd(1)
    ncmp1=cnsd(2)
!
    call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nec)
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
    call dismoi('NB_CMP_MAX', nomgd, 'GRANDEUR', repi=ncmpmx)
    call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nbec)
    call dismoi('NUM_GD', nomgd, 'GRANDEUR', repi=gd)
!
!
!     -- SI CNO EXISTE DEJA, ON LE DETRUIT :
    call detrsd('CHAM_NO', cno)
!
! - PROF_CHNO name
!
    if (prchnz .eq. ' ') then
        if (base .eq. 'G') then
            noojb='12345678.PRCHN00000.PRNO'
            call gnomsd(' ', noojb, 15, 19)
            prchno=noojb(1:19)
        else
            call gcncon('.', prchno)
        endif
        l_chck_prchno = .false.
    else
        prchno = prchnz
        l_chck_prchno = .true.
    endif
!
! - Create PROF_CHNO ?
!
    call jeexin(prchno//'.PRNO', iexi)
    l_crea_prchno = (iexi.eq.0)
    if (l_crea_prchno) then
        l_chck_prchno = .false.
    endif
!
!
!     1- REMPLISSAGE DE .TMP_NUCMP ET .TMP_NUCM1 :
!     --------------------------------------------
    AS_ALLOCATE(vi=tmp_nucmp, size=ncmpmx)
    AS_ALLOCATE(vi=tmp_nucm1, size=ncmp1)
!
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', jcmpgd)
    do icmp1 = 1, ncmp1
        nomcmp=cnsc(icmp1)
        icmp=indik8(zk8(jcmpgd),nomcmp,1,ncmpmx)
        ASSERT(icmp.gt.0)
        tmp_nucmp(icmp)=icmp1
        tmp_nucm1(icmp1)=icmp
    end do
!
! - Check PROF_CHNO
!
    if (l_chck_prchno) then
        valk(1)=cno
        valk(2)=prchno
        call jeexin(prchno//'.REFN', iexi2)
        if (iexi2 .gt. 0) then
            nume_equa = prchno
!         -- SI PRCHNO VIENT D'UN NUME_EQUA, ON PEUT VERIFIER maillage et grandeur
            call jeveuo(nume_equa(1:19)//'.REFN', 'L', vk24=refn)
            if ((refn(1).ne.ma) .or. (refn(2) .ne.nomgd)) then
!             -- ON ACCEPTE : DEPL_R / DEPL_C
                if ((nomgd(1:5).eq.'DEPL_') .and. (refn(2) (1:5).eq.'DEPL_')) then
                else
                    call utmess('F', 'CALCULEL4_6', nk=2, valk=valk)
                endif
            endif
        else
            call jelira(jexnum(prchno//'.PRNO', 1), 'LONMAX', prno_length)
            ASSERT(prno_length .eq. nbno*(nbec+2))
        endif
    endif
!
!
!     2- ON CREE (SI NECESSAIRE) LE PROF_CHNO  :
!     ------------------------------------------
    if (l_crea_prchno) then
!
!       2.1 ON COMPTE LES CMPS PORTEES PAR CNS :
        nb_equa=0
        do k = 1, nbno*ncmp1
            if (zl(jcnsl-1+k)) then
                nb_equa=nb_equa+1
            endif
        end do
        if (nb_equa .eq. 0) then
            valk(1)=cns
            valk(2)=cno
            messag='CALCULEL2_12'
            goto 70
!
        endif
!
!       2.2 ALLOCATION DES OBJETS :
        call profchno_crsd(prchno, base, nb_equa, meshz = ma,&
                           gran_namez = nomgd, l_coll_const=.true.)
        call jecroc(jexnum(prchno//'.PRNO', 1))
!
!       2.3 REMPLISSAGE DE .PRNO :
        call jeveuo(jexnum(prchno//'.PRNO', 1), 'E', jprn2)
        do ino = 1, nbno
            do icmp1 = 1, ncmp1
                if (zl(jcnsl-1+(ino-1)*ncmp1+icmp1)) then
                    icmp=tmp_nucm1(icmp1)
                    iec=(icmp-1)/30+1
                    reste=icmp-30*(iec-1)
                    code=lshift(1,reste)
                    idg2=jprn2-1+((2+nec)*(ino-1))+2+iec
                    zi(idg2)=ior(zi(idg2),code)
                    zi(jprn2-1+((2+nec)*(ino-1))+2)=zi(jprn2-1+&
                    ((2+nec)*(ino-1))+2)+1
                endif
            end do
        end do
!
        ico=0
        do ino = 1, nbno
            zi(jprn2-1+((2+nec)*(ino-1))+1)=ico+1
            ico=ico+zi(jprn2-1+((2+nec)*(ino-1))+2)
        end do
        call jelibe(prchno//'.PRNO')
!
!       2.4 CREATION  DE .DEEQ :
!       POUR DES RAISONS DE PERFORMANCES, IL VAUT MIEUX LE
!       FAIRE PLUTARD.
    endif
!
! - Get number of equations
!
    call jeexin(prchno//'.NEQU', iexi)
    if (iexi.eq.0) then
        call jelira(prchno//'.NUEQ', 'LONMAX', nb_equa)
    else
        call jeveuo(prchno//'.NEQU', 'L', vi = v_nequ)
        nb_equa = v_nequ(1)
    endif
!
! - Create node field
!
    call vtcreb(cno, base, tsca,&
                meshz = ma, prof_chnoz = prchno, idx_gdz = gd, nb_equa_inz = nb_equa)
    call jeveuo(cno//'.REFE','E',jrefe)
    call jeveuo(cno//'.VALE','E',jvale)
!
!
!     5-BIS ON CREE SI NECESSAIRE LE .DEEQ DU PROF_CHNO
!     ----------------------------------------------------
    if (l_crea_prchno) then
!
! ----- Create object local components (field) => global components (catalog)
!
        call cmpcha(cno, cmp_name, cata_to_field, field_to_cata, nb_cmpz = ncmp)
!       -- POUR ECONOMISER LA MEMOIRE (PENDANT PTEEQU)
!          ON LIBERE TEMPORAIREMENT .CNSV ET .CNSL :
        call jelibe(cns//'.CNSV')
        call jelibe(cns//'.CNSL')
        call pteequ(prchno, base, nb_equa, gd, ncmp,&
                    field_to_cata)
!
        AS_DEALLOCATE(vi = cata_to_field)
        AS_DEALLOCATE(vi = field_to_cata)
        AS_DEALLOCATE(vk8 = cmp_name)
        call jeveuo(cns//'.CNSV', 'L', jcnsv)
        call jeveuo(cns//'.CNSL', 'L', jcnsl)
    endif
!
!
!     6- ON REMPLIT LE .VALE :
!     -----------------------------------

    call jeveuo(prchno//'.DEEQ', 'L', vi=deeq)
!
    do ieq2 = 1, nb_equa
        ino=deeq(2*(ieq2-1)+1)
        icmp=deeq(2*(ieq2-1)+2)
        if (ino*icmp .gt. 0) then
            nomcmp=zk8(jcmpgd-1+icmp)
            icmp1=tmp_nucmp(icmp)
!
            if (icmp1 .eq. 0) then
                if (prol0 .eq. 'NON') then
                    call jenuno(jexnum(ma//'.NOMNOE', ino), nomno)
                    valk(1)=nomcmp
                    valk(2)=nomno
                    valk(3)=cno
                    messag='CALCULEL2_13'
                    goto 70
!
                else
                    ASSERT(prol0.eq.'OUI')
                    if (tsca .eq. 'R') then
                        zr(jvale-1+ieq2)=0.d0
!
                    else if (tsca.eq.'C') then
                        zc(jvale-1+ieq2)=(0.d0,0.d0)
!
                    else if (tsca.eq.'I') then
                        zi(jvale-1+ieq2)=0
!
                    else if (tsca.eq.'L') then
                        zl(jvale-1+ieq2)=.false.
!
                    else if (tsca.eq.'K8') then
                        zk8(jvale-1+ieq2)=' '
!
                    else
                        ASSERT(.false.)
                    endif
                    goto 60
!
                endif
            endif
!
            if (zl(jcnsl-1+(ino-1)*ncmp1+icmp1)) then
                if (tsca .eq. 'R') then
!
! ----------------- Test for protect when nb_equa.ne.nb_dof
!
                    if (zr(jvale-1+ieq2).ne.0.d0) then
                        if (zr(jcnsv-1+(ino-1)*ncmp1+icmp1).ne.zr(jvale-1+ieq2)) then
                            ASSERT(.false.)
                        endif
                    endif
                    zr(jvale-1+ieq2)=zr(jcnsv-1+(ino-1)*ncmp1+icmp1)
!
                else if (tsca.eq.'C') then
!
! ----------------- Test for protect when nb_equa.ne.nb_dof
!
                    if (zc(jvale-1+ieq2).ne.(0.d0,0.d0)) then
                        if (zc(jvale-1+ieq2).ne.zc(jcnsv-1+(ino-1)*ncmp1+icmp1)) then
                            ASSERT(.false.)
                        endif
                    endif
                    zc(jvale-1+ieq2)=zc(jcnsv-1+(ino-1)*ncmp1+icmp1)
!
                else if (tsca.eq.'I') then
!
! ----------------- Test for protect when nb_equa.ne.nb_dof
!
                    if (zi(jvale-1+ieq2).ne.0) then
                        if (zi(jvale-1+ieq2).ne.zi(jcnsv-1+(ino-1)*ncmp1+icmp1)) then
                            ASSERT(.false.)
                        endif
                    endif
                    zi(jvale-1+ieq2)=zi(jcnsv-1+(ino-1)*ncmp1+icmp1)
!
                else if (tsca.eq.'L') then
!
! ----------------- Test for protect when nb_equa.ne.nb_dof
!
                    if (.not.zl(jvale-1+ieq2)) then
                        if (zl(jvale-1+ieq2).neqv.zl(jcnsv-1+(ino-1)*ncmp1+icmp1)) then
                            ASSERT(.false.)
                        endif
                    endif
                    zl(jvale-1+ieq2)=zl(jcnsv-1+(ino-1)*ncmp1+icmp1)
!
                else if (tsca.eq.'K8') then
!
! ----------------- Test for protect when nb_equa.ne.nb_dof
!
                    if (zk8(jvale-1+ieq2).ne.' ') then
                        if (zk8(jvale-1+ieq2).ne.zk8(jcnsv-1+(ino-1)*ncmp1+icmp1)) then
                            ASSERT(.false.)
                        endif
                    endif
                    zk8(jvale-1+ieq2)=zk8(jcnsv-1+(ino-1)*ncmp1+icmp1)
!
                else
                    ASSERT(.false.)
                endif
!
            else
                if (prol0 .eq. 'NON') then
                    call jenuno(jexnum(ma//'.NOMNOE', ino), nomno)
                    valk(1)=nomcmp
                    valk(2)=nomno
                    valk(3)=cno
                    messag='CALCULEL2_13'
                    goto 70
!
                else
                    ASSERT(prol0.eq.'OUI')
                    if (tsca .eq. 'R') then
                        zr(jvale-1+ieq2)=0.d0
!
                    else if (tsca.eq.'C') then
                        zc(jvale-1+ieq2)=(0.d0,0.d0)
!
                    else if (tsca.eq.'I') then
                        zi(jvale-1+ieq2)=0
!
                    else if (tsca.eq.'L') then
                        zl(jvale-1+ieq2)=.false.
!
                    else if (tsca.eq.'K8') then
                        zk8(jvale-1+ieq2)=' '
!
                    else
                        ASSERT(.false.)
                    endif
                    goto 60
!
                endif
            endif
        endif
 60     continue
    end do
!
!
!     7 - POUR ECONOMISER LES PROF_CHNO, ON REGARDE SI
!         LE PRECEDENT NE CONVIENDRAIT PAS :
!     -----------------------------------------------------
    if (prchnz .eq. ' ' .and. base .eq. 'G') then
        read (prchno(15:19),'(I5)') nuprf
        if (nuprf .gt. 0) then
            prnoav=prchno
            call codent(nuprf-1, 'D0', prnoav(15:19))
            if (idensd('PROF_CHNO',prchno,prnoav)) then
                call detrsd('PROF_CHNO', prchno)
                zk24(jrefe-1+2)=prnoav
            endif
        endif
    endif
!
!
    iret=0
    goto 80
!
!
!     -- MESSAGES D'ERREUR:
!     ---------------------
 70 continue
    ASSERT(kstop.eq.'F' .or. kstop.eq.'A' .or. kstop.eq.' ')
    iret=1
    call detrsd('CHAMP', cno)
    if (kstop .eq. ' ') goto 80
!
    if (messag .eq. 'CALCULEL2_12') then
        call utmess(kstop, 'CALCULEL2_12', nk=2, valk=valk)
    else if (messag.eq.'CALCULEL2_13') then
        call utmess(kstop, 'CALCULEL2_13', nk=3, valk=valk)
    else
        ASSERT(.false.)
    endif
!
!
!
 80 continue
    AS_DEALLOCATE(vi=tmp_nucmp)
    AS_DEALLOCATE(vi=tmp_nucm1)
    call jedema()
!     CALL UTIMSD(6,2,.TRUE.,.TRUE.,CNO,1,' ')
end subroutine
