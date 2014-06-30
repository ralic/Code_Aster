subroutine cescel(cesz, ligrez, optini, nompaz, prolz,&
                  nncp, basez, celz, kstop, iret)
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
!
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "jeveux.h"
#include "asterc/cheksd.h"
#include "asterc/indik8.h"
#include "asterc/isnnem.h"
#include "asterc/r8nnem.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbelem.h"
#include "asterfort/nopar2.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=*) :: cesz, celz, basez, ligrez, optini, nompaz, prolz
    character(len=1) :: kstop
    integer :: nncp, iret
! ------------------------------------------------------------------
! BUT : TRANSFORMER UN CHAM_ELEM_S (CESZ) EN CHAM_ELEM (CELZ)
! ------------------------------------------------------------------
! ARGUMENTS:
! ==========
! CESZ    IN/JXIN  K19 : SD CHAM_ELEM_S A TRANSFORMER
! LIGREZ  IN/JXIN  K19 : SD LIGREL QUI SERA ASSOCIE A CELZ
! OPTINI  IN       K16 : OPTION QUI SERA ASSOCIEE A CELZ
!         SI OPTINI=' ' : ON PREND OPTINI='TOU_INI_ELNO'
!                         OU OPTINI='TOU_INI_ELGA'
!                         SELON LE TYPE DE CESZ
! NOMPAZ  IN       K8  : NOM DU PARAMETRE "IN" OU "OUT" DANS OPTINI
!         SI NOMPAZ=' ' : ON CHERCHE LE BON CANDIDAT DANS LES
!                         PARAMETRES "IN" ET "OUT" DE OPTINI
!         ATTENTION : SI NOMPAZ=' ' ET QU'IL EXISTE PLUSIEURS
!                     PARAMETRES ASSOCIES A LA MEME GRANDEUR
!                     CELA CONDUIRA A UNE ERREUR <F>
!                     => IL VAUT MIEUX FOURNIR NOMPAZ !
! PROLZ   IN       K3  :
!    /'NON' : ERREUR <F> SI IL EXISTE DES
!             DES VALEURS DE CEL QUI NE SONT PAS AFFECTEES PAR CES.
!             => ON N'INVENTE AUCUNE VALEUR
!    /'OUI' : LE CHAM_ELEM CEL EST PROLONGE
!             PAR DES VALEURS NULLES LA OU CES N'EST PAS DEFINI.
!             SI LA GRANDEUR EST NEUT_F, ON MET LA FONCTION "&FOZERO"
!    /'CHL' : (UTILISE PAR CHLIGR)
!             PROLONGE PAR "ZERO" LES MAILLES DE CEL QUI NE SONT
!             PAS DU TOUT AFFECTEES DANS CES (NOUVELLES MAILLES)
!             ARRETE EN ERREUR <F> SI DES MAILLES DE CEL PORTENT
!             DES CMPS INCONNUES DANS CES
!    /'NAN' : LE CHAM_ELEM CEL EST PROLONGE
!             PAR DES VALEURS "NOT A NUMBER" LA OU CES N'EST PAS DEFINI.
! NNCP   OUT       I   : NOMBRE DE VALEURS DE CESZ NON RECOPIEES
!                        DANS CELZ
! BASEZ   IN       K1  : BASE DE CREATION POUR CELZ : G/V/L
! CELZ    IN/JXOUT K19 : SD CHAM_ELEM A CREER
! KSTOP   IN       K1  : COMPORTEMENT EN CAS DE PROBLEME :
!              / 'A' : ON EMET UNE ALARME ET ON REND IRET > 0
!              / 'F' : ON EMET UNE ERREUR FATALE
!              / ' ' : ON N'EMET PAS DE MESSAGE
! IRET    OUT       I  : CODE DE RETOUR :
!              / 0 : OK
!              / 1 : LE CHAM_ELEM N'A PAS PU ETRE CREE
!
!-----------------------------------------------------------------------
!
    logical(kind=1) :: dbg
!     ------------------------------------------------------------------
    integer :: icmp, nec,  jcesd, jcesv, jcesl, gd
    integer :: jnucm2, jnucm1,  i
    integer :: ncmpmx, ncmp1, jcmpgd, icmp1, k, iopt, iadg
    integer :: jcelv, neq, nbvces,  nbvcop, nbvaco
    integer :: igr, iel,  illiel,  nbgr, imolo, jmolo
    integer :: nbpt, ico, ipt, numa, iad, ieq, iad2
    integer :: jdceld, jdcell,  ima, nbma, nbspt, ispt, icmpmx
    integer :: adiel, jlpt,  lgcata, ncdyn, cumu, nbel, nptmx
    integer :: nbsp, nbcmp, isp, nbpt2, vali(2), inan
    logical :: diff
    logical(kind=1) :: prol, prol2
    character(len=1) :: base
    character(len=8) :: ma, nomgd, nomcmp, nompar, nomma, licmp(2)
    character(len=3) :: tsca, knan
    character(len=4) :: typces
    character(len=16) :: option
    character(len=19) :: ces, cel, ligrel, dcel
    character(len=24) :: valk(5), messag
    character(len=3) :: prol0
    real(kind=8) :: rnan
    character(len=8), pointer :: cesc(:) => null()
    character(len=8), pointer :: cesk(:) => null()
    integer, pointer :: liel(:) => null()
    integer, pointer :: dcelv(:) => null()
    integer, pointer :: celd(:) => null()
    integer, pointer :: copi(:) => null()
    integer, pointer :: long_pt_cumu(:) => null()
!
#define numail(igr,iel) liel(zi(illiel+igr-1)+iel-1)
!     ------------------------------------------------------------------
    call jemarq()
!
    dbg=.true.
    dbg=.false.
!
    base = basez
    ces = cesz
    cel = celz
    option = optini
    nompar = nompaz
    ligrel = ligrez
    prol0 = prolz
!
    do i = 1, 3
        valk(i)=' '
    end do
    valk(4) = cel
    valk(5) = ces
!
!
!
!     PROL : AUTORISATION DE PROLONGER (MEME UNE CMP ISOLEE)
!     PROL2: AUTORISATION DE PROLONGER UNE MAILLE ENTIEREMENT VIERGE
    if (prol0 .eq. 'OUI') then
        prol = .true.
        prol2 = .true.
!
    else if (prol0.eq.'NON') then
        prol = .false.
        prol2 = .false.
!
    else if (prol0.eq.'CHL') then
        prol = .false.
        prol2 = .true.
!
    else if (prol0.eq.'NAN') then
        prol = .true.
        prol2 = .true.
!
    else
        ASSERT(.false.)
    endif
!
!
!     -- SI CEL EXISTE DEJA, ON LE DETRUIT :
    call detrsd('CHAM_ELEM', cel)
!
    call jeveuo(ces//'.CESK', 'L', vk8=cesk)
    call jeveuo(ces//'.CESD', 'L', jcesd)
    call jeveuo(ces//'.CESC', 'L', vk8=cesc)
    call jeveuo(ces//'.CESV', 'L', jcesv)
    call jeveuo(ces//'.CESL', 'L', jcesl)
!     -- OBJET .COPI TEMPORAIRE POUR VERIFIER QUE TOUTES LES
!        COMPOSANTES DE CES ONT ETE RECOPIEES
    call jelira(ces//'.CESV', 'LONMAX', nbvces)
    AS_ALLOCATE(vi=copi, size=nbvces)
!
    ma = cesk(1)
    nomgd = cesk(2)
    typces = cesk(3)
    if (nomgd .eq. 'VAR2_R') nomgd = 'VARI_R'
!
    nbma = zi(jcesd-1+1)
    ncmp1 = zi(jcesd-1+2)
!
    call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nec)
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
    call dismoi('NB_CMP_MAX', nomgd, 'GRANDEUR', repi=ncmpmx)
    call dismoi('NUM_GD', nomgd, 'GRANDEUR', repi=gd)
!
!
!     1- REMPLISSAGE DE .NUCM2 ET .NUCM1 (SI NOMGD /='VARI_R'):
!     -----------------------------------------------------------------
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', jcmpgd)
    if (nomgd .ne. 'VARI_R') then
        call wkvect('&&CESCEL.NUCM1', 'V V I', ncmp1, jnucm1)
        call wkvect('&&CESCEL.NUCM2', 'V V I', ncmpmx, jnucm2)
!
        do icmp1 = 1, ncmp1
            nomcmp = cesc(icmp1)
            icmp = indik8(zk8(jcmpgd),nomcmp,1,ncmpmx)
            if (icmp .eq. 0) then
                valk(1) = nomcmp
                valk(2) = nomgd
                valk(3) = cel
                messag = 'CALCULEL_52'
                goto 240
!
            endif
            zi(jnucm2-1+icmp) = icmp1
            zi(jnucm1-1+icmp1) = icmp
        end do
    endif
!
!
!     -- ALLOCATION ET REMPLISSAGE DE 2 PETITS VECTEURS D'INDIRECTION
!       ENTRE LES CMPS (SI VARI_R) :
!     ----------------------------------------------------------------
    if (nomgd .eq. 'VARI_R') then
        ncmpmx = 0
        do icmp1 = 1, ncmp1
            nomcmp = cesc(icmp1)
            read (nomcmp(2:8),'(I7)') icmp
            ncmpmx = max(ncmpmx,icmp)
        end do
        ASSERT(ncmpmx.gt.0)
        call wkvect('&&CESCEL.NUCM1', 'V V I', ncmp1, jnucm1)
        call wkvect('&&CESCEL.NUCM2', 'V V I', ncmpmx, jnucm2)
        do icmp1 = 1, ncmp1
            nomcmp = cesc(icmp1)
            read (nomcmp(2:8),'(I7)') icmp
            zi(jnucm2-1+icmp) = icmp1
            zi(jnucm1-1+icmp1) = icmp
        end do
    endif
!
!
!
!     2- ON ALLOUE LE CHAM_ELEM CEL "VIERGE"
!     =========================================
!
!     2.1 DETERMINATION DE OPTION SI NECESSAIRE :
!     -------------------------------------------
    if (option .eq. ' ') then
        if (typces .eq. 'ELNO') then
            option = 'TOU_INI_ELNO'
!
        else if (typces.eq.'ELGA') then
            option = 'TOU_INI_ELGA'
!
        else if (typces.eq.'ELEM') then
            option = 'TOU_INI_ELEM'
!
        else
            ASSERT(.false.)
        endif
    endif
    call jenonu(jexnom('&CATA.OP.NOMOPT', option), iopt)
!
!
    if (iopt .eq. 0) then
        valk(1) = optini
        messag = 'CALCULEL_53'
        goto 240
!
    endif
!
!
!     2.2 DETERMINATION DE NOMPAR SI NECESSAIRE :
!     -------------------------------------------
    if (nompar .eq. ' ') nompar = nopar2(option,nomgd,'INOUT')
!
!
!     2.3 CREATION DE DCEL :
!     ----------------------------------------------
    licmp(1) = 'NPG_DYN'
    licmp(2) = 'NCMP_DYN'
    dcel = '&&CESCEL.DCEL'
    call cescre('V', dcel, 'ELEM', ma, 'DCEL_I',&
                2, licmp, [-1], [-1], [-2])
    call jeveuo(dcel//'.CESD', 'L', jdceld)
    call jeveuo(dcel//'.CESV', 'E', vi=dcelv)
    call jeveuo(dcel//'.CESL', 'E', jdcell)
    do ima = 1, nbma
!       -- NBRE DE SOUS-POINTS :
        call cesexi('C', jdceld, jdcell, ima, 1,&
                    1, 1, iad)
        ASSERT(iad.lt.0)
        zl(jdcell-1-iad) = .true.
        dcelv(1-1-iad) = zi(jcesd-1+5+4* (ima-1)+2)
!
!       -- NBRE DE CMPS "DYNAMIQUES" (POUR VARI_R) :
        call cesexi('C', jdceld, jdcell, ima, 1,&
                    1, 2, iad)
        ASSERT(iad.lt.0)
        zl(jdcell-1-iad) = .true.
        if (nomgd .eq. 'VARI_R') then
            nbpt = zi(jcesd-1+5+4* (ima-1)+1)
            nbsp = zi(jcesd-1+5+4* (ima-1)+2)
            nbcmp = zi(jcesd-1+5+4* (ima-1)+3)
            icmpmx = 0
            do icmp1 = 1, nbcmp
                icmp = zi(jnucm1-1+icmp1)
                do ipt = 1, nbpt
                    do isp = 1, nbsp
                        call cesexi('C', jcesd, jcesl, ima, ipt,&
                                    isp, icmp1, iad2)
                        if (iad2 .gt. 0) icmpmx = icmp
                    end do
                end do
            end do
            dcelv(1-1-iad) = icmpmx
!
        else
            dcelv(1-1-iad) = 0
        endif
    end do
!
!
!     2.4 ALLOCATION DU CHAM_ELEM :
!     ----------------------------------------------
    call alchml(ligrel, option, nompar, base, cel,&
                iret, dcel)
    if (iret .eq. 1) then
        valk(1) = nompar
        valk(2) = option
        valk(3) = ligrel
        messag = 'CALCULEL_54'
        goto 240
!
    endif
!
!     3- ON REMPLIT LE .CELV :
!     ===================================================
    call jeveuo(cel//'.CELV', 'E', jcelv)
    call jelira(cel//'.CELV', 'LONMAX', neq)
    call jeveuo(cel//'.CELD', 'L', vi=celd)
    nbgr = celd(2)
    call jeveuo(ligrel//'.LIEL', 'L', vi=liel)
    call jeveuo(jexatr(ligrel//'.LIEL', 'LONCUM'), 'L', illiel)
!
!
!     3.1 ON INITIALISE CELV AVEC "NAN" SI NECESSAIRE :
!     -------------------------------------------------
    if (prol0 .eq. 'NAN') then
        rnan = r8nnem()
        inan = isnnem()
        knan = '???'
        if (tsca .eq. 'R') then
            do ieq = 1, neq
                zr(jcelv-1+ieq) = rnan
            end do
        else if (tsca.eq.'C') then
            do ieq = 1, neq
                zc(jcelv-1+ieq) = dcmplx(rnan,rnan)
            end do
        else if (tsca.eq.'I') then
            do ieq = 1, neq
                zi(jcelv-1+ieq) = inan
            end do
        else if (tsca.eq.'K8') then
            do ieq = 1, neq
                zk8(jcelv-1+ieq) = knan
            end do
        else if (tsca.eq.'K24') then
            do ieq = 1, neq
                zk24(jcelv-1+ieq) = knan
            end do
            ASSERT(.false.)
        endif
    endif
!
!
!     3.2 ON INITIALISE CELV AVEC "&FOZERO" SI NEUT_F :
!     -----------------------------------------------------
    if (prol0 .eq. 'OUI' .and. nomgd .eq. 'NEUT_F') then
        ASSERT(tsca.eq.'K8')
        do ieq = 1, neq
            zk8(jcelv-1+ieq) = '&FOZERO'
        end do
    endif
!
!
!     3.2 CAS NOMGD /= 'VARI_R' :
!     ---------------------------------------------------
    if (nomgd .ne. 'VARI_R') then
!
!       3.2.1 ALLOCATION DE 2 VECTEURS DE TRAVAIL :
        nptmx = zi(jcesd-1+3)
        do igr = 1, nbgr
            imolo = celd(celd(4+igr)+2)
            if (imolo .eq. 0) goto 90
            call jeveuo(jexnum('&CATA.TE.MODELOC', imolo), 'L', jmolo)
            nbpt = mod(zi(jmolo-1+4),10000)
            nptmx = max(nptmx,nbpt)
 90         continue
        end do
!
        call wkvect('&&CESCEL.LONG_PT', 'V V I', nptmx, jlpt)
        AS_ALLOCATE(vi=long_pt_cumu, size=nptmx)
!
!       3.2.2 BOUCLE SUR LES GREL DU LIGREL
        do igr = 1, nbgr
            imolo = celd(celd(4+igr)+2)
            if (imolo .eq. 0) goto 170
!
            call jeveuo(jexnum('&CATA.TE.MODELOC', imolo), 'L', jmolo)
            diff = (zi(jmolo-1+4).gt.10000)
            nbpt = mod(zi(jmolo-1+4),10000)
            nbel = nbelem(ligrel,igr)
!
!         -- CALCUL DU NOMBRE DE CMPS POUR CHAQUE POINT
!            ET DU CUMUL SUR LES POINTS PRECEDENTS :
            do ipt = 1, nbpt
                ico = 0
                k = 1
                if (diff) k = ipt
                iadg = jmolo - 1 + 4 + (k-1)*nec + 1
                do icmp = 1, ncmpmx
                    if (exisdg(zi(iadg),icmp)) ico = ico + 1
                end do
                zi(jlpt-1+ipt) = ico
            end do
!
            cumu = 0
            do ipt = 1, nbpt
                long_pt_cumu(ipt) = cumu
                cumu = cumu + zi(jlpt-1+ipt)
            end do
!
            do ipt = 1, nbpt
                ico = 0
                k = 1
                if (diff) k = ipt
                iadg = jmolo - 1 + 4 + (k-1)*nec + 1
                do icmp = 1, ncmpmx
                    if (exisdg(zi(iadg),icmp)) then
                        ico = ico + 1
                        icmp1 = zi(jnucm2-1+icmp)
                        if (icmp1 .eq. 0) then
                            if (prol) then
                                goto 150
!
                            else
                                nomcmp = zk8(jcmpgd-1+icmp)
                                messag = 'CALCULEL_55'
                                goto 240
!
                            endif
                        endif
!
                        do iel = 1, nbel
                            numa = numail(igr,iel)
!
!                 -- QUE FAIRE SI LA MAILLE EST TARDIVE ?
                            if (numa .lt. 0) then
                                if (prol2) then
                                    goto 140
!
                                else
                                    messag = 'CALCULEL_56'
                                    goto 240
!
                                endif
                            endif
!
                            nbpt2 = zi(jcesd-1+5+4* (numa-1)+1)
                            if (nbpt .ne. nbpt2) then
                                if ((nbpt2.eq.0) .and. prol2) then
                                    goto 140
!
                                else
                                    if (nbpt .lt. nbpt2 .or. .not.prol) then
                                        call jenuno(jexnum(ma// '.NOMMAI', numa), nomma)
                                        valk(1) = nomma
                                        valk(2) = nomgd
                                        vali(1)=nbpt
                                        vali(2)=nbpt2
                                        messag = 'CALCULEL_57'
                                        goto 240
                                    endif
!
                                endif
                            endif
!
!
                            nbspt = celd(celd(4+igr)+4+4* (iel-1)+1)
                            nbspt = max(nbspt,1)
                            adiel = celd(celd(4+igr)+4+4* (iel-1)+4)
                            do ispt = 1, nbspt
!
!
                                call cesexi('C', jcesd, jcesl, numa, ipt,&
                                            ispt, icmp1, iad)
                                if (iad .le. 0) then
                                    if (prol) then
                                        goto 130
!
                                    else
                                        nomcmp = zk8(jcmpgd-1+icmp)
                                        call jenuno(jexnum(ma// '.NOMMAI', numa), nomma)
                                        valk(1) = nomcmp
                                        valk(2) = nomma
                                        messag = 'CALCULEL_58'
                                        goto 240
!
                                    endif
                                endif
!
                                ieq = adiel - 1 + nbspt*long_pt_cumu(ipt) + (ispt-1)*zi(jlpt-1+ip&
                                      &t) + ico
                                if (tsca .eq. 'R') then
                                    zr(jcelv-1+ieq) = zr(jcesv-1+iad)
!
                                else if (tsca.eq.'I') then
                                    zi(jcelv-1+ieq) = zi(jcesv-1+iad)
!
                                else if (tsca.eq.'C') then
                                    zc(jcelv-1+ieq) = zc(jcesv-1+iad)
!
                                else if (tsca.eq.'L') then
                                    zl(jcelv-1+ieq) = zl(jcesv-1+iad)
!
                                else if (tsca.eq.'K8') then
                                    zk8(jcelv-1+ieq) = zk8(jcesv-1+ iad)
!
                                else if (tsca.eq.'K16') then
                                    zk16(jcelv-1+ieq) = zk16(jcesv-1+ iad)
!
                                else if (tsca.eq.'K24') then
                                    zk24(jcelv-1+ieq) = zk24(jcesv-1+ iad)
!
                                else
                                    ASSERT(.false.)
                                endif
                                copi(iad) = 1
130                             continue
                            end do
140                         continue
                        end do
                    endif
150                 continue
                end do
            end do
170         continue
        end do
!
!
!     3.3 CAS NOMGD == 'VARI_R' :
!     ---------------------------------------------------
    else
        do igr = 1, nbgr
            imolo = celd(celd(4+igr)+2)
            if (imolo .eq. 0) goto 220
!
            call jeveuo(jexnum('&CATA.TE.MODELOC', imolo), 'L', jmolo)
            diff = (zi(jmolo-1+4).gt.10000)
!         CAS (ZI(JMOLO-1+4).GT.10000) RESTE A PROGRAMMER
            ASSERT(.not.diff)
            nbpt = mod(zi(jmolo-1+4),10000)
            lgcata = celd(celd(4+igr)+3)
            ASSERT(nbpt.eq.lgcata)
            nbel = nbelem(ligrel,igr)
!
!
            do iel = 1, nbel
!
                nbspt = celd(celd(4+igr)+4+4* (iel-1)+1)
                nbspt = max(nbspt,1)
                ncdyn = celd(celd(4+igr)+4+4* (iel-1)+2)
                adiel = celd(celd(4+igr)+4+4* (iel-1)+4)
                numa = numail(igr,iel)
!
!           -- QUE FAIRE SI LA MAILLE EST TARDIVE ?
                if (numa .lt. 0) then
                    if (prol2) goto 210
                    messag = 'CALCULEL_56'
                    goto 240
!
                endif
!
                nbpt2 = zi(jcesd-1+5+4* (numa-1)+1)
                if (nbpt .ne. nbpt2) then
                    if ((nbpt2.eq.0) .and. prol2) then
                        goto 210
!
                    else
                        call jenuno(jexnum(ma//'.NOMMAI', numa), nomma)
                        valk(1) = nomma
                        valk(2) = nomgd
                        vali(1)=nbpt
                        vali(2)=nbpt2
                        messag = 'CALCULEL_57'
                        goto 240
!
                    endif
                endif
!
                do ipt = 1, nbpt
                    do ispt = 1, nbspt
                        do icmp = 1, ncdyn
                            icmp1 = zi(jnucm2-1+icmp)
                            if (icmp1 .eq. 0) goto 180
                            call cesexi('C', jcesd, jcesl, numa, ipt,&
                                        ispt, icmp1, iad)
                            if (iad .le. 0) then
                                if (prol) then
                                    goto 180
!
                                else
                                    nomcmp = 'V'
                                    call codent(icmp, 'G', nomcmp(2:8))
                                    call jenuno(jexnum(ma//'.NOMMAI', numa), nomma)
                                    valk(1) = nomcmp
                                    valk(2) = nomma
                                    messag = 'CALCULEL_58'
                                    goto 240
!
                                endif
                            endif
!
                            ieq = adiel - 1 + ((ipt-1)*nbspt+ispt-1)* ncdyn + icmp
                            if (tsca .eq. 'R') then
                                zr(jcelv-1+ieq) = zr(jcesv-1+iad)
!
                            else if (tsca.eq.'I') then
                                zi(jcelv-1+ieq) = zi(jcesv-1+iad)
!
                            else if (tsca.eq.'C') then
                                zc(jcelv-1+ieq) = zc(jcesv-1+iad)
!
                            else if (tsca.eq.'L') then
                                zl(jcelv-1+ieq) = zl(jcesv-1+iad)
!
                            else if (tsca.eq.'K8') then
                                zk8(jcelv-1+ieq) = zk8(jcesv-1+iad)
!
                            else
                                ASSERT(.false.)
                            endif
                            copi(iad) = 1
180                         continue
                        end do
                    end do
                end do
210             continue
            end do
220         continue
        end do
    endif
!
!
!     -- CALCUL DU NOMBRE DE CMPS NON RECOPIEES (NNCP):
!     ------------------------------------------------------
    nbvcop = 0
    nbvaco = 0
    do iad = 1, nbvces
        if (zl(jcesl-1+iad)) nbvaco = nbvaco + 1
        if (copi(iad) .eq. 1) nbvcop = nbvcop + 1
    end do
    nncp = nbvaco - nbvcop
    iret = 0
    goto 250
!
!
!     -- MESSAGES D'ERREUR:
!     ---------------------
240 continue
    iret = 1
    ASSERT(kstop.eq.'F' .or. kstop.eq.'A' .or. kstop.eq.' ')
    call detrsd('CHAMP', cel)
    if (kstop .eq. ' ') goto 250
!
!
    if (messag .eq. 'CALCULEL_52') then
        call utmess(kstop, 'CALCULEL_52', nk=4, valk=valk)
    else if (messag.eq.'CALCULEL_53') then
        call utmess(kstop, 'CALCULEL_53', nk=4, valk=valk)
    else if (messag.eq.'CALCULEL_54') then
        call utmess(kstop, 'CALCULEL_54', nk=4, valk=valk)
    else if (messag.eq.'CALCULEL_55') then
        valk(1) = nomcmp
        call utmess(kstop, 'CALCULEL_55', nk=4, valk=valk)
    else if (messag.eq.'CALCULEL_56') then
        call utmess(kstop, 'CALCULEL_56', nk=4, valk=valk)
    else if (messag.eq.'CALCULEL_57') then
        call utmess(kstop, 'CALCULEL_57', nk=5, valk=valk, ni=2,&
                    vali=vali)
    else if (messag.eq.'CALCULEL_58') then
        call utmess(kstop, 'CALCULEL_58', nk=4, valk=valk)
    else
        ASSERT(.false.)
    endif
!
!
!
250 continue
    if (dbg) then
        call cheksd(cel, 'SD_CHAM_ELEM', iret)
        ASSERT(iret.eq.0)
    endif
!
    call detrsd('CHAM_ELEM_S', dcel)
    AS_DEALLOCATE(vi=copi)
    call jedetr('&&CESCEL.NUCM1')
    call jedetr('&&CESCEL.NUCM2')
    call jedetr('&&CESCEL.LONG_PT')
    AS_DEALLOCATE(vi=long_pt_cumu)
!
    call jedema()
end subroutine
