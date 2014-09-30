subroutine alchml(ligrez, optioz, nompaz, basz, celz,&
                  iret, dcelz)
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/digdel.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/modat2.h"
#include "asterfort/nbelem.h"
#include "asterfort/nbgrel.h"
#include "asterfort/nopar2.h"
#include "asterfort/scalai.h"
#include "asterfort/typele.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/nucalc.h"
!
    character(len=*) :: ligrez, dcelz, celz, basz, optioz, nompaz
    character(len=19) :: ligrel, cel, dcel
    character(len=16) :: option
    character(len=8) :: nompar
    character(len=1) :: base
    integer :: iret
! ----------------------------------------------------------------------
!  BUT : CREER UN CHAM_ELEM "VIERGE"
!
!  ARGUMENTS :
!  LIGREZ IN/JXIN  K19 : SD LIGREL SUR LEQUEL ON ALLOUE LE CHAM_ELEM
!  OPTIOZ IN       K16 : NOM DE L'OPTION SERVANT A DECRIRE LE CHAM_ELEM
!  NOMPAZ IN       K8  : NOM DU PARAMETRE (IN OU OUT) DE L'OPTION
!                        SERVANT A DECRIRE LE CHAM_ELEM
!                  - SI NOMPAZ N'EST PAS FOURNI (' '), ON SE SERT
!                    DU PARAMETRE "OUT" S'IL EST UNIQUE. SINON : <F>
!  BASZ   IN       K1  : 'G','V','L'
!  CELZ   IN/JXOUT K19 : SD CHAM_ELEM A CREER
!  IRET   OUT      I   : CODE RETOUR :
!                        0 -> LE CHAMP A ETE CREE
!                        1 -> LE CHAMP N'A PAS ETE CREE CAR
!                             AUCUN TYPE_ELEM DU LIGREL NE CONNAIT
!                             LE PARAMETRE DE L'OPTION
!
!  ARGUMENTS SUPPLEMENTAIRES POUR ALLOUER UN CHAM_ELEM "ETENDU" :
!  ------------------------------------------------------------------
!  DCELZ   IN/JXIN  K19 :
!    SD CHAM_ELEM_S PERMETTANT DE CREER UN CHAM_ELEM "ETENDU".
!    LA GRANDEUR ASSOCIEE A DCELZ DOIT ETRE "DCEL_I"
!    ET LES CMPS DOIVENT ETRE "NPG_DYN" ET "NCMP_DYN" (DANS CET ORDRE)
!
!  SI DCELZ = ' '
!     LE CHAM_ELEM N'EST PAS ETENDU.
!
!  SI DCELZ /= ' '   :  LE CHAM_ELEM EST ETENDU :
!     LES MAILLES TARDIVES SONT ALORS INTERDITES DANS LIGREZ
!     TEMPORAIREMENT, ON VA PRENDRE NB_VARI = 107 POUR LES
!     MAILLES TARDIVES. CETTE GLUTE SERA RETIREE EN 5.4
!
!
! ----------------------------------------------------------------------
    character(len=8) :: scal, nomgd
    character(len=16) :: nomte1, nomte, ma, ma2, kbid
    character(len=24) :: nomolo
    character(len=24) :: valk(3)
    integer :: ngrel, igrel, te, te1, mode, long, jceld, ncmpv, debgrl
    integer :: gd, jcelk, iopt, iprem, nel, iel, lgcata, nbspt
    integer :: ncdyn, lgchel,numc
    integer :: ibid, modmx, iamolo, itych, itych1, neltot
    integer :: illiel, jdcesd, jdcesl
    integer :: ima, ncmpv2, kk, ityplo, nbpoin
    aster_logical :: lmult
    integer, pointer :: liel(:) => null()
    integer, pointer :: cesv(:) => null()
    character(len=8), pointer :: cesk(:) => null()
    character(len=8), pointer :: cesc(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
    cel = celz
    dcel = dcelz
    ligrel = ligrez
    option = optioz
    nompar = nompaz
    if (nompar .eq. ' ') nompar=nopar2(option,' ','OUT')
    base = basz
!
!
    ngrel = nbgrel(ligrel)
    call jenonu(jexnom('&CATA.OP.NOMOPT', option), iopt)
    call jeveuo(ligrel//'.LIEL', 'L', vi=liel)
    call jeveuo(jexatr(ligrel//'.LIEL', 'LONCUM'), 'L', illiel)
    call dismoi('NOM_MAILLA', ligrel, 'LIGREL', repk=ma)
!
!
!     1- LE CHAM_ELEM DOIT-IL ETRE CREE ?
!     ----------------------------------
    modmx = 0
    do igrel = 1, ngrel
        te = typele(ligrel,igrel)
        mode = modat2(iopt,te,nompar)
        modmx = max(modmx,mode)
    end do
    if (modmx .eq. 0) then
        iret = 1
        goto 60
    else
        iret = 0
    endif
!
!
!     2- QUELLE EST LA GRANDEUR ASSOCIEE AU CHAM_ELEM ?
!     -----------------------------------------------------------
    call jeveuo(jexnum('&CATA.TE.MODELOC', modmx), 'L', iamolo)
    gd = zi(iamolo-1+2)
    call jenuno(jexnum('&CATA.GD.NOMGD', gd), nomgd)
    scal = scalai(gd)
!
!
!     3- DOIT-ON CREER UN CHAMP ETENDU ? (LMULT.EQ..TRUE.)
!        ------------------------------------------------------
    if (dcelz .eq. ' ') then
!       -- CHAMP NON-ETENDU :
        lmult = .false.
    else
!       -- CHAMP ETENDU : DCEL EST FOURNI PAR L'APPELANT
        lmult = .true.
        dcel = dcelz
    endif
!
!
!     3.1 SI CHAMP ETENDU : ON RECUPERE QUELQUES ADRESSES :
!     --------------------------------------------------------
    if (lmult) then
        call jeveuo(dcel//'.CESK', 'L', vk8=cesk)
        call jeveuo(dcel//'.CESC', 'L', vk8=cesc)
        call jeveuo(dcel//'.CESD', 'L', jdcesd)
        call jeveuo(dcel//'.CESL', 'L', jdcesl)
        call jeveuo(dcel//'.CESV', 'L', vi=cesv)
!
!
!       -- QUELQUES VERIFICATIONS :
        ma2 = cesk(1)
        if (ma2 .ne. ma) then
            valk(1) = ma2
            valk(2) = ma
            call utmess('F', 'CALCULEL_4', nk=2, valk=valk)
        endif
        ASSERT(zi(jdcesd-1+2).eq.2)
        ASSERT(zi(jdcesd-1+3).eq.1)
        ASSERT(zi(jdcesd-1+4).eq.1)
!
        kbid = cesk(2)
        ASSERT(kbid.eq.'DCEL_I')
!
        kbid = cesc(1)
        ASSERT(kbid.eq.'NPG_DYN')
        kbid = cesc(2)
        ASSERT(kbid.eq.'NCMP_DYN')
    endif
!
!
!
!     4- OBJET .CELD :
!     -------------------
    neltot = 0
    do igrel = 1, ngrel
        neltot = neltot + nbelem(ligrel,igrel)
    end do
!
    long = 4 + ngrel + 4*ngrel + 4*neltot
    call wkvect(cel//'.CELD', base//' V I', long, jceld)
    call jeecra(cel//'.CELD', 'DOCU', cval='CHML')
!
    zi(jceld-1+1) = gd
    zi(jceld-1+2) = ngrel
    zi(jceld-1+3) = 1
    zi(jceld-1+4) = 0
!
!     NCMPV: LONGUEUR DE .CELV  (+1)
    ncmpv = 1
!
!     DEBGRL: DEBUT DE DESCRIPTION DU GREL DANS .CELD
    debgrl = 4 + ngrel
!
    iprem = 0
    do igrel = 1, ngrel
        nel = nbelem(ligrel,igrel)
        te = typele(ligrel,igrel)
        mode = modat2(iopt,te,nompar)
        zi(jceld-1+4+igrel) = debgrl
        zi(jceld-1+debgrl+1) = nel
        zi(jceld-1+debgrl+2) = mode
        ncmpv2 = ncmpv
        
!       -- faut-il verifier que numc != -1 ?
        if (igrel.eq.1) then
            numc=nucalc(iopt,te,1)
        else
            numc=nucalc(iopt,te,0)
        endif
        if (numc .eq. -1) then
            call jenuno(jexnum('&CATA.TE.NOMTE', te), nomte)
            valk(1)=nomte
            valk(2)=option
            call utmess('F', 'CALCULEL_30', nk=2, valk=valk)
        endif
!
        if (mode .gt. 0) then
            iprem = iprem + 1
            call jeveuo(jexnum('&CATA.TE.MODELOC', mode), 'L', iamolo)
            ityplo = zi(iamolo-1+1)
            if (ityplo .gt. 3) then
                call jenuno(jexnum('&CATA.TE.NOMMOLOC', mode), nomolo)
                call utmess('F', 'CALCULEL_11', sk=nomolo)
            else
                nbpoin = zi(iamolo-1+4)
                if ((ityplo.eq.2) .and. (nbpoin.gt.10000)) then
                    call jenuno(jexnum('&CATA.TE.NOMMOLOC', mode), nomolo)
                    call utmess('F', 'CALCULEL_12', sk=nomolo)
                endif
            endif
!
            itych = zi(iamolo-1+1)
!
            if (iprem .eq. 1) then
                itych1 = itych
                te1 = te
            else
                if (itych*itych1 .lt. 0) goto 50
            endif
!
            lgcata = digdel(mode)
            zi(jceld-1+debgrl+3) = lgcata
!
            do iel = 1, nel
!
!           4.1 CALCUL DE NBSPT ET NCDYN POUR CHAQUE ELEMENT :
!           --------------------------------------------------
!
!           -- CAS D'UN CHAM_ELEM ETENDU :
                if (lmult) then
                    ima = liel(zi(illiel+igrel-1)+iel-1)
!
!             -- SI LA MAILLE APPARTIENT AU MAILLAGE, ON SE SERT
!                DE DCEL, SINON ON ARRETE LE CODE:
                    if (ima .gt. 0) then
!
                        call cesexi('C', jdcesd, jdcesl, ima, 1,&
                                    1, 1, kk)
                        if (kk .gt. 0) then
                            nbspt = max(cesv(kk),1)
                        else
                            nbspt = 1
                        endif
!
                        ncdyn = 0
                        if (nomgd .eq. 'VARI_R') then
                            call cesexi('C', jdcesd, jdcesl, ima, 1,&
                                        1, 2, kk)
                            if (kk .gt. 0) ncdyn = cesv(kk)
                        endif
!
!             -- CAS DES MAILLES TARDIVES :
                    else
                        nbspt = 1
                        ncdyn = 0
                        if (nomgd .eq. 'VARI_R') ncdyn = 1
                    endif
!
!           -- CAS D'UN CHAM_ELEM NON-ETENDU :
                else
                    nbspt = 1
                    ncdyn = 0
                    if (nomgd .eq. 'VARI_R') ncdyn = 1
                endif
!
!
!           4.2 AFFECTATION DES VALEURS DANS CELD :
!           ---------------------------------------
                zi(jceld-1+debgrl+4+ (iel-1)*4+1) = nbspt
                zi(jceld-1+debgrl+4+ (iel-1)*4+2) = ncdyn
                zi(jceld-1+3) = max(zi(jceld-1+3),nbspt)
                zi(jceld-1+4) = max(zi(jceld-1+4),ncdyn)
!
                lgchel = lgcata*nbspt*max(1,ncdyn)
                zi(jceld-1+debgrl+4+ (iel-1)*4+3) = lgchel
                zi(jceld-1+debgrl+4+ (iel-1)*4+4) = ncmpv
                ncmpv = ncmpv + lgchel
            end do
!
        endif
!
        zi(jceld-1+debgrl+4) = ncmpv - ncmpv2
        debgrl = debgrl + 4 + 4*nel
    end do
!
!
!     5- OBJET .CELV:
!     ------------------------
    call wkvect(cel//'.CELV', base//' V '//scal(1:4), ncmpv-1, ibid)
!
!
!     6- OBJET .CELK:
!     ------------------------
!         ETENDU A 5 K24  LIGREL / OPTION / ELNO-ELGA /
!                         NUME_COUCHE (0 TOUS I IEME COUCHE)
!                         NIVE_COUCHE (INF MOY SUP)
    call wkvect(cel//'.CELK', base//' V K24', 7, jcelk)
    zk24(jcelk-1+1) = ligrel
    zk24(jcelk-1+2) = option
    zk24(jcelk-1+6) = nompar
    if (itych1 .eq. 1) then
        zk24(jcelk-1+3) = 'ELEM'
    else if (itych1.eq.2) then
        zk24(jcelk-1+3) = 'ELNO'
    else if (itych1.eq.3) then
        zk24(jcelk-1+3) = 'ELGA'
    else
        ASSERT(.false.)
    endif
    zk24(jcelk-1+4) = ' '
    zk24(jcelk-1+5) = ' '
    zk24(jcelk-1+7) = 'MPI_COMPLET'
!
    goto 60
!
!
!     7- SECTION ERREUR:
!     ------------------------
 50 continue
!     CE CAS DE FIGURE NE DEVRAIT PLUS EXISTER APRES VERIF DANS
!     CAVER1  (COHERENCE DES TYPE_ELEM AVEC L'OPTION):
    call jenuno(jexnum('&CATA.TE.NOMTE', te), nomte)
    call jenuno(jexnum('&CATA.TE.NOMTE', te1), nomte1)
    valk(1) = option
    valk(2) = nomte1
    valk(3) = nomte
    call utmess('F', 'CALCULEL_14', nk=3, valk=valk)
!
!
!     8- FIN NORMALE:
!     ----------------
 60 continue
!
!
    call jedema()
!
!
end subroutine
