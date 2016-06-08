subroutine op0148()
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
!   RESTITUTION D'UN INTERSPECTRE DE REPONSE MODALE DANS LA BASE
!   PHYSIQUE  OPERATEUR REST_SPEC_PHYS
!-----------------------------------------------------------------------
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/ordis.h"
#include "asterfort/reliem.h"
#include "asterfort/rsexch.h"
#include "asterfort/speph0.h"
#include "asterfort/spephy.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
!-----------------------------------------------------------------------
    integer :: i, ibid, icham, icham1, ichref, icode, idep
    integer :: idis, ifoi, ifor, ifreq, ifsic, ifsvk, im
    integer :: imod1, imr, in, inoei, inoen, inumo, inuor
    integer :: irefba, isip, isip1, isip2, ismf, itypfl
    integer :: iv, ivale, ivale1, ivite, jnuor, nbfo1, nbm
    integer :: nbmr, nbn, nbp, nbpf, nnn, nplace
    integer :: npv, numod, i1, i3, il, ivitef, lnumi, lrefes
    integer :: ioptch, nbmcl, nbcmp
    aster_logical :: intmod, intphy
    character(len=8) :: nomu, table, nommai, nomcmp, cmp1, depla(3)
    ! character(len=8) :: veccmp(1)
    character(len=8) :: limocl(2), typem, maillage, modmec
    character(len=16) :: concep, cmd, optcal, optcha
    character(len=19) :: base, typflu, nomcha
    character(len=24) :: fsic, fsvk, vite, numo, freq, refeba, sipo
    character(len=24) :: nomnoe, chrefe, chvale, nomobj
    character(len=24) :: chnumi, chfreq
    real(kind=8) :: epsi, val, vitef
!
    integer :: lfreq
!
    data depla  /'DX      ','DY      ','DZ      '/
!
!
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
!
    call getres(nomu, concep, cmd)
    call getvtx(' ', 'NOM_CHAM', scal=optcha)
!   seule la 1ere semble utile, nbcmp est ignoré, les autres seront lues par speph0
    call getvtx(' ', 'NOM_CMP', scal=nomcmp, nbret=nbcmp)
!
! --- 3.RECUPERATION DU NOM DE LA TABLE
!       VERIFICATION DES PARAMETRES ---
!
    call getvid(' ', 'INTE_SPEC_GENE', scal=table)
!
    call getvid(' ', 'BASE_ELAS_FLUI', scal=base, nbret=ibid)
    if (ibid .eq. 0) then
        call speph0(nomu, table)
        goto 9999
    endif
!C
!
! --- 1.DETERMINATION DU CAS DE CALCUL ---
!
    if (optcha(1:4) .eq. 'DEPL') then
        ioptch = 1
    else if (optcha(1:4).eq.'VITE') then
        ioptch = 2
    else if (optcha(1:4).eq.'ACCE') then
        ioptch = 3
    else
        ioptch = 4
    endif
!
    if (ioptch .le. 3) then
        do 10 idep = 1, 3
            if (nomcmp .eq. depla(idep)) goto 11
 10     continue
 11     continue
    else
        if (nomcmp(1:4) .eq. 'SMFY') then
            ismf = 5
        else
            ismf = 6
        endif
        call getvid(' ', 'MODE_MECA', scal=modmec)
    endif
!
    call getvtx(' ', 'OPTION', scal=optcal)
    intphy = .false.
    intmod = .false.
    if (optcal(1:4) .eq. 'TOUT') intphy = .true.
    if (optcal(6:9) .eq. 'TOUT') intmod = .true.
!
!
! --- 2.RECUPERATION DES OBJETS DE LA BASE MODALE PERTURBEE ---
!
    call getvid(' ', 'BASE_ELAS_FLUI', scal=base)
!
    refeba = base//'.REMF'
    call jeveuo(refeba, 'L', irefba)
    typflu = zk8(irefba)
    fsic = typflu//'.FSIC'
    call jeveuo(fsic, 'L', ifsic)
    itypfl = zi(ifsic)
    if (itypfl .eq. 1 .and. ioptch .ne. 4) then
        fsvk = typflu//'.FSVK'
        call jeveuo(fsvk, 'L', ifsvk)
        cmp1 = zk8(ifsvk+1)
        if (nomcmp(1:2) .ne. cmp1(1:2)) then
            call utmess('A', 'MODELISA5_80')
        endif
    endif
    modmec = zk8(irefba+1)
    call dismoi('NOM_MAILLA', modmec, 'RESULTAT', repk=maillage)
!
    vite = base//'.VITE'
    call jeveuo(vite, 'L', ivite)
    call jelira(vite, 'LONUTI', npv)
    call getvr8(' ', 'VITE_FLUI', scal=vitef)
    call getvr8(' ', 'PRECISION', scal=epsi)
!
    ivitef = 1
    do i3 = 1, npv
        val = zr(ivite-1+i3)-vitef
        if (abs(val) .lt. epsi) then
            ivitef = i3
        endif
    end do
!
    numo = base//'.NUMO'
    call jeveuo(numo, 'L', inumo)
    call jelira(numo, 'LONUTI', nbm)
!
    freq = base//'.FREQ'
    call jeveuo(freq, 'L', ifreq)
!
! --- RECUPERATION DU NOM DU MAILLAGE ---
!
    iv = 1
    write(nomcha,'(A8,A5,2I3.3)') base(1:8),'.C01.',zi(inumo),iv
    chrefe = nomcha//'.REFE'
    call jeveuo(chrefe, 'L', ichref)
    nommai = zk24(ichref)(1:8)
    nomnoe = nommai//'.NOMNOE'
    call jelira(nomnoe, 'NOMUTI', nbp)
!
!
! --- 3.RECUPERATION DU NOM DE LA TABLE ---
!
    call getvid(' ', 'INTE_SPEC_GENE', scal=table)
!
! --- CARACTERISATION DU CONTENU DE LA TABLE   ---
! --- INTERSPECTRES OU AUTOSPECTRES UNIQUEMENT ---
!
    chnumi = table//'.NUMI'
    chfreq = table//'.DISC'
    call jeveuo(chnumi, 'L', lnumi)
    call jelira(chnumi, 'LONMAX', nbmr)
!
    nomobj = '&&OP0148.TEMP.NUOR'
    call wkvect(nomobj, 'V V I', nbmr, jnuor)
    do i1 = 1, nbmr
        zi(jnuor-1+i1) = zi(lnumi-1+i1)
    end do
    call ordis(zi(jnuor), nbmr)
    call wkvect('&&OP0148.MODE', 'V V I', nbmr, inuor)
    nnn = 1
    zi(inuor) = zi(jnuor)
    do i = 2, nbmr
        if (zi(jnuor+i-1) .ne. zi(inuor+nnn-1)) then
            nnn = nnn + 1
            zi(inuor+nnn-1) = zi(jnuor+i-1)
        end if
    end do
    nbmr = nnn
    do im = 1, nbm
        if (zi(inumo+im-1) .eq. zi(inuor)) then
            imod1 = im
            goto 31
        endif
    end do
    call utmess('F', 'MODELISA5_78')
 31 continue
!
    call jelira(chfreq, 'LONMAX', nbpf)
    call jeveuo(chfreq, 'L', lfreq)
!
! --- 4.RECUPERATION DES NOEUDS ---
!
    typem = 'NO_NOEUD'
    nbmcl = 2
    limocl(1) = 'GROUP_NO'
    limocl(2) = 'NOEUD'
    call reliem(' ', maillage, typem, ' ', 1,&
                nbmcl, limocl, limocl, '&&OP0148.TEMP.NOEN', nbn)
    call jeveuo('&&OP0148.TEMP.NOEN','L',inoen)
    call wkvect('&&OP0148.TEMP.NOEI', 'V V I ', nbn, inoei)
    do in = 0, nbn-1
        call jenonu(jexnom(nomnoe, zk8(inoen+in)), zi(inoei+in))
    end do
!
!
! --- 5.SI RESTITUTION D'INTERSPECTRES DE DEPLACEMENTS, VITESSES ---
! --- OU ACCELERATIONS                                           ---
! --- => RECUPERATION DES DEFORMEES MODALES AUX NOEUDS CHOISIS   ---
!
! --- SINON (RESTITUTION D'INTERSPECTRES DE CONTRAINTES)         ---
! --- => RECUPERATION DES CONTRAINTES MODALES AUX NOEUDS CHOISIS ---
!
!
    call wkvect('&&OP0148.TEMP.CHAM', 'V V R', nbmr*nbn, icham)
!
    if (ioptch .ne. 4) then
!
        do 50 imr = 1, nbmr
            write(nomcha,'(A8,A5,2I3.3)') base(1:8),'.C01.', zi(inuor+&
            imr-1),iv
            chvale = nomcha//'.VALE'
            call jeveuo(chvale, 'L', ivale)
            do 60 in = 1, nbn
                nplace = zi(inoei+in-1)
                icham1 = icham + nbn*(imr-1) + in - 1
                ivale1 = ivale + 6*(nplace-1) + idep - 1
                zr(icham1) = zr(ivale1)
 60         continue
            call jelibe(chvale)
 50     continue
!
    else
!
        do 70 imr = 1, nbmr
            numod = zi(inuor + imr - 1)
            call rsexch('F', modmec, 'SIPO_ELNO', numod, sipo,&
                        icode)
            sipo = sipo(1:19)//'.CELV'
            call jeveuo(sipo, 'L', isip)
            do 80 in = 1, nbn
                nplace = zi(inoei+in-1)
                icham1 = icham + nbn*(imr-1) + in - 1
                if (nplace .eq. 1) then
                    zr(icham1) = zr(isip+ismf-1)
                else if (nplace.eq.nbp) then
                    isip1 = isip + 6 + 12*(nbp-2) + ismf - 1
                    zr(icham1) = zr(isip1)
                else
                    isip1 = isip + 6 + 12*(nplace-2) + ismf - 1
                    isip2 = isip1 + 6
                    zr(icham1) = (zr(isip1)+zr(isip2))/2.d0
                endif
 80         continue
 70     continue
!
    endif
!
! --- 6.CREATION D'UNE MATRICE POUR STOCKER LES SPECTRES ---
! ---   POUR UNE VITESSE DONNEE                          ---
!
    nbfo1 = (nbmr* (nbmr+1))/2
    call wkvect('&&OP0148.TEMP.FONR', 'V V R', nbpf*nbfo1, ifor)
    call wkvect('&&OP0148.TEMP.FONI', 'V V R', nbpf*nbfo1, ifoi)
    call wkvect('&&OP0148.TEMP.DISC', 'V V R', nbpf, idis)
!
    call wkvect(nomu//'.REFE', 'G V K16', 3, lrefes)
    zk16(lrefes) = optcha
    zk16(lrefes+1) = optcal
    zk16(lrefes+2) = 'FREQ'
!
    do 380 il = 1, nbpf
        zr(idis+il-1) = zr(lfreq+il-1)
380 continue
!
! --- 7.REALISATION DU CALCUL ---
!
    call spephy(ioptch, intphy, intmod, nomu, table,&
                zr(ifreq), zr(icham), zr(ifor), zr(ifoi), zr(idis),&
                zk8(inoen), nomcmp, zi(inuor), nbmr, nbn,&
                imod1, nbpf, nbm, ivitef)
!
    call titre()
!
!
9999 continue
    call jedema()
end subroutine
