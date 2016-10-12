subroutine charci(chcine, mfact, mo, type)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getmjm.h"
#include "asterfort/gettco.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/chcsur.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscre.h"
#include "asterfort/cnsred.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvc8.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/imprsd.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/reliem.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/rs_getfirst.h"
!
    character(len=*) :: chcine, mfact, mo
    character(len=1) :: type
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
! OBJET :
!        TRAITEMENT DES MOTS CLES FACTEURS DE L'OPERATEUR
!        CREATION D'UN CHAM_NO_S CONTENANT LES DEGRES IMPOSES
!
!-----------------------------------------------------------------------
! VAR  CHCINE  K*19    : NOM DE LA CHARGE CINEMATIQUE
! IN   MFACT   K*16    : MOT CLE FACTEUR A TRAITER
!                        MOTS CLES ADMIS : MECA_IMPO
!                                          THER_IMPO
!                                          ACOU_IMPO
! IN   MO      K*      : NOM DU MODELE
! IN   TYPE    K*1     : 'R','C' OU 'F' TYPE DES MOTS CLES
    integer :: ifm, niv, icmp, cmp, ier, ino, nbno, nuno
    integer :: ioc, jcnsv, jcnsl, idino, nbobm
    integer :: idnddl, idvddl, nbddl, iddl, i, idprol
    integer :: ila, nbcmp, jcmp, noc, n1, iret
    integer :: jnoxfl, nlicmp, mxcmp, icmpmx, nume_first
    parameter (mxcmp=100)
!
    character(len=2) :: typ
    character(len=8) :: k8b, ma, nomgd, nogdsi, gdcns, nono
    character(len=8) :: evoim, licmp(20), chcity(mxcmp)
    character(len=16) :: mfac, k16b, motcle(5), phenom, typco, chcino(mxcmp)
    character(len=19) :: chci, cns, cns2, depla, noxfem
    character(len=24) :: cino, cnuddl, cvlddl, nprol, valk(2)
    character(len=80) :: titre
    aster_logical :: lxfem
    character(len=8), pointer :: afck(:) => null()
    data nprol/'                   .PROL'/
! --- DEBUT -----------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
    chci = chcine
    call jeveuo(chci//'.AFCK', 'E', vk8=afck)
    mfac = mfact
    call getfac(mfac, noc)
!
    call dismoi('NOM_MAILLA', mo, 'MODELE', repk=ma)
    call dismoi('PHENOMENE', mo, 'MODELE', repk=phenom)
    call dismoi('NOM_GD', phenom, 'PHENOMENE', repk=nomgd)
    call dismoi('NOM_GD_SI', nomgd, 'GRANDEUR', repk=nogdsi)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nogdsi), 'L', jcmp)
    call jelira(jexnom('&CATA.GD.NOMCMP', nogdsi), 'LONMAX', nbcmp)
    cns='&&CHARCI.CNS'
!
!    --------------------------------------------------------
!    MODELE X-FEM
!    --------------------------------------------------------
    call jeexin(mo(1:8)//'.XFEM_CONT', ier)
    if (ier .eq. 0) then
        lxfem = .false.
    else
        lxfem = .true.
        noxfem = '&&CHARCI.NOXFEM'
        call cnocns(mo(1:8)//'.NOXFEM', 'V', noxfem)
        call jeveuo(noxfem//'.CNSL', 'L', jnoxfl)
    endif
!
!     -- CAS DE EVOL_IMPO : ON IMPOSE TOUS LES DDLS DU 1ER CHAMP
!     ------------------------------------------------------------
    evoim=' '
    if (noc .eq. 0) then
        ASSERT(mfac.eq.'MECA_IMPO'.or.mfac.eq.'THER_IMPO')
        call getvid(' ', 'EVOL_IMPO', scal=evoim, nbret=n1)
        ASSERT(n1.eq.1)
        call getvtx(' ', 'NOM_CMP', nbval=20, vect=licmp, nbret=nlicmp)
        ASSERT(nlicmp.ge.0)
!
        call gettco(evoim, typco)
        if (typco .eq. 'EVOL_THER') then
            afck(1)='CITH_FT'
        else if (typco.eq.'EVOL_ELAS'.or.typco.eq.'EVOL_NOLI') then
            afck(1)='CIME_FT'
        else
            ASSERT(.false.)
        endif
        afck(3)=evoim
!
!       -- C'EST LE CHAMP DU 1ER NUMERO D'ORDRE QUI IMPOSE SA LOI:
        !call rsorac(evoim, 'PREMIER', ibid, rbid, k8b,&
        !            cbid, 0.d0, 'ABSOLU', iord, 1,&
        !            iret)
        !ASSERT(iret.eq.1)
        call rs_getfirst(evoim, nume_first)
        if (mfac .eq. 'MECA_IMPO') then
            call rsexch('F', evoim, 'DEPL', nume_first, depla,&
                        iret)
        else
            call rsexch('F', evoim, 'TEMP', nume_first, depla,&
                        iret)
        endif
        call cnocns(depla, 'V', cns)
!
!       -- SI NOM_CMP EST UTILISE, IL FAUT "REDUIRE" CNS :
        if (nlicmp .gt. 0) then
            cns2='&&CHARCI.CNS2'
            call cnsred(cns, 0, [0], nlicmp, licmp,&
                        'V', cns2)
            call detrsd('CHAM_NO_S', cns)
            call copisd('CHAM_NO_S', 'V', cns2, cns)
            call detrsd('CHAM_NO_S', cns2)
        endif
        goto 200
    endif
!
!
!
! --- NOM DE TABLEAUX DE TRAVAIL :
    cino = '&&CHARCI.INO'
    cnuddl = '&&CHARCI.NUMDDL'
    cvlddl = '&&CHARCI.VALDDL'
!
    motcle(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    motcle(3) = 'GROUP_NO'
    motcle(4) = 'NOEUD'
    motcle(5) = 'TOUT'
    ASSERT((type.eq.'F').or.(type.eq.'R').or.(type.eq.'C'))
!
    if (type .eq. 'F') then
        typ = 'K8'
        gdcns = nomgd
        gdcns(5:6) = '_F'
    else if (type.eq.'R') then
        typ = type
        gdcns = nomgd
    else if (type.eq.'C') then
        typ = type
        gdcns = nomgd
    endif
!
!
! --- CREATION D'UN CHAM_NO_S
!     POUR LIMITER LA TAILLE DU CHAM_NO_S,
!     ON DETERMINE LE PLUS GRAND NUMERO DE CMP ELIMINEE
!     ---------------------------------------------------------
    icmpmx=0
    do ioc = 1, noc
        call getmjm(mfac, ioc, mxcmp, chcino, chcity,&
                    nbobm)
        ASSERT(nbobm.gt.0)
        do iddl = 1, nbobm
            icmp = indik8( zk8(jcmp), chcino(iddl)(1:8), 1, nbcmp )
            icmpmx=max(icmpmx,icmp)
        end do
    end do
    ASSERT(icmpmx.gt.0)
    call cnscre(ma, gdcns, icmpmx, zk8(jcmp), 'V',&
                cns)
!
!
! --- REMPLISSAGE DU CHAM_NO_S
    call jeveuo(cns//'.CNSV', 'E', jcnsv)
    call jeveuo(cns//'.CNSL', 'E', jcnsl)
!
    do ioc = 1, noc
!
! ----- NOEUDS A CONTRAINDRE :
        call reliem(' ', ma, 'NU_NOEUD', mfac, ioc,&
                    5, motcle, motcle, cino, nbno)
        if (nbno .eq. 0) goto 100
        call jeveuo(cino, 'L', idino)
!
! ----- DDL A CONTRAINDRE :
        call getmjm(mfac, ioc, mxcmp, chcino, chcity,&
                    nbobm)
!
! ----- LECTURE DES MOTS CLES RELATIFS AUX VALEURS IMPOSEES
        call wkvect(cnuddl, ' V V K8', nbobm, idnddl)
        call wkvect(cvlddl, ' V V '//typ, nbobm, idvddl)
        nbddl = 0
        do iddl = 1, nbobm
            k16b = chcino(iddl)
            do i = 1, 5
                if (k16b .eq. motcle(i)) goto 110
            end do
!
            zk8(idnddl+nbddl) = k16b(1:8)
!
! -------   verification que la composante existe dans la grandeur
            icmp = indik8( zk8(jcmp), k16b(1:8), 1, nbcmp )
            ASSERT(icmp .ne. 0)
!
! -------   Pour XFEM : DX=U0 se traduit par une relation lineaire entre plusieurs ddls
!           => AFFE_CHAR_CINE est interdit :
            if (lxfem) then
                if ((k16b.eq.'DX') .or. (k16b.eq.'DY') .or. (k16b.eq.'DZ')) then
                    do ino = 1, nbno
                        nuno = zi(idino-1+ino)
                        if (zl(jnoxfl-1+2*nuno)) then
                            call jenuno(jexnum(ma//'.NOMNOE', nuno), nono)
                            valk(1)=k16b
                            valk(2)=nono
                            call utmess('F', 'ALGELINE2_22', nk=2, valk=valk)
                        endif
                    end do
                endif
            endif
!
            if (type .eq. 'R') then
                call getvr8(mfac, k16b, iocc=ioc, scal=zr(idvddl+nbddl), nbret=ila)
            endif
            if (type .eq. 'C') then
                call getvc8(mfac, k16b, iocc=ioc, scal=zc(idvddl+nbddl), nbret=ila)
            endif
            if (type .eq. 'F') then
                call getvid(mfac, k16b, iocc=ioc, scal=zk8(idvddl+nbddl), nbret=ila)
            endif
            nbddl = nbddl+1
110         continue
        end do
!
! --- on recherche si, quand on a des fonct. il y en a une = f(temps)
!
        if (type .eq. 'F') then
            do i = 1, nbddl
                nprol(1:8) = zk8(idvddl-1+i)
                call jeveuo(nprol, 'L', idprol)
                if (zk24(idprol+2) .eq. 'INST') then
                    afck(1)(5:7) = '_FT'
                    goto 122
                    else if (( zk24(idprol).eq.'NAPPE').and. ( zk24(&
                idprol+6).eq.'INST')) then
                    afck(1)(5:7) = '_FT'
                    goto 122
                endif
            end do
        endif
122     continue
!
! ----- affectation dans le cham_no_s
        if (type .eq. 'R') then
            do cmp = 1, nbddl
                k8b = zk8(idnddl-1+cmp)
                icmp = indik8( zk8(jcmp), k8b, 1, nbcmp )
                do ino = 1, nbno
                    nuno = zi(idino-1+ino)
                    zr(jcnsv+(nuno-1)*icmpmx+icmp-1) = zr(idvddl-1+ cmp)
                    zl(jcnsl+(nuno-1)*icmpmx+icmp-1) = .true.
                end do
            end do
        else if (type .eq. 'C') then
            do cmp = 1, nbddl
                k8b = zk8(idnddl-1+cmp)
                icmp = indik8( zk8(jcmp), k8b, 1, nbcmp )
                do ino = 1, nbno
                    nuno = zi(idino-1+ino)
                    zc(jcnsv+(nuno-1)*icmpmx+icmp-1) = zc(idvddl-1+ cmp)
                    zl(jcnsl+(nuno-1)*icmpmx+icmp-1) = .true.
                end do
            end do
        else if (type .eq. 'F') then
            do cmp = 1, nbddl
                k8b = zk8(idnddl-1+cmp)
                icmp = indik8( zk8(jcmp), k8b, 1, nbcmp )
                do ino = 1, nbno
                    nuno = zi(idino-1+ino)
                    zk8(jcnsv+(nuno-1)*icmpmx+icmp-1) = zk8(idvddl-1+ cmp)
                    zl(jcnsl+(nuno-1)*icmpmx+icmp-1) = .true.
                end do
            end do
        endif
!
        call jedetr(cino)
        call jedetr(cnuddl)
        call jedetr(cvlddl)
!
100     continue
    end do
200 continue
!
!
    if (( niv.ge.2) .and. (evoim.eq.' ')) then
        titre = '******* IMPRESSION DU CHAMP DES DDL IMPOSES *******'
        call imprsd('CHAMP_S', cns, ifm, titre)
    endif
!
!
!   -- creation de la sd affe_char_cine
    call chcsur(chci, cns, type, mo, nogdsi)
    call detrsd('CHAMP', cns)
!
!   -- si evol_impo : il ne faut pas utiliser les valeurs de chci :
    if (evoim .ne. ' ') call jedetr(chci//'.AFCV')
!
    if (lxfem) then
        call jedetr(noxfem)
    endif
!
    call jedema()
end subroutine
