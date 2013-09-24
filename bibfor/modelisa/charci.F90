subroutine charci(chcine, mfact, mo, type)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getmjm.h"
#include "asterc/gettco.h"
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
#include "asterfort/wkvect.h"
!
    character(len=*) :: chcine, mfact, mo
    character(len=1) :: type
!-----------------------------------------------------------------------
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
    real(kind=8) :: rbid
    complex(kind=8) :: cbid
    integer :: ibid, ifm, niv, icmp, cmp, ier, ino, nbno, nuno
    integer :: ioc, jcnsv, jcnsl, idino, nbobm
    integer :: idnddl, idvddl, nbddl, iddl, i, idprol, jafck
    integer :: ila, nbcmp, jcmp, noc, n1, iord(1), iret
    integer :: jnoxfl, nlicmp, mxcmp, icmpmx
    parameter (mxcmp=100)
!
    character(len=2) :: typ
    character(len=8) :: k8b, ma, nomgd, nogdsi, gdcns
    character(len=8) :: evoim, licmp(20), chcity(mxcmp)
    character(len=16) :: mfac, k16b, motcle(5), phenom, typco, chcino(mxcmp)
    character(len=19) :: chci, cns, cns2, depla, noxfem
    character(len=24) :: cino, cnuddl, cvlddl, nprol
    character(len=80) :: titre
    logical :: lxfem
    data nprol/'                   .PROL'/
! --- DEBUT -----------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
    chci = chcine
    call jeveuo(chci//'.AFCK', 'E', jafck)
    mfac = mfact
    call getfac(mfac, noc)
!
    call dismoi('F', 'NOM_MAILLA', mo, 'MODELE', ibid,&
                ma, ier)
    call dismoi('F', 'PHENOMENE', mo, 'MODELE', ibid,&
                phenom, ier)
    call dismoi('F', 'NOM_GD', phenom, 'PHENOMENE', ibid,&
                nomgd, ier)
    call dismoi('F', 'NOM_GD_SI', nomgd, 'GRANDEUR', ibid,&
                nogdsi, ier)
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
            zk8(jafck-1+1)='CITH_FT'
        else if (typco.eq.'EVOL_ELAS'.or.typco.eq.'EVOL_NOLI') then
            zk8(jafck-1+1)='CIME_FT'
        else
            ASSERT(.false.)
        endif
        zk8(jafck-1+3)=evoim
!
!       -- C'EST LE CHAMP DU 1ER NUMERO D'ORDRE QUI IMPOSE SA LOI:
        call rsorac(evoim, 'PREMIER', ibid, rbid, k8b,&
                    cbid, 0.d0, 'ABSOLU', iord, 1,&
                    iret)
        ASSERT(iret.eq.1)
        if (mfac .eq. 'MECA_IMPO') then
            call rsexch('F', evoim, 'DEPL', iord(1), depla,&
                        iret)
        else
            call rsexch('F', evoim, 'TEMP', iord(1), depla,&
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
    do 101 ioc = 1, noc
        call getmjm(mfac, ioc, mxcmp, chcino, chcity,&
                    nbobm)
        ASSERT(nbobm.gt.0)
        do 111 iddl = 1, nbobm
            icmp = indik8( zk8(jcmp), chcino(iddl)(1:8), 1, nbcmp )
            icmpmx=max(icmpmx,icmp)
111      continue
101  end do
    ASSERT(icmpmx.gt.0)
    call cnscre(ma, gdcns, icmpmx, zk8(jcmp), 'V',&
                cns)
!
!
! --- REMPLISSAGE DU CHAM_NO_S
    call jeveuo(cns//'.CNSV', 'E', jcnsv)
    call jeveuo(cns//'.CNSL', 'E', jcnsl)
!
    do 100 ioc = 1, noc
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
        do 110 iddl = 1, nbobm
            k16b = chcino(iddl)
            do 112 i = 1, 5
                if (k16b .eq. motcle(i)) goto 110
112          continue
!
            zk8(idnddl+nbddl) = k16b(1:8)
!
! ------- VERIFICATION QUE LA COMPOSANTE EXISTE DANS LA GRANDEUR
            icmp = indik8( zk8(jcmp), k16b(1:8), 1, nbcmp )
            ASSERT(icmp .ne. 0)
!
! ------- VERIFICATION DE LA COMPOSANTE SUR LES NOEUDS XFEM
            if (lxfem) then
                if (k16b(1:1) .eq. 'D') then
                    do 113 ino = 1, nbno
                        nuno = zi(idino-1+ino)
                        ASSERT(.not.zl(jnoxfl-1+2*nuno))
113                  continue
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
110      continue
!
! --- ON RECHERCHE SI UNE QUAND ON A DES FONCT. IL Y EN A UNE = F(TPS)
!
        if (type .eq. 'F') then
            do 120 i = 1, nbddl
                nprol(1:8) = zk8(idvddl-1+i)
                call jeveuo(nprol, 'L', idprol)
                if (zk24(idprol+2) .eq. 'INST') then
                    zk8(jafck)(5:7) = '_FT'
                    goto 122
                    else if (( zk24(idprol).eq.'NAPPE').and. ( zk24(&
                idprol+6).eq.'INST')) then
                    zk8(jafck)(5:7) = '_FT'
                    goto 122
                endif
120          continue
        endif
122      continue
!
! ----- AFFECTATION DANS LE CHAM_NO_S
!
        if (type .eq. 'R') then
            do 130 cmp = 1, nbddl
                k8b = zk8(idnddl-1+cmp)
                icmp = indik8( zk8(jcmp), k8b, 1, nbcmp )
                do 132 ino = 1, nbno
                    nuno = zi(idino-1+ino)
                    zr(jcnsv+(nuno-1)*icmpmx+icmp-1) = zr(idvddl-1+ cmp)
                    zl(jcnsl+(nuno-1)*icmpmx+icmp-1) = .true.
132              continue
130          continue
        else if (type .eq. 'C') then
            do 140 cmp = 1, nbddl
                k8b = zk8(idnddl-1+cmp)
                icmp = indik8( zk8(jcmp), k8b, 1, nbcmp )
                do 142 ino = 1, nbno
                    nuno = zi(idino-1+ino)
                    zc(jcnsv+(nuno-1)*icmpmx+icmp-1) = zc(idvddl-1+ cmp)
                    zl(jcnsl+(nuno-1)*icmpmx+icmp-1) = .true.
142              continue
140          continue
        else if (type .eq. 'F') then
            do 150 cmp = 1, nbddl
                k8b = zk8(idnddl-1+cmp)
                icmp = indik8( zk8(jcmp), k8b, 1, nbcmp )
                do 152 ino = 1, nbno
                    nuno = zi(idino-1+ino)
                    zk8(jcnsv+(nuno-1)*icmpmx+icmp-1) = zk8(idvddl-1+ cmp)
                    zl(jcnsl+(nuno-1)*icmpmx+icmp-1) = .true.
152              continue
150          continue
        endif
!
        call jedetr(cino)
        call jedetr(cnuddl)
        call jedetr(cvlddl)
!
100  end do
200  continue
!
!
    if (( niv.ge.2) .and. (evoim.eq.' ')) then
        titre = '******* IMPRESSION DU CHAMP DES DEGRES IMPOSES *******'
        call imprsd('CHAMP_S', cns, ifm, titre)
    endif
!
!
! --- CREATION DE LA SD AFFE_CHAR_CINE
    call chcsur(chci, cns, type, mo, nogdsi)
    call detrsd('CHAMP', cns)
!
!     -- SI EVOL_IMPO : IL NE FAUT PAS UTILISER LES VALEURS DE CHCI :
    if (evoim .ne. ' ') call jedetr(chci//'.AFCV')
!
    if (lxfem) then
        call jedetr(noxfem)
    endif
!
    call jedema()
end subroutine
