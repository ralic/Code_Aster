subroutine elimdi(charge, lisrel, nomgd, nbdual, nbsurc)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterc/getvtx.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/juveca.h"
#include "asterfort/wkvect.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!
    character(len=19), intent(in) :: lisrel
    character(len=8), intent(in) :: charge
    character(len=8), intent(in) :: nomgd
    integer, intent(out) :: nbdual
    integer, intent(out) :: nbsurc
!
! -----------------------------------------------------------------
!  MODIFICATION DE LA SD_LISTE_RELA  LISREL :
!
!  SI METHODE='ELIMINATION' :
!      * ON CREE UNE SD_CHAR_CINE POUR LES RELATIONS N'AYANT
!        QU'UN SEUL TERME.
!      * ON SUPPRIME DE LISREZ LES RELATIONS ELIMINEES
! -----------------------------------------------------------------
!   OUT : NBDUAL : NOMBRE DE RELATIONS A DUALISER
!   OUT : NBSURC : NOMBRE DE RELATIONS EN DOUBLON
!
!
! --------- VARIABLES LOCALES ---------------------------
    character(len=24) :: meth
    character(len=19) :: charci
    character(len=4) :: typcoe, typval
    character(len=8) :: nono, nocmp, mailla, typeci
    character(len=8) :: kbid, modele
    integer :: nelim, irela, nbterm, jrlsu, jrlnt, jrlco, jrldd
    integer :: jrlpo, jrltc, jrlnr, nbrela, jrlno, ibid, indsur
    integer :: nucmp, jafci, jafck, jafcv, ideca1, nuno
    integer :: jrltv, jrlbe, ier, nelim1, ico, jncmp, nbcmp, jprnm
    integer :: nucmp2, icmp, nbec, ndumin
    logical :: elim
    real(kind=8) :: rcoef
    integer :: iarg
!
!
    call jemarq()
!
    call jeveuo(lisrel//'.RLSU', 'E', jrlsu)
    call jeveuo(lisrel//'.RLNR', 'L', jrlnr)
    call jeveuo(lisrel//'.RLNT', 'L', jrlnt)
    call jeveuo(lisrel//'.RLCO', 'L', jrlco)
    call jeveuo(lisrel//'.RLDD', 'L', jrldd)
    call jeveuo(lisrel//'.RLNO', 'L', jrlno)
    call jeveuo(lisrel//'.RLPO', 'L', jrlpo)
    call jeveuo(lisrel//'.RLTC', 'L', jrltc)
    call jeveuo(lisrel//'.RLTV', 'L', jrltv)
    call jeveuo(lisrel//'.RLBE', 'L', jrlbe)
!
    nbrela=zi(jrlnr)
    ASSERT(nbrela.gt.0)
    typcoe=zk8(jrltc)(1:4)
    typval=zk8(jrltv)(1:4)
!
!
    if (getexm(' ','METHODE') .eq. 1) then
        call getvtx(' ', 'METHODE', 0, iarg, 1,&
                    meth, ibid)
        ASSERT(ibid.eq.1)
        elim=(meth.eq.'ELIMINATION')
    else
        elim=.false.
    endif
    if (.not.elim) goto 999
!
!
!
!     1. Y-A-T-IL DES RELATIONS CANDIDATES A L'ELIMINATION ?
!        ON ELIMINE LES RELATIONS N'AYANT QU'UN SEUL TERME
!     -------------------------------------------------------
    nelim=0
    do irela = 1, nbrela
        indsur=zi(jrlsu-1+irela)
        if (indsur .ne. 1) then
            ASSERT(indsur.eq.0)
            nbterm=zi(jrlnt+irela-1)
            if (nbterm .eq. 1) then
                nelim=nelim+1
                zi(jrlsu-1+irela)=2
            endif
        endif
    end do
    if (nelim .eq. 0) goto 999
!
!
!     1B. ON CONSERVE QUELQUES RELATIONS DUALISEES POUR QUE
!        STAT_NON_LINE PUISSE CALCULER RESI_GLOB_RELA
!        NDUMIN : NOMBRE MINIMUM DE RELATIONS DUALISEES A CONSERVER
!     ---------------------------------------------------------------
    ndumin=0
    nelim=max(nelim-ndumin,0)
!
    ico=0
    do irela = 1, nbrela
        indsur=zi(jrlsu-1+irela)
        if (indsur .eq. 2) then
            ico=ico+1
            if (ico .gt. nelim) then
                zi(jrlsu-1+irela)=0
            else
            endif
        endif
    end do
    if (nelim .eq. 0) goto 999
!
!
!
!     2. ALLOCATION/RECUPERATION DE LA SD_CHAR_CINE :
!     ----------------------------------------------
    charci=charge//'.ELIM'
    call dismoi('F', 'NOM_MODELE', charge, 'CHARGE', ibid,&
                modele, ibid)
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                mailla, ibid)
    call jeexin(charci//'.AFCK', ier)
    if (ier .eq. 0) then
        call wkvect(charci//'.AFCK', 'G V K8', 3, jafck)
        zk8(jafck-1+2)=modele
        typeci='CIXX_YY'
        if (nomgd .eq. 'DEPL_R') then
            typeci(3:4)='ME'
        else if (nomgd.eq.'TEMP_R') then
            typeci(3:4)='TH'
        else if (nomgd.eq.'PRES_C') then
            typeci(3:4)='AC'
        else
            ASSERT(.false.)
        endif
        if (typval .eq. 'REEL') then
            typeci(6:7)='RE'
        else if (typval.eq.'COMP') then
            typeci(6:7)='CX'
        else if (typval.eq.'FONC') then
            typeci(6:7)='FO'
        else
            ASSERT(.false.)
        endif
        zk8(jafck-1+1)=typeci
!
        call wkvect(charci//'.AFCI', 'G V I', 3*nelim+1, jafci)
        zi(jafci-1+1)=nelim
!
        if (typval .eq. 'REEL') then
            call wkvect(charci//'.AFCV', 'G V R', nelim, jafcv)
        else if (typval.eq.'COMP') then
            call wkvect(charci//'.AFCV', 'G V C', nelim, jafcv)
        else if (typval.eq.'FONC') then
            call wkvect(charci//'.AFCV', 'G V K8', nelim, jafcv)
        endif
        ico=0
    else
        call jeveuo(charci//'.AFCI', 'L', jafci)
        nelim1=zi(jafci-1+1)
        call juveca(charci//'.AFCI', 3*(nelim1+nelim)+1)
        call juveca(charci//'.AFCV', (nelim1+nelim))
        call jeveuo(charci//'.AFCI', 'E', jafci)
        zi(jafci-1+1)=nelim1+nelim
        ico=nelim1
    endif
!
!
!
!     3. REMPLISSAGE DE LA SD_CHAR_CINE :
!     ------------------------------------
    call jeveuo(charci//'.AFCI', 'E', jafci)
    call jeveuo(charci//'.AFCV', 'E', jafcv)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', jncmp)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', nbcmp, kbid)
    call jelira(lisrel//'.RLCO', 'TYPE', ibid, kbid)
    ASSERT(kbid.eq.'R'.or.kbid.eq.'C')
    if (kbid .eq. 'R') ASSERT(typcoe.eq.'REEL')
    if (kbid .eq. 'C') ASSERT(typcoe.eq.'COMP')
    call dismoi('F', 'NB_EC', nomgd, 'GRANDEUR', nbec,&
                kbid, ibid)
    call jeveuo(modele//'.MODELE    .PRNM', 'L', jprnm)
!
    do irela = 1, nbrela
        if (zi(jrlsu-1+irela) .ne. 2) goto 21
!
        ico=ico+1
        ideca1=zi(jrlpo-1+irela)-zi(jrlnt-1+irela)+1
!
!       -- ON VERIFIE QUE LE COEFFICIENT EST BIEN 1. :
        if (typcoe .eq. 'REEL') then
            rcoef=zr(jrlco-1+ideca1)
        else if (typval.eq.'COMP') then
            rcoef=abs(zc(jrlco-1+ideca1))
        endif
        ASSERT(abs(rcoef-1.d0).lt.1.d-4)
!
        nono=zk8(jrlno-1+ideca1)
        nocmp=zk8(jrldd-1+ideca1)
        call jenonu(jexnom(mailla//'.NOMNOE', nono), nuno)
        nucmp=indik8(zk8(jncmp),nocmp,1,nbcmp)
        ASSERT(nucmp.gt.0)
!
!       -- ON COMPTE LES CMPS PORTEES PAR LE NOEUD <= NUCMP
        nucmp2 = 0
        do icmp = 1, nbcmp
            if (exisdg(zi(jprnm-1+nbec*(nuno-1)+1),icmp)) then
                nucmp2 = nucmp2 + 1
                if (icmp .eq. nucmp) goto 123
            endif
        enddo
        ASSERT(.false.)
123     continue
!
        zi(jafci+3*(ico-1)+1)=nuno
        zi(jafci+3*(ico-1)+2)=nucmp2
        if (typval .eq. 'REEL') then
            zr(jafcv-1+ico)=zr(jrlbe-1+irela)
        else if (typval.eq.'COMP') then
            zc(jafcv-1+ico)=zc(jrlbe-1+irela)
        else if (typval.eq.'FONC') then
            ASSERT(zk24(jrlbe-1+irela)(9:24).eq.' ')
            zk8(jafcv-1+ico)=zk24(jrlbe-1+irela)(1:8)
        else
            ASSERT(.false.)
        endif
21      continue
!
    end do
!
!
!
!     4. CALCUL DE NBDUAL ET NBSURC:
!     ------------------------------
999 continue
    nbdual=0
    nbsurc=0
    do irela = 1, nbrela
        if (zi(jrlsu-1+irela) .eq. 0) nbdual=nbdual+1
        if (zi(jrlsu-1+irela) .eq. 1) nbsurc=nbsurc+1
    end do
!
    call jedema()
end subroutine
