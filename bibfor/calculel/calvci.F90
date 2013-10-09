subroutine calvci(nomci, nomnu, nbchci, lchci, inst,&
                  base)
    implicit none
! person_in_charge: jacques.pellet at edf.fr
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
!
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/cnocns.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/fointe.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsinch.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: nomci, lchci(*), nomnu
    character(len=1) :: base
    real(kind=8) :: inst
    integer :: nbchci
! ----------------------------------------------------------------------
! BUT  :  CALCUL DU CHAM_NO CONTENANT UN VECTEUR LE CINEMATIQUE
! ---     ASSOCIE A UNE LISTE DE CHAR_CINE_* A UN INSTANT INST
!         CHAR_CINE ET AYANT COMME PROF_CHNO CELUI DE NOMNU
!                              ---
!                              ! 0. SI I DDLS NON IMPOSE DANS
!         NOMCI(1:19).VALE(I) =!       LA LISTE DES CHAR_CINE
!                              ! U0(NI,INST) SINON
!                              ---
!             OU NI EST LE NUMERO DANS LE MAILLAGE DU NOEUD
!                   SUPPORTANT LE DDL NUMERO I DANS LA NUMEROTATION
!          U0(NI,INST)= VALEUR DU CHARGEMENT ASSOCIE A LA
!                       DERNIERE CHAR_CINE IMPOSANT I
! ----------------------------------------------------------------------
! IN/JXVAR  K*19 NOMCI  : NOM DU CHAM_NO CREE A PARTIR DE LA LISTE DE
!                   CHAR_CINE ET AYANT COMME PROF_CHNO CELUI DE NOMNU
! IN  K*14 NOMNU  : NOM DE LA NUMEROTATION SUPPORTANT LE CHAM_NO
! IN  I    NBCHCI : NOMBRE DE CHAR_CINE DE LA LISTE LCHCI
! IN  K*24 LCHCI  : LISTE DES NOMS DES CHARGES CINEMATIQUES ENTRANT
!                   DANS LE CALCUL DU CHAM_NO NOMCI
! IN  R*8  INST   : INSTANT
! IN  K*1  BASE   : BASE SUR LAQUELLE ON CREE LE CHAM_NO
!-----------------------------------------------------------------------
!     FONCTIONS JEVEUX
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!     VARIABLES LOCALES
!----------------------------------------------------------------------
    integer :: iddes, nec, ivvale, jnueq, jprno, icoor, ichcin, jafci
    integer :: jafcv, nbimp, nimp, n, ni, nddl, nn, nueq, ier
    integer :: neq, numgd, idtyp, jdlci, jafck
    integer :: jcn1k, jcn1d, jcn1c, jcn1v, jcn1l, icmp, icmp1, ino, jnocmp
    integer :: nbcmp1, jdeeq, imaill, vali(1)
    character(len=1) :: typval
    character(len=4) :: phen
    logical :: fonc
    real(kind=8) :: valp(4), res, valr(1)
    character(len=8) :: nomma, gd, nomf, evoim, nocmp, nomch
    character(len=14) :: nu
    character(len=16) :: nomp(4)
    character(len=19) :: vcine, charci, cnoimp, cnsimp
    character(len=24) :: vvale, valk(4)
!----------------------------------------------------------------------
!                DEBUT DES INSTRUCTIONS
!----------------------------------------------------------------------
    call jemarq()
    if (nbchci .eq. 0) goto 9999
    vcine = nomci
    nu = nomnu
    vvale = vcine//'.VALE'
    valr(1)=inst
    cnoimp='&&CALVCI.CNOIMP'
    cnsimp='&&CALVCI.CNSIMP'
!
!
! --- CREATION DU CHAM_NO ( SI IL EXISTE DEJA ON LE DETRUIT )
!     ---------------------------------------------------------
    call detrsd('CHAMP_GD', vcine)
    call jedetr(vcine//'.DLCI')
    call dismoi('NB_EQUA', nu, 'NUME_DDL', repi=neq)
    call dismoi('NOM_GD', nu, 'NUME_DDL', repk=gd)
    call dismoi('NOM_MAILLA', nu, 'NUME_DDL', repk=nomma)
    call jenonu(jexnom('&CATA.GD.NOMGD', gd), numgd)
    call jeveuo('&CATA.GD.TYPEGD', 'L', idtyp)
    typval = zk8(idtyp-1+numgd)
    call jeveuo(jexnum('&CATA.GD.DESCRIGD', numgd), 'L', iddes)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', numgd), 'L', jnocmp)
    nec = zi(iddes+2 )
    call vtcreb(vcine, nu, base, typval, neq)
!
! --- ALLOCATION DE VCINE.DLCI QUI SERVIRA DANS NMCVCI :
    call wkvect(vcine//'.DLCI', 'V V I', neq, jdlci)
!
    call jeveuo(vvale, 'E', ivvale)
    call jeveuo(nu//'.NUME.NUEQ', 'L', jnueq)
    call jeveuo(nu//'.NUME.DEEQ', 'L', jdeeq)
    call jenonu(jexnom(nu//'.NUME.LILI', '&MAILLA'), imaill)
    call jeveuo(jexnum(nu//'.NUME.PRNO', imaill), 'L', jprno)
    call jeveuo(nomma//'.COORDO    .VALE', 'L', icoor)
!
!
! --- BOUCLE SUR LES CHARGES CINEMATIQUES :
    do 1 ichcin = 1, nbchci
        charci = lchci(ichcin)
        call jeveuo(charci//'.AFCK', 'L', jafck)
        phen=zk8(jafck-1+1)(1:4)
        fonc=zk8(jafck-1+1)(5:7).eq.'_FT'
        evoim=zk8(jafck-1+3)
        call jeveuo(charci//'.AFCI', 'L', jafci)
        if (evoim .eq. ' ') call jeveuo(charci//'.AFCV', 'L', jafcv)
!
!
!       -- CAS DE EVOL_IMPO : ON PREPARE ...
!       ---------------------------------------
        if (evoim .ne. ' ') then
!         -- IL FAUT INTERPOLER EVOIM A L'INSTANT: INST :
            if (gd .eq. 'DEPL_R') then
                nomch='DEPL'
            else if (gd.eq.'TEMP_R') then
                nomch='TEMP'
            else
                ASSERT(.false.)
            endif
            ASSERT(fonc)
            call rsinch(evoim, nomch, 'INST', inst, cnoimp,&
                        'EXCLU', 'EXCLU', 2, 'V', ier)
            call cnocns(cnoimp, 'V', cnsimp)
            call detrsd('CHAMP', cnoimp)
            call jeveuo(cnsimp//'.CNSK', 'L', jcn1k)
            call jeveuo(cnsimp//'.CNSD', 'L', jcn1d)
            call jeveuo(cnsimp//'.CNSC', 'L', jcn1c)
            call jelira(cnsimp//'.CNSC', 'LONMAX', nbcmp1)
            ASSERT(nbcmp1.eq.zi(jcn1d-1+2))
            call jeveuo(cnsimp//'.CNSV', 'L', jcn1v)
            call jeveuo(cnsimp//'.CNSL', 'L', jcn1l)
            valk(1)=evoim
        endif
!
!
!       -- AFFECTATION DES VALEURS IMPOSEES
!       ---------------------------------------
        nbimp = zi(jafci)
!
!
!
!       -- CAS DES VALEURS REELLES :
!       ---------------------------------
        if (typval .eq. 'R') then
            do 10 nimp = 1, nbimp
                n =3*(nimp-1)+jafci
!           -- NI : NUMERO DU NOEUD
                ni = zi(n+1)
!           -- NDDL : NUMERO DE LA COMPOSANTE (POUR LE NOEUD NI)
                nddl = zi(n+2)
                nn = (nec+2)*(ni-1)
                nueq =zi(jnueq-1+ zi(jprno+nn)+nddl-1)
!
!
!           -- CAS EVOL_IMPO (CNSIMP):
!           ----------------------------------
                if (evoim .ne. ' ') then
                    ino=zi(jdeeq-1+2*(nueq-1)+1)
                    icmp=zi(jdeeq-1+2*(nueq-1)+2)
                    ASSERT(ino.eq.ni)
                    nocmp=zk8(jnocmp-1+icmp)
                    vali(1)=ino
                    valk(2)=nocmp
                    icmp1=indik8(zk8(jcn1c),nocmp,1,nbcmp1)
                    ASSERT(icmp1.gt.0)
                    if (.not.zl(jcn1l-1+(ino-1)*nbcmp1+icmp1)) then
                        call utmess('F', 'CALCULEL_2', nk=2, valk=valk, si=vali(1),&
                                    sr=valr(1))
                    endif
                    res = zr(jcn1v-1+(ino-1)*nbcmp1+icmp1)
                    zr(ivvale-1+nueq) = res
!
!
!           -- CAS "NORMAL" (OBJET .AFCV) :
!           ----------------------------------
                else if (.not.fonc) then
                    zr(ivvale-1+nueq) = zr(jafcv-1+nimp)
!
!
!           -- CAS FONCTION :
!           -----------------
                else if (fonc) then
                    nomf = zk8(jafcv-1+nimp)
                    nomp(1)='INST'
                    nomp(2)='X'
                    nomp(3)='Y'
                    nomp(4)='Z'
                    valp(1)=inst
                    valp(2)=zr(icoor+3*(ni-1)+0)
                    valp(3)=zr(icoor+3*(ni-1)+1)
                    valp(4)=zr(icoor+3*(ni-1)+2)
                    call fointe('F ', nomf, 4, nomp, valp,&
                                res, ier)
                    zr(ivvale-1+nueq) = res
                else
                    call utmess('F', 'CALCULEL_37')
                endif
!
                zi(jdlci-1+nueq) = 1
 10         continue
!
!
!
!       -- CAS DES VALEURS COMPLEXES :
!       ---------------------------------
        else if (typval.eq.'C') then
            ASSERT(phen.eq.'CIAC')
            ASSERT(.not.fonc)
            do 20 nimp = 1, nbimp
                n =3*(nimp-1)+jafci
                ni = zi(n+1)
                nddl = zi(n+2)
                nn = (nec+2)*(ni-1)
                nueq =zi(jnueq-1+ zi(jprno+nn)+nddl-1)
                zc(ivvale-1+nueq) = zc(jafcv-1+nimp)
                zi(jdlci-1+nueq) = 1
 20         continue
!
!
        else
            ASSERT(.false.)
        endif
  1 end do
!
9999 continue
!
    call detrsd('CHAMP', cnsimp)
    call jedema()
end subroutine
