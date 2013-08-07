subroutine reexi1(nu, mo, ma, nlili, nm,&
                  nl, nbntt)
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
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: mo, ma
    character(len=14) :: nu
    integer :: nlili, nm, nl, nbntt
! ----------------------------------------------------------------------
!     BUT:  CETTE ROUTINE SERT A :
!
!           1) RENDRE : MO, MA, NLILI, NM, NL, NBNTT
!
!           2) RENDRE DANS LE NUME_DDL L' OBJET .EXI1
!              (POUR DIRE QUELS SONT LES NOEUDS (TARDIFS OU NON)
!               INTERVENANT REELLEMENT DANS LA NUMEROTATION
!               PLUS L'EXISTENCE D'UN NOEUD FACTICE 0 )
!             ATTENTION : CET OBJET A UN CONTENU PLUS RICHE QUE
!                         NECESSAIRE EN PREVISION DE 'RCMK'
!
!     IN:
!     ---
!       NU : NOM DU NUME_DDL  AUQUEL ON VA AJOUTER  L' OBJET .EXI1
!           (ON SE SERT EN ENTREE DU SEUL OBJET NU//'.NUME.LILI')
!
!     OUT:
!     ---- MO : NOM DU MODELE SOUS-JACENT AU NUME_DDL
!          MA : NOM DU MAILLAGE SOUS-JACENT AU NUME_DDL
!          NLILI: NOMBRE DE LIGREL DE L'OBJET .LILI
!
!         SOIT NM LE NOMBRE DE NOEUDS PHYSIQUES DU MAILLAGE (LILI(1))
!              NL LE NOMBRE DE NOEUDS TARDIFS DU MAILLAGE
!              N2 LE NOMBRE DE NOEUDS TARDIFS DU MODELE (LILI(2))
!              N3 LE NOMBRE DE NOEUDS TARDIFS DE LA 1ERE CHARGE(LILI(3))
!              .....
!              NP LE NOMBRE DE NOEUDS TARDIFS DE LA DERE CHARGE(LILI(P))
!
!          NBNOM = NM+NL      (NOMBRE MAX DE NOEUDS DU MAILLAGE)
!          NBNOT = N2+...+NP  (NOMBRE MAX DE NOEUDS TARDIFS DU MODELE
!                              ET DE LA LISTE DE CHARGES)
!          NBNTT = NBNOM+NBNOT
!
!        NU EST COMPLETE PAR .EXI1
!        -------------------------
!        .EXI1(*) EST DIMENSIONNE A NBNTT+1
!
!        SOIT LA NUMEROTATION IMPLICITE TOTALE :
!          1- LE NOEUD FACTICE 0
!          1- LES NOEUDS PHYSIQUES DU MAILLAGE (NI) / ORDRE DE .NOMNOE
!          3- LES NOEUDS TARDIFS DU MAILLAGE (&I)   /
!          4- LES NOEUDS TARDIFS DU MODELE   (&LMI)
!          5- LES NOEUDS TARDIFS DE LA CHARGE 1 (&LCH1I)
!           - ...
!           - LES NOEUDS TARDIFS DE LA CHARGE P (&LCH1P)
!
!        .EXI1(0)=1
!        POUR I=1,NBNTT
!        .EXI1(I) >0 SI LE NOEUD I EXISTE DANS LE NUME_DDL
!                 =0 SINON
!                  (EN FAIT EXI1(I) CONTIENT UN MAJORANT DU NOMBRE
!                  DE NOEUDS CONNECTES AU NOEUD I. CET OBJET SERVIRA
!                  DANS 'RCMK' A ALLOUER LA TABLE DE CONNECTIVITE
!                  INVERSE)
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=8) :: kbid, exiele
    character(len=24) :: nomli2, mo2
    character(len=19) :: nomlig
!
!
!     -- RECUPERATION DU NOM DU MODELE SOUS-JACENT A LA NUMEROTATION :
!     ----------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iaconx, iaexi1, iagrel, ialiel, iamail, ianbno
    integer :: ianema, iasssa, ibid, iel, ier, ierd, igrel
    integer :: iino, ilconx, ili, illiel, ilnema, ima, ino
    integer :: iret, j, jjno, jno, nbel, nbgrel, nbnm
    integer :: nbnom, nbnot, nbsma, nbssa, nma
!-----------------------------------------------------------------------
    call jemarq()
    call jelira(nu//'.NUME.LILI', 'NOMMAX', nlili)
    do 41, i=1,nlili
    call jenuno(jexnum(nu//'.NUME.LILI', i), mo2)
    if (mo2(9:15) .eq. '.MODELE') then
        mo= mo2(1:8)
        goto 42
    endif
    41 end do
    call u2mesk('F', 'ASSEMBLA_35', 1, nu)
!
42  continue
    call dismoi('F', 'NOM_MAILLA', mo, 'MODELE', ibid,&
                ma, ier)
!
!     -- CALCUL DE NBNOM:
!     -------------------
    call dismoi('F', 'NB_NO_MAILLA', mo, 'MODELE', nm,&
                kbid, ier)
    call dismoi('F', 'NB_MA_MAILLA', mo, 'MODELE', nma,&
                kbid, ier)
    call dismoi('F', 'NB_NL_MAILLA', mo, 'MODELE', nl,&
                kbid, ier)
    nbnom=nm+nl
!
!     -- CALCUL DE NBNOT ET NBNTT :
!     -----------------------------
    nbnot=0
    do 1 , ili=2,nlili
    call jenuno(jexnum(nu//'.NUME.LILI', ili), nomli2)
    nomlig=nomli2(1:19)
    call jeveuo(nomlig//'.NBNO', 'L', ianbno)
    nbnot= nbnot+zi(ianbno)
    1 end do
    nbntt= nbnom+nbnot
!
!
!     -- ALLOCATION DE .EXI1
!     ----------------------
    call wkvect(nu//'.EXI1', 'V V I', nbntt+1, iaexi1)
!
    zi(iaexi1) = 1
!
    if (nma .gt. 0) then
        call jeveuo(ma//'.CONNEX', 'L', iaconx)
        call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', ilconx)
    endif
!
!
!     -- 1ERE ETAPE : (SUPER)MAILLES DU MAILLAGE:
!     -------------------------------------------
    call dismoi('F', 'NB_SS_ACTI', mo, 'MODELE', nbssa,&
                kbid, ierd)
    call dismoi('F', 'NB_SM_MAILLA', mo, 'MODELE', nbsma,&
                kbid, ierd)
    if (nbssa .gt. 0) then
        call jeveuo(mo//'.MODELE    .SSSA', 'L', iasssa)
    else
        goto 22
    endif
!
    do 21, ima = 1, nbsma
    if (zi(iasssa-1+ima) .eq. 1) then
        call jeveuo(jexnum(ma//'.SUPMAIL', ima), 'L', iamail)
        call jelira(jexnum(ma//'.SUPMAIL', ima), 'LONMAX', nbnm)
        do 23, i=1,nbnm
        ino=zi(iamail-1+i)
        if (ino .eq. 0) call u2mess('F', 'ASSEMBLA_36')
        do 24, j=i+1,nbnm
        jno=zi(iamail-1+j)
        zi(iaexi1+ino)=zi(iaexi1+ino) +1
        zi(iaexi1+jno)=zi(iaexi1+jno) +1
24      continue
23      continue
    endif
    21 end do
!
22  continue
!
!     -- 2EME ETAPE : MAILLES TARDIVES (OU NON) DES LIGRELS:
!                     (MODELE + LISTE DE CHARGES)
!     ------------------------------------------------------
    nbnot=0
    do 31 , ili=2,nlili
    call jenuno(jexnum(nu//'.NUME.LILI', ili), nomli2)
    nomlig=nomli2(1:19)
    call dismoi('F', 'EXI_ELEM', nomlig, 'LIGREL', ibid,&
                exiele, ierd)
    if (exiele(1:3) .eq. 'NON') goto 31
!
    call jeveuo(nomlig//'.LIEL', 'L', ialiel)
    call jeveuo(jexatr(nomlig//'.LIEL', 'LONCUM'), 'L', illiel)
    call jelira(nomlig//'.LIEL', 'NMAXOC', nbgrel)
!
!
    call jeexin(nomlig//'.NEMA', iret)
    if (iret .gt. 0) then
        call jeveuo(nomlig//'.NEMA', 'L', ianema)
        call jeveuo(jexatr(nomlig//'.NEMA', 'LONCUM'), 'L', ilnema)
    endif
!
    do 32, igrel = 1, nbgrel
    nbel= zi(illiel-1+igrel+1)-zi(illiel-1+igrel) -1
    iagrel= ialiel + zi(illiel-1+igrel) -1
!
    do 33,iel= 1, nbel
    ima= zi(iagrel -1 +iel)
    if (ima .gt. 0) then
        nbnm= zi(ilconx-1+ima+1)-zi(ilconx-1+ima)
        iamail= iaconx + zi(ilconx-1+ima) -1
    else
        nbnm= zi(ilnema-1-ima+1)-zi(ilnema-1-ima) -1
        iamail = ianema + zi(ilnema-1-ima) -1
    endif
!
    do 34, i=1,nbnm
    ino=zi(iamail-1+i)
    if (ino .eq. 0) call u2mess('F', 'ASSEMBLA_36')
    iino=ino
    if (ino .lt. 0) iino=nbnom+nbnot-ino
    if (nbnm .eq. 1) zi(iaexi1+iino)=zi(iaexi1+iino) +1
    do 35, j=i+1,nbnm
    jno=zi(iamail-1+j)
    jjno=jno
    if (jno .lt. 0) jjno=nbnom+nbnot-jno
    zi(iaexi1+iino)=zi(iaexi1+iino) +1
    zi(iaexi1+jjno)=zi(iaexi1+jjno) +1
35  continue
34  continue
33  continue
32  continue
    call jeveuo(nomlig//'.NBNO', 'L', ianbno)
    nbnot= nbnot+zi(ianbno)
    31 end do
!
!
!
    call jedema()
end subroutine
