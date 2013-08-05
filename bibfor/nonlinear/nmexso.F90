subroutine nmexso(noma, result, sddyna, numedd)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! aslint: disable=
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
#include "asterfort/ndynkk.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: noma, result
    character(len=19) :: sddyna
    character(len=24) :: numedd
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (DYNAMIQUE)
!
! INITIALISATION FORCES DE SOL
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  RESULT : NOM DU RESULTAT
! IN  SDDYNA : SD DYNAMIQUE
! IN  NUMEDD : NUME_DDL
!
! ----------------------------------------------------------------------
!
    character(len=15) :: sdexso
    character(len=19) :: sdexsz
    character(len=8) :: k8bid, cnfsol
    character(len=8) :: nomacr
    character(len=24) :: magrno, maille, nprno
    character(len=24) :: tabequ, tabfrq, tabinf, nomres
    integer :: ieqint, jfrq, iddint, jnomre
    character(len=24) :: gnintf, tabrig, tabmas, tabamo
    integer :: jrig, jmas, jamo
    integer :: gd, aprno
    real(kind=8) :: pasa, pasm, pas, ainst, rinst
    integer :: iamacr, idno
    integer :: jdveis
    integer :: ibid, iret
    integer :: ifreq, i1, i2, inoe, ino, ima, iddl, icmp
    character(len=24) :: uniamo, unirig, unimas, unifor
    integer :: unitea, uniter, unitem, unitef
    integer :: unifrq
    integer :: nfreq, nfreqm, nfreqa
    integer :: nbmode, nbmod2, nbno, nddint, ncmp, nec, neq
    character(len=24) :: nchsol
    integer :: jchsol
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    tabfrq = '&&NMEXSO.FREQ'
!
! --- ACCES NUMEROTATION
!
    call dismoi('F', 'NB_EQUA', numedd, 'NUME_DDL', neq,&
                k8bid, iret)
    call dismoi('F', 'NUM_GD_SI', numedd, 'NUME_DDL', gd,&
                k8bid, iret)
    nec = nbec(gd)
!
! --- ACCES SD EXCIT_SOL
!
    call ndynkk(sddyna, 'SDEXSO', sdexsz)
    sdexso = sdexsz(1:15)
!
! --- SAUVEGARDE NOM DU RESULTAT
!
    nomres = sdexso(1:15)//'.RESU'
    call wkvect(nomres, 'V V K8', 1, jnomre)
    zk8(jnomre) = result
!
! --- RECUPERATION CHARGE
!
    nchsol = sdexso(1:15)//'.CHAR'
    call jeveuo(nchsol, 'L', jchsol)
    cnfsol = zk8(jchsol)
    call jeveuo(cnfsol//'.CHME.VEISS', 'L', jdveis)
!
! --- NOMS FICHIERS
!
    unirig = zk24(jdveis-1+1)
    unimas = zk24(jdveis-1+2)
    uniamo = zk24(jdveis-1+3)
    unifor = zk24(jdveis-1+4)
    uniter = 0
    unitem = 0
    unitea = 0
    unitef = 0
!
! --- GROUP_NO_INTERF
!
    gnintf = zk24(jdveis-1+5)
    if (gnintf .eq. ' ') then
        maille = zk24(jdveis-1+6)
        call jeveuo(noma//'.NOMACR', 'L', iamacr)
        call jenonu(jexnom(noma//'.SUPMAIL', maille), ima)
        nomacr = zk8(iamacr-1+ima)
        call jelira(nomacr//'.LINO', 'LONMAX', nbno, k8bid)
        call jeveuo(nomacr//'.LINO', 'L', idno)
    else
        magrno = noma(1:8)//'.GROUPENO'
        call jelira(jexnom(magrno, gnintf), 'LONUTI', nbno, k8bid)
        call jeveuo(jexnom(magrno, gnintf), 'L', idno)
    endif
!
! --- NOMBRE DE DDL INTERNES
!
    nprno = numedd(1:14)//'.NUME.PRNO'
    call jenonu(jexnom(nprno(1:19)//'.LILI', '&MAILLA'), ibid)
    call jeveuo(jexnum(nprno, ibid), 'L', aprno)
    nddint = 0
    do 10 ino = 1, nbno
        inoe = zi(idno+ino-1)
        ncmp = zi(aprno + (nec+2)*(inoe-1) + 2 - 1 )
        nddint = nddint+ncmp
10  end do
!
! --- TABLEAU DES NUMEROS D EQUATION ACTIFS DE L INTERFACE
!
    tabequ = sdexso(1:15)//'.EQINT'
    call wkvect(tabequ, 'V V I', nddint, ieqint)
    iddint=0
    do 20 ino = 1, nbno
        inoe = zi(idno+ino-1)
        ncmp = zi(aprno + (nec+2)*(inoe-1) + 2 - 1 )
        iddl = zi(aprno + (nec+2)*(inoe-1) + 1 - 1 )
        do 21 icmp = 1, ncmp
            iddint = iddint+1
            zi(ieqint+iddint-1) = iddl+icmp-1
21      continue
20  end do
!
! --- OUVERTURE DES FICHIERS
!
    if (unirig .ne. ' ') read (unirig,'(I24)') uniter
    if (unimas .ne. ' ') read (unimas,'(I24)') unitem
    if (uniamo .ne. ' ') read (uniamo,'(I24)') unitea
    if (unifor .ne. ' ') read (unifor,'(I24)') unitef
!
! --- QUEL FICHIER VA DONNER LA FREQUENCE ?
!
    unifrq = 0
    if (uniter .eq. 0) then
        if (unitem .eq. 0) then
            unifrq = unitea
        else
            unifrq = unitem
        endif
    else
        unifrq = uniter
    endif
!
    if (unifrq .eq. 0) ASSERT(.false.)
    if (unifrq .eq. uniter) call u2mess('I', 'DYNAMIQUE_20')
    if (unifrq .eq. unitem) call u2mess('I', 'DYNAMIQUE_21')
    if (unifrq .eq. unitea) call u2mess('I', 'DYNAMIQUE_22')
!
! --- LECTURE DU PAS D'ACTUALISATION
!
    rewind unifrq
    read(unifrq,*) ainst,pas
    nfreq = nint(ainst)
    call u2mesg('I', 'DYNAMIQUE_23', 0, k8bid, 1,&
                nfreq, 1, pas)
!
! --- TABLEAU DES FREQUENCES
!
    call wkvect(tabfrq, 'V V R', nfreq, jfrq)
    do 110 ifreq = 1, nfreq
        zr(jfrq+ifreq-1) = (ifreq-1)*pas
110  end do
!
! --- VERIFICATIONS
!
    if (unitem .ne. 0) then
        if (unifrq .ne. unitem) then
            rewind unitem
            read(unitem,*) ainst,pasm
            nfreqm = int(ainst)
            if (nfreqm .ne. nfreq) then
                call u2mess('F', 'DYNAMIQUE_30')
            endif
        endif
    endif
    if (unitea .ne. 0) then
        if (unifrq .ne. unitea) then
            rewind unitea
            read(unitea,*) ainst,pasa
            nfreqa = int(ainst)
            if (nfreqa .ne. nfreq) then
                call u2mess('F', 'DYNAMIQUE_31')
            endif
        endif
    endif
!
! --- TAILLE TABLEAUX
!
    nbmode = nddint
    nbmod2 = nbmode*nbmode
!
! --- SAUVEGARDE INFORMATIONS
!
    tabinf = sdexso(1:15)//'.TABI'
    call wkvect(tabinf, 'V V R', 3, iddint)
    zr(iddint-1+1) = pas
    zr(iddint-1+2) = unitef
    zr(iddint-1+3) = nddint
!
! --- LECTURE MATRICE REDUITE RIGIDITE A L'INTERFACE
!
    tabrig = sdexso(1:15)//'.RIGT'
    call wkvect(tabrig, 'V V R', nbmod2*nfreq, jrig)
    if (uniter .ne. 0) then
        do 120 ifreq = 1, nfreq
            read(uniter,*) rinst
            read(uniter,1000) ((zr(jrig+(ifreq-1)*nbmod2+(i2-1)&
            *nbmode+i1-1), i2=1,nbmode),i1=1,nbmode)
120      continue
    endif
!
! --- LECTURE MATRICE REDUITE MASSE A L'INTERFACE
!
    tabmas = sdexso(1:15)//'.MAST'
    call wkvect(tabmas, 'V V R', nbmod2*nfreq, jmas)
    if (unitem .ne. 0) then
        do 130 ifreq = 1, nfreq
            read(unitem,*) rinst
            read(unitem,1000) ((zr(jmas+(ifreq-1)*nbmod2+(i2-1)&
            *nbmode+i1-1), i2=1,nbmode),i1=1,nbmode)
130      continue
    endif
!
! --- LECTURE MATRICE REDUITE AMORTISSEMENT A L'INTERFACE
!
    tabamo = sdexso(1:15)//'.AMOT'
    call wkvect(tabamo, 'V V R', nbmod2*nfreq, jamo)
    if (unitea .ne. 0) then
        do 140 ifreq = 1, nfreq
            read(unitea,*) rinst
            read(unitea,1000) ((zr(jamo+(ifreq-1)*nbmod2+(i2-1)&
            *nbmode+i1-1), i2=1,nbmode),i1=1,nbmode)
140      continue
    endif
!
    call jedetr(tabfrq)
    1000 format((6(1x,1pe13.6)))
    call jedema()
end subroutine
