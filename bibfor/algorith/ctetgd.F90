subroutine ctetgd(basmod, numd, numg, nbsec, teta,&
                  nbtet)
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
!***********************************************************************
!    P. RICHARD     DATE 11/03/91
!-----------------------------------------------------------------------
!  BUT:     < CALCUL DE LA MATRICE TETA GAUCHE-DROITE >
!
!   CALCUL DE LA MATRICE TETA PERMETTANT DE PASSER DES  DDL DE
!  L'INTERFACE DROITE A CEUX DE L'INTERFACE GAUCHE
!
!-----------------------------------------------------------------------
!
! BASMOD   /I/: NOM UTLISATEUR DE LA BASE MODALE
! NUMD     /I/: NUMERO DE L'INTERFACE DROITE
! NUMG     /I/: NUMERO DE L'INTERFACE GAUCHE
! NBSEC    /I/: NOMBRE DE SECTEURS
! TETA     /O/: MATRICE CARREE DE CHANGEMENT DE REPERE RECHERCHE
! NBTET    /I/: DIMENSION DELA MATRICE TETA
!
!
!
!
!
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterfort/amppr.h"
#include "asterfort/bmnodi.h"
#include "asterfort/dismoi.h"
#include "asterfort/intet0.h"
#include "asterfort/isdeco.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
!-----------------------------------------------------------------------
    integer :: i, ibid, icomp, ier, iloci, ilocj, inod
    integer :: inog, iret, j, k, lldesc, llnod, llnog
    integer :: nbcmp, nbcpmx, nbdcou, nbddr, nbdga, nbec
    integer :: nbnod, nbnog, nbnot, nbsec, nbtet, noer, numd
    integer :: numg
    real(kind=8) :: angle, pi, x
!-----------------------------------------------------------------------
    parameter   (nbcpmx=300)
    character(len=24) :: valk(2)
    character(len=8) :: basmod, mailla, typddl(10), nomnoe, tyd, intf, kbid
    real(kind=8) :: xd(10), xg(10), xtd(10), xtg(10), tet0(10, 10)
    real(kind=8) :: teta(nbtet, nbtet)
    logical :: nook
    integer :: idecd(nbcpmx), idecg(nbcpmx)
    integer :: vali(2)
!
!-----------------------------------------------------------------------
!
    data typddl /'DX','DY','DZ','DRX','DRY','DRZ',&
     &              '?','?','PRES','PHI'/
    data nook /.false./
!
!-----------------------------------------------------------------------
!
    call jemarq()
    pi=r8pi()
!
!-----------------RECUPERATION DES CONCEPTS AMONT-----------------------
!
!
!
    call dismoi('F', 'REF_INTD_PREM', basmod, 'RESU_DYNA', ibid,&
                intf, iret)
    call dismoi('F', 'NOM_MAILLA', intf, 'INTERF_DYNA', ibid,&
                mailla, iret)
!
!----------------RECUPERATION DU NOMBRE D'ENTIERS CODES-----------------
!
    call dismoi('F', 'NB_CMP_MAX', intf, 'INTERF_DYNA', nbcmp,&
                kbid, ier)
    call dismoi('F', 'NB_EC', intf, 'INTERF_DYNA', nbec,&
                kbid, ier)
    if (nbec .gt. 10) then
        call utmess('F', 'MODELISA_94')
    endif
!
!
!
!-------------------REQUETTE DESCRIPTEUR DES DEFORMEES STATIQUES--------
!
    call jeveuo(intf//'.IDC_DEFO', 'L', lldesc)
    call jelira(intf//'.IDC_DEFO', 'LONMAX', nbnot)
!**************************************************************
    nbnot = nbnot/(2+nbec)
!      NBNOT=NBNOT/3
!**************************************************************
!
!-----------REQUETTE SUR DEFINITION INTERFACES DROITE ET GAUCHE---------
!
    call jeveuo(jexnum(intf//'.IDC_LINO', numd), 'L', llnod)
    call jeveuo(jexnum(intf//'.IDC_LINO', numg), 'L', llnog)
!
!
!--------------RECUPERATION NOMBRE DE NOEUDS AUX INTERFACES-------------
!
    call jelira(jexnum(intf//'.IDC_LINO', numd), 'LONMAX', nbnod)
!
    call jelira(jexnum(intf//'.IDC_LINO', numg), 'LONMAX', nbnog)
!
    if (nbnod .ne. nbnog) then
        vali (1) = nbnod
        vali (2) = nbnog
        call utmess('F', 'ALGORITH14_99', ni=2, vali=vali)
    endif
!
!
!--------------RECUPERATION NOMBRE DE DDL AUX INTERFACES----------------
!
    kbid=' '
    call bmnodi(basmod, kbid, '          ', numd, 0,&
                ibid, nbddr)
    kbid=' '
    call bmnodi(basmod, kbid, '          ', numg, 0,&
                ibid, nbdga)
    if (nbdga .ne. nbddr) then
        vali (1) = nbddr
        vali (2) = nbdga
        call utmess('F', 'ALGORITH15_1', ni=2, vali=vali)
    endif
!
!
    if (nbddr .ne. nbtet) then
        vali (1) = nbddr
        vali (2) = nbtet
        call utmess('F', 'ALGORITH15_2', ni=2, vali=vali)
    endif
!
!----------------------CALCUL DU TETA ELEMENTAIRE-----------------------
!
    angle=2*pi/nbsec
    call intet0(angle, tet0, 3)
!
!
    nbdcou=0
    do 10 i = 1, nbnod
        inod=zi(llnod+i-1)
!******************************************************************
!        ICODD=ZI(LLDESC+2*NBNOT+INOD-1)
        inog=zi(llnog+i-1)
!        ICODG=ZI(LLDESC+2*NBNOT+INOG-1)
        call isdeco(zi(lldesc+2*nbnot+(inod-1)*nbec+1-1), idecd, 10)
        call isdeco(zi(lldesc+2*nbnot+(inog-1)*nbec+1-1), idecg, 10)
!******************************************************************
        do 20 j = 1, 10
            if (idecd(j) .eq. 1) then
                xd(j)=1.d0
            else
                xd(j)=0.d0
            endif
!
            if (idecg(j) .eq. 1) then
                xg(j)=1.d0
            else
                xg(j)=0.d0
            endif
20      continue
!
!
        do 30 j = 1, 10
            xtd(j)=0.d0
            xtg(j)=0.d0
            do 40 k = 1, 10
                xtd(j)=xtd(j)+abs(tet0(j,k))*xd(k)
                xtg(j)=xtg(j)+abs(tet0(k,j))*xg(k)
40          continue
30      continue
!
!
!    VERIFICATION SUR COHERENCE DES DDL INTERFACES
!
        do 50 j = 1, 10
            if (xtd(j) .gt. 0.d0 .and. xg(j) .eq. 0.d0) then
                noer=zi(lldesc+inog-1)
                call jenuno(jexnum(mailla//'.NOMNOE', noer), nomnoe)
                tyd=typddl(j)
                call utmess('E', 'ALGORITH15_3')
                valk (1) = tyd
                valk (2) = nomnoe
                call utmess('E', 'ALGORITH15_4', nk=2, valk=valk)
                nook=.true.
            endif
            if (xtg(j) .gt. 0.d0 .and. xd(j) .eq. 0.d0) then
                noer=zi(lldesc+inod-1)
                call jenuno(jexnum(mailla//'.NOMNOE', noer), nomnoe)
                tyd=typddl(j)
                call utmess('E', 'ALGORITH15_3')
                valk (1) = tyd
                valk (2) = nomnoe
                call utmess('E', 'ALGORITH15_6', nk=2, valk=valk)
                nook=.true.
            endif
!
50      continue
!
        if (nook) then
            call utmess('F', 'ALGORITH15_7')
        endif
!
        iloci=0
        icomp=0
        do 60 j = 1, 10
            if (idecg(j) .gt. 0) then
                iloci=iloci+1
                ilocj=0
                icomp=icomp+1
                do 70 k = 1, 10
                    if (idecd(k) .gt. 0) then
                        ilocj=ilocj+1
                        x=tet0(j,k)
                        call amppr(teta, nbddr, nbddr, x, 1,&
                                   1, nbdcou+ iloci, nbdcou+ilocj)
                    endif
70              continue
            endif
60      continue
!
        nbdcou=nbdcou+icomp
!
10  continue
!
    call jedema()
end subroutine
