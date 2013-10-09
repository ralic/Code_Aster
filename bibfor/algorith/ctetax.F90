subroutine ctetax(basmod, numa, nbsec, teta, nbtet)
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
!  BUT:    < CALCUL DE TETA AXE >
!
!   SUBROUTINE SPECIFIQUE AU CALCUL CYCLIQUE
!
!  CALCUL DE LA MATRICE TETAX PERMETTANT DE PASSER DES  DDL DE
!  L'INTERFACE AXE A CEUX DE L'INTERFACE AXE COMPTE TENU
!       D'UN NOMBRE DE SECTEURS DONNE
!      MATRICE ANTISYMETRIQUE STOCKEE PLEINE
!
! ARRET:SI DIMENSION EN ENTREE DIFFERENTE DE  DIMENSION EFFECTIVE
!-----------------------------------------------------------------------
!
! BASMOD   /I/: NOM UTLISATEUR DE LA BASE MODALE
! NUMA     /I/: NUMERO DE L'INTERFACE DEFINISSANT LES POINTS DE L'AXE
! NBSEC    /I/: NOMBRE DE SECTEURS COMPOSANT LA STRUCTURE GLOBALE
! TETA     /O/: MATRICE CARREE DE CHANGEMENT DE BASE
! NBTET   /I/: DIMENSION DE LA MATRICE DE CHANGEMENT DE BASE
!
!
!
!
!
!      NTA EST LE NOMBRE DE CMP TRAITEE EN CYCLIQUE
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
    integer :: i, ibid(1), icomp, iloci, ilocj, inoa
    integer ::  j, k, lldesc, llnoa, nbcmp
    integer :: nbcpmx, nbdax, nbdcou, nbec, nbnoa, nbnot, nbsec
    integer :: nbtet, noer, nta, numa
    real(kind=8) :: angle, pi, x
!-----------------------------------------------------------------------
    parameter   (nbcpmx=300)
    parameter   (nta=10)
    character(len=24) :: valk(2)
    character(len=8) :: basmod, mailla, typddl(6), nomnoe, tyd, intf, kbid
    real(kind=8) :: xa(10), xta(10), tet0(10, 10), teta(nbtet, nbtet)
    logical :: nook
    integer :: ideca(nbcpmx)
    integer :: vali(2)
!
!-----------------------------------------------------------------------
!
    data typddl /'DX','DY','DZ','DRX','DRY','DRZ'/
    data nook /.false./
!
!-----------------------------------------------------------------------
!
    call jemarq()
    pi=r8pi()
!
!-------------------RECUPERATION DU MAILLAGE----------------------------
!
    call dismoi('REF_INTD_PREM', basmod, 'RESU_DYNA', repk=intf)
    call dismoi('NOM_MAILLA', intf, 'INTERF_DYNA', repk=mailla)
!
!----------------RECUPERATION DU NOMBRE D'ENTIERS CODES-----------------
!
    call dismoi('NB_CMP_MAX', intf, 'INTERF_DYNA', repi=nbcmp)
    call dismoi('NB_EC', intf, 'INTERF_DYNA', repi=nbec)
    if (nbec .gt. 10) then
        call utmess('F', 'MODELISA_94')
    endif
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
!
!---------------REQUETTE SUR DEFINITION INTEFACES AXE-------------------
!
    call jeveuo(jexnum(intf//'.IDC_LINO', numa), 'L', llnoa)
!
    call jelira(jexnum(intf//'.IDC_LINO', numa), 'LONMAX', nbnoa)
!
!-------------RECUPERATION NOMBRE DE DDL INTERFACE AXE------------------
!
    kbid=' '
    call bmnodi(basmod, kbid, '         ', numa, 0,&
                ibid, nbdax)
!
    if (nbdax .ne. nbtet) then
        vali (1) = nbdax
        vali (2) = nbtet
        call utmess('F', 'ALGORITH14_93', ni=2, vali=vali)
    endif
!
!
!----------------------CALCUL DU TETA ELEMENTAIRE-----------------------
!
    angle=2*pi/nbsec
    call intet0(angle, tet0, 3)
!
!
    nbdcou=0
    do i = 1, nbnoa
        inoa=zi(llnoa+i-1)
!*************************************************************
!        ICOD=ZI(LLDESC+2*NBNOT+INOA-1)
        call isdeco(zi(lldesc+2*nbnot+(inoa-1)*nbec+1-1), ideca, nbcmp)
        do j = 1, nta
!*************************************************************
            if (ideca(j) .eq. 1) then
                xa(j)=1.d0
            else
                xa(j)=0.d0
            endif
!
        end do
!
!
        do j = 1, nta
            xta(j)=0.d0
            do k = 1, nta
                xta(j)=xta(j)+tet0(j,k)*xa(k)
            end do
        end do
!
!
!    VERIFICATION SUR COHERENCE DES DDL INTERFACES
!
        do j = 1, nta
            if (xta(j) .gt. 0.d0 .and. xa(j) .eq. 0.d0) then
                noer=zi(lldesc+inoa-1)
                call jenuno(jexnum(mailla//'.NOMNOE', noer), nomnoe)
                tyd=typddl(j)
                call utmess('E', 'ALGORITH14_94')
                valk (1) = tyd
                valk (2) = nomnoe
                call utmess('E', 'ALGORITH14_95', nk=2, valk=valk)
                nook=.true.
            endif
!
            if (xa(j) .gt. 0.d0 .and. xta(j) .eq. 0.d0) then
                noer=zi(lldesc+inoa-1)
                call jenuno(jexnum(mailla//'.NOMNOE', noer), nomnoe)
                tyd=typddl(j)
                call utmess('E', 'ALGORITH14_94')
                valk (1) = tyd
                valk (2) = nomnoe
                call utmess('E', 'ALGORITH14_95', nk=2, valk=valk)
                nook=.true.
            endif
!
        end do
!
        if (nook) then
            call utmess('F', 'ALGORITH14_94')
        endif
!
        nbdcou=0
        iloci=0
        icomp=0
        do j = 1, nta
            if (ideca(j) .gt. 0) then
                iloci=iloci+1
                ilocj=0
                icomp=icomp+1
                do k = 1, nta
                    if (ideca(k) .gt. 0) then
                        ilocj=ilocj+1
                        x=tet0(j,k)
                        call amppr(teta, nbdax, nbdax, [x], 1,&
                                   1, nbdcou+ iloci, nbdcou+ilocj)
                    endif
                end do
            endif
        end do
!
        nbdcou=nbdcou+icomp
!
    end do
!
    call jedema()
end subroutine
