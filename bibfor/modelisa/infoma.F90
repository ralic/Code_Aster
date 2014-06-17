subroutine infoma(nomu)
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
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    character(len=8) :: nomu
!
!     IMPRESSION DES INFOS (1 OU 2)
!
!
    character(len=32) :: lisnoe, lismai, lisgrn, lisgrm
    character(len=32) :: comnoe, commai, comgrn, comgrm
    character(len=24) :: conxv, grpnov, grpmav, nomnoe, titre, cooval
    character(len=24) :: nom, nommai
    character(len=8) :: type
    integer :: niv, ifm, nn, nbno, j, idec, iad1, nbcoor,  nbma
    integer :: nbltit, iad, i, nbnoeu, nbmail, nbgrno, nbgrma
    integer :: nbmmai, n1, nbmmax, ityp
    parameter (nbmmax=100)
    integer :: dimmai(nbmmax),  iret
    character(len=8) :: mclmai(nbmmax)
    integer, pointer :: typmail(:) => null()
    integer, pointer :: dime(:) => null()
!
!
!
    data lisnoe/'LISTE DES NOEUDS                '/
    data lismai/'LISTE DES MAILLES               '/
    data lisgrn/'LISTE DES GROUPES DE NOEUDS     '/
    data lisgrm/'LISTE DES GROUPES DE MAILLES    '/
    data comnoe/'NOMBRE DE NOEUDS                '/
    data commai/'NOMBRE DE MAILLES               '/
    data comgrn/'NOMBRE DE GROUPES DE NOEUDS     '/
    data comgrm/'NOMBRE DE GROUPES DE MAILLES    '/
!
    call jemarq()
!
!
    conxv = nomu//'.CONNEX'
    grpnov = nomu//'.GROUPENO'
    grpmav = nomu//'.GROUPEMA'
    nomnoe = nomu//'.NOMNOE'
    nommai = nomu//'.NOMMAI'
    titre = nomu//'           .TITR'
    cooval = nomu//'.COORDO    .VALE'
!
!
!
    call infniv(ifm, niv)
!
    call jeexin(grpmav, iret)
    if (iret .gt. 0) then
        call jelira(grpmav, 'NMAXOC', nbgrma)
    else
        nbgrma=0
    endif
    call jeexin(grpnov, iret)
    if (iret .gt. 0) then
        call jelira(grpnov, 'NMAXOC', nbgrno)
    else
        nbgrno=0
    endif
!
!
    call jelira(titre, 'LONMAX', nbltit)
    call jelira(nomnoe, 'NOMMAX', nbnoeu)
    call jelira(nommai, 'NOMMAX', nbmail)
    call jeveuo(nomu//'.DIME', 'L', vi=dime)
    call jeveuo(nomu//'.TYPMAIL', 'L', vi=typmail)
    nbcoor=dime(6)
!
!
    call jelira('&CATA.TM.NOMTM', 'NOMMAX', nbmmai)
    do 20 i = 1, nbmmai
        dimmai(i) = 0
        call jenuno(jexnum('&CATA.TM.NOMTM', i), mclmai(i))
20  end do
    do 21 i = 1, nbmail
        ityp=typmail(i)
        ASSERT((ityp.gt.0).and.(ityp.lt.100))
        dimmai(ityp)=dimmai(ityp)+1
21  end do
!
!
!
!
! -     ECRITURE DE L EN TETE
! ----------------------------------
    if (niv .ge. 1) then
        write (ifm,802) nomu,niv
        call jeveuo(titre, 'L', iad)
        do 40 i = 1, nbltit
            write (ifm,801) zk80(iad+i-1)
40      continue
        write (ifm,804) comnoe,nbnoeu
        write (ifm,804) commai,nbmail
        do 50 i = 1, nbmmai
            if (dimmai(i) .ne. 0) write (ifm,806) mclmai(i),dimmai(i)
50      continue
!
        if (nbgrno .ne. 0) then
            write (ifm,804) comgrn,nbgrno
            do 60 i = 1, nbgrno
                call jeexin(jexnum(grpnov, i), iret)
                if (iret .eq. 0) goto 60
                call jenuno(jexnum(grpnov, i), nom)
                call jelira(jexnum(grpnov, i), 'LONUTI', n1)
                write (ifm,808) nom,n1
60          continue
        endif
!
        if (nbgrma .ne. 0) then
            write (ifm,804) comgrm,nbgrma
            do 70 i = 1, nbgrma
                call jeexin(jexnum(grpmav, i), iret)
                if (iret .eq. 0) goto 70
                call jenuno(jexnum(grpmav, i), nom)
                call jelira(jexnum(grpmav, i), 'LONUTI', n1)
                write (ifm,808) nom,n1
70          continue
        endif
    endif
!
!
!
    if (niv .ge. 2) then
!
        write (ifm,803) lisnoe
        call jeveuo(cooval, 'L', iad)
        do 80 i = 1, nbnoeu
            call jenuno(jexnum(nomnoe, i), nom)
            idec = iad + (i-1)*3
            write (ifm,701) i,nom, (zr(idec+j-1),j=1,nbcoor)
80      continue
!
        write (ifm,803) lismai
        do 90 i = 1, nbmail
            call jenuno(jexnum(nommai, i), nom)
            call jeveuo(jexnum(conxv, i), 'L', iad1)
            call jelira(jexnum(conxv, i), 'LONMAX', nbno)
            ityp=typmail(i)
            call jenuno(jexnum('&CATA.TM.NOMTM', ityp), type)
            if (nbno .le. 5) then
                write (ifm,702) i,nom,type, (zi(iad1+j-1),j=1,nbno)
            else
                write (ifm,702) i,nom,type, (zi(iad1+j-1),j=1,5)
                write (ifm,703) (zi(iad1+j-1),j=6,nbno)
            endif
90      continue
!
        if (nbgrno .ne. 0) then
            write (ifm,803) lisgrn
            do 100 i = 1, nbgrno
                call jeexin(jexnum(grpnov, i), iret)
                if (iret .eq. 0) goto 100
                call jenuno(jexnum(grpnov, i), nom)
                call jeveuo(jexnum(grpnov, i), 'L', iad)
                call jelira(jexnum(grpnov, i), 'LONUTI', nbno)
                nn = nbno
                if (nn .le. 5) then
                    write (ifm,704) i,nom,nbno, (zi(iad+j-1),j=1,nn)
                else
                    write (ifm,704) i,nom,nbno, (zi(iad+j-1),j=1,5)
                    write (ifm,703) (zi(iad+j-1),j=6,nn)
                endif
100          continue
        endif
!
        if (nbgrma .ne. 0) then
            write (ifm,803) lisgrm
            do 110 i = 1, nbgrma
                call jeexin(jexnum(grpmav, i), iret)
                if (iret .eq. 0) goto 110
                call jenuno(jexnum(grpmav, i), nom)
                call jeveuo(jexnum(grpmav, i), 'L', iad)
                call jelira(jexnum(grpmav, i), 'LONUTI', nbma)
                nn = nbma
                if (nbma .le. 5) then
                    write (ifm,704) i,nom,nbma, (zi(iad+j-1),j=1,nn)
                else
                    write (ifm,704) i,nom,nbma, (zi(iad+j-1),j=1,5)
                    write (ifm,703) (zi(iad+j-1),j=6,nn)
                endif
110          continue
        endif
    endif
    write (ifm,809)
!
    call jedema()
!
!
    701 format (2x,i8,2x,a24,10x,3 (d14.5,2x))
    702 format (2x,i8,2x,a24,2x,a8,5 (2x,i8))
    703 format (100 (30x,5 (2x,i8),/))
    704 format (2x,i8,2x,a24,2x,i8,5 (2x,i8))
    801 format (a80)
    802 format (/,'------------ MAILLAGE ',a8,&
     &       ' - IMPRESSIONS NIVEAU ',i2,' ------------',/)
    803 format (/,15x,'------  ',a32,'  ------',/)
    804 format (/,a32,i12)
    806 format (30x,a8,5x,i12)
    808 format (30x,a24,2x,i12)
    809 format (/,80('-'),/)
!
end subroutine
