subroutine deelpo(caelem, noma, numail, phie)
    implicit none
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterc/r8prem.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
    integer :: numail
    real(kind=8) :: phie
    character(len=8) :: caelem, noma
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
! RECUPERATION DU DIAMETRE EXTERIEUR D'UN ELEMENT DE POUTRE
!-----------------------------------------------------------------------
!  IN : CAELEM : NOM DU CONCEPT DE TYPE CARA_ELEM
!  IN : NOMA   : NOM DU CONCEPT DE TYPE MAILLAGE
!  IN : NUMAIL : NUMERO DE LA MAILLE CORRESPONDANTE
!  OUT: PHIE   : DIAMETRE EXTERIEUR SUR L'ELEMENT
!-----------------------------------------------------------------------
!
!
    integer :: ias, iasbon, iasedi, iasmax, icad, icav, icmp, ier
    integer :: icode, iglma, igrand, ima, inomcp, irang1
    integer :: irang2, iranv1, iranv2, iret, nbcmp, nbma
    integer :: nuenti, numa, nbec
!
!
    real(kind=8) :: difr, r1, r2, tolr
!
    character(len=8) :: nomcmp(2), nomail, k8bid
    character(len=19) :: carte
    character(len=24) :: cadesc, cavale, calima, gpmama, nomama
!
    data nomcmp /'R1      ','R2      '/
!
!-----------------------------------------------------------------------
    call jemarq()
!
!-----1.ACCES AUX OBJETS UTILES
!
    gpmama = noma//'.GROUPEMA'
!
    carte = caelem//'.CARGEOPO'
    cadesc = carte//'.DESC'
    cavale = carte//'.VALE'
    calima = carte//'.LIMA'
    call jeexin(cadesc, iret)
    if (iret .eq. 0) then
        call utmess('F', 'ALGELINE_33')
    endif
    call jeveuo(cadesc, 'L', icad)
    call jeveuo(cavale, 'L', icav)
!
!     DETERMINATION DU NUMERO D'ASSOCIATION CORRESPONDANT DANS LA CARTE
    iasmax = zi(icad+1)
    iasedi = zi(icad+2)
    iasbon = 0
!
    do 10 ias = 1, iasedi
        icode = zi(icad+3+2*(ias-1))
        nuenti = zi(icad+3+2*(ias-1)+1)
!        SI AFFECATION SUR UN GROUPE DE MAILLE
        if (icode .eq. 2) then
            call jeveuo(jexnum(gpmama, nuenti), 'L', iglma)
            call jelira(jexnum(gpmama, nuenti), 'LONMAX', nbma)
!        SI AFFECATION SUR UNE LISTE DE MAILLE
        else if (icode.eq.3) then
            call jeveuo(jexnum(calima, nuenti), 'L', iglma)
            call jelira(jexnum(calima, nuenti), 'LONMAX', nbma)
        endif
!        RECHERCHE DE LA MAILLE
        do 30 ima = 1, nbma
            numa = zi(iglma+ima-1)
            if (numa .eq. numail) then
                iasbon = ias
                goto 40
            endif
30      continue
!
10  end do
!
    if (iasbon .eq. 0) then
        nomama = noma//'.NOMMAI'
        call jenuno(jexnum(nomama, numail), nomail)
        call utmess('F', 'ALGELINE_34', sk=nomail)
    endif
!
40  continue
!
!     EXTRACTION DES RAYONS EXTERIEURS AUX DEUX EXTREMITES DE L'ELEMENT
!       SI LE RAYON EXTERIEUR EST CONSTANT SUR L'ELEMENT, ON DEDUIT
!       LE DIAMETRE EXTERIEUR
!
    igrand = zi(icad)
    call jelira(jexnum('&CATA.GD.NOMCMP', igrand), 'LONMAX', nbcmp)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', 'CAGEPO'), 'L', inomcp)
!     NOMBRE D'ENTIERS CODES DANS LA CARTE
    call dismoi('F', 'NB_EC', 'CAGEPO', 'GRANDEUR', nbec,&
                k8bid, ier)
    irang1 = indik8( zk8(inomcp) , nomcmp(1) , 1 , nbcmp )
    irang2 = indik8( zk8(inomcp) , nomcmp(2) , 1 , nbcmp )
    if (irang1 .eq. 0 .or. irang2 .eq. 0) then
        call utmess('F', 'ALGELINE_35')
    endif
    icode = zi(icad-1+3+2*iasmax+nbec*(iasbon-1)+1)
    iranv1 = 0
    do 61 icmp = 1, irang1
        if (exisdg([icode],icmp)) iranv1 = iranv1 + 1
61  end do
    iranv2 = 0
    do 62 icmp = 1, irang2
        if (exisdg([icode],icmp)) iranv2 = iranv2 + 1
62  end do
    if (iranv1 .eq. 0 .or. iranv2 .eq. 0) then
        call utmess('F', 'ALGELINE_36')
    endif
!
    r1 = zr(icav+nbcmp*(iasbon-1)+iranv1-1)
    r2 = zr(icav+nbcmp*(iasbon-1)+iranv2-1)
    if (r1 .eq. 0.d0 .or. r2 .eq. 0.d0) then
        call utmess('F', 'ALGELINE_37')
    endif
    tolr = r8prem()
    difr = dble(abs(r1-r2))
    if (difr .gt. r1*tolr) then
        call utmess('F', 'ALGELINE_38')
    endif
!
    phie = 2.d0*r1
!
    call jedema()
!
end subroutine
