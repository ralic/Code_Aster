subroutine irmase(nofimd, typsec, nbrcou, nbsect, nummai,&
                  sdcarm, nomase)
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterfort/as_mficlo.h"
#include "asterfort/as_mfiope.h"
#include "asterfort/as_mmhcow.h"
#include "asterfort/as_msmcre.h"
#include "asterfort/as_msmnsm.h"
#include "asterfort/as_msmsmi.h"
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: sdcarm
    character(len=*) :: nofimd, typsec, nomase
    integer :: nbrcou, nbsect, nummai
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: nicolas.sellenet at edf.fr
! ----------------------------------------------------------------------
!  IMPR_RESU - IMPRESSION DES MAILLAGES DE SECTION
!  -    -                     --           --
! ----------------------------------------------------------------------
!
! IN  :
!   NOFIMD  K*   ENTIER LIE AU FICHIER MED OUVERT
!   TYPSEC  K*   TYPE DE DE SECTION (COQUE, TUYAU OU PMF)
!   NBRCOU  I    NOMBRE DE COUCHES (COQUE ET TUYAU)
!   NBSECT  I    NOMBRE DE TUYAU
!   NUMMAI  I    NUMERO DE LA MAILLE REFERENCE D'UNE PMF
!   SDCARM  K8   CARA_ELEM CONVERTIT EN CHAM_ELEM_S
!   NOMASE  K*   NOM MED DU MAILLAGE SECTION
!
!
    integer :: idfimd, nbpoin, ipoint, jcoopt, nbrayo, icouch, irayon
    integer :: edleaj, postmp, codret, edcart, jmasup, jcesd
    integer :: edfuin, ndim, nbmasu, imasup, edcar2, jcesl
    integer :: nbcmp, isp, icmp, iad
    parameter    (edleaj = 1)
    parameter    (edcart = 0)
    parameter    (edfuin = 0)
!
    character(len=8) :: saux08
    character(len=16) :: nocoor(3), uncoor(3), nocoo2(3), uncoo2(3)
    character(len=64) :: nomasu
    character(len=200) :: desmed
!
    real(kind=8) :: xmin, xmax, delta, rmin, rmax, theta, dtheta
!
    aster_logical :: lmstro
    real(kind=8), pointer :: cesv(:) => null()
    character(len=8), pointer :: cesc(:) => null()
!
    data nocoor  /'X               ', 'Y               ', 'Z               '/
    data uncoor  /'INCONNU         ', 'INCONNU         ', 'INCONNU         '/
!
    desmed = ' '
    if (nbrcou .eq. 0 .and. nbsect .eq. 0 .and. nummai .eq. 0) goto 9999
!
    call as_mfiope(idfimd, nofimd, edleaj, codret)
    if (codret .ne. 0) then
        saux08='mfiope'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
!
!     -- RELECTURE DES ELEMENTS DE STRUCTURES DEJA PRESENTS
    nbmasu = 0
    call as_msmnsm(idfimd, nbmasu, codret)
    if (codret .ne. 0) then
        saux08='msmnsm'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
    lmstro = .false.
    if (nbmasu .ne. 0) then
        call wkvect('&&IRMASE.MAIL_SUPP', 'V V K80', nbmasu, jmasup)
        do 40 imasup = 1, nbmasu
            call as_msmsmi(idfimd, imasup, nomasu, ndim, desmed,&
                           edcar2, nocoo2, uncoo2, codret)
            if (codret .ne. 0) then
                saux08='msmsmi'
                call utmess('F', 'DVP_97', sk=saux08, si=codret)
            endif
            if (nomasu .eq. nomase) lmstro = .true.
 40     continue
        call jedetr('&&IRMASE.MAIL_SUPP')
        if (lmstro) goto 9999
    endif
!
    ndim = 0
    if (typsec .eq. 'COQUE') then
!
        ndim = 1
        nbpoin = 3*nbrcou
        call wkvect('&&IRMASE.COOR_PTS', 'V V R', nbpoin, jcoopt)
        delta = 2.d0/nbrcou
        xmin = -1.d0
        xmax = -1.d0+delta
        do 10 ipoint = 1, nbrcou
            zr(jcoopt+(ipoint-1)*3)=xmin
            zr(jcoopt+(ipoint-1)*3+1)=(xmax+xmin)/2.d0
            zr(jcoopt+(ipoint-1)*3+2)=xmax
            xmin = xmin+delta
            xmax = xmax+delta
 10     continue
!
    else if (typsec.eq.'TUYAU') then
!
        ndim = 2
        nbrayo = (nbsect*2)+1
        nbpoin = 3*nbrayo*nbrcou
        call wkvect('&&IRMASE.COOR_PTS', 'V V R', 2*nbpoin, jcoopt)
!
        dtheta = r8pi()/nbsect
        theta = 0.d0
!
        rmin = 0.5d0
        rmax = 1.d0
        postmp = 0
        do 20 icouch = 1, nbrcou
            do 30 irayon = 1, nbrayo
                zr(jcoopt+postmp) = rmin*cos(theta)
                zr(jcoopt+postmp+1) = rmin*sin(theta)
                zr(jcoopt+postmp+2) = (rmin+rmax)/2.d0*cos(theta)
                zr(jcoopt+postmp+3) = (rmin+rmax)/2.d0*sin(theta)
                zr(jcoopt+postmp+4) = rmax*cos(theta)
                zr(jcoopt+postmp+5) = rmax*sin(theta)
                postmp = postmp+6
                theta = theta+dtheta
 30         continue
            rmin = rmin+0.5d0
            rmax = rmax+0.5d0
            theta = 0.d0
 20     continue
        ASSERT(postmp.eq.2*nbpoin)
!
    else if (typsec.eq.'PMF') then
!
        ndim = 2
        call jeveuo(sdcarm//'.CAFIBR    .CESC', 'L', vk8=cesc)
        call jeveuo(sdcarm//'.CAFIBR    .CESD', 'L', jcesd)
        call jeveuo(sdcarm//'.CAFIBR    .CESV', 'L', vr=cesv)
        call jeveuo(sdcarm//'.CAFIBR    .CESL', 'L', jcesl)
!
        ASSERT(zi(jcesd+5+4*(nummai-1)).eq.1)
        nbpoin = zi(jcesd+5+4*(nummai-1)+1)
        call wkvect('&&IRMASE.COOR_PTS', 'V V R', 2*nbpoin, jcoopt)
        nbcmp = zi(jcesd+5+4*(nummai-1)+2)
!       YG       ZG       AIRE     YP       ZP       GX       NUMGR
        ASSERT(nbcmp.eq.7)
        ASSERT(cesc(1).eq.'YG      '.and. cesc(2).eq.'ZG      ')
        ASSERT(cesc(3).eq.'AIRE    '.and. cesc(4).eq.'YP      ')
        ASSERT(cesc(5).eq.'ZP      '.and. cesc(6).eq.'GX      ')
        ASSERT(cesc(7).eq.'NUMGR   ')
!
        postmp = 0
        do 50 isp = 1, nbpoin
            do 60 icmp = 1, 2
                call cesexi('S', jcesd, jcesl, nummai, 1,&
                            isp, icmp, iad)
                zr(jcoopt+postmp) = cesv(iad)
                postmp = postmp+1
 60         continue
 50     continue
!
    else
        ASSERT(.false.)
    endif
!
!     -- DEFINITION DU MAILLAGE SUPPORT MED
    call as_msmcre(idfimd, nomase, ndim, desmed, edcart,&
                   nocoor, uncoor, codret)
    if (codret .ne. 0) then
        saux08='msmcre'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
!
!     -- DEFINITION DES NOEUDS DU MAILLAGE SUPPORT MED
    call as_mmhcow(idfimd, nomase, zr(jcoopt), edfuin, nbpoin,&
                   codret)
    if (codret .ne. 0) then
        saux08='mmhcow'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
!
    call as_mficlo(idfimd, codret)
    if (codret .ne. 0) then
        saux08='mficlo'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
!
    call jedetr('&&IRMASE.COOR_PTS')
!
9999 continue
!
end subroutine
