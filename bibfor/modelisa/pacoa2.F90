subroutine pacoa2(lisi1z, lisi2z, lonli1, lonli2, noma1z,&
                  noma2z, liso1z, liso2z, lonlis)
    implicit none
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/pacoor.h"
#include "asterfort/padist.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: lisi1z, lisi2z, noma1z, noma2z, liso1z, liso2z
!---------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!---------------------------------------------------------------------
!     BUT: TRIER 2 LISTES DE NOEUDS LISI1Z ET LISI2Z DE MANIERE A
!     METTRE EN VIS A VIS LES NOEUDS DES 2 LISTES. MAIS L'UNE DES 2
!     LISTES PEUT ETRE PLUS PETITE (CAS DES MAILLES POI1). ON CHERCHE
!     DONC LES NOEUDS DE LA PLUS GRANDE LISTE IMAGES DES NOEUDS DE LA
!     PLUS PETITE LISTE (INJECTION).
!     LES LISTES TRIEES OBTENUES A PARTIR DE LISI1Z ET LISI2Z
!     SONT RESPECTIVEMENT LISO1Z ET LISO2Z, LA CORRESPONDANCE
!     ENTRE LES NOEUDS DES 2 LISTES EST ASSUREE DE LA MANIERE
!     SUIVANTE :
!          POUR I =1, LONLIS = MIN(LONLI1,LONLI2)
!          LISO1Z(I) EST EN VIS-AVIS AVEC LISO2Z(I)
!
!     LES LISTES LISI1Z, LISI2Z, LISO1Z ET LISO2Z CONTIENNENT
!     LES NOMS DES NOEUDS (CE SONT DES LISTES DE K8).
!
!---------------------------------------------------------------------
! ARGUMENTS D'ENTREE:
! IN   LISI1Z     K24 : NOM DE LA 1ERE LISTE
! IN   LISI2Z     K24 : NOM DE LA 2EME LISTE
! IN   LONLI1     I   : LONGUEUR DE LA LISTE LISIZ1
! IN   LONLI2     I   : LONGUEUR DE LA LISTE LISIZ2
! IN   NOMA1Z     K8  : NOM DU MAILLAGE DE LA 1ERE LISTE
! IN   NOMA2Z     K8  : NOM DU MAILLAGE DE LA 2EME LISTE
! OUT  LISO1Z     K24 : NOM DE LA 1ERE LISTE TRIEE
! OUT  LISO2Z     K24 : NOM DE LA 2EME LISTE TRIEE
! OUT  LONLIS     I   : LONGUEUR COMMUNE DE LISO1Z ET LISO2Z
!
    real(kind=8) :: x1(3), x2(3)
    character(len=8) :: noma1, noma2, m8blan
    character(len=8) :: nomno1, nomno2, nomo1, nomo2
    character(len=24) :: lisin1, lisin2, lisou1, lisou2
    character(len=24) :: valk(3)
    character(len=24) :: noeum1, noeum2
    integer :: lonli1, lonli2, lonlis, iret, idlou1, idlou2, idlin1, idlin2
    integer :: ino1
    integer :: idlou3, idlou4, lonmax, idlinv, i1, nuno1, j2, i2, ino2, nuno2
    integer :: j1
!
!-----------------------------------------------------------------------
    real(kind=8) :: d, dmin
!-----------------------------------------------------------------------
    call jemarq()
    lisin1 = lisi1z
    lisin2 = lisi2z
    lisou1 = liso1z
    lisou2 = liso2z
    lonlis = min(lonli1, lonli2)
    noma1 = noma1z
    noma2 = noma2z
    noeum1 = noma1//'.NOMNOE'
    noeum2 = noma2//'.NOMNOE'
    m8blan = '        '
!
    call jeexin(lisou1, iret)
    if (iret .ne. 0) then
        call jedetr(lisou1)
    endif
    call jeexin(lisou1, iret)
    if (iret .ne. 0) then
        call jedetr(lisou1)
    endif
!
! --- CREATION SUR LA VOLATILE DES LISTES DE K8 LISOU1 ET LISOU2
! --- DE LONGUEUR LONLIS
!
    call jeexin(lisou1, iret)
    if (iret .ne. 0) then
        call jedetr(lisou1)
    endif
    call jeexin(lisou2, iret)
    if (iret .ne. 0) then
        call jedetr(lisou2)
    endif
    call wkvect(lisou1, 'V V K8', lonlis, idlou1)
    call wkvect(lisou2, 'V V K8', lonlis, idlou2)
!
    call jeveuo(lisin1, 'L', idlin1)
    call jeveuo(lisin2, 'L', idlin2)
!
! --- VECTEURS DE TRAVAIL
!
    call wkvect('&&PACOAP.LISOU3', 'V V K8', lonlis, idlou3)
    call wkvect('&&PACOAP.LISOU4', 'V V K8', lonlis, idlou4)
    lonmax = max(lonli1,lonli2)
    call wkvect('&&PACOAP.LISINV', 'V V K8', lonmax, idlinv)
!
! --- CONSTITUTION DE LA PREMIERE CORRESPONDANCE ENTRE LES LISTES
! --- DE NOEUDS LISIN1 ET LISIN2 ENTRE NO1 DONNE ET NO2 SELON LE
! --- CRITERE : NO2 = NO DANS LISIN2 / D(NO1,NO2) = MIN D(NO1,NO)
!
    if (lonli1 .le. lonli2) then
        do 10 i1 = 1, lonli1
            nomno1 = zk8(idlin1+i1-1)
            call jenonu(jexnom(noeum1, nomno1), nuno1)
            call pacoor(noma1, nuno1, 0, x1)
            dmin = r8gaem()
            j2 = 0
            do 20 i2 = 1, lonli2
                nomo2 = zk8(idlin2+i2-1)
                call jenonu(jexnom(noeum2, nomo2), ino2)
                call pacoor(noma2, ino2, 0, x2)
                d = padist( 3, x1, x2 )
                if (d .lt. dmin) then
                    dmin = d
                    nomno2 = nomo2
                    nuno2 = ino2
                    j2 = i2
                endif
20          continue
!
            if (j2 .eq. 0) then
                call utmess('F', 'MODELISA6_3', sk=nomno1)
            endif
!
            if (zk8(idlinv+j2-1) .eq. m8blan) then
                zk8(idlou1+i1-1) = nomno1
                zk8(idlou2+i1-1) = nomno2
                zk8(idlinv+j2-1) = nomno1
            else
                valk (1) = nomno2
                valk (2) = nomno1
                valk (3) = zk8(idlinv+j2-1)
                call utmess('F', 'MODELISA8_77', nk=3, valk=valk)
            endif
10      continue
    else
        do 30 i2 = 1, lonli2
            nomno2 = zk8(idlin2+i2-1)
            call jenonu(jexnom(noeum2, nomno2), nuno2)
            call pacoor(noma2, nuno2, 0, x2)
            dmin = r8gaem()
            j1 = 0
            do 40 i1 = 1, lonli1
                nomo1 = zk8(idlin1+i1-1)
                call jenonu(jexnom(noeum1, nomo1), ino1)
                call pacoor(noma1, ino1, 0, x1)
!
                d = padist( 3, x1, x2 )
                if (d .lt. dmin) then
                    dmin = d
                    nomno1 = nomo1
                    nuno1 = ino1
                    j1 = i1
                endif
40          continue
!
            if (j1 .eq. 0) then
                call utmess('F', 'MODELISA6_3', sk=nomno2)
            endif
!
            if (zk8(idlinv+j1-1) .eq. m8blan) then
                zk8(idlou1+i2-1) = nomno1
                zk8(idlou2+i2-1) = nomno2
                zk8(idlinv+j1-1) = nomno1
            else
                valk (1) = nomno2
                valk (2) = nomno1
                valk (3) = zk8(idlinv+j1-1)
                call utmess('F', 'MODELISA8_77', nk=3, valk=valk)
            endif
30      continue
    endif
!
    call jedetr('&&PACOAP.LISOU3')
    call jedetr('&&PACOAP.LISOU4')
    call jedetr('&&PACOAP.LISINV')
!
    call jedema()
end subroutine
