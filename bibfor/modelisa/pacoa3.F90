subroutine pacoa3(noeud1, noeud2, lonli1, lonli2, dmin0,&
                  nomaz, liso1z, liso2z, lonlis)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/padist.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: noeud1(*), noeud2(*), lonli1, lonli2, lonlis
    character(len=*) :: nomaz, liso1z, liso2z
    real(kind=8) :: dmin0
!---------------------------------------------------------------------
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
!---------------------------------------------------------------------
!     BUT: TRIER 2 LISTES DE NOEUDS LISI1Z ET LISI2Z DE MANIERE A
!     METTRE EN VIS A VIS LES NOEUDS DES 2 LISTES. MAIS L'UNE DES 2
!     LISTES PEUT ETRE PLUS PETITE (CAS DES MAILLES POI1). ON CHERCHE
!     DONC LES NOEUDS DE LA PLUS GRANDE LISTE IMAGES DES NOEUDS DE LA
!     PLUS PETITE LISTE (INJECTION) DONT LA DISTANCE EST INFERIEURE A
!     DMIN.
!     LES LISTES TRIEES OBTENUES A PARTIR DE LISI1Z ET LISI2Z
!     SONT RESPECTIVEMENT LISO1Z ET LISO2Z, LA CORRESPONDANCE
!     ENTRE LES NOEUDS DES 2 LISTES EST ASSUREE DE LA MANIERE
!     SUIVANTE :
!          POUR I =1, LONLIS < MIN(LONLI1,LONLI2)
!          LISO1Z(I) EST EN VIS-AVIS AVEC LISO2Z(I)
!
!     LES LISTES LISI1Z, LISI2Z, LISO1Z ET LISO2Z CONTIENNENT
!     LES NOMS DES NOEUDS (CE SONT DES LISTES DE K8).
!
!---------------------------------------------------------------------
! ARGUMENTS D'ENTREE:
! IN   NOEUD1     I   : 1ERE LISTE DES NUMEROS DE NOEUDS
! IN   NOEUD2     I   : 2EME LISTE DES NUMEROS DE NOEUDS
! IN   LONLI1     I   : LONGUEUR DE LA LISTE LISIZ1
! IN   LONLI2     I   : LONGUEUR DE LA LISTE LISIZ2
! IN   DMIN0      R8  : DISTANCE MAXIMAL DE VIS A VIS DES NOEUDS
! IN   NOMAZ      K8  : NOM DU MAILLAGE
! OUT  LISO1Z     K24 : NOM DE LA 1ERE LISTE TRIEE
! OUT  LISO2Z     K24 : NOM DE LA 2EME LISTE TRIEE
! OUT  LONLIS     I   : LONGUEUR COMMUNE DE LISO1Z ET LISO2Z
!
    real(kind=8) :: x1(3), x2(3), d, dmin
    character(len=8) :: noma
    character(len=8) :: nomno1, nomno2, nomno3
    character(len=24) :: lisou1, lisou2, nomnoe
    character(len=24) :: valk(3)
    integer :: iret, idlou1, idlou2, ino1,  lonlim
    integer :: lonmax, idlinv, i1, nuno1, j2, i2, ino2, nuno2, j1
    real(kind=8), pointer :: vale(:) => null()
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    lisou1 = liso1z
    lisou2 = liso2z
    lonlim = min(lonli1, lonli2)
    noma = nomaz
!
    nomnoe = noma//'.NOMNOE         '
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
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
! --- DE LONGUEUR LONLIM
!
    call jeexin(lisou1, iret)
    if (iret .ne. 0) then
        call jedetr(lisou1)
    endif
    call jeexin(lisou2, iret)
    if (iret .ne. 0) then
        call jedetr(lisou2)
    endif
    call wkvect(lisou1, 'V V I', lonlim, idlou1)
    call wkvect(lisou2, 'V V I', lonlim, idlou2)
!
! --- VECTEURS DE TRAVAIL
!
    lonmax = max(lonli1,lonli2)
    call wkvect('&&PACOAP.LISINV', 'V V I', lonmax, idlinv)
!
! --- CONSTITUTION DE LA PREMIERE CORRESPONDANCE ENTRE LES LISTES
! --- DE NOEUDS LISIN1 ET LISIN2 ENTRE NO1 DONNE ET NO2 SELON LE
! --- CRITERE : NO2 = NO DANS LISIN2 / D(NO1,NO2) = MIN D(NO1,NO)
!
    lonlis = 0
    if (lonli1 .le. lonli2) then
        do 10 i1 = 1, lonli1
            nuno1 = noeud1(i1)
            x1(1) = vale(3*(nuno1-1)+1)
            x1(2) = vale(3*(nuno1-1)+2)
            x1(3) = vale(3*(nuno1-1)+3)
            dmin = dmin0
            j2 = 0
            do 20 i2 = 1, lonli2
                ino2 = noeud2(i2)
                x2(1) = vale(3*(ino2-1)+1)
                x2(2) = vale(3*(ino2-1)+2)
                x2(3) = vale(3*(ino2-1)+3)
                d = padist( 3, x1, x2 )
                if (d .lt. dmin) then
                    dmin = d
                    nuno2 = ino2
                    j2 = i2
                endif
20          continue
!
            if (j2 .eq. 0) then
                nuno2 = 0
                j2 = 0
            endif
!
            if (j2 .gt. 0) then
                if (zi(idlinv+j2-1) .eq. 0) then
                    lonlis = lonlis + 1
                    zi(idlou1+lonlis-1) = nuno1
                    zi(idlou2+lonlis-1) = nuno2
                    zi(idlinv+j2-1) = nuno1
                else
                    call jenuno(jexnum(nomnoe, nuno1), nomno1)
                    call jenuno(jexnum(nomnoe, nuno2), nomno2)
                    call jenuno(jexnum(nomnoe, zi(idlinv+j2-1)), nomno3)
                    valk (1) = nomno2
                    valk (2) = nomno1
                    valk (3) = nomno3
                    call utmess('F', 'MODELISA8_77', nk=3, valk=valk)
                endif
            endif
10      continue
    else
        do 30 i2 = 1, lonli2
            nuno2 = noeud2(i2)
            x2(1) = vale(3*(nuno2-1)+1)
            x2(2) = vale(3*(nuno2-1)+2)
            x2(3) = vale(3*(nuno2-1)+3)
            dmin = dmin0
            j1 = 0
            do 40 i1 = 1, lonli1
                ino1 = noeud1(i1)
                x1(1) = vale(3*(ino1-1)+1)
                x1(2) = vale(3*(ino1-1)+2)
                x1(3) = vale(3*(ino1-1)+3)
                d = padist( 3, x1, x2 )
                if (d .lt. dmin) then
                    dmin = d
                    nuno1 = ino1
                    j1 = i1
                endif
40          continue
!
            if (j1 .eq. 0) then
                nuno1 = 0
                j1 = 0
            endif
!
            if (j1 .gt. 0) then
                if (zi(idlinv+j1-1) .eq. 0) then
                    lonlis = lonlis + 1
                    zi(idlou1+lonlis-1) = nuno1
                    zi(idlou2+lonlis-1) = nuno2
                    zi(idlinv+j1-1) = nuno2
                else
                    call jenuno(jexnum(nomnoe, nuno1), nomno1)
                    call jenuno(jexnum(nomnoe, nuno2), nomno2)
                    call jenuno(jexnum(nomnoe, zi(idlinv+j1-1)), nomno3)
                    valk (1) = nomno2
                    valk (2) = nomno1
                    valk (3) = nomno3
                    call utmess('F', 'MODELISA8_77', nk=3, valk=valk)
                endif
            endif
30      continue
    endif
!
    call jedetr('&&PACOAP.LISINV')
!
    call jedema()
end subroutine
