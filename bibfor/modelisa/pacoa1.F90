subroutine pacoa1(noeud1, noeud2, lonlis, nomaz, liso1z,&
                  liso2z)
    implicit none
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/padist.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: lonlis, noeud1(*), noeud2(*)
    character(len=*) :: nomaz, liso1z, liso2z
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     BUT: TRIER 2 LISTES DE NOEUDS NOEUD1 ET LISI2Z DE MANIERE A
!     METTRE EN VIS A VIS LES NOEUDS DES 2 LISTES
!     LES LISTES TRIEES OBTENUES A PARTIR DE NOEUD1 ET LISI2Z
!     SONT RESPECTIVEMENT LISO1Z ET LISO2Z, LA CORRESPONDANCE
!     ENTRE LES NOEUDS DES 2 LISTES EST ASSUREE DE LA MANIERE
!     SUIVANTE :
!          POUR I =1, LONLIS
!          LISO1Z(I) EST EN VIS-AVIS AVEC LISO2Z(I)
!
!     LES LISTES NOEUD1, LISI2Z, LISO1Z ET LISO2Z CONTIENNENT
!     LES NUMEROS DES NOEUDS (CE SONT DES LISTES DE I).
!
!     ROUTINE INSPIREE DE  PACOAP
!
!-----------------------------------------------------------------------
! ARGUMENTS D'ENTREE:
! IN   NOEUD1     I   : 1ERE LISTE DES NUMEROS DE NOEUDS
! IN   LISI2Z     I   : 2EME LISTE DES NUMEROS DE NOEUDS
! IN   LONLIS     I   : LONGUEUR COMMUNE DE CES 2 LISTES
! IN   NOMAZ      K8  : NOM DU MAILLAGE
! OUT  LISO1Z     K24 : NOM DE LA 1ERE LISTE TRIEE
! OUT  LISO2Z     K24 : NOM DE LA 2EME LISTE TRIEE
!
    integer ::  iret, idlou1, idlou2,    i1, i2
    integer :: ino1, nuno1, ino2, nuno2, j1, j2, i, j, iexcor, trouv1, trouv2
    integer :: jfac1, jfac2, jfond, im, nbfa, nbfo
    real(kind=8) :: dmin, d, x1(3), x2(3)
    character(len=8) :: noma
    character(len=8) :: nomno1, nomno2, nomno3
    character(len=24) :: lisou1, lisou2
    character(len=24) :: valk(3)
    character(len=24) :: nomnoe, grpnoe
    integer, pointer :: lisinv(:) => null()
    integer, pointer :: lisou3(:) => null()
    integer, pointer :: lisou4(:) => null()
    real(kind=8), pointer :: vale(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
    lisou1 = liso1z
    lisou2 = liso2z
    noma = nomaz
!
    nomnoe = noma//'.NOMNOE         '
    grpnoe = noma//'.GROUPENO       '
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
!
    call jeexin(jexnom(grpnoe, 'FACE1'), iret)
    if (iret .eq. 0) then
        nbfa = 0
        nbfo = 0
    else
        call jeveuo(jexnom(grpnoe, 'FACE1'), 'L', jfac1)
        call jeveuo(jexnom(grpnoe, 'FACE2'), 'L', jfac2)
        call jelira(jexnom(grpnoe, 'FACE2'), 'LONUTI', nbfa)
        call jeveuo(jexnom(grpnoe, 'FONDFISS'), 'L', jfond)
        call jelira(jexnom(grpnoe, 'FONDFISS'), 'LONUTI', nbfo)
    endif
!
    call jeexin(lisou1, iret)
    if (iret .ne. 0) call jedetr(lisou1)
    call jeexin(lisou2, iret)
    if (iret .ne. 0) call jedetr(lisou2)
!
! --- CREATION SUR LA VOLATILE DES LISTES DE K8 LISOU1 ET LISOU2
! --- DE LONGUEUR LONLIS
!
    call wkvect(lisou1, 'V V I', lonlis, idlou1)
    call wkvect(lisou2, 'V V I', lonlis, idlou2)
!
! --- VECTEURS DE TRAVAIL
!
    AS_ALLOCATE(vi=lisou3, size=lonlis)
    AS_ALLOCATE(vi=lisou4, size=lonlis)
    AS_ALLOCATE(vi=lisinv, size=lonlis)
    do 5 i1 = 1, lonlis
        lisinv(i1) = 0
 5  end do
!
! --- CONSTITUTION DE LA PREMIERE CORRESPONDANCE ENTRE LES LISTES
! --- DE NOEUDS NOEUD1 ET NOEUD2 ENTRE NO1 DONNE ET NO2 SELON LE
! --- CRITERE : NO2 = NO DANS NOEUD2 / D(NO1,NO2) = MIN D(NO1,NO)
!
    do 10 i1 = 1, lonlis
        nuno1 = noeud1(i1)
        x1(1) = vale(3*(nuno1-1)+1)
        x1(2) = vale(3*(nuno1-1)+2)
        x1(3) = vale(3*(nuno1-1)+3)
        dmin = r8gaem()
        j2 = 0
!
        trouv1 = 0
        do 11 im = 1, nbfo
            if (zi(jfond+im-1) .eq. nuno1) goto 13
11      continue
        do 12 im = 1, nbfa
            if (zi(jfac1+ im-1) .eq. nuno1) then
                trouv1 = 1
                goto 13
            endif
            if (zi(jfac2+ im-1) .eq. nuno1) then
                trouv1 = 2
                goto 13
            endif
12      continue
!
13      continue
        do 20 i2 = 1, lonlis
            ino2 = noeud2(i2)
            x2(1) = vale(3*(ino2-1)+1)
            x2(2) = vale(3*(ino2-1)+2)
            x2(3) = vale(3*(ino2-1)+3)
            d = padist( 3, x1, x2 )
!
            trouv2 = 0
            do 21 im = 1, nbfo
                if (zi(jfond+im-1) .eq. ino2) goto 23
21          continue
            do 22 im = 1, nbfa
                if (zi(jfac1+ im-1) .eq. ino2) then
                    trouv2 = 1
                    goto 23
                endif
                if (zi(jfac2+ im-1) .eq. ino2) then
                    trouv2 = 2
                    goto 23
                endif
22          continue
!
23          continue
            if (( d .lt. dmin ) .and. (trouv1.eq.trouv2)) then
                dmin = d
                nuno2 = ino2
                j2 = i2
            endif
20      continue
!
        if (j2 .eq. 0) then
            call utmess('F', 'MODELISA6_3', sk=nomno1)
        endif
!
        if (lisinv(j2) .eq. 0) then
            zi(idlou1+i1-1) = nuno1
            zi(idlou2+i1-1) = nuno2
            lisinv(j2) = nuno1
        else
            call jenuno(jexnum(nomnoe, nuno1), nomno1)
            call jenuno(jexnum(nomnoe, nuno2), nomno2)
            call jenuno(jexnum(nomnoe, lisinv(j2)), nomno3)
            valk (1) = nomno2
            valk (2) = nomno1
            valk (3) = nomno3
            call utmess('F', 'MODELISA8_77', nk=3, valk=valk)
        endif
!
10  end do
!
    do 30 i1 = 1, lonlis
        lisinv(i1) = 0
30  end do
!
! --- CONSTITUTION DE LA SECONDE CORRESPONDANCE ENTRE LES LISTES
! --- DE NOEUDS NOEUD1 ET NOEUD2 ENTRE NO2 DONNE ET NO1 SELON LE
! --- CRITERE : NO1 = NO DANS NOEUD1 / D(NO1,NO2) = MIN D(NO,NO2)
! --- LA CORRESPONDANCE EST DEFINIE PAR LA CONSTITUTION DES LISTES
! --- LISOU3 ET LISOU4.
!
    do 40 i2 = 1, lonlis
        nuno2 = noeud2(i2)
        x2(1) = vale(3*(nuno2-1)+1)
        x2(2) = vale(3*(nuno2-1)+2)
        x2(3) = vale(3*(nuno2-1)+3)
        dmin = r8gaem()
        j1 = 0
!
        trouv2 = 0
        do 41 im = 1, nbfo
            if (zi(jfond+im-1) .eq. nuno2) goto 43
41      continue
        do 42 im = 1, nbfa
            if (zi(jfac1+ im-1) .eq. nuno2) then
                trouv2 = 1
                goto 43
            endif
            if (zi(jfac2+ im-1) .eq. nuno2) then
                trouv2 = 2
                goto 43
            endif
42      continue
!
43      continue
        do 50 i1 = 1, lonlis
            ino1 = noeud1(i1)
            x1(1) = vale(3*(ino1-1)+1)
            x1(2) = vale(3*(ino1-1)+2)
            x1(3) = vale(3*(ino1-1)+3)
            d = padist( 3, x1, x2 )
!
            trouv1 = 0
            do 51 im = 1, nbfo
                if (zi(jfond+im-1) .eq. ino1) goto 53
51          continue
            do 52 im = 1, nbfa
                if (zi(jfac1+ im-1) .eq. ino1) then
                    trouv1 = 1
                    goto 53
                endif
                if (zi(jfac2+ im-1) .eq. ino1) then
                    trouv1 = 2
                    goto 53
                endif
52          continue
!
53          continue
            if (( d .lt. dmin ) .and. (trouv1.eq.trouv2)) then
                dmin = d
                nuno1 = ino1
                j1 = i1
            endif
50      continue
!
        if (j1 .eq. 0) then
            call utmess('F', 'MODELISA6_3', sk=nomno2)
        endif
!
        if (lisinv(j1) .eq. 0) then
            lisou3(i2) = nuno1
            lisou4(i2) = nuno2
            lisinv(j1) = nuno2
        else
            call jenuno(jexnum(nomnoe, nuno1), nomno1)
            call jenuno(jexnum(nomnoe, nuno2), nomno2)
            call jenuno(jexnum(nomnoe, lisinv(j1)), nomno3)
            valk (1) = nomno1
            valk (2) = nomno2
            valk (3) = nomno3
            call utmess('F', 'MODELISA8_77', nk=3, valk=valk)
        endif
!
40  end do
!
! --- VERIFICATION DE LA COHERENCE DES COUPLES FORMES D'UNE PART
! --- PAR LISOU1 ET LISOU2 ET D'AUTRE-PART DES COUPLES 'INVERSES'
! --- FORMES PAR LISOU3 ET LISOU4
!
    do 60 i = 1, lonlis
        iexcor = 0
        do 70 j = 1, lonlis
            if (zi(idlou1+i-1) .eq. lisou3(j)) then
                iexcor = 1
                if (zi(idlou2+i-1) .ne. lisou4(j)) then
                    call jenuno(jexnum(nomnoe, zi(idlou1+i-1)), nomno1)
                    call jenuno(jexnum(nomnoe, zi(idlou2+i-1)), nomno2)
                    call jenuno(jexnum(nomnoe, lisou4(j)), nomno3)
                    valk (1) = nomno1
                    valk (2) = nomno2
                    valk (3) = nomno3
                    call utmess('F', 'MODELISA8_79', nk=3, valk=valk)
                endif
            endif
70      continue
!
        if (iexcor .eq. 0) then
            call jenuno(jexnum(nomnoe, zi(idlou1+i-1)), nomno1)
            valk (1) = nomno1
            valk (2) = ' '
            valk (3) = ' '
            call utmess('F', 'MODELISA8_80', nk=3, valk=valk)
        endif
!
60  end do
!
! --- MENAGE
!
    AS_DEALLOCATE(vi=lisou3)
    AS_DEALLOCATE(vi=lisou4)
    AS_DEALLOCATE(vi=lisinv)
!
    call jedema()
end subroutine
