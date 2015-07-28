subroutine gdirec(noma, fond, chaine, nomobj, nomnoe,&
                  coorn, nbnoeu, dire3, milieu)
    implicit none
!
!     ------------------------------------------------------------------
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
!
! FONCTION REALISEE:
!
!     POUR CHAQUE NOEUD DU FOND DE FISSURE GAMM0 ON CALCULE
!     LA DIRECTION DU CHAMP THETA
!
!     ETAPE 1 :
!              RECUPERER TOUTES LES MAILLES D'UNE LEVRE CONTENANT GAMM0
!              PUIS TRI SUR CELLES AYANT 2 NOEUDS APPARTENANT A GAMM0
!
!     ETAPE 2 :
!               CALCUL POUR CHAQUE NOEUDS DE GAMM0 DE LA DIRECTION
!                  CAS TRIA : APPEL 1 FOIS A GDIRE3
!                  CAS QUAD : APPEL 2 FOIS A GDIRE3 ET MOYENNE
! ENTREE:
!     ------------------------------------------------------------------
!        NOMA   : NOM DU MAILLAGE
!        FOND   : NOM DU CONCEPT DE DEFI_FOND_FISS
!        CHAINE : 'LEVRESUP' OU 'LEVREINF'
!        NOMOBJ : NOM DE L'OBJET CONTENANT LES NOMS DE NOEUDS
!        NOMNOE : NOMS DES NOEUDS
!        COORN  : NOM DE L'OBJET CONTENANT LES COORDONNEES DES NOEUDS
!        NBNOEU : NOMBRE DE NOEUDS DE GAMM0
!
! SORTIE:
!        DIRE3 : OBJET CONTENANT LA DIRECTION DE THETA
!        MILIEU: .TRUE.  : ELEMENT QUADRATIQUE
!                .FALSE. : ELEMENT LINEAIRE
!     ------------------------------------------------------------------
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/gdire3.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=24) :: obj3, dire1, dire2, dire3, numno
    character(len=24) :: conex, nomobj, coorn
    character(len=8) :: fond, noma, noeug, nomno2
    character(len=8) :: chaine, type, nomnoe(*)
!
    integer :: nbnoeu, lobj3, compta, comptc, nn
    integer :: iadma1, iadma3, iadrco, iamase, inumno
    integer :: noeud1, noeud2, noeud4, iadrlv
    integer :: k1, k2, k3, k4, mc, permu
!
    real(kind=8) :: coord(3, 4), a1, a2, a3, b1, b2, b3, c1, c2, c3
    real(kind=8) :: s1, s2, s3, dir1, dir2, dir3, norme
!
    aster_logical :: sommet, milieu
!
!
! OBJETS DEFINISSANT LA CONNECTIVITE  ET LE TYPE DES MAILLES
!
!-----------------------------------------------------------------------
    integer :: i, iadtyp, iatyma, ibid, il, in2, ino2
    integer :: ir, j, k, noeud3
!-----------------------------------------------------------------------
    call jemarq()

    conex = noma//'.CONNEX'
    call jeveuo(noma//'.TYPMAIL', 'L', iatyma)
!
! OBJET CONTENANT LES MAILLES DE LA LEVRE
!
    obj3 = fond//'.'//chaine//'.MAIL'
    call jelira(obj3, 'LONMAX', lobj3)
    call jeveuo(obj3, 'L', iadrlv)
    call jeveuo(coorn, 'L', iadrco)
!
! ALLOCATION D'OBJETS DE TRAVAIL
!
    dire1 = '&&DIRECT.MAIL1'//'          '
    dire2 = '&&DIRECT.MAIL2'//'          '
    numno = '&&NUME        '//'          '
    call wkvect(dire1, 'V V K8', 3*nbnoeu, iadma1)
    call wkvect(dire2, 'V V K8', nbnoeu-1, iadma3)    
    call wkvect(dire3, 'V V R', 3*nbnoeu, in2)
    call wkvect(numno, 'V V I', 2*lobj3, inumno)
!
! ON RECUPERE LES MAILLES DE LA LEVRE QUI CONTIENNENT
! UN NOEUD DE GAMM0
!
    compta = 0
    do 200 i = 1, nbnoeu
        do 150 j = 1, lobj3
            call jenonu(jexnom(noma//'.NOMMAI', zk8(iadrlv+j-1)), ibid)
            call jeveuo(jexnum(conex, ibid), 'L', iamase)
            iadtyp=iatyma-1+ibid
            call jenuno(jexnum('&CATA.TM.NOMTM', zi(iadtyp)), type)
            if (type(1:4) .eq. 'QUAD') then
                nn = 4
            else if (type(1:4).eq.'TRIA') then
                nn = 3
            else
                call utmess('F', 'ELEMENTS_83')
            endif
            do 100 k = 1, nn
                call jenuno(jexnum(nomobj, zi(iamase+k-1)), noeug)
                if (noeug .eq. nomnoe(i)) then
                    zk8(iadma1+compta+1-1) = zk8(iadrlv+j-1)
                    compta = compta + 1
                    zi(inumno+compta-1) = i
                endif
100             continue
150         continue
200     end do
!
    comptc = 0
    do 351 i = 1, compta-1
        if (zk8(iadma1+i-1) .ne. '0') then
            do 451 j = i+1, compta
                if (zi(inumno+j-1) .lt. (zi(inumno+i-1)+3)) then
                    if (zk8(iadma1+i-1) .eq. zk8(iadma1+j-1)) then
                        zk8(iadma3+comptc+1-1) = zk8(iadma1+j-1)
                        zk8(iadma1+j -1) = '0'
                        comptc = comptc + 1
                    endif
                endif
451         continue
        endif
351 end do
!
!  CALCUL DE LA DIRECTION DE THETA POUR LES NOEUDS DE GAMMO
!
    a1 = 0.d0
    b1 = 0.d0
    c1 = 0.d0
    mc = 1
    sommet = .true.
    do 500 i = 1, nbnoeu
        s1 = a1
        s2 = b1
        s3 = c1
!
        k1 = 0
        if ((sommet) .and. (i.ne.nbnoeu)) then
            call jenonu(jexnom(noma//'.NOMMAI', zk8(iadma3+mc-1)), ibid)
            call jeveuo(jexnum(conex, ibid), 'L', iamase)
            iadtyp=iatyma-1+ibid
            call jenuno(jexnum('&CATA.TM.NOMTM', zi(iadtyp)), type)
            if (type(1:4) .eq. 'QUAD') then
                nn = 4
                if (type(5:5) .eq. '4') then
                    sommet = .true.
                    milieu = .false.
                else
                    sommet = .false.
                    milieu = .true.
                endif
            else if (type(1:4).eq.'TRIA') then
                nn = 3
                if (type(5:5) .eq. '3') then
                    sommet = .true.
                    milieu = .false.
                else
                    sommet = .false.
                    milieu = .true.
                endif
            endif
            mc = mc + 1
            do 600 k = 1, nn
                call jenuno(jexnum(nomobj, zi(iamase+k-1)), noeug)
                if (noeug .eq. nomnoe(i)) then
                    k1 = k
                endif
600         continue
            k2 = k1+1
            k3 = k1+2
            if (k2 .ge. (nn+1)) then
                k2 = mod(k2,nn)
            endif
            if (k3 .ge. (nn+1)) then
                k3 = mod(k3,nn)
            endif
            noeud1 = zi(iamase+k1-1)
            noeud2 = zi(iamase+k2-1)
            noeud3 = zi(iamase+k3-1)
!
            if (type(1:4) .eq. 'TRIA') then
                ino2 = 0
                do 550 il = 1, nbnoeu
                    call jenuno(jexnum(nomobj, noeud2), nomno2)
                    if (nomno2 .eq. nomnoe(il)) then
                        ino2 = ino2 + 1
                    endif
550             continue
                if (ino2 .eq. 0) then
                    permu = noeud2
                    noeud2 = noeud3
                    noeud3 = permu
                endif
                coord(1,1) = zr(iadrco+(noeud1-1)*3+1-1)
                coord(1,2) = zr(iadrco+(noeud2-1)*3+1-1)
                coord(1,3) = zr(iadrco+(noeud3-1)*3+1-1)
                coord(2,1) = zr(iadrco+(noeud1-1)*3+2-1)
                coord(2,2) = zr(iadrco+(noeud2-1)*3+2-1)
                coord(2,3) = zr(iadrco+(noeud3-1)*3+2-1)
                coord(3,1) = zr(iadrco+(noeud1-1)*3+3-1)
                coord(3,2) = zr(iadrco+(noeud2-1)*3+3-1)
                coord(3,3) = zr(iadrco+(noeud3-1)*3+3-1)
                call gdire3(coord, a1, b1, c1, 1)
                if (i .eq. 1) then
                    zr(in2+(i-1)*3+1-1) = a1
                    zr(in2+(i-1)*3+2-1) = b1
                    zr(in2+(i-1)*3+3-1) = c1
                else
                    dir1 = (a1+s1)/2
                    dir2 = (b1+s2)/2
                    dir3 = (c1+s3)/2
                    norme = sqrt(dir1*dir1 + dir2*dir2 + dir3*dir3)
                    zr(in2+(i-1)*3+1-1) = dir1/norme
                    zr(in2+(i-1)*3+2-1) = dir2/norme
                    zr(in2+(i-1)*3+3-1) = dir3/norme
                endif
            endif
            if (type(1:4) .eq. 'QUAD') then
                k4 = k1+3
                if (k4 .ge. (nn+1)) then
                    k4 = mod(k4,nn)
                endif
                noeud4 = zi(iamase+k4-1)
                ino2 = 0
                do 650 ir = 1, nbnoeu
                    call jenuno(jexnum(nomobj, noeud2), nomno2)
                    if (nomno2 .eq. nomnoe(ir)) then
                        ino2 = ino2 + 1
                    endif
650             continue
                if (ino2 .eq. 0) then
                    permu = noeud2
                    noeud2 = noeud4
                    noeud4 = permu
                endif
                coord(1,1) = zr(iadrco+(noeud1-1)*3+1-1)
                coord(1,2) = zr(iadrco+(noeud2-1)*3+1-1)
                coord(1,3) = zr(iadrco+(noeud3-1)*3+1-1)
                coord(1,4) = zr(iadrco+(noeud4-1)*3+1-1)
                coord(2,1) = zr(iadrco+(noeud1-1)*3+2-1)
                coord(2,2) = zr(iadrco+(noeud2-1)*3+2-1)
                coord(2,3) = zr(iadrco+(noeud3-1)*3+2-1)
                coord(2,4) = zr(iadrco+(noeud4-1)*3+2-1)
                coord(3,1) = zr(iadrco+(noeud1-1)*3+3-1)
                coord(3,2) = zr(iadrco+(noeud2-1)*3+3-1)
                coord(3,3) = zr(iadrco+(noeud3-1)*3+3-1)
                coord(3,4) = zr(iadrco+(noeud4-1)*3+3-1)
                call gdire3(coord, a2, b2, c2, 1)
                call gdire3(coord, a3, b3, c3, 2)
                a1 = (a2+a3)/2
                b1 = (b2+b3)/2
                c1 = (c2+c3)/2
                if (i .eq. 1) then
                    zr(in2+(i-1)*3+1-1) = a1
                    zr(in2+(i-1)*3+2-1) = b1
                    zr(in2+(i-1)*3+3-1) = c1
                else
                    dir1 = (a1+s1)/2
                    dir2 = (b1+s2)/2
                    dir3 = (c1+s3)/2
                    norme = sqrt(dir1*dir1 + dir2*dir2 + dir3*dir3)
                    zr(in2+(i-1)*3+1-1) = dir1/norme
                    zr(in2+(i-1)*3+2-1) = dir2/norme
                    zr(in2+(i-1)*3+3-1) = dir3/norme
                endif
            endif
        else if (.not.(sommet).or.(i.eq.nbnoeu)) then
            zr(in2+(i-1)*3+1-1) = s1
            zr(in2+(i-1)*3+2-1) = s2
            zr(in2+(i-1)*3+3-1) = s3
            sommet = .true.
        endif
500 end do
!
!  CAS DU FOND DE FISSURE FERME, ON MOYENNE LES VECTEURS A CHAQUE
!  EXTREMITE
!
    if (nomnoe(1) .eq. nomnoe(nbnoeu)) then
        dir1 = ( zr(in2+(1 -1)*3+1-1)+ zr(in2+(nbnoeu-1)*3+1-1) )/2
        dir2 = ( zr(in2+(1 -1)*3+2-1)+ zr(in2+(nbnoeu-1)*3+2-1) )/2
        dir3 = ( zr(in2+(1 -1)*3+3-1)+ zr(in2+(nbnoeu-1)*3+3-1) )/2
        norme = sqrt(dir1*dir1 + dir2*dir2 + dir3*dir3)
        zr(in2+(1 -1)*3+1-1) = dir1/norme
        zr(in2+(1 -1)*3+2-1) = dir2/norme
        zr(in2+(1 -1)*3+3-1) = dir3/norme
        zr(in2+(nbnoeu-1)*3+1-1) = dir1/norme
        zr(in2+(nbnoeu-1)*3+2-1) = dir2/norme
        zr(in2+(nbnoeu-1)*3+3-1) = dir3/norme
    endif
!
! DESTRUCTION D'OBJETS DE TRAVAIL
!
    call jedetr(dire1)
    call jedetr(dire2)
    call jedetr(numno)
!
    call jedema()
end subroutine
