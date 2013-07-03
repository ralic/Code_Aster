subroutine tablco(char, noma, nsuco, nmaco, nnoco)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/cncinv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/juveca.h"
#include "asterfort/wkvect.h"
    character(len=8) :: char
    character(len=8) :: noma
    integer :: nsuco, nmaco, nnoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES)
!
! CONSTRUCTION DES CONNECTIVITES INVERSES
!
! ----------------------------------------------------------------------
!
!
! CONSTRUCTION DU TABLEAU INVERSE DONNANT POUR CHAQUE NOEUD DE CONTACT
! LA LISTE DES MAILLES QUI LE CONTIENNENT AU SEIN DE LA MEME SURFACE.
! CONSTRUCTION DU TABLEAU DIRECT DONNANT POUR CHAQUE MAILLE DE CONTACT
! LA LISTE DES NOEUDS QU'ELLE CONTIENT AU SEIN DE LA MEME SURFACE.
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  NSUCO  : NOMBRE TOTAL DE SURFACES DE CONTACT
! IN  NMACO  : NOMBRE TOTAL DE MAILLES DES SURFACES
! IN  NNOCO  : NOMBRE TOTAL DE NOEUDS DES SURFACES
!
!
!
!
    integer :: nbnom, ilmaco, numglm, iamaco
    integer :: ima, ino, nbid, i, no
    integer :: numno, numa
    integer :: nmax, nbno, nbma
    integer :: isuco, jdecno, jdecma
    integer :: jtrav, jtrav2, inc, long
    character(len=24) :: defico
    character(len=8) :: k8bid
    character(len=24) :: pzone, psurma, psurno, contma, contno
    integer :: jzone, jsuma, jsuno, jmaco, jnoco
    character(len=24) :: manoco, pmano, nomaco, pnoma
    integer :: jmano, jpoma, jpono, jnoma
    integer :: nnn, iainve, ilinve
    integer :: numalo
    character(len=19) :: coninv
    integer :: nmano, nnoma
!
! ----------------------------------------------------------------------
!
!     FONCTION "FORMULE" D'ACCES AU NOMBRE DE NEOUDS DES MAILLES
    nbnom(ima)=zi(ilmaco+ima)-zi(ilmaco-1+ima)
!
!     FONCTION "FORMULE" D'ACCES A LA CONNECTIVITE DES MAILLES
    numglm(ima,ino)=zi(iamaco-1+zi(ilmaco+ima-1)+ino-1)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    nmax = max(nnoco,nmaco)
    nbid = 20*nmax
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    pzone = defico(1:16)//'.PZONECO'
    psurma = defico(1:16)//'.PSUMACO'
    psurno = defico(1:16)//'.PSUNOCO'
    contma = defico(1:16)//'.MAILCO'
    contno = defico(1:16)//'.NOEUCO'
    call jeveuo(pzone, 'L', jzone)
    call jeveuo(psurma, 'L', jsuma)
    call jeveuo(psurno, 'L', jsuno)
    call jeveuo(contma, 'L', jmaco)
    call jeveuo(contno, 'L', jnoco)
!
! --- CONNECTIVITES INVERSES
!
    coninv='&&TABLCO.CONINV'
    call cncinv(noma, zi(jmaco), nmaco, 'V', coninv)
    call jeveuo(noma//'.CONNEX', 'L', iamaco)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', ilmaco)
!
!
!
    call wkvect('&&TABLCO.TRAV', 'V V I', nnoco, jtrav)
    call wkvect('&&TABLCO.TRAV2', 'V V I', nmaco, jtrav2)
!
! ======================================================================
! TABLEAU MANOCO : TABLEAU INVERSE NOEUDS->MAILLES
!                  LISTE DES MAILLES DE CONTACT AUXQUELLES APPARTIENT
!                  CHAQUE NOEUD DE CONTACT, POUR CHAQUE SURFACE.
! TABLEAU NOMACO : TABLEAU DIRECT MAILLES->NOEUDS
!                  LISTE DES NOEUDS DE CHAQUE MAILLE DE CONTACT
! ======================================================================
!
    manoco = defico(1:16)//'.MANOCO'
    pmano = defico(1:16)//'.PMANOCO'
    nomaco = defico(1:16)//'.NOMACO'
    pnoma = defico(1:16)//'.PNOMACO'
!
! ======================================================================
!         REMPLISSAGE DU TABLEAU INVERSE NOEUDS->MAILLES MANOCO
!            ET CONSTRUCTION DU POINTEUR PAMANO CORRESPONDANT
!            CALCUL DE LA NORMALE EN CHAQUE NOEUD DE CONTACT
!                        BOUCLE SUR LES SURFACES
! ======================================================================
!
    call wkvect(manoco, 'G V I', nbid, jmano)
    call wkvect(pmano, 'G V I', nnoco+1, jpoma)
    zi(jpoma) = 0
!
    inc = 0
    long = nbid
!
    call jeveuo(coninv, 'L', iainve)
    call jeveuo(jexatr(coninv, 'LONCUM'), 'L', ilinve)
    do 40 isuco = 1, nsuco
!
! ------- ADRESSES DE DEBUT DANS LES LISTES CONTNO ET CONTMA
! ------- ET NOMBRE DE NOEUDS ET MAILLES POUR LA SURFACE ISUCO
!
        jdecno = zi(jsuno+isuco-1)
        jdecma = zi(jsuma+isuco-1)
        nbno = zi(jsuno+isuco) - zi(jsuno+isuco-1)
        nbma = zi(jsuma+isuco) - zi(jsuma+isuco-1)
!
! ------- EXAMEN DES NOEUDS DE LA SURFACE
        do 30 ino = 1, nbno
!
            zi(jtrav+jdecno+ino-1) = 0
!
! ----- NUMERO DU NOEUD
!
            numno = zi(jnoco+jdecno+ino-1)
!
! ----- EXAMEN DE TOUTES LES MAILLES DE LA SURFACE
!
            nnn=zi(ilinve+numno)-zi(ilinve-1+numno)
            do 20 ima = 1, nnn
                numalo=zi(iainve-1+zi(ilinve-1+numno)+ima-1)
                if (numalo .le. zi(jsuma+isuco) .and. numalo .gt. zi(jsuma+ isuco-1)) then
                    inc=inc+1
                    if (inc .gt. long) then
                        long = 2*long
                        call juveca(manoco, long)
                        call jeveuo(manoco, 'E', jmano)
                    endif
                    zi(jmano-1+inc)=numalo
                    zi(jtrav+jdecno+ino-1) = zi(jtrav+jdecno+ino-1) + 1
                endif
20          continue
!
! ----- INCREMENTATION DU POINTEUR PMANO
!
            zi(jpoma+jdecno+ino) = zi(jpoma+jdecno+ino-1) + zi(jtrav+ jdecno+ino-1)
!
30      continue
!
40  end do
!
! ======================================================================
!       VERIFICATION DE LA LONGUEUR DU TABLEAU MANOCO ET STOCKAGE
! ======================================================================
!
    nmano = zi(jpoma+nnoco)
    if (nmano .gt. long) then
        call assert(.false.)
    endif
    call jeecra(manoco, 'LONUTI', nmano, k8bid)
!
! ======================================================================
!         REMPLISSAGE DU TABLEAU DIRECT MAILLES->NOEUDS NOMACO
!            ET CONSTRUCTION DU POINTEUR PNOMA CORRESPONDANT
!                        BOUCLE SUR LES SURFACES
! ======================================================================
!
    call wkvect(pnoma, 'G V I', nmaco+1, jpono)
    zi(jpono) = 0
!
    long = nbid
    inc=0
    do 70 isuco = 1, nsuco
        jdecno = zi(jsuno+isuco-1)
        nbno = zi(jsuno+isuco) - zi(jsuno+isuco-1)
        jdecma = zi(jsuma+isuco-1)
        do 50 ino = 1, nbno
            numno = zi(jnoco+jdecno+ino-1)
            nnn=zi(ilinve+numno)-zi(ilinve-1+numno)
            do 60 ima = 1, nnn
                numalo=zi(iainve-1+zi(ilinve-1+numno)+ima-1)
                if (numalo .le. zi(jsuma+isuco) .and. numalo .gt. zi(jsuma+ isuco-1)) then
                    inc=inc+1
                    if (inc .gt. long) then
                        long = 2*long
                    endif
                endif
60          continue
50      continue
70  end do
!
    call wkvect(nomaco, 'G V I', long, jnoma)
!
! ------- ADRESSES DE DEBUT DANS LES LISTES CONTNO ET CONTMA
! ------- ET NOMBRE DE NOEUDS ET MAILLES POUR LA SURFACE ISUCO
!
    inc=0
    do 80 isuco = 1, nsuco
        jdecno = zi(jsuno+isuco-1)
        nbno = zi(jsuno+isuco) - zi(jsuno+isuco-1)
        nbma = zi(jsuma+isuco) - zi(jsuma+isuco-1)
        jdecma = zi(jsuma+isuco-1)
!
! ------- EXAMEN DES MAILLES DE LA SURFACE
!
        do 90 ino = 1, nbno
            numno = zi(jnoco+jdecno+ino-1)
            nnn=zi(ilinve+numno)-zi(ilinve-1+numno)
            do 100 ima = 1, nnn
                numalo=zi(iainve-1+zi(ilinve-1+numno)+ima-1)
                if (numalo .le. zi(jsuma+isuco) .and. numalo .gt. zi(jsuma+ isuco-1)) then
                    zi(jtrav2+numalo-1)=zi(jtrav2+numalo-1)+1
                endif
100          continue
90      continue
!
        do 110 ima = 1, nbma
            zi(jpono+jdecma+ima)= zi(jpono+jdecma+ima-1)+ zi(jtrav2+&
            jdecma+ima-1)
!
!
! ----- NUMERO DE LA MAILLE ET ADRESSE DE SES NOEUDS
!
            numa = zi(jmaco+jdecma+ima-1)
!
! --- COMPARAISON AVEC LES NOEUDS DE CONTACT DE LA SURFACE :
! --- ON STOCKERA LES NOEUDS DE LA MAILLE DANS L'ORDRE OU ILS
! --- APPARAISSENT, D'OU L'INVERSION DES BOUCLES 130 ET 140
!
            do 130 i = 1, nbnom(numa)
                no=numglm(numa,i)
                do 140 ino = 1, nbno
                    numno = zi(jnoco+jdecno+ino-1)
                    if (no .eq. numno) then
                        inc = inc + 1
                        zi(jnoma+inc-1) = jdecno + ino
                        goto 130
                    endif
140              continue
130          continue
110      continue
!
80  end do
!
! ======================================================================
!       VERIFICATION DE LA LONGUEUR DU TABLEAU NOMACO ET STOCKAGE
! ======================================================================
!
    nnoma = zi(jpono+nmaco)
    if (nnoma .gt. long) then
        call assert(.false.)
    endif
    call jeecra(nomaco, 'LONUTI', nnoma, k8bid)
!
!
    call jedetr('&&TABLCO.TRAV')
    call jedetr('&&TABLCO.TRAV2')
    call jedetr(coninv)
! ======================================================================
    call jedema()
end subroutine
