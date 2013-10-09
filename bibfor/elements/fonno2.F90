subroutine fonno2(macofo, noma, nbmac, nbnoff, nbnose,&
                  nbmax, noeu, tablev)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    character(len=8) :: noma, noeu
    character(len=19) :: macofo
    integer :: nbmac, nbnoff, nbnose, nbmax, tablev(2)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!       ----------------------------------------------------------------
!      PARMI LES MAILLES CONNECTEES AU SEGMENT DU FOND, FILTRAGE DES
!          MAILLES CONNECTEES A 1 LEVRE (CAD AYANT UNE FACE LIBRE)
!          -> REMPLISSAGE DE TABLEV
!       ----------------------------------------------------------------
!    ENTREES
!       MACOFO : VECTEUR DES MAILLES (PRINCIPALES) CONNECTEES AU SEGMENT
!                DU FOND DE FISSURE COURANT
!       NOMA   : NOM DU MAILLAGE
!       NBMAC  : NOMBRE DE MAILLES CONNECTEES AU SEGMENT DU FOND ET DE
!                DE DIMENSION NDIM
!       NBNOFF : NOMBRE DE NOEUD EN FOND DE FISSURE
!       NBNOSE : NOMBRE DE NOEUD PAR SEGMENT
!       NBMAX  : NOMBRE DE NOEUDS MAX COMMUNS A DEUX MAILLES CONNEXES
!       NOEU   : NOM DU NOEUD SOMMET COURANT
!    SORTIE
!       TABLEV : VECTEUR CONTNANT LES NUMEROS DES DEUX MAILLES
!                CONNECTEES AU NOEUD SOMMET COURANT ET AUX LEVRES
!
!
    integer :: jmaco, iatyma, jno1, jno2, typ11, typ22
    integer :: inp, inq, inr, ins, nbno1, nbno2
    integer :: comp2, comp3, comp4
    character(len=8) :: typ1, typ2
    character(len=9) :: valk(1)
!
!     -----------------------------------------------------------------
!
    call jemarq()
!
!
!     RECUPERATION DE L'ADRESSE DES TYPFON DE MAILLES
    call jeveuo(noma//'.TYPMAIL', 'L', iatyma)
!
!     RECUPERATION DU VECTEUR DES MAILLES CONNECTEES AU SEGMENT DU FOND
    call jeveuo(macofo, 'L', jmaco)
!
    comp4=0
    do inp = 1, nbmac
        comp3=0
        call jeveuo(jexnum( noma//'.CONNEX', zi(jmaco-1 + inp)), 'L', jno1)
!       NOMBRE DE NOEUDS LA MAILLE
        typ11= iatyma-1+zi(jmaco-1 + inp)
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(typ11)), typ1)
        call dismoi('NBNO_TYPMAIL', typ1, 'TYPE_MAILLE', repi=nbno1)
!       POUR CHAQUE MAILLE VOISINE (NOEUD FOND COMMUN ET MEME
!       DIMENSION TOPO
        do inq = 1, nbmac
            comp2=0
            call jeveuo(jexnum( noma//'.CONNEX', zi(jmaco-1 + inq)), 'L', jno2)
            typ22= iatyma-1+zi(jmaco-1 + inq)
            call jenuno(jexnum('&CATA.TM.NOMTM', zi(typ22)), typ2)
            call dismoi('NBNO_TYPMAIL', typ2, 'TYPE_MAILLE', repi=nbno2)
!         ON COMPTE LE NOMBRE DE NOEUDS COMMUNS AFIN D'ISOLER
!         LES MAILLES DE BORD
            do inr = 1, nbno1
                do ins = 1, nbno2
                    if (zi(jno1-1+inr) .eq. zi(jno2-1+ins)) then
                        comp2=comp2+1
                    endif
                end do
            end do
!         SI LES DEUX MAILLES ONT DES NOEUDS EN COMMUN EN DEHORS DU
!         FOND MAIS PAS TOUS
            if (( (nbnoff.eq.1) .and. (comp2.ne.nbno1) .and. ( comp2.ge.nbnose) ) .or.&
                ((nbnoff.gt.1).and.(comp2.ne.nbno1) .and.(comp2.ge.nbmax) ) .or.&
                (nbmac.eq.1)) then
                comp3=comp3+1
            endif
        end do
!       ON GARDE LES MAILLES CONNECTEES QU'A 1 SEULE AUTRE MAILLE
        if (comp3 .eq. 1) then
            comp4=comp4+1
            ASSERT(comp4.le.2)
            tablev(comp4)=zi(jmaco-1 + inp)
        endif
    end do
!
!     SI AUCUNE MAILLE DE CE TYPE N'EST TROUVE
    if (comp4 .eq. 0) then
        valk(1) = noeu
        call utmess('F', 'RUPTURE0_31', sk=valk(1))
    endif
    call jedema()
end subroutine
