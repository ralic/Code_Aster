subroutine utmasu(mail, kdim, nlima, lima, nomob1,&
                  coor, nbmavo, mailvo, coince)
    implicit none
! person_in_charge: jacques.pellet at edf.fr
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/indiis.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/oriem0.h"
#include "asterfort/utmavo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: lima(*), nlima, nbmavo, mailvo(*)
    real(kind=8) :: coor(*)
    character(len=2) :: kdim
    character(len=8) :: mail
    character(len=*) :: nomob1
    logical :: coince
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
! BUT : RECUPERER LA COUCHE DES MAILLES QUI "BORDENT"
!       UNE LISTE DE MAILLES DE "PEAU"
!
!     PEAU 2D (TRIA,QUAD) => MAILLES 3D
!     PEAU 1D (SEG)       => MAILLES 2D
!
!   ARGUMENTS :
!   -----------
!     MAIL (IN)  : NOM DU MAILLAGE
!     KDIM (IN)  : / '3D' RECHERCHE LES MAILLES 3D VOISINES
!                  / '2D' RECHERCHE LES MAILLES 2D VOISINES
!     NLIMA (IN)  : NOMBRE DE MAILLES DE LIMA
!     LIMA  (IN)  : LISTE DES NUMEROS DES MAILLES DE PEAU
!
!     NOMOB1 (IN/JXOUT) : NOM DE L' OJB A CREER (VECTEUR D'ENTIERS)
!       CE VECTEUR EST DE LONGUEUR NLIMA :
!       POUR CHAQUE MAILLE DE PEAU, IL CONTIENT UNE MAILLE QUI
!       "BORDE" CETTE MAILLE DE PEAU.
!
!     NBMAVO (IN) : / NB DE MAILLES DE MAILVO
!                   / 0
!     MAILVO (IN) : / LISTE DE MAILLE "CANDIDATES"
!                   / 0 (SI NBMAVO=0)
!     MAILVO EST UN SOUS-ENSEMBLE DES MAILLES DU MAILLAGE
!     QUI PERMET D'"ORIENTER" LE CHOIX DES MAILLES DE NOMOB1
!     MAILVO EST UTILISE EN PARTICULIER QUAND ON VEUT REORIENTER
!     DES FACETTES QUI SONT INSERREES ENTRE DES MAILLES VOLUMIQUES:
!        ORIE_PEAU_3D ("GROUP_MA_VOLU")
!        ORIE_PEAU_2D ("GROUP_MA_SURF")
!
!     COINCE (IN) : /.TRUE.  /.FALSE.
!       SI .TRUE. ON ACCEPTE QU'UNE MAILLE DE PEAU SOIT "COINCEE"
!       ENTRE 2 MAILLES DE PART ET D'AUTRE DE LA PEAU.
!       SINON, ON EMET UNE ERREUR FATALE.
!       SI .TRUE., ON CHOISIT LA MAILLE TELLE QUE SA NORMALE SORTANTE
!       SOIT LA MEME QUE CELLE DE LA MAILLE DE PEAU.
!
!-----------------------------------------------------------------------
!
    integer :: p1, p2, p3, p4, jm3d, nbmat, im1, im2
    integer :: ima, numa, nnoe, ino, nbm, i, k, indi, nnoem, nnoe1
    integer :: ifm, niv, ipos, itypma, nutyma
    integer :: lisnoe(27), indmai
    logical :: first
    character(len=8) :: k8b, nomail, type
    character(len=16) :: oper, k16b
    character(len=24) :: nomavo, valk(4)
!     ------------------------------------------------------------------
    call jemarq()
    call infniv(ifm, niv)
!
    ASSERT(kdim.eq.'3D'.or.kdim.eq.'2D')
    first = .false.
    call getres(k8b, k16b, oper)
!
    call jeveuo(jexatr(mail//'.CONNEX', 'LONCUM'), 'L', p2)
    call jeveuo(mail//'.CONNEX', 'L', p1)
    call jeveuo(mail//'.TYPMAIL', 'L', itypma)
!
! --- CREATION DE NOMOB1:
!     -------------------
    call wkvect(nomob1, 'V V I', nlima, jm3d)
!
! --- RECUPERATION DES MAILLES VOISINES DE LIMA :
!     ---------------------------------------------
    nomavo = '&&UTMASU.MAILLE_VOISINE '
    call utmavo(mail, kdim, lima, nlima, 'V',&
                nomavo, nbmavo, mailvo)
    call jeveuo(jexatr(nomavo, 'LONCUM'), 'L', p4)
    call jeveuo(nomavo, 'L', p3)
!
!
! --- ON REMPLIT NOMOB1 :
!     ------------------
    do 100 ima = 1, nlima
        numa = lima(ima)
        nutyma=zi(itypma+numa-1)
        nnoe = zi(p2+numa)-zi(p2-1+numa)
        ASSERT(nnoe .le. 27)
        do 80 ino = 1, nnoe
            lisnoe(ino) = zi(p1-1+zi(p2+numa-1)+ino-1)
80      continue
        nbmat = zi(p4+ima+1-1) - zi(p4+ima-1)
        nbm = 0
        do 10 i = 1, nbmat
            im2 = zi(p3+zi(p4+ima-1)-1+i-1)
            if (im2 .eq. 0) goto 10
            if (zi(p1+zi(p2+im2-1)-1) .eq. 0) goto 10
            nnoem = zi(p2+im2) - zi(p2-1+im2)
!
            do 12 k = 1, nnoe
                indi = indiis(zi(p1+zi(p2+im2-1)-1),lisnoe(k),1,nnoem)
                if (indi .eq. 0) goto 10
12          continue
            nbm = nbm + 1
            if (nbm .eq. 1) then
                zi(jm3d+ima-1) = im2
            else
!              -- CAS OU LA MAILLE DE PEAU EST BORDEE PAR PLUS
!                 D'UNE MAILLE
                im1 = zi(jm3d+ima-1)
                nnoe1 = zi(p2+im1) - zi(p2-1+im1)
                call jenuno(jexnum('&CATA.TM.NOMTM', nutyma), type)
                call oriem0(kdim, type, coor, zi(p1+zi(p2+im1-1)-1), nnoe1,&
                            zi(p1+zi(p2+im2-1)-1), nnoem, lisnoe, nnoe, ipos,&
                            indmai)
                if (ipos .eq. 0) then
!                -- SI IPOS=0, LES 2 MAILLES SONT DU MEME COTE, ON PEUT
!                   CONSERVER LA 1ERE.
                else
                    if (indmai .lt. 0) then
                        ASSERT(indmai.eq.-1 .or. indmai.eq.-2)
                        if (indmai .eq. -1) then
                            call jenuno(jexnum(mail//'.NOMMAI', im1), valk( 1))
                        else
                            call jenuno(jexnum(mail//'.NOMMAI', im2), valk( 1))
                        endif
                        call utmess('F', 'CALCULEL2_32', sk=valk(1))
                    endif
!
!                -- SINON, IM2 ET IM1 SONT DE PART ET D'AUTRE DE NUMA
                    if (.not.coince) then
                        call jenuno(jexnum(mail//'.NOMMAI', numa), valk( 1))
                        call jenuno(jexnum(mail//'.NOMMAI', im1), valk( 2))
                        call jenuno(jexnum(mail//'.NOMMAI', im2), valk( 3))
                        call utmess('F', 'PREPOST4_97', nk=3, valk=valk)
                    else
                        zi(jm3d+ima-1) = im2
                    endif
                endif
            endif
10      continue
!
!
        if (nbm .eq. 0 .and. niv .gt. 1) then
            call jenuno(jexnum(mail//'.NOMMAI', numa), nomail)
            if (first) then
                valk(1)=nomail
                call utmess('A+', 'PREPOST6_29', sk=valk(1))
            else
                valk (1)= nomail
                call utmess('A+', 'PREPOST6_30', sk=valk(1))
            endif
            first = .true.
        endif
!
100  end do
!
    call jedetr(nomavo)
    call jedema()
!
end subroutine
