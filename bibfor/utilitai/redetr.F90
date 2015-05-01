subroutine redetr(matelz)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/asmpi_comm_vect.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
#include "asterfort/zerosd.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=*) :: matelz
! person_in_charge: jacques.pellet at edf.fr
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
! ======================================================================
!
!      BUT: DETRUIRE DANS LE MATR_ELEM  MATELZ LES RESUELEM NULS
!           MAIS EN PRENANT GARDE QU'IL RESTE QUELQUE CHOSE
!
!     IN/OUT  : MATELZ = NOM DE LA SD MATR_ELEM A NETTOYER
!
!
    integer :: iret1, iexi, iexiav
    integer :: izero, ico, k, nb1, nbdet, nb1av
    aster_logical :: ldetr
    character(len=19) :: matele, resuel
    character(len=24), pointer :: relr(:) => null()
    integer, pointer :: adetr(:) => null()
    character(len=24), pointer :: tempor(:) => null()
!
    call jemarq()
!
    matele=matelz
    ldetr=.false.
!
!     -- SI LE MATR_ELEM NE CONTIENT QUE DES MACRO-ELEMENTS,
!        L'OBJET .RELR N'EXISTE PAS ET IL N'Y A RIEN A FAIRE :
    call jeexin(matele//'.RELR', iexi)
    iexi=min(1,abs(iexi))
    iexiav=iexi
    call asmpi_comm_vect('MPI_MAX', 'I', sci=iexi)
    iexi=min(1,abs(iexi))
    ASSERT(iexi.eq.iexiav)
    if (iexi .eq. 0) goto 60
!
    call jeveuo(matele//'.RELR', 'E', vk24=relr)
    call jelira(matele//'.RELR', 'LONUTI', nb1)
!
!     -- LE MATR_ELEM DOIT CONTENIR LE MEME NOMBRE DE RESUELEM
!        SUR TOUS LES PROCESSEURS :
    nb1av=nb1
    call asmpi_comm_vect('MPI_MAX', 'I', sci=nb1)
    ASSERT(nb1.eq.nb1av)
!
!     -- SI LE MATR_ELEM NE CONTIENT QU'1 RESUELEM OU AUCUN,
!        IL NE FAUT RIEN DETRUIRE
    if (nb1 .eq. 1 .or. nb1 .eq. 0) goto 60
!
!     -- CREATION DES OBJETS TEMPORAIRES DE TRAVAIL
!        ET DU BOOLEEN POUR DESTRUCTION A LA SORTIE
    ldetr=.true.
    AS_ALLOCATE(vk24=tempor, size=nb1)
    AS_ALLOCATE(vi=adetr, size=nb1)
!
!     -- ON EXAMINE LES RESUELEM CANDIDATS A LA DESTRUCTION :
!        ADETR(K)=1 : LE NOM EST ' '
!        ADETR(K)=2 : LE NOM EST NON ' ' MAIS LA SD N'EXISTE PAS
!        ADETR(K)=3 : LA SD EXISTE ET ELLE EST NULLE
!        ADETR(K)=0 : LA SD EXISTE ET ELLE EST NON NULLE
!     REMARQUE : LES CAS 1 ET 2 N'EXISTENT PAS ENCORE
!                J'ESPERE QU'ILS N'ARRIVERONT JAMAIS
    do 10 k = 1, nb1
        adetr(k)=0
        resuel=relr(k)(1:19)
        if (resuel .eq. ' ') then
            ASSERT(.false.)
            adetr(k)=1
            goto 10
        endif
!
!       -- EXISTENCE DU RESU_ELEM ?
        call exisd('RESUELEM', resuel, iret1)
        if (iret1 .eq. 0) then
            adetr(k)=2
            ASSERT(.false.)
            goto 10
        endif
!
!
!       -- SI LE RESU_ELEM EST NUL SUR TOUS LES PROCS,
!          ON PEUT LE DETRUIRE:
        izero=1
        if (zerosd('RESUELEM',resuel)) izero=0
        call asmpi_comm_vect('MPI_MAX', 'I', sci=izero)
        if (izero .eq. 0) then
            adetr(k)=3
        else
            adetr(k)=0
        endif
 10 end do
!
!
!     -- ON COMPTE LES RESUELEM A DETRUIRE :
    nbdet=0
    do 20 k = 1, nb1
        if (adetr(k) .eq. 3) nbdet=nbdet+1
 20 end do
    if (nbdet .eq. 0) goto 60
!
!     -- ON DETRUIT LES RESULEM NULS (ON EN GARDE AU MOINS 1) :
!        ON PART DE LA FIN CAR LA MATRICE NON SYMETRIQUE EST
!        EN GENERAL STOCKEE APRES LA SYMETRIQUE
    nbdet=min(nbdet,nb1-1)
    ico=0
    do 30 k = nb1, 1, -1
        if (adetr(k) .eq. 3) then
            ico=ico+1
            if (ico .gt. nbdet) goto 31
            resuel = relr(k)(1:19)
            call detrsd('RESUELEM', resuel)
            relr(k) = ' '
        endif
 30 end do
 31 continue
!
!     -- ON COMPACTE LE MATR_ELEM POUR QUE TOUS SES RESUELEM
!        SOIENT "VRAIS"
    ico=0
    do 40 k = 1, nb1
        resuel=relr(k)(1:19)
        if (resuel .ne. ' ') then
            ico=ico+1
            tempor(ico) = resuel
        endif
 40 end do
    ASSERT(ico.gt.0)
!
    call jeecra(matele//'.RELR', 'LONUTI', ico)
    do 50 k = 1, ico
        relr(k) = tempor(k)
 50 end do
!
 60 continue
!
!     -- DESTRUCTION DES OBJETS TEMPORAIRES SI BESOIN
    if (ldetr) then
        AS_DEALLOCATE(vk24=tempor)
        AS_DEALLOCATE(vi=adetr)
    endif
!
    call jedema()
!
end subroutine
