subroutine relagm(mo, ma, nm, nl, newn,&
                  oldn)
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
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=8) :: mo, ma
    integer :: nm, nl, newn(*), oldn(*)
! ----------------------------------------------------------------------
!     BUT:
!           RENUMEROTER  LES NOEUDS TARDIFS DU MAILLAGE
!           (NOEUDS DE LAGRANGE PROVENANT DES SOUS_STRUCTURES)
!           (CES NOEUDS DOIVENT EN EFFET TOUJOURS ENCADRER LES
!            NOEUDS PHYSIQUES CONTRAINTS)
!
!     IN/OUT:     CF. ROUTINE RENUNO
!     ------
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    integer :: nbnoma, nbnore
    logical(kind=1) :: exilag
!
!
!
!     -- SI LE MODELE N'A PAS DE SOUS-STRUCTURES ON RESSORT :
!     --------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i,  iamail,    ico
    integer :: icol, il, ima, ino, inomax, inomin
    integer :: iold, iprem, iret, itypi, nbnm, nbsma, nbssa
    integer, pointer :: avap(:) => null()
    integer, pointer :: oldt(:) => null()
    integer, pointer :: sssa(:) => null()
    integer, pointer :: typl(:) => null()
!
!-----------------------------------------------------------------------
    call jemarq()
    call dismoi('NB_SS_ACTI', mo, 'MODELE', repi=nbssa)
    call dismoi('NB_SM_MAILLA', mo, 'MODELE', repi=nbsma)
    if (nbssa .gt. 0) then
        call jeveuo(mo//'.MODELE    .SSSA', 'L', vi=sssa)
        call jeexin(ma//'.TYPL', iret)
        if (iret .gt. 0) call jeveuo(ma//'.TYPL', 'L', vi=typl)
    else
        goto 999
    endif
!
!     -- L'OBJET SUIVANT CONTIENDRA EN REGARD DES NUMEROS DE NOEUDS
!        PHYSIQUES DU MAILLAGE UN ENTIER (+1, OU 0) POUR DIRE
!        SI CE NOEUD EST PRECEDE OU SUIVI (+1) DE NOEUDS DE LAGRANGE :
    AS_ALLOCATE(vi=avap, size=nm)
!
!
!     -- .OLDT EST UN .OLDN TEMPORAIRE QUE L'ON RECOPIERA A LA FIN
    nbnoma= nm+nl
    AS_ALLOCATE(vi=oldt, size=nbnoma)
!
!
!     -- BOUCLE SUR LES (SUPER)MAILLES
!     --------------------------------
    icol= 0
    do ima = 1, nbsma
        exilag=.false.
        if (sssa(ima) .eq. 1) then
            call jeveuo(jexnum(ma//'.SUPMAIL', ima), 'L', iamail)
            call jelira(jexnum(ma//'.SUPMAIL', ima), 'LONMAX', nbnm)
!
!         -- ON REGARDE LES NUMEROS PHYSIQUES MAX ET MIN DE LA MAILLE:
            iprem =0
            do i = 1, nbnm
                ino=zi(iamail-1+i)
                if ((ino.gt.0) .and. (ino.le.nm)) then
                    iprem=iprem+1
                    if (iprem .eq. 1) then
                        inomax=ino
                        inomin=ino
                    endif
                    if (newn(ino) .gt. newn(inomax)) then
                        inomax=ino
                    endif
                    if (newn(ino) .lt. newn(inomin)) then
                        inomin=ino
                    endif
                else
                    icol=icol+1
                endif
            end do
!
!
!         -- ON SE SERT DE LA FIN DU VECTEUR .NEWN POUR STOCKER EN FACE
!         DE CHAQUE LAGRANGE LE NUMERO DU NOEUD PHYSIQUE PRES DUQUEL
!         ON DOIT LE DEPLACER (+INOMAX : DERRIERE) (-INOMIN : DEVANT)
!
            do i = 1, nbnm
                ino=zi(iamail-1+i)
                if (ino .gt. nm) then
                    exilag=.true.
                    itypi=typl(ino-nm)
                    if (itypi .eq. -1) then
                        newn(ino)=-inomin
                    else if (itypi.eq.-2) then
                        newn(ino)= inomax
                    else
                        ASSERT(.false.)
                    endif
                endif
            end do
!
            if (exilag) then
                avap(inomin)= 1
                avap(inomax)= 1
            endif
!
        endif
!
    end do
!
    if (icol .eq. 0) goto 999
!
!
!     -- ON REMPLIT .OLDT AVEC LES NOEUDS DE .OLDN ET LES LAGRANGES:
!     -------------------------------------------------------------
    ico= 0
    do i = 1, nm
        iold=oldn(i)
        if (iold .eq. 0) goto 32
        if (avap(iold) .eq. 1) then
!
            do il = 1, nl
                if (newn(nm+il) .eq. -iold) then
                    ico = ico+1
                    oldt(ico)=nm+il
                endif
            end do
!
            ico = ico+1
            oldt(ico)=iold
!
            do il = 1, nl
                if (newn(nm+il) .eq. +iold) then
                    ico = ico+1
                    oldt(ico)=nm+il
                endif
            end do
!
        else
            ico = ico+1
            oldt(ico)=iold
        endif
    end do
 32 continue
    nbnore= ico
!
!     -- ON RECOPIE .OLDT DANS .OLDN ET ON REMET .NEWN A JOUR :
!     ---------------------------------------------------------
    do i = 1, nbnoma
        newn(i) =0
        oldn(i) =0
    end do
!
    do i = 1, nbnore
        oldn(i) = oldt(i)
        newn(oldt(i)) =i
    end do
!
!
999 continue
!
    AS_DEALLOCATE(vi=avap)
    AS_DEALLOCATE(vi=oldt)
!
    call jedema()
end subroutine
