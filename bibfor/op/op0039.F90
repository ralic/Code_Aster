subroutine op0039()
    implicit none
! ----------------------------------------------------------------------
! person_in_charge: nicolas.sellenet at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     BUT:
!       IMPRIMER DES RESULTATS ET DES MAILLAGE
!       PROCEDURE IMPR_RESU
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/deprecated_algom.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/irmail.h"
#include "asterfort/irmfac.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdexma.h"
#include "asterfort/ulexis.h"
#include "asterfort/ulisog.h"
#include "asterfort/ulopen.h"
#include "asterfort/utmess.h"
#include "asterfort/w039ca.h"
#include "asterfort/wkvect.h"
#include "asterfort/asmpi_info.h"
    mpi_int :: mrank, msize
    integer :: nocc, iocc, ifc, ifi, versio, nbrank
    integer :: nmail, nresu, ncham, nres, n11
    integer :: nmo, nn, nmod, nforma, ngibi, nproc
!
    real(kind=8) :: versi2, eps
!
    character(len=8) :: modele, noma, form, nomare, nomsq, proc
    character(len=8) :: resu, resure(9)
    character(len=16) :: fich
    character(len=24) :: valk(6)
!
    aster_logical :: lresu, lmod
    aster_logical :: lmail, lgmsh
!
! ----------------------------------------------------------------------
!
    call asmpi_info(rank=mrank, size=msize)
    nbrank = to_aster_int(mrank)
    call jemarq()
    call infmaj()
!       
!   --- PROC0 = 'OUI' pour effectuer les impressions uniquement sur le processeur de rang 0 ---
    call getvtx(' ', 'PROC0', scal=proc, nbret=nproc)
    if ( proc .eq. 'NON' ) then 
      nbrank = 0 
    endif   
!
    if ( nbrank .eq. 0 ) then 
!     --- RECUPERATION DU NOMBRE DE MISES EN FACTEUR DU MOT-CLE RESU ---
       call getfac('RESU', nocc)
!
       do iocc = 1, nocc
           call getvtx('RESU', 'NOEUD_CMP', iocc=iocc, nbval=0, nbret=nmo)
           if (nmo .ne. 0) then
               nn = nmo / 2
               if (2*nn .ne. nmo) then
                   call utmess('F', 'PREPOST3_65')
               endif
           endif
       end do
!
!     -----------------
!     --- LE MODELE ---
!     -----------------
       lmod = .false.
       modele = ' '
       call getvid(' ', 'MODELE', scal=modele, nbret=nmod)
       if (nmod .ne. 0) lmod= .true.
       nomare=' '
!
!     ---------------------------------------------
!     --- FORMAT, FICHIER ET UNITE D'IMPRESSION ---
!     ---------------------------------------------
!
!     --- FORMAT ---
      call getvtx(' ', 'FORMAT', scal=form, nbret=nforma)
!
!     --- VERIFICATION DE LA COHERENCE ENTRE LE MAILLAGE ---
!     --- PORTANT LE RESULTAT ET LE MAILLAGE DONNE PAR   ---
!     --- L'UTILISATEUR DANS IMPR_RESU(FORMAT='IDEAS')   ---
!
       if (form(1:5) .eq. 'IDEAS') then
           call getvid('RESU', 'RESULTAT', iocc=1, scal=resu, nbret=nres)
           call getvid('RESU', 'MAILLAGE', iocc=1, scal=noma, nbret=nmail)
           if (nres*nmail .gt. 0) then
               call dismoi('NOM_MAILLA', resu, 'RESULTAT', repk=nomsq)
               if (nomsq .ne. noma) then
                   valk(1)=noma
                   valk(2)=nomsq
                   valk(3)=resu
                   call utmess('A', 'PREPOST3_74', nk=3, valk=valk)
               endif
           endif
       endif
!
!
!     --- VERSION D'ECRITURE  ----
       versio = 0
       if (form(1:5) .eq. 'IDEAS') then
           versio = 5
           call getvis(' ', 'VERSION', scal=versio, nbret=ngibi)
       else if (form(1:4) .eq. 'GMSH') then
           versio = 1
           versi2 = 1.0d0
           eps = 1.0d-6
           call getvr8(' ', 'VERSION', scal=versi2, nbret=ngibi)
           if (versi2 .gt. 1.0d0-eps .and. versi2 .lt. 1.0d0+eps) then
               versio = 1
           else if (versi2.gt.1.2d0-eps.and.versi2.lt.1.2d0+eps) then
               versio = 2
           endif
       endif
!
!     --- FICHIER ---
       ifi = 0
       fich = 'F_'//form
       call getvis(' ', 'UNITE', scal=ifi, nbret=n11)
       ifc = ifi
       if (.not. ulexis( ifi )) then
           call ulopen(ifi, ' ', fich, 'NEW', 'O')
       endif
!
!     -- VERIFICATIONS POUR GMSH :
      if (form(1:4) .eq. 'GMSH') then
        lmail=.false.
        lresu=.false.
        do iocc = 1, nocc
            call getvid('RESU', 'MAILLAGE', iocc=iocc, scal=noma, nbret=nmail)
            call getvid('RESU', 'RESULTAT', iocc=iocc, scal=resu, nbret=nresu)
            call getvid('RESU', 'CHAM_GD', iocc=iocc, scal=resu, nbret=ncham)
            if (nresu .ne. 0 .or. ncham .ne. 0) then
                lresu=.true.
                goto 220
            endif
            if (nmail .ne. 0) lmail=.true.
220         continue
        end do
        if (lmail .and. lresu) then
            call utmess('A', 'PREPOST3_68')
            goto 999
        endif
      endif
      lgmsh = .false.
!
!     --- BOUCLE SUR LE NOMBRE DE MISES EN FACTEUR ---
!     -----------------------------------------------------------------
      do iocc = 1, nocc
!
        call irmfac(iocc, form, ifi, versio,&
                    modele, noma, nomare, resure(iocc), lgmsh)
!
      end do
!
!     -- IMPRESSION DES CARTES DE DONNEES DE CHAM_MATER,  ... :
      call w039ca(ifi, form)
!
!
999   continue
    endif
    call jedema()
end subroutine
