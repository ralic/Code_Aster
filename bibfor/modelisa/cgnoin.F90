subroutine cgnoin(mofaz, iocc, nomaz, lisnoz, nbno)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ltnotb.h"
#include "asterfort/pj2dco.h"
#include "asterfort/pj3dco.h"
#include "asterfort/pj4dco.h"
#include "asterfort/reliem.h"
#include "asterfort/tbliva.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: iocc, nbno
    character(len=*) :: mofaz, nomaz, lisnoz
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
!
!       CGNOIN -- TRAITEMENT DE L'OPTION INCLUSION
!                 DU MOT FACTEUR CREA_GROUP_NO DE
!                 LA COMMANDE DEFI_GROUP
!
! -------------------------------------------------------
!  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_NO'
!  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
!  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
!  LISNOZ        - JXVAR - K24  - : NOM DE LA LISTE DE NOEUDS RETENUS
!  NBNO          - OUT   -  I   - : LONGUEUR DE CETTE LISTE
! -------------------------------------------------------
!
    integer :: nbmc, nbno1, jma1, nbno2, jno2
    integer :: n1
    integer :: ino2, jlisno, i, ibid, iret
    complex(kind=8) :: c16b
    character(len=8) :: noma2, k8bid, ncas, noma1
    character(len=16) :: motcle(2), tymocl(2)
    character(len=16) :: motfac
    character(len=24) :: mesma1, mesno2, lisnoi
    character(len=16) :: corres
    character(len=19) :: tablg
    aster_logical :: ldmax
    real(kind=8) :: distma, armin, r8b
    integer, pointer :: litrav(:) => null()
    integer, pointer :: pjef_nb(:) => null()
!     -----------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS :
!     ---------------
    r8b=0.d0
    motfac=mofaz
    noma2=nomaz
    lisnoi=lisnoz
!
!
! --- RECUPERATION DES MAILLES DE GROUP_MA_1 :
!     -----------------------------------------
    call getvid(motfac, 'MAILLAGE_INCL', iocc=iocc, scal=noma1, nbret=n1)
    if (n1 .eq. 0) noma1=noma2
    mesma1='&&CGNOIN.MAILLE1'
    nbmc=1
    motcle(1)='GROUP_MA_INCL'
    tymocl(1)='GROUP_MA'
    call reliem(' ', noma1, 'NU_MAILLE', motfac, iocc,&
                nbmc, motcle, tymocl, mesma1, nbno1)
    ASSERT(nbno1.gt.0)
    call jeveuo(mesma1, 'L', jma1)
!
!
! --- RECUPERATION DES NOEUDS DE GROUP_MA :
!     ------------------------------------------
    mesno2='&&CGNOIN.NOEUD2'
    nbmc=1
    motcle(1)='GROUP_MA'
    tymocl(1)='GROUP_MA'
    call reliem(' ', noma2, 'NU_NOEUD', motfac, iocc,&
                nbmc, motcle, tymocl, mesno2, nbno2)
    ASSERT(nbno2.gt.0)
    call jeveuo(mesno2, 'L', jno2)
!
!
! --- CREATION DE LA SD CORRESP_2_MAILLA   :
!     -----------------------------------------
    corres='&&CGNOIN.CORRES'
    call getvtx(motfac, 'CAS_FIGURE', iocc=iocc, scal=ncas, nbret=n1)
!
    ldmax=.true.
    call getvr8(motfac, 'DISTANCE_MAX', iocc=iocc, scal=distma, nbret=n1)
    if (n1 .eq. 0) then
!
!       POUR QUE LES NOEUDS SITUES SUR LES FRONTIERES INTER-ELEMENTS
!       SOIENT SUREMENT RETENUS, IL FAUT UNE PETITE TOLERANCE :
!       0.01*AR_MIN :
        call ltnotb(noma2, 'CARA_GEOM', tablg)
        call tbliva(tablg, 0, ' ', [ibid], [r8b],&
                    [c16b], k8bid, k8bid, [r8b], 'AR_MIN',&
                    k8bid, ibid, armin, c16b, k8bid,&
                    iret)
        ASSERT(iret.eq.0)
        ASSERT(armin.gt.0.d0)
        distma=0.01d0*armin
    endif
!
    if (ncas .eq. '2D') then
        call pj2dco('PARTIE', noma1, noma2, nbno1, zi(jma1),&
                    nbno2, zi(jno2), ' ', ' ', corres,&
                    ldmax, distma)
    else if (ncas.eq.'3D') then
        call pj3dco('PARTIE', noma1, noma2, nbno1, zi(jma1),&
                    nbno2, zi(jno2), ' ', ' ', corres,&
                    ldmax, distma)
    else if (ncas.eq.'2.5D') then
        call pj4dco('PARTIE', noma1, noma2, nbno1, zi(jma1),&
                    nbno2, zi(jno2), ' ', ' ', corres,&
                    ldmax, distma, ' ')
    else
        ASSERT(.false.)
    endif
!
!
! --- EXPLOITATION DE LA SD CORRESP_2_MAILLA POUR DETERMINER
!     LES NOEUDS A RETENIR :
!     --------------------------------------------------------
    call dismoi('NB_NO_MAILLA', noma2, 'MAILLAGE', repi=nbno2)
    AS_ALLOCATE(vi=litrav, size=nbno2)
    call jeveuo(corres//'.PJEF_NB', 'L', vi=pjef_nb)
!
!
    nbno=0
    do ino2 = 1, nbno2
        if (pjef_nb(ino2) .gt. 0) then
            nbno=nbno+1
            litrav(nbno)=ino2
        endif
    end do
!
!
!
!
! --- ALLOCATION DU VECTEUR DES NUMEROS DES MAILLES RETENUES
!     --------------------------------------------------------
    call wkvect(lisnoi, 'V V I', max(nbno, 1), jlisno)
    do i = 1, nbno
        zi(jlisno-1+i)=litrav(i)
    end do
!
    AS_DEALLOCATE(vi=litrav)
    call jedetr(mesma1)
    call jedetr(mesno2)
    call detrsd('CORRESP_2_MAILLA', corres)
!
    call jedema()
!
end subroutine
