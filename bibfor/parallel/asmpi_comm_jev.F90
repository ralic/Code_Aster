subroutine asmpi_comm_jev(optmpi, nomjev)
!-----------------------------------------------------------------------
!    - FONCTION REALISEE : SUR-COUCHE MPI
!
!      FAIRE UN ECHANGE BCAST/REDUCE/ALL_REDUCE SUR UN OBJET JEVEUX
!
! ARGUMENTS D'APPELS
! IN OPTMPI :
!      /'MPI_SUM' == 'ALLREDUCE + SUM'
!      /'MPI_MAX' == 'ALLREDUCE + MAX'
!      /'MPI_MIN' == 'ALLREDUCE + MIN'
!      /'REDUCE'  == 'REDUCE + SUM' : TOUS -> 0
!      /'BCAST'   == 'BCAST'        : 0    -> TOUS
!
! IN NOMJEV : K24 : NOM JEVEUX DU VECTEUR A COMMUNIQUER
!----------------------------------------------------------------------
! person_in_charge: jacques.pellet at edf.fr
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
    implicit none
#include "asterf_types.h"
#include "asterf.h"
#include "jeveux.h"
#include "asterc/asmpi_comm.h"
#include "asterc/loisem.h"
#include "asterfort/asmpi_comm_vect.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
#include "asterfort/jxveri.h"
!
! DECLARATION PARAMETRES D'APPELS
    character(len=*) :: optmpi
    character(len=24) :: nomjev
!
#ifdef _USE_MPI
#include "mpif.h"
#include "asterf_mpi.h"

! DECLARATION VARIABLES LOCALES
    integer :: jnomjv, iexi, bcrank, ibid
    integer :: iobj, nbobj, nlong
    mpi_int :: nbpro4, mpicou, nbv
    mpi_int, parameter :: pr0=0
    character(len=1) :: typsca, xous
    character(len=8) :: kbid,stock
    logical(kind=1) :: unseul
!
! ---------------------------------------------------------------------
    call jemarq()


!   -- s'il n'y a qu'un seul proc, il n'y a rien a faire :
!   ------------------------------------------------------
    call asmpi_comm('GET', mpicou)
    call asmpi_info(mpicou, size=nbpro4)
    if (nbpro4 .eq. 1) goto 999


    call jelira(nomjev, 'XOUS', ibid, xous)
    ASSERT(xous.eq.'S'.or. xous.eq.'X')
    call jelira(nomjev, 'TYPE', ibid, typsca)

    if (xous .eq. 'X') then
        call jelira(nomjev, 'NMAXOC', nbobj, kbid)
        call jelira(nomjev, 'STOCKAGE', cval=stock)
        unseul=.false.
        if (stock.eq.'CONTIG') then
            nbobj=1
            unseul=.true.
        endif
    else
        nbobj=1
        unseul=.true.
    endif


    bcrank=0

    do 10, iobj = 1,nbobj
        if (unseul) then
            ASSERT (nbobj.eq.1)
            call jeveuo(nomjev, 'E', jnomjv)
            call jelira(nomjev, 'LONMAX', nlong, kbid)
        else
            call jeexin(jexnum(nomjev, iobj), iexi)
            if (iexi .eq. 0) goto 10
            call jeveuo(jexnum(nomjev, iobj), 'E', jnomjv)
            call jelira(jexnum(nomjev, iobj), 'LONMAX', nlong, kbid)
        endif

        nbv = to_mpi_int(nlong)

        if (typsca .eq. 'R') then
            call asmpi_comm_vect(optmpi, typsca, nlong, bcrank, vr=zr(jnomjv))
        else if (typsca.eq.'C') then
            call asmpi_comm_vect(optmpi, typsca, nlong, bcrank, vc=zc(jnomjv))
        else if (typsca.eq.'I') then
            call asmpi_comm_vect(optmpi, typsca, nlong, bcrank, vi=zi(jnomjv))
        else if (typsca.eq.'S') then
            call asmpi_comm_vect(optmpi, typsca, nlong, bcrank, vi4=zi4(jnomjv))
        else
            ASSERT(.false.)
        endif

        if (xous .eq. 'X') call jelibe(jexnum(nomjev, iobj))
10  continue


999 continue
    call jedema()
#else
    character(len=1) :: kdummy
    kdummy = optmpi(1:1)
    kdummy = nomjev(1:1)
#endif
end subroutine
