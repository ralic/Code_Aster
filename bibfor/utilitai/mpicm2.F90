subroutine mpicm2(optmpi, nomjev)
!-----------------------------------------------------------------------
!    - FONCTION REALISEE : SUR-COUCHE MPI
!
!      FAIRE UN ECHANGE BCAST/REDUCE/ALL_REDUCE SUR UN OBJET JEVEUX
!
! ARGUMENTS D'APPELS
! IN OPTMPI :
!      /'MPI_SUM' == 'ALLREDUCE + SUM'
!      /'REDUCE'  == 'REDUCE + SUM' : TOUS -> 0
!      /'BCAST'   == 'BCAST'        : 0    -> TOUS
!
! IN NOMJEV : K24 : NOM JEVEUX DU VECTEUR A COMMUNIQUER
!----------------------------------------------------------------------
! person_in_charge: jacques.pellet at edf.fr
! CORPS DU PROGRAMME
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
! DECLARATION PARAMETRES D'APPELS
#include "asterf.h"
#include "jeveux.h"
!
#include "asterc/loisem.h"
#include "asterfort/assert.h"
#include "asterfort/comcou.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mpichk.h"
#include "asterfort/mpierr.h"
#include "asterfort/u2mesk.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
#include "blas/dcopy.h"
#include "blas/zcopy.h"
    character(len=32) :: jexnom, jexatr
    character(len=*) :: optmpi
    character(len=24) :: nomjev
!
#ifdef _USE_MPI
#include "mpif.h"
! DECLARATION VARIABLES LOCALES
    integer :: jnomjv, iexi
    integer :: ibid
    integer :: jtrav, k
    integer :: iobj, nbobj, nlong, iret
    integer(kind=4) :: iermpi, lint, lint4, lr8, lc8, nbpro4, n4, mpicou
    character(len=1) :: typsca, xous
    character(len=8) :: kbid
    character(len=24) :: notrav
!
! ---------------------------------------------------------------------
    call jemarq()
! --- COMMUNICATEUR MPI DE TRAVAIL
    mpicou=comcou(1)
! --- COMPTEUR
    call uttcpu('CPU.CMPI.1', 'DEBUT', ' ')
!
!     -- INITIALISATIONS :
!     --------------------
    if (loisem() .eq. 8) then
        lint=MPI_INTEGER8
    else
        lint=MPI_INTEGER
    endif
    lint4=MPI_INTEGER4
    lr8 = MPI_DOUBLE_PRECISION
    lc8 = MPI_DOUBLE_COMPLEX
!
    notrav='&&MPICM2.TRAV'
!
!     -- S'IL N'Y A QU'UN SEUL PROC, IL N'Y A RIEN A FAIRE :
    call MPI_COMM_SIZE(mpicou, nbpro4, iermpi)
    call mpierr(iermpi)
    if (nbpro4 .eq. 1) goto 9999
!
!     -- VERIFICATION RENDEZ-VOUS
    iret=1
    call mpichk(nbpro4, iret)
    if (iret .ne. 0) then
        call u2mesk('I', 'APPELMPI_83', 1, optmpi)
        goto 9999
    endif
!
    if (optmpi .eq. 'BCAST') then
!     ---------------------------------
        call jelira(nomjev, 'XOUS', ibid, xous)
        ASSERT(xous.eq.'S')
        call jelira(nomjev, 'TYPE', ibid, typsca)
        call jelira(nomjev, 'LONMAX', nlong, kbid)
!
        n4=nlong
        call jeveuo(nomjev, 'E', jnomjv)
!
        if (typsca .eq. 'R') then
            call MPI_BCAST(zr(jnomjv), n4, lr8, 0, mpicou,&
                           iermpi)
        else if (typsca.eq.'I') then
            call MPI_BCAST(zi(jnomjv), n4, lint, 0, mpicou,&
                           iermpi)
        else if (typsca.eq.'S') then
            call MPI_BCAST(zi4(jnomjv), n4, lint4, 0, mpicou,&
                           iermpi)
        else
            ASSERT(.false.)
        endif
        call mpierr(iermpi)
!
    else if (optmpi.eq.'REDUCE') then
!     ---------------------------------
        call jelira(nomjev, 'XOUS', ibid, xous)
        ASSERT(xous.eq.'S')
        call jelira(nomjev, 'TYPE', ibid, typsca)
        call jelira(nomjev, 'LONMAX', nlong, kbid)
!
        n4=nlong
        call jeveuo(nomjev, 'E', jnomjv)
!
        if (typsca .eq. 'R') then
            call wkvect(notrav, 'V V R', nlong, jtrav)
            call dcopy(n4, zr(jnomjv), 1, zr(jtrav), 1)
            call MPI_REDUCE(zr(jtrav), zr(jnomjv), n4, lr8, MPI_SUM,&
                            0, mpicou, iermpi)
        else if (typsca.eq.'C') then
            call wkvect(notrav, 'V V C', nlong, jtrav)
            call zcopy(n4, zc(jnomjv), 1, zc(jtrav), 1)
            call MPI_REDUCE(zc(jtrav), zc(jnomjv), n4, lc8, MPI_SUM,&
                            0, mpicou, iermpi)
        else if (typsca.eq.'I') then
            call wkvect(notrav, 'V V I', nlong, jtrav)
            do 1, k=1,n4
            zi(jtrav-1+k)=zi(jnomjv-1+k)
 1          continue
            call MPI_REDUCE(zi(jtrav), zi(jnomjv), n4, lint, MPI_SUM,&
                            0, mpicou, iermpi)
        else if (typsca.eq.'S') then
            call wkvect(notrav, 'V V S', nlong, jtrav)
            do 2, k=1,n4
            zi4(jtrav-1+k)=zi4(jnomjv-1+k)
 2          continue
            call MPI_REDUCE(zi4(jtrav), zi4(jnomjv), n4, lint4, MPI_SUM,&
                            0, mpicou, iermpi)
        else
            ASSERT(.false.)
        endif
        call mpierr(iermpi)
        call jedetr(notrav)
!
    else if (optmpi.eq.'MPI_SUM') then
!     -----------------------------------
!       REDUCTION + DIFFUSION DE L'OBJET JEVEUX NOMJEV
!       REMARQUE : NOMJEV PEUT ETRE UNE COLLECTION
        call jelira(nomjev, 'TYPE', ibid, typsca)
        call jelira(nomjev, 'XOUS', ibid, xous)
        if (xous .eq. 'X') then
            call jelira(nomjev, 'NMAXOC', nbobj, kbid)
        else
            nbobj=1
        endif
!
        do 10, iobj = 1,nbobj
        if (xous .eq. 'S') then
            call jeveuo(nomjev, 'E', jnomjv)
            call jelira(nomjev, 'LONMAX', nlong, kbid)
        else
            call jeexin(jexnum(nomjev, iobj), iexi)
            if (iexi .eq. 0) goto 10
            call jeveuo(jexnum(nomjev, iobj), 'E', jnomjv)
            call jelira(jexnum(nomjev, iobj), 'LONMAX', nlong, kbid)
        endif
!
        n4=nlong
!
        if (typsca .eq. 'R') then
            call wkvect(notrav, 'V V R', nlong, jtrav)
            call dcopy(n4, zr(jnomjv), 1, zr(jtrav), 1)
            call MPI_ALLREDUCE(zr(jtrav), zr(jnomjv), n4, lr8, MPI_SUM,&
                               mpicou, iermpi)
        else if (typsca.eq.'C') then
            call wkvect(notrav, 'V V C', nlong, jtrav)
            call zcopy(n4, zc(jnomjv), 1, zc(jtrav), 1)
            call MPI_ALLREDUCE(zc(jtrav), zc(jnomjv), n4, lc8, MPI_SUM,&
                               mpicou, iermpi)
        else if (typsca.eq.'I') then
            call wkvect(notrav, 'V V I', nlong, jtrav)
            do 3, k=1,n4
            zi(jtrav-1+k)=zi(jnomjv-1+k)
 3          continue
            call MPI_ALLREDUCE(zi(jtrav), zi(jnomjv), n4, lint, MPI_SUM,&
                               mpicou, iermpi)
        else if (typsca.eq.'S') then
            call wkvect(notrav, 'V V S', nlong, jtrav)
            do 4, k=1,n4
            zi4(jtrav-1+k)=zi4(jnomjv-1+k)
 4          continue
            call MPI_ALLREDUCE(zi4(jtrav), zi4(jnomjv), n4, lint4, MPI_SUM,&
                               mpicou, iermpi)
        else
            ASSERT(.false.)
        endif
        call jedetr(notrav)
!
        if (xous .eq. 'X') call jelibe(jexnum(nomjev, iobj))
10      continue
        call mpierr(iermpi)
!
    else
        ASSERT(.false.)
    endif
!
9999  continue
! --- COMPTEUR
    call uttcpu('CPU.CMPI.1', 'FIN', ' ')
    call jedema()
#endif
end subroutine
