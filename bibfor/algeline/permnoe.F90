subroutine permnoe(maillage, deform, nbmod, nbno)
    implicit none
! ----------------------------------------------------------------------
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
! ======================================================================
!
! Permute a given nodal field (deform) according to the increasing 
! order of curvilinear abscissas as defined in the mesh (maillage)
! ----------------------------------------------------------------------
!
! person_in_charge: hassan.berro at edf.fr    
!
#include "jeveux.h"
#include "blas/dcopy.h"
#include "asterfort/assert.h"
#include "asterfort/i2extf.h"
#include "asterfort/i2sens.h"
#include "asterfort/i2tgrm.h"
#include "asterfort/i2vois.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/jxveri.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
!   ====================================================================
!   = 0 =   Variable declarations and initialization
!   ====================================================================
!
!   -0.1- Input/output arguments
    character(len=8), intent(in) :: maillage
    integer, intent(in) :: nbmod
    integer, intent(in) :: nbno
    real(kind=8) :: deform(nbno, nbmod)
!
!   -0.2- Local variables
    integer :: i, j, iexi, labs, nbrma, nbseg2, nbpoi1, kseg
    integer :: im, itypm, iseg2, iacnex, jgcnx, ino, nbse2
    integer :: numno, nbchm, isens, mi, ing, ind
    character(len=8) :: typm
    character(len=24) :: cooabs, conseg, typseg, nommas, connex, typmai
!
    integer, pointer          :: grmai(:)  => null()
    integer, pointer          :: vois1(:)  => null()
    integer, pointer          :: vois2(:)  => null()
    integer, pointer          :: ptch(:)   => null()
    integer, pointer          :: lnoe(:)   => null()
    integer, pointer          :: v_ach(:)  => null()
    integer, pointer          :: maille(:)  => null()
    real(kind=8), pointer     :: copyv(:)  => null()
    integer, pointer          :: tym(:)    => null()


!
!   -0.3- Initialization
    cooabs = maillage//'.ABSC_CURV .VALE'
    nommas = maillage//'.NOMMAI'
    connex = maillage//'.CONNEX'
    typmai = maillage//'.TYPMAIL'

    call jemarq()
!
!   ====================================================================
!
    call jeexin(cooabs, iexi)
    if (iexi .eq. 0) then
        call utmess('F', 'UTILITAI2_84')
    endif
    call jeveuo(cooabs, 'L', labs)   

!     --- LECTURE DES CARACTERISTIQUES DU GROUPE DE MAILLES : ADRESSE
!                   ET NOMBRE DE MAILLES
!
    call jelira(nommas, 'NOMUTI', nbrma)
    AS_ALLOCATE(nbrma, vi = grmai)
    do i = 1, nbrma
        grmai(i) = i
    end do
!
!     --- CREATION D OBJETS TEMPORAIRES ---
!
    AS_ALLOCATE(nbrma  , vi = vois1)
    AS_ALLOCATE(nbrma  , vi = vois2)
    AS_ALLOCATE(nbrma+1, vi = ptch)
    AS_ALLOCATE(nbrma+1, vi = lnoe)
    AS_ALLOCATE(nbrma  , vi = maille)
    AS_ALLOCATE(2*nbrma, vi = v_ach)
!
!     TRI DES MAILLES POI1 ET SEG2
    nbseg2 = 0
    nbpoi1 = 0
    kseg   = 0
    do im = 1, nbrma
        call jeveuo(typmai, 'L', itypm)
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(itypm+im-1)), typm)
        if (typm .eq. 'SEG2') then
            kseg           = zi(itypm+im-1)
            nbseg2         = nbseg2+1
            maille(nbseg2) = im
        else if (typm .eq. 'POI1') then
            nbpoi1        = nbpoi1+1
        else
            call utmess('F', 'MODELISA_2')
        endif
    end do
    conseg='&&PERMNO.CONNEX'
    typseg='&&PERMNO.TYPMAI'
    call wkvect(typseg, 'V V I', nbrma, vi = tym)
    do im = 1, nbrma
        tym(im) = kseg
    end do

!     IL FAUT CREER UNE TABLE DE CONNECTIVITE POUR LES SEG2
    call jecrec(conseg, 'V V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbseg2)
    call jeecra(conseg, 'LONT', 2*nbseg2)
    do iseg2 = 1, nbseg2
        im = maille(iseg2)
        call jelira(jexnum(connex, im), 'LONMAX', nbse2)
        call jeveuo(jexnum(connex, im), 'L', iacnex)
        call jeecra(jexnum(conseg, iseg2), 'LONMAX', nbse2)
        call jeveuo(jexnum(conseg, iseg2), 'E', jgcnx)
        do ino = 1, nbse2
            numno           = zi(iacnex-1+ino)
            zi(jgcnx+ino-1) = numno
        end do
    end do
!
    call i2vois(conseg, typseg, grmai, nbseg2, vois1,&
                vois2)
    call i2tgrm(vois1, vois2, nbseg2, v_ach, ptch,&
                nbchm)
    call i2sens(v_ach, nbseg2*2, grmai, nbseg2, conseg,&
                typseg, zr(labs))

!     --- CREATION D UNE LISTE ORDONNEE DE NOEUDS ---
    do i = 1, nbseg2
        isens = 1
        mi = v_ach(i)
        if (mi .lt. 0) then
            mi    = -mi
            isens = -1
        endif
        call i2extf(mi, 1, conseg, typseg, ing,&
                    ind)
        if (isens .eq. 1) then
            lnoe(i)   = ing
            lnoe(i+1) = ind
        else
            lnoe(i+1) = ing
            lnoe(i)   = ind
        endif
    end do
!
    ASSERT(nbno.eq.(nbseg2+1))
    AS_ALLOCATE(nbno*nbmod, vr = copyv)
    call dcopy(nbno*nbmod, deform, 1, copyv, 1)

    do i = 1, nbno
        do j = 1, nbmod
            deform(i, j) = copyv(lnoe(i)+(j-1)*nbno)
        end do
    end do

    AS_DEALLOCATE(vi = grmai)
    AS_DEALLOCATE(vi = vois1)
    AS_DEALLOCATE(vi = vois2)
    AS_DEALLOCATE(vi = ptch)
    AS_DEALLOCATE(vi = lnoe)
    AS_DEALLOCATE(vi = v_ach)
    AS_DEALLOCATE(vi = maille)
    AS_DEALLOCATE(vr = copyv)

    call jedetr(typseg)
    call jedetr(conseg)

    call jedema()

end subroutine