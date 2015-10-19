subroutine pcldlt(matf, mat, niremp, base)
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
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gnomsd.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mtmchc.h"
#include "asterfort/pccoef.h"
#include "asterfort/pcfact.h"
#include "asterfort/pcstru.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/ldlt_matr.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=*) :: matf, mat, base
!-----------------------------------------------------------------------
!  fonction  :
!     Creation d'une matrice de preconditionnement matf
!     par factorisation ldlt plus ou moins complete de la matrice mat
!     stockee sous forme morse.
!     on peut choisir le degre de remplissage : niremp

!-----------------------------------------------------------------------
! out k*  matf   : nom de la matr_asse de preconditionnement
!                  remarque : ce n'est pas vraiment une matr_asse :
!                             elle a un stockage morse "etendu"
!                             et elle contient une factorisee LDLT !
! in  k*  mat    : nom de la matr_asse a preconditionner
! in  i   niremp : niveau de remplissage voulu pour matf
! in  k*  base   : nom de la base sur laquelle on cree matf ('G' ou 'V')
!-----------------------------------------------------------------------
    aster_logical :: complt
    character(len=1) :: bas1
    integer :: iret, nequ, ncoef, nblc
    integer :: jvalm, i, nzmax, niremp
    integer :: ier, k,jvalf
    real(kind=8) :: dnorm, epsi
    character(len=19) :: matfac, matas, matas1
    character(len=1) :: tysca
    character(len=14) :: nu, nuf
    character(len=24) :: noobj, nperm
    character(len=8) :: ma
    integer, pointer :: icpcx(:) => null()
    integer, pointer :: icpd(:) => null()
    integer, pointer :: icplx(:) => null()
    integer, pointer :: smdif(:) => null()
    integer, pointer :: smdi2(:) => null()
    real(kind=8), pointer :: vect(:) => null()
    real(kind=8), pointer :: vtravail(:) => null()
    integer(kind=4), pointer :: smhc(:) => null()
    integer(kind=4), pointer :: smhc1(:) => null()
    integer(kind=4), pointer :: smhcf(:) => null()
    integer, pointer :: smde(:) => null()
    integer, pointer :: smdi(:) => null()
    character(len=24), pointer :: refa(:) => null()
    character(len=24), pointer :: refaf(:) => null()
    integer, pointer :: perm(:) => null()

!----------------------------------------------------------------------
    call jemarq()

    matas1 = mat
    matfac = matf
    bas1 = base


!   -- Pour que la factorisation LDLT (incomplete) soit plus efficace,
!      il faut souvent renumeroter la matrice :
!      matas1 -> matas
!   ------------------------------------------------------------------
    nperm=matfac//'.PERM'
    matas='&&PCLDLT.MATR'
    call jedetr(nperm)
    call ldlt_matr(matas1,matas,nperm,bas1)


!   -- Prise en compte des ddls elimines (pour matas) :
!   ---------------------------------------------------
    call jeveuo(matas//'.REFA', 'L', vk24=refa)
    if (refa(3).ne.' ') then
        ASSERT(refa(3).ne.'ELIMF')
        if (refa(3) .eq. 'ELIML') call mtmchc(matas, 'ELIMF')
        ASSERT(refa(3).ne.'ELIML')
    endif



!   1. CALCUL DE : MA,NU,SMDI,SMHC,SMDE
!       NEQU,NCOEF
!       + QQUES VERIFS
!   ------------------------------------------
    ma = refa(1)(1:8)
    nu = refa(2)(1:14)

    call jeexin(nu//'.SMOS.SMDI', iret)
    if (iret .eq. 0) then
        call utmess('F', 'ALGELINE3_21', sk=matas)
    endif

    call jeveuo(nu//'.SMOS.SMDI', 'L', vi=smdi)
    call jeveuo(nu//'.SMOS.SMHC', 'L', vi4=smhc)
    call jeveuo(nu//'.SMOS.SMDE', 'L', vi=smde)
    nequ = smde(1)
    ncoef = smde(2)

    nblc = smde(3)
    if (nblc .ne. 1) then
        call utmess('F', 'ALGELINE3_22')
    endif

    call jelira(jexnum(matas//'.VALM', 1), 'TYPE', cval=tysca)
    if (tysca .eq. 'C') then
        call utmess('F', 'ALGELINE3_23')
    endif



!   1. CREATION DU NUME_DDL ASSOCIE A MATFAC :
!   ------------------------------------------

!   -- DETERMINATION DU NOM DE LA SD CACHEE NUME_DDL
    noobj ='12345678.NU000.NUME.PRNO'
    call gnomsd(' ', noobj, 12, 14)
    nuf=noobj(1:14)
    call copisd('NUME_DDL', bas1, nu, nuf)


!   2. CREATION DE MATFAC.REFA
!   ---------------------------
    call jedetr(matfac//'.REFA')
    call wkvect(matfac//'.REFA', bas1//' V K24 ', 20, vk24=refaf)
    refaf(1) = ma
    refaf(2) = nuf
    refaf(9) = 'MS'
    refaf(10) = 'NOEU'
    refaf(11)='MPI_COMPLET'


!   2. CALCUL DE EPSI POUR PCFACT ET ALLOCATION DE .VTRAVAIL:
!   ---------------------------------------------------------
    call jeveuo(jexnum(matas//'.VALM', 1), 'L', jvalm)
    AS_ALLOCATE(vr=vtravail, size=nequ)
    dnorm = 0.d0
    do i = 1, nequ
        dnorm = max(abs(zr(jvalm-1+smdi(i))),dnorm)
    end do
    epsi = 1.d-16*dnorm
    call jelibe(jexnum(matas//'.VALM', 1))


!   3. ON BOUCLE SUR PCSTRU JUSQU'A TROUVER LA TAILLE DE LA
!      FUTURE FACTORISEE :
!   ------------------------------------------------
    nzmax = ncoef
    AS_ALLOCATE(vi=smdi2, size=nequ+1)

    do k = 1, 2*niremp+2
        AS_ALLOCATE(vi=icpd, size=nequ)
        AS_ALLOCATE(vi=icplx, size=nequ+1)
        call jedetr('&&PCLDLT.SMHCF')
        call wkvect('&&PCLDLT.SMHCF', 'V V S', 2*nzmax, vi4=smhc1)
        AS_ALLOCATE(vi=icpcx, size=nzmax)

        call pcstru(nequ, smdi, smhc, smdi2, smhc1,&
                    icpd, icpcx, icplx, niremp, complt,&
                    nzmax, 0, ier)

        AS_DEALLOCATE(vi=icplx)
        AS_DEALLOCATE(vi=icpcx)
        AS_DEALLOCATE(vi=icpd)
        if (ier .eq. 0) goto 777
        nzmax=ier
    end do
    call utmess('F', 'ALGELINE3_24')
777 continue


!     -- ON MET A JOUR NUF.SMDI ET NUF.SMHC  :
!   ------------------------------------------------
    call jeveuo(nuf//'.SMOS.SMDI', 'E', vi=smdif)
    do k=1,nequ
        smdif(k) = smdi2(k)
    enddo
    AS_DEALLOCATE(vi=smdi2)

    call jedetr(nuf//'.SMOS.SMHC')
    call wkvect(nuf//'.SMOS.SMHC', bas1//' V S', nzmax, vi4=smhcf)
    do k=1,nzmax
        smhcf(k) = smhc1(k)
    enddo
    call jedetr('&&PCLDLT.SMHCF')



!   -- ON ALLOUE MATFAC.VALM :
!   ------------------------------------------------
    call jedetr(matfac//'.VALM')
    call jecrec(matfac//'.VALM', bas1//' V '//tysca, 'NU', 'DISPERSE', 'CONSTANT',&
                1)
    call jeecra(matfac//'.VALM', 'LONMAX', nzmax)
    call jecroc(jexnum(matfac//'.VALM', 1))


!   -- ON INJECTE MATAS.VALM DANS MATFAC.VALM :
!   ------------------------------------------------
    call jeveuo(jexnum(matas//'.VALM', 1), 'L', jvalm)
    call jeveuo(jexnum(matfac//'.VALM', 1), 'E', jvalf)
    call pccoef(nequ, smdi, smhc, zr(jvalm), smdif,&
                smhcf, zr(jvalf), vtravail)
    call jelibe(jexnum(matas//'.VALM', 1))


!   -- ON FACTORISE MATFAC.VALM :
!   ------------------------------------------------
    AS_ALLOCATE(vr=vect, size=nequ)
    call pcfact(matas, nequ, smdif, smhcf, zr(jvalf),&
                zr(jvalf), vect, epsi)


!   -- menage :
!   -------------
    call dismoi('NOM_NUME_DDL', matas, 'MATR_ASSE', repk=nu)
    call detrsd('NUME_DDL', nu)
    call detrsd('MATR_ASSE', matas)

    AS_DEALLOCATE(vr=vect)
    AS_DEALLOCATE(vr=vtravail)


!   -- Prise en compte des ddls elimines (pour matas1) :
!   -----------------------------------------------------
    call jeveuo(matas1//'.REFA', 'L', vk24=refa)
    ASSERT(refa(3).ne.'ELIMF')
    if (refa(3) .eq. 'ELIML') call mtmchc(matas1, 'ELIMF')
    ASSERT(refa(3).ne.'ELIML')



    call jedema()
end subroutine
