subroutine apmamd(kptsc)
    implicit none
!           CONFIGURATION MANAGEMENT OF EDF VERSION
! ==================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D              WWW.CODE-ASTER.ORG
!
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
! MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS
! PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE
! LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,
! BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ==================================================================
! person_in_charge: nicolas.sellenet at edf.fr
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mpicm0.h'
    include 'asterfort/wkvect.h'
    integer :: kptsc
!----------------------------------------------------------------
!
!  REMPLISSAGE DE LA MATRICE PETSC (INSTANCE NUMERO KPTSC)
!  DANS LE CAS MATR_DISTRIBUEE
!
!----------------------------------------------------------------
!
#ifdef _HAVE_PETSC
!
!================================================================
# include "finclude/petscsys.h"
# include "finclude/petscvec.h"
# include "finclude/petscmat.h"
# include "finclude/petscksp.h"
# include "finclude/petscpc.h"
!================================================================
!----------------------------------------------------------------
!     AU PLUS 5 MATRICES PETSC SONT GEREES A LA FOIS
    integer :: nmxins
    parameter   (nmxins=5)
!
!     VARIABLES LOCALES
    integer :: nsmdi, nsmhc, nz, nvalm, nlong
    integer :: jsmdi, jsmhc, jdxi1, jdxi2, jdval1, jdval2, jvalm, jvalm2
    integer :: k, iligl, jcoll, nzdeb, nzfin, nuno1, nucmp1, nuno2, nbproc
    integer :: iterm, jterm, jcolg, iligg, jnugll, nucmp2, procol, jprddl
    integer :: jnequ, nloc, nglo, prolig, rang, jnequl
!
    character(len=19) :: nomat, nosolv
    character(len=16) :: idxi1, idxi2, trans1, trans2
    character(len=14) :: nonu
    character(len=4) :: kbid
!
    logical :: lmnsy
!
    real(kind=8) :: valm
!
    parameter (idxi1 ='&&APETS2.IDXI1__')
    parameter (idxi2 ='&&APETS2.IDXI2__')
    parameter (trans1='&&APETS2.TRANS1_')
    parameter (trans2='&&APETS2.TRANS2_')
!
!     COMMUN DE SAUVEGARDE DES INSTANCES
    character(len=19) :: nomats(nmxins), nosols(nmxins)
    character(len=14) :: nonus(nmxins)
    Mat :: ap(nmxins)
    KSP :: kp(nmxins)
    Vec :: b, x
    common /spetsc/ ap,kp,b,x,nomats,nosols,nonus
!----------------------------------------------------------------
!     Variables PETSc
    PetscInt :: low, high, neql, neqg, ierr
    Mat :: a
!----------------------------------------------------------------
    call jemarq()
!
!     -- LECTURE DU COMMUN
    nomat = nomats(kptsc)
    nosolv = nosols(kptsc)
    nonu = nonus(kptsc)
    a = ap(kptsc)
!
    call jeveuo(nonu//'.SMOS.SMDI', 'L', jsmdi)
    call jelira(nonu//'.SMOS.SMDI', 'LONMAX', nsmdi, kbid)
    call jeveuo(nonu//'.SMOS.SMHC', 'L', jsmhc)
    call jelira(nonu//'.SMOS.SMHC', 'LONMAX', nsmhc, kbid)
    call jeveuo(nonu//'.NUME.NEQU', 'L', jnequ)
    call jeveuo(nonu//'.NUML.NEQU', 'L', jnequl)
    call jeveuo(nonu//'.NUML.NLGP', 'L', jnugll)
    call jeveuo(nonu//'.NUML.PDDL', 'L', jprddl)
    call mpicm0(rang, nbproc)
    nloc = zi(jnequl)
    nglo = zi(jnequ)
    neql = nloc
    neqg = nglo
    nz=zi(jsmdi-1+nloc)
!
    call jelira(nomat//'.VALM', 'NMAXOC', nvalm, kbid)
    if (nvalm .eq. 1) then
        lmnsy=.false.
    else if (nvalm.eq.2) then
        call assert(.false.)
        lmnsy=.true.
    else
        call assert(.false.)
    endif
!
    call jeveuo(jexnum(nomat//'.VALM', 1), 'L', jvalm)
    call jelira(jexnum(nomat//'.VALM', 1), 'LONMAX', nlong, kbid)
    call assert(nlong.eq.nz)
    if (lmnsy) then
        call jeveuo(jexnum(nomat//'.VALM', 2), 'L', jvalm2)
        call jelira(jexnum(nomat//'.VALM', 2), 'LONMAX', nlong, kbid)
        call assert(nlong.eq.nz)
    endif
!
!     low donne la premiere ligne stockee localement
!     high donne la premiere ligne stockee par le processus (rang+1)
!     ATTENTION ces indices commencent a zero (convention C de PETSc)
    call MatGetOwnershipRange(a, low, high, ierr)
    call assert(ierr.eq.0)
!
    call wkvect(idxi1, 'V V S', nloc, jdxi1)
    call wkvect(idxi2, 'V V S', nloc, jdxi2)
    call wkvect(trans1, 'V V R', nloc, jdval1)
    call wkvect(trans2, 'V V R', nloc, jdval2)
!
    iterm=0
    jterm=0
!
!     Recopie de la matrice
!     C'est PETSc qui s'occupe de la recopie des termes vers
!     le bon processeur
    call MatSetValue(a, zi(jnugll)-1, zi(jnugll)-1, zr(jvalm), ADD_VALUES,&
                     ierr)
!
    do jcoll = 2, nloc
        nzdeb = zi(jsmdi+jcoll-2) + 1
        nzfin = zi(jsmdi+jcoll-1)
        procol = zi(jprddl+jcoll-1)
        jcolg = zi(jnugll+jcoll-1)
        do k = nzdeb, nzfin
            iligl = zi4(jsmhc-1+k)
            iligg = zi(jnugll-1+iligl)
            prolig = zi(jprddl-1+iligl)
            jterm=jterm+1
            valm=zr(jvalm-1+k)
            zr(jdval2+jterm-1)=valm
            zi4(jdxi2+jterm-1)=iligg-1
            if (iligg .ne. jcolg) then
                iterm=iterm+1
                valm=zr(jvalm-1+k)
                zr(jdval1+iterm-1)=valm
                zi4(jdxi1+iterm-1)=iligg-1
            endif
        end do
        call MatSetValues(a, jterm, zi4(jdxi2), 1, jcolg-1,&
                          zr(jdval2), ADD_VALUES, ierr)
        call MatSetValues(a, 1, jcolg-1, iterm, zi4(jdxi1),&
                          zr(jdval1), ADD_VALUES, ierr)
        iterm=0
        jterm=0
    end do
!
    call jelibe(nonu//'.SMOS.SMDI')
    call jelibe(nonu//'.SMOS.SMHC')
    call jelibe(jexnum(nomat//'.VALM', 1))
    if (lmnsy) call jelibe(jexnum(nomat//'.VALM', 2))
!
    call jedetr(idxi1)
    call jedetr(idxi2)
    call jedetr(trans1)
    call jedetr(trans2)
!
    call jedema()
!
#endif
!
end subroutine
