subroutine mtcmbl(nbcomb, typcst, const, limat, matrez,&
                  ddlexc, numedd, elim)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cbval2.h"
#include "asterfort/cbvale.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/idenob.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedup1.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtconl.h"
#include "asterfort/mtdefs.h"
#include "asterfort/mtdscr.h"
#include "asterfort/mtmchc.h"
#include "asterfort/prosmo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: nbcomb
    character(len=*) :: typcst(nbcomb), ddlexc
    character(len=*) :: matrez, numedd
    character(len=*) :: limat(nbcomb)
    character(len=5) :: elim
    real(kind=8) :: const(*)
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     combinaison lineaire de matrices  :
!     -------------------------------------
!     mat_res= somme(alpha_i*mat_i)
!
!       *  les matrices (mat_i) doivent avoir la meme numerotation des
!           ddls mais elles peuvent avoir des connectivites differentes
!          (i.e. des stockages differents)
!       *  les matrices (mat_i) sont reelles ou complexes
!       *  les matrices (mat_i) sont symetriques ou non
!       *  les coefficients (alpha_i) sont reels ou complexes
!       *  on peut melanger matrices reelles et complexes et les types
!          (r/c) des coefficients. on peut faire par exemple :
!          mat_res= alpha_r1*mat_c1 + alpha_c2*mat_r2
!       *  mat_res doit etre allouee avant l'appel a mtcmbl
!          cela veut dire que son type (r/c) est deja determine.
!       *  si type(mat_res)=r et que certains (mat_i/alpha_i) sont c,
!          cela veut simplement dire que mat_res contiendra la partie
!          reelle de la combinaison lineaire (qui est complexe)
!
!---------------------------------------------------------------------
! in  i  nbcomb = nombre de matrices a combiner
! in  v(k1) typcst = type des constantes (r/c)
! in  v(r)  const  = tableau de r*8    des coeficients
!     attention : const peut etre de dimension > nbcomb car
!                 les coefs complexes sont stockes sur 2 reels
! in  v(k19) limat = liste des noms des matr_asse a combiner
! in/jxout k19 matrez = nom de la matr_asse resultat
!        cette matrice doit avoir ete creee au prealable (mtdefs)
! in  k* ddlexc = nom du ddl a exclure ("lagr"/" " )
!
! si les matrices combinees n'ont pas le meme stockage, il faut
! creer un nouveau nume_ddl pour ce stockage :
! in/jxout  k14 numedd = nom du nume_ddl sur lequel s'appuiera matrez
!        si numedd ==' ', le nom du nume_ddl sera obtenu par gcncon
!        si numedd /=' ', on prendra numedd comme nom de nume_ddl
! in    k5  : / 'elim=' : si les matrices a combiner n'ont pas les memes
!                         ddls elimines (char_cine) => erreur <f>
!             / 'elim1' : la matrice resultat aura les memes ddls
!                         elimines que la 1ere matrice de la liste limat
!---------------------------------------------------------------------
    character(len=1) :: base, bas2, typres
    character(len=8) :: typmat, kmpic, kmpic1, kmatd
    character(len=19) :: matemp, mat1, matres, mati
    character(len=24) :: valk(2)
    integer :: jrefar, jrefai, ier, ier1
    integer :: i, lres, nbloc, lgbloc
    aster_logical :: reutil, symr, symi, matd, nosymr
    character(len=24) :: kxfem
    character(len=24), pointer :: refa1(:) => null()
    character(len=24), pointer :: refa(:) => null()
    integer, pointer :: lispoint(:) => null()
!   -----------------------------------------------------------------
    call jemarq()
!
    ASSERT(elim.eq.'ELIM=' .or. elim.eq.'ELIM1')
!
    matres = matrez
    mat1=limat(1)
    ASSERT(nbcomb.ge.1)
    call jelira(matres//'.REFA', 'CLAS', cval=base)
    call jelira(matres//'.VALM', 'TYPE', cval=typres)
    call jelira(matres//'.VALM', 'NMAXOC', nbloc)
    call jelira(matres//'.VALM', 'LONMAX', lgbloc)
    ASSERT(nbloc.eq.1.or.nbloc.eq.2)
    call jeveuo(matres//'.REFA', 'E', jrefar)
    ASSERT(zk24(jrefar-1+9) (1:1).eq.'M')
    symr = zk24(jrefar-1+9) .eq. 'MS'
    if (symr) then
        ASSERT(nbloc.eq.1)
    else
        ASSERT(nbloc.eq.2)
    endif
!
    ASSERT(ddlexc.eq.' '.or.ddlexc.eq.'LAGR')
    AS_ALLOCATE(vi=lispoint, size=nbcomb)
    reutil=.false.
    do i = 1, nbcomb
        ASSERT(typcst(i).eq.'R'.or.typcst(i).eq.'C')
        mati=limat(i)
        call jeveuo(mati//'.REFA', 'L', jrefai)
        if (zk24(jrefai-1+3) .eq. 'ELIMF') call mtmchc(mati, 'ELIML')
        call mtdscr(mati)
        call jeveuo(mati//'.&INT', 'E', lispoint(i))
        call jelira(mati//'.VALM', 'TYPE', cval=typmat)
        call jelira(mati//'.VALM', 'NMAXOC', nbloc)
        call jeveuo(mati//'.REFA', 'L', jrefai)
        symi = zk24(jrefai-1+9) .eq. 'MS'
        if (symi) then
            ASSERT(nbloc.eq.1)
        else
            ASSERT(nbloc.eq.2)
            nosymr=.not.symr
            ASSERT(nosymr)
        endif
        call dismoi('XFEM', mati, 'MATR_ASSE', repk=kxfem)
        if (kxfem .eq. 'XFEM_PRECOND') call utmess('F', 'XFEMPRECOND_3', nk=1, valk=mati)
        if (mati .eq. matres) reutil=.true.
    end do
!
!
!   -- si la matrice resultat est l'une de celles a combiner,
!      il ne faut pas la detruire !
!   ------------------------------------------------------------
    if (reutil) then
        matemp='&&MTCMBL.MATEMP'
        call mtdefs(matemp, matres, 'V', typres)
    else
        matemp = matres
    endif
    call jelira(matemp//'.REFA', 'CLAS', cval=bas2)
!
!
!   -- verif. de la coherence mpi des matrices a combiner
!   ----------------------------------------------------
    call dismoi('MPI_COMPLET', mat1, 'MATR_ASSE', repk=kmpic1)
    if (kmpic1 .eq. 'OUI') then
        zk24(jrefar-1+11)='MPI_COMPLET'
    else
        zk24(jrefar-1+11)='MPI_INCOMPLET'
    endif
    matd = .false.
    call dismoi('MATR_DISTR', mat1, 'MATR_ASSE', repk=kmatd)
    if (kmatd .eq. 'OUI') then
        matd = .true.
        zk24(jrefar-1+11)='MATR_DISTR'
    endif
    do i = 2, nbcomb
        mati=limat(i)
        call dismoi('MPI_COMPLET', mati, 'MATR_ASSE', repk=kmpic)
        if (kmpic .ne. kmpic1) then
            valk(1)=mat1
            valk(2)=mati
            call utmess('F', 'CALCULEL6_55', nk=2, valk=valk)
        endif
        call dismoi('MATR_DISTR', mati, 'MATR_ASSE', repk=kmatd)

!       il est necessaire que toutes les matrices qu'on cherche a
!       combiner soit du meme type (soit toutes distribuees,
!       soit toutes completes mais surtout pas de melange !)
        if (kmatd .eq. 'OUI') then
            ASSERT(matd)
        else
            ASSERT(.not.matd)
        endif
    end do
!
!
!   --- verif. de la coherence des numerotations des matrices a combiner
!   ------------------------------------------------------------------
    call jeveuo(mat1//'.REFA', 'L', vk24=refa1)
    ier1 = 0
    do i = 2, nbcomb
        mati=limat(i)
        call jeveuo(mati//'.REFA', 'L', jrefai)
        if (refa1(2) .ne. zk24(jrefai-1+2)) ier1 = 1
        if (refa1(2) .ne. zk24(jrefai-1+2)) ier1 = 1
        if (refa1(1) .ne. zk24(jrefai-1+1)) then
            call utmess('F', 'ALGELINE2_9')
        endif
        if (elim .eq. 'ELIM=') then
            if (.not.idenob(mat1//'.CCID',mati//'.CCID')) then
                valk(1)=mat1
                valk(2)=mati
!               -- si on ne fait pas le DEALLOCATE, on ne peut pas executer
!                  le test zzzz213a qui fait un try/except
!                  puis execute a nouveau cette routine
                AS_DEALLOCATE(vi=lispoint)
                call utmess('F', 'ALGELINE2_10', nk=2, valk=valk)
            endif
        endif
    end do
!
!
!
!   -- 2) combinaison lineaire des .VALM des matrices :
!   ====================================================
!
!    --   cas ou les matrices a combiner ont le meme profil :
!   ----------------------------------------------------------
    if (ier1 .eq. 0) then
        call mtdscr(matemp)
        call jeveuo(matemp//'.&INT', 'E', lres)
        call cbvale(nbcomb, typcst, const, lispoint, typres,&
                    lres, ddlexc, matd)
!
!   --   cas ou les matrices a combiner n'ont pas le meme profil :
!   ---------------------------------------------------------------
    else
!       si les matrices sont distribuee mais n'ont pas le meme
!       profil, on plante !
        if (matd) then
            call utmess('F', 'ALGELINE5_1')
        endif
        call prosmo(matemp, limat, nbcomb, base, numedd,&
                    symr, typres)
        call mtdscr(matemp)
        call jeveuo(matemp//'.&INT', 'E', lres)
        call cbval2(nbcomb, typcst, const, lispoint, typres,&
                    lres, ddlexc)
    endif
!
!
!   -- ddl elimines :
!   ===================
    call jeveuo(matemp//'.REFA', 'L', vk24=refa)
    call jedetr(matemp//'.CCID')
    call jedetr(matemp//'.CCVA')
    call jedetr(matemp//'.CCLL')
    call jedetr(matemp//'.CCII')
    call jedup1(mat1//'.CCID', bas2, matemp//'.CCID')
    call jeexin(matemp//'.CCID', ier)
    if (ier .gt. 0) refa(3)='ELIML'
!
!
!   -- construction du descripteur de la matrice resultat :
!   =========================================================
    call mtdscr(matemp)
    call jeveuo(matemp(1:19)//'.&INT', 'E', lres)
!
!
!   -- combinaison lineaire des .CONL des matrices si necessaire :
!   ===============================================================
    if (ddlexc .ne. 'LAGR') then
        call mtconl(nbcomb, typcst, const, lispoint, typres,&
                    lres)
    else
        call jedetr(zk24(zi(lres+1))(1:19)//'.CONL')
    endif
!
!
!   -- on remet la matrice dans l'etat 'asse' :
!   -------------------------------------------
    call jeveuo(matres//'.REFA', 'E', jrefar)
    zk24(jrefar-1+8)='ASSE'
!
    if (reutil) then
        call copisd('MATR_ASSE', base, matemp, matres)
        call detrsd('MATR_ASSE', matemp)
    endif
!
    AS_DEALLOCATE(vi=lispoint)
!
    call jedema()
end subroutine
