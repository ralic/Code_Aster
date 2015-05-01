subroutine elg_calc_matm_red(matas1, matas2, bas1)
    implicit none
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcncon.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jerazo.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=19) :: matas1, matas2
    character(len=1) :: bas1
!--------------------------------------------------------------
! BUT :
!   Créer la matrice aster matas2 correspondant à la matrice
!   Kproj de matas1 ainsi que son nume_ddl
!
! in/jxin   : matas1 : sd_matr_asse
! in/jxout  : matas2 : sd_matr_asse "reduite" (sans lagranges)
! in        : bas1 : 'G'/'V' (pour la création de matas2)
!
! Remarque : on crée egalement un nume_ddl (sous-terrain) pour
!            matas2.
!---------------------------------------------------------------
!
#ifdef _HAVE_PETSC
#include "elim_lagr.h"
!
!================================================================
    character(len=1) :: kbid
    character(len=8) :: ma, nomgds
    character(len=14) :: nu1, nu2
    character(len=19) :: ligrmo
    integer :: ibid, ico, kdiag, neq2, nnz2
    integer :: ilig, jcol, jnzcol, jsmdi2, ndiag, k, jsmhc2
    integer :: jvalm2,  j1, ier, ieq1, ieq2, nbnl, nbno
    integer :: nbnom,  jdeeq2, jprno2, nec, icmp, icmpav, ino, inoav
    integer :: k1ec, k2ec, k3ec, k3ecav
    integer, allocatable :: nbddl(:), nueq(:), dejavu(:)
    PetscInt :: ierr, n1, nterm
    PetscInt, allocatable :: irow(:)
    real(kind=8), allocatable :: vrow(:)
    integer, pointer :: deeq(:) => null()
    character(len=24), pointer :: refa(:) => null()
!----------------------------------------------------------------
    call jemarq()
!
!
!     1. On dupplique les 2 SD => NU2, MATAS2 :
!     -----------------------------------------
    call dismoi('NOM_NUME_DDL', matas1, 'MATR_ASSE', repk=nu1)
    call gcncon('_', nu2)
!
    call copisd('MATR_ASSE', bas1, matas1, matas2)
    call copisd('NUME_DDL', bas1, nu1, nu2)
!
!
!     2. On corrige ce qui doit l'etre :
!     -----------------------------------
    call jedetr(matas2//'.CONL')
!
!     2.1 On corrige MATAS2.REFA :
!     -----------------------------
    call jeveuo(matas2//'.REFA', 'E', vk24=refa)
    refa(2)=nu2
    ASSERT(refa(8).eq.'ASSE')
!     -- pour l'instant, on ne traite que les matrices symétriques :
    if (refa(9) .ne. 'MS') call utmess('F', 'ELIMLAGR_5')
    if (refa(10) .ne. 'NOEU') call utmess('F', 'ELIMLAGR_6')
    ASSERT(refa(11).eq.'MPI_COMPLET')
!
!     -- la matrice MATAS2 n'est pas concernée par ELIM_LAGR :
    refa(19)=' '
    refa(20)=matas1
!
!
!     2.2 On calcule MATAS2.VALM, NU2.NUME.SMDI et NU2.SMOS.SMHC :
!     -------------------------------------------------------------
!
!     -- calcul de :
!       neq2 : nombre de ddls de MATAS2
!       nnz2 : nombre de termes dans .VALM(1)
    neq2=size(melim(ke)%indred)
    if (neq2 .eq. 0) call utmess('F', 'ELIMLAGR_7')
!
!     -- on parcourt la matrice Kproj pour repérer ses termes non nuls
    allocate(irow(neq2))
    allocate(vrow(neq2))
    call wkvect('&&ELG_CALC_MATM_RED.NZCO', 'V V I', neq2, jnzcol)
    ndiag=0
    nnz2=0
    do ilig = 0, neq2-1
        call MatGetRow(melim(ke)%kproj, to_petsc_int(ilig), nterm, irow(1), vrow(1),&
                       ierr)
        do k = 1, nterm
            jcol=irow(k)
            if (jcol .eq. ilig) ndiag=ndiag+1
            if (jcol .ge. ilig) then
                nnz2=nnz2+1
                zi(jnzcol-1+jcol+1)=zi(jnzcol-1+jcol+1)+1
            endif
        enddo
        call MatRestoreRow(melim(ke)%kproj, to_petsc_int(ilig), nterm, irow(1), vrow(1),&
                           ierr)
    enddo
    ASSERT(ndiag.eq.neq2)
!
!
!     -- allocation de MATAS2.VALM :
!     ------------------------------
    call jedetr(matas2//'.VALM')
    call jecrec(matas2//'.VALM', bas1//' V R', 'NU', 'DISPERSE', 'CONSTANT',&
                1)
    call jecroc(jexnum(matas2//'.VALM', 1))
    call jeecra(matas2//'.VALM', 'LONMAX', nnz2, kbid)
    call jeveuo(jexnum(matas2//'.VALM', 1), 'E', jvalm2)
!
!     -- calcul de NU2.SMOS.SMDI :
!     ----------------------------
    call jedetr(nu2//'.SMOS.SMDI')
    call wkvect(nu2//'.SMOS.SMDI', bas1//' V I', neq2, jsmdi2)
    ico=0
    do ilig = 1, neq2
        ico=ico+zi(jnzcol-1+ilig)
        zi(jsmdi2-1+ilig)=ico
    enddo
!
!     -- calcul de NU2.SMOS.SMHC et remplissage de matas2.VALM  :
!     -----------------------------------------------------------
    call jedetr(nu2//'.SMOS.SMHC')
    call wkvect(nu2//'.SMOS.SMHC', bas1//' V S', nnz2, jsmhc2)
    call jerazo('&&ELG_CALC_MATM_RED.NZCO', neq2, 1)
    do ilig = 0, neq2-1
        call MatGetRow(melim(ke)%kproj,to_petsc_int(ilig), nterm, irow(1), vrow(1),&
                       ierr)
        do k = 1, nterm
            jcol=irow(k)
            if (jcol .ge. ilig) then
                n1=zi(jnzcol-1+jcol+1)+1
                if (jcol .eq. 0) then
                    kdiag=0
                else
                    kdiag=zi(jsmdi2-1+jcol)
                endif
                zi4(jsmhc2-1+kdiag+n1)=ilig+1
                zr(jvalm2-1+kdiag+n1)=vrow(k)
                zi(jnzcol-1+jcol+1)=n1
            endif
        enddo
        call MatRestoreRow(melim(ke)%kproj, to_petsc_int(ilig), nterm, irow(1), vrow(1),&
                           ierr)
    enddo
!
!
!     2.3  Correction des autres objets de NU2 :
!     --------------------------------------------
    call jeveuo(nu2//'.NUME.REFN', 'E', j1)
    ma=zk24(j1-1+1)(1:8)
    nomgds=zk24(j1-1+2)(1:8)
    call dismoi('NB_EC', nomgds, 'GRANDEUR', repi=nec)
!
!     nu2.NSLV :
!     ----------
    call jeveuo(nu2//'.NSLV', 'E', j1)
    zk24(j1-1+1)=' '
!
!     nu2.NEQU :
!     ----------
    call jeveuo(nu2//'.NUME.NEQU', 'E', j1)
    zi(j1-1+1)=neq2
!
!     nu2.SMDE :
!     ----------
    call jeveuo(nu2//'.SMOS.SMDE', 'E', j1)
    zi(j1-1+1)=neq2
    zi(j1-1+2)=nnz2
!
!     nu2.DELG :
!     ----------
    call jedetr(nu2//'.NUME.DELG')
    call wkvect(nu2//'.NUME.DELG', bas1//' V I', neq2, j1)
    do k = 1, neq2
        zi(j1-1+k)=0
    enddo
!
!     nu2.NUEQ :
!     -----------
    call jedetr(nu2//'.NUME.NUEQ')
    call wkvect(nu2//'.NUME.NUEQ', bas1//' V I', neq2, j1)
    do k = 1, neq2
        zi(j1-1+k)=k
    enddo
!
!     nu2.LILI :
!     ----------
    call jedetr(nu2//'.NUME.LILI')
    call jecreo(nu2//'.NUME.LILI', bas1//' N K24')
    call jeecra(nu2//'.NUME.LILI', 'NOMMAX', 2, '  ')
    call jecroc(jexnom(nu2//'.NUME.LILI', '&MAILLA'))
!
!     -- pour éviter de se planter dans vpstor.f (ligne 160)
!        qui fait un dismoi('nom_modele',...) indésirable :
    call jenuno(jexnum(nu1//'.NUME.LILI', 2), ligrmo)
    call jecroc(jexnom(nu2//'.NUME.LILI', ligrmo))
!
!
!     nu2.DEEQ :
!     ----------
    call jeveuo(nu1//'.NUME.DEEQ', 'L', vi=deeq)
    call jedetr(nu2//'.NUME.DEEQ')
    call wkvect(nu2//'.NUME.DEEQ', bas1//' V I', 2*neq2, jdeeq2)
    do ieq2 = 1, neq2
        ieq1=melim(ke)%indred(ieq2)
        ASSERT(deeq(2*(ieq1-1)+1).gt.0)
        zi(jdeeq2-1+2*(ieq2-1)+1)=deeq(2*(ieq1-1)+1)
        zi(jdeeq2-1+2*(ieq2-1)+2)=deeq(2*(ieq1-1)+2)
    enddo
!
!     nu2.PRNO (calculé à partir de .DEEQ):
!     -------------------------------------
    call jedetr(nu2//'.NUME.PRNO')
    call jecrec(nu2//'.NUME.PRNO', bas1//' V I ', 'NU', 'CONTIG', 'VARIABLE',&
                1)
    call dismoi('NB_NO_MAILLA', ma, 'MAILLAGE', repi=nbno)
    call dismoi('NB_NL_MAILLA', ma, 'MAILLAGE', repi=nbnl)
    ASSERT(nbnl.eq.0)
    nbnom = nbno + nbnl
    call jeecra(jexnum(nu2//'.NUME.PRNO', 1), 'LONMAX', nbnom*(nec+2), kbid)
    call jeveuo(jexnum(nu2//'.NUME.PRNO', 1), 'E', jprno2)
!
!     -- On vérifie que les ddls d'un noeud se suivent
!        dans le bon ordre.
!        On compte le nombre de ddls par noeud => nbddl(ino)
!        On calcule nueq pour chaque noeud     => nueq(ino)
    allocate(nbddl(nbnom)); nbddl=0
    allocate(nueq(nbnom)) ; nueq=0
    allocate(dejavu(nbnom)); dejavu=0
    nbddl=0
    inoav=0
    icmpav=0
    do ieq2 = 1, neq2
        ino=zi(jdeeq2-1+2*(ieq2-1)+1)
        icmp=zi(jdeeq2-1+2*(ieq2-1)+2)
        ASSERT(ino.gt.0)
        ASSERT(icmp.gt.0)
        if (ino .ne. inoav) then
            ASSERT(dejavu(ino).eq.0)
            dejavu(ino)=1
            inoav=ino
            icmpav=icmp
            nueq(ino)=ieq2
        else
            ASSERT(icmp.gt.icmpav)
            icmpav=icmp
        endif
        nbddl(ino)=nbddl(ino)+1
    enddo
!
!     -- remplissage de .PRNO :
    do ino = 1, nbnom
        zi(jprno2-1+(2+nec)*(ino-1)+1)=nueq(ino)
        zi(jprno2-1+(2+nec)*(ino-1)+2)=nbddl(ino)
    enddo
!     -- mise à jour des entiers codés :
    do ieq2 = 1, neq2
        ino=zi(jdeeq2-1+2*(ieq2-1)+1)
        icmp=zi(jdeeq2-1+2*(ieq2-1)+2)
        k1ec=(icmp-1)/30
        k2ec=mod(icmp,30)
        if (k2ec .eq. 0) then
            k2ec=30
            k1ec=k1ec-1
        endif
        k3ecav=zi(jprno2-1+(2+nec)*(ino-1)+2+k1ec+1)
        k3ec=ior(2**k2ec,k3ecav)
        zi(jprno2-1+(2+nec)*(ino-1)+2+k1ec+1)=k3ec
    enddo
!
!
!
!
!
    deallocate(irow,vrow,nbddl,nueq,dejavu)
    call jedetr('&&ELG_CALC_MATM_RED.NZCO')
    call jedema()
#else
    call utmess('F', 'ELIMLAGR_1')
#endif
!
end subroutine
