subroutine nugllo(nu, base, solveu)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: nicolas.sellenet at edf.fr
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/nupodd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=14) :: nu
    character(len=2) :: base
    character(len=19) :: solveu
!----------------------------------------------------------------------
!---- OBJET : CREATION D'UN CHAMP .NUML A LA S.D. NUME_DDL NU
!             CE .NUML DECRIT LA NUMEROTATION LOCALE UTILE POUR MUMPS
!             FONCTIONNANT EN AVEC UNE MATRICE ASSEMBLEE DISTRIBUEE
!----------------------------------------------------------------------
! IN K14 NU     : NOM DU NUME_DDL
! IN K2   BASE  : BASE(1:1) : BASE POUR CREER LE NUME_DDL
!                    (SAUF LE PROF_CHNO)
!               : BASE(2:2) : BASE POUR CREER LE PROF_CHNO
!----------------------------------------------------------------------
!
!
    integer :: nbma, jconx2
    integer :: rang, numa, nbnoma, nbno, ino, nuno
    integer :: nec, nlili, neql, idprn2, ili, ntot
    integer :: idpr21, idpr22, numinc, numec, nddl
    integer :: neqg, iddl, jnulg
    integer :: jnueql, ddl1g, ddl1l, jdelgl, j1
    integer :: iel, igr, nel, k1, n1, j, ilib
    integer :: nbproc, vali(1), jnugl, ieqg
!
    character(len=8) :: noma, partit, mo
    character(len=19) :: ligrmo, nomlig
!----------------------------------------------------------------------
    aster_logical :: ldist, ldgrel
    integer, pointer :: ddl_pres(:) => null()
    integer, pointer :: delg_tmp(:) => null()
    integer, pointer :: tab_eq(:) => null()
    integer, pointer :: tab_no(:) => null()
    integer, pointer :: adli(:) => null()
    integer, pointer :: connex(:) => null()
    character(len=24), pointer :: prtk(:) => null()
    integer, pointer :: delg(:) => null()
    integer, pointer :: adne(:) => null()
    integer, pointer :: dime(:) => null()
    character(len=24), pointer :: slvk(:) => null()
    integer, pointer :: prn1(:) => null()
    integer, pointer :: maille(:) => null()
    mpi_int :: mrank, msize
!
!---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS PRNO DES S.D. LIGREL
!     REPERTORIEES DANS LE CHAMP LILI DE NUME_DDL ET A LEURS ADRESSES
!     ZZPRNO(ILI,NUNOEL,1) = NUMERO DE L'EQUATION ASSOCIEES AU 1ER DDL
!                            DU NOEUD NUNOEL DANS LA NUMEROTATION LOCALE
!                            AU LIGREL ILI DE .LILI
!     ZZPRNO(ILI,NUNOEL,2) = NOMBRE DE DDL PORTES PAR LE NOEUD NUNOEL
!     ZZPRNO(ILI,NUNOEL,2+1) = 1ER CODE
!     ZZPRNO(ILI,NUNOEL,2+NEC) = NEC IEME CODE
!
!      IZZPRN(ILI,NUNOEL,L) = (IDPRN1-1+ZI(IDPRN2+ILI-1)+
!     &                       (NUNOEL-1)* (NEC+2)+L-1)
#define zzprno(ili,nunoel,l) prn1(zi(idprn2+ili-1)+ \
    (nunoel-1)*(nec+2)+l-1)
!
#define izzpr2(ili,nunoel,l) (idpr21-1+zi(idpr22+ili-1)+ \
    (nunoel-1)*(nec+2)+l-1)
#define zzprn2(ili,nunoel,l) zi(idpr21-1+zi(idpr22+ili-1)+ \
    (nunoel-1)*(nec+2)+l-1)
!
!---- NBRE DE GROUPES D'ELEMENTS (DE LIEL) DU LIGREL ILI
!
#define zzngel(ili) adli(1+3*(ili-1))
!
!---- NBRE D ELEMENTS DU LIEL IGREL DU LIGREL ILI DU REPERTOIRE TEMP.
!     .MATAS.LILI(DIM DU VECTEUR D'ENTIERS .LILI(ILI).LIEL(IGREL) )
!
#define zznelg(ili,igrel) zi(adli(1+3*(ili-1)+2)+igrel)- \
    zi(adli(1+3*(ili-1)+2)+igrel-1)-1
!
!---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS LIEL DES S.D. LIGREL
!     REPERTORIEES DANS LE REPERTOIRE TEMPORAIRE .MATAS.LILI
!     ZZLIEL(ILI,IGREL,J) =
!      SI LA JIEME MAILLE DU LIEL IGREL DU LIGREL ILI EST:
!          -UNE MAILLE DU MAILLAGE : SON NUMERO DANS LE MAILLAGE
!          -UNE MAILLE TARDIVE : -POINTEUR DANS LE CHAMP .NEMA
!
#define zzliel(ili,igrel,j) zi(adli(1+3*(ili-1)+1)-1+ \
    zi(adli(1+3*(ili-1)+2)+igrel-1)+j-1)
!
!---- NBRE DE NOEUDS DE LA MAILLE TARDIVE IEL ( .NEMA(IEL))
!     DU LIGREL ILI REPERTOIRE .LILI
!     (DIM DU VECTEUR D'ENTIERS .LILI(ILI).NEMA(IEL) )
!
#define zznsup(ili,iel) zi(adne(1+3*(ili-1)+2)+iel)- \
    zi(adne(1+3*(ili-1)+2)+iel-1)-1
!
!---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS NEMA DES S.D. LIGREL
!     REPERTORIEES DANS LE REPERTOIRE TEMPO. .MATAS.LILI
!
#define zznema(ili,iel,j) zi(adne(1+3*(ili-1)+1)-1+ \
    zi(adne(1+3*(ili-1)+2)+iel-1)+j-1)
!
!----------------------------------------------------------------------
!
    call jemarq()
!
!---- SUPPRESSION DU .NUML
    call detrsd('NUML_DDL', nu)
!
!---- RECHERCHE DU MAILLAGE ET DU NOMBRE DE MAILLES ET DE NOEUDS
    call dismoi('NOM_MAILLA', nu, 'NUME_DDL', repk=noma)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbnoma)
!
!---- ON VERIFIE QU'IL N'Y A PAS DE SUPER-MAILLES
    call jeveuo(noma//'.DIME', 'L', vi=dime)
    ASSERT(dime(4).eq.0)
!
!---- ON RAMENE EN MEMOIRE LES OBJETS DU .NUME :
!     CALCUL DE NEQG, NLILI
    call jeveuo(nu//'     .ADNE', 'L', vi=adne)
    call jeveuo(nu//'     .ADLI', 'L', vi=adli)
    call jeveuo(nu//'.NUME.DELG', 'L', vi=delg)
    call jeveuo(nu//'.NUME.PRNO', 'L', vi=prn1)
    call jeveuo(jexatr(nu//'.NUME.PRNO', 'LONCUM'), 'L', idprn2)
    call jelira(nu//'.NUME.PRNO', 'NMAXOC', nlili)
    call jeveuo(nu//'.NUME.NEQU', 'L', j1)
    neqg=zi(j1)
!
!---- ON CREE LE TABLEAU &&NUGLLO.TAB_NO DONT LE ROLE EST DE SE SOUVENIR
!     SI UN NOEUD DU MAILLAGE A DEJA ETE TRAITE (ECONOMIE DE CPU)
    AS_ALLOCATE(vi=tab_no, size=nbnoma)
!
!---- CREATION DU TABLEAU &&NUGLLO.TAB_EQ QUI SERVIRA A CREER LE .NUEQ
    AS_ALLOCATE(vi=tab_eq, size=neqg)
    call wkvect(nu//'.NUML.NUGL', base(1:1)//' V I', neqg, jnugl)
!
!---- RECHERCHE DU TABLEAU PARTITION
    call dismoi('NOM_MODELE', nu, 'NUME_DDL', repk=mo)
    call dismoi('NOM_LIGREL', mo, 'MODELE', repk=ligrmo)
    call dismoi('PARTITION', ligrmo, 'LIGREL', repk=partit)
    ldist=.false.
    ldgrel=.false.
    call asmpi_info(rank=mrank, size=msize)
    rang = to_aster_int(mrank)
    nbproc = to_aster_int(msize)
    if (partit .ne. ' ') then
        ASSERT(nbproc.gt.1)
        ldist=.true.
        call jeveuo(partit//'.PRTK', 'L', vk24=prtk)
        ldgrel=prtk(1) .eq. 'GROUP_ELEM'
        if (.not.ldgrel) then
            call jeveuo(partit//'.NUPROC.MAILLE', 'L', vi=maille)
        endif
    endif
    ASSERT(ldist)
!
!---- ALLOCATION DU PRNO DE NUML :
    call jecrec(nu//'.NUML.PRNO', base(1:1)//' V I ', 'NU', 'CONTIG', 'VARIABLE',&
                nlili)
    do ili = 1, nlili
        call jelira(jexnum(nu//'.NUME.PRNO', ili), 'LONMAX', n1)
        call jeecra(jexnum(nu//'.NUML.PRNO', ili), 'LONMAX', n1)
!       -- CALCUL DU NOMBRE D'ENTIERS CODES :
        if (ili .eq. 1) nec=n1/nbnoma-2
    end do
!
!---- LECTURE DE LA CONNECTIVITE
    call jeveuo(noma//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
!---- RECHERCHE DES ADRESSES DU .PRNO DE .NUML
    call jeveuo(nu//'.NUML.PRNO', 'E', idpr21)
    call jeveuo(jexatr(nu//'.NUML.PRNO', 'LONCUM'), 'L', idpr22)
!
    AS_ALLOCATE(vi=ddl_pres, size=neqg)
    AS_ALLOCATE(vi=delg_tmp, size=neqg)
!
!
!---- REMPLISSAGE DU .PRNO ET DU TABLEAU &&NUGLLO.TAB_EQ
!     QUI SERVIRA A CREER LE .NUEQ
!     --------------------------------------------------------------
    numinc=1
    do ili = 2, nlili
        call jenuno(jexnum(nu//'.NUME.LILI', ili), nomlig)
        if (ili .eq. 2) then
            ASSERT(nomlig.eq.ligrmo)
        endif
        do igr = 1, zzngel(ili)
            if (ldgrel .and. mod(igr,nbproc) .ne. rang) goto 90
            nel=zznelg(ili,igr)
            do iel = 1, nel
                numa=zzliel(ili,igr,iel)
                ASSERT(numa.ne.0)
                if (.not.ldgrel) then
                    if (numa .gt. 0) then
                        if (maille(numa) .ne. rang) goto 80
                    else
                        if (rang .ne. 0) goto 80
                    endif
                endif
!
                if (numa .gt. 0) then
!             -- MAILLE DU MAILLAGE :
                    nbno=zi(jconx2+numa)-zi(jconx2+numa-1)
                    do ino = 1, nbno
                        nuno=connex(zi(jconx2+numa-1)+ino-1)
                        if (tab_no(nuno) .eq. 1) goto 40
!
                        ddl1g=zzprno(1,nuno,1)
                        nddl=zzprno(1,nuno,2)
!
                        zi(izzpr2(1,nuno,1))=numinc
                        zi(izzpr2(1,nuno,2))=nddl
                        do numec = 1, nec
                            zi(izzpr2(1,nuno,2+numec))=zzprno(1,nuno,2+numec)
                        end do
!
                        do iddl = 1, nddl
                            ddl_pres(1+ddl1g+iddl-2)=1
                            tab_eq(1+numinc-1+iddl-1)=ddl1g+iddl-1
                        end do
                        numinc=numinc+nddl
                        tab_no(nuno)=1
 40                     continue
                    end do
!
                else
!             -- MAILLE TARDIVE :
                    numa=-numa
                    nbno=zznsup(ili,numa)
                    do k1 = 1, nbno
                        nuno=zznema(ili,numa,k1)
                        if (nuno .lt. 0) then
                            nuno=-nuno
                            ilib=ili
                        else
                            if (tab_no(nuno) .eq. 1) goto 70
                            ilib=1
                            tab_no(nuno)=1
                        endif
                        ddl1g=zzprno(ilib,nuno,1)
                        if (ddl_pres(ddl1g) .eq. 1) goto 70
                        zi(izzpr2(ilib,nuno,1))=numinc
                        nddl=zzprno(ilib,nuno,2)
                        zi(izzpr2(ilib,nuno,2))=nddl
                        do numec = 1, nec
                            zi(izzpr2(ilib,nuno,2+numec))=zzprno(ilib,nuno, 2+numec)
                        end do
                        do iddl = 1, nddl
                            ddl_pres(1+ddl1g+iddl-2)=1
                            tab_eq(1+numinc-1+iddl-1)=ddl1g+iddl-1
                            delg_tmp(1+numinc-1+iddl-1)=delg(1+&
                            ddl1g-1+iddl-1)
                        end do
                        numinc=numinc+nddl
 70                     continue
                    end do
                endif
 80             continue
            end do
 90         continue
        end do
    end do
    neql=numinc-1
!
!
!---- ON VERIFIE QUE CHAQUE PROC A AU MOINS UN DDL
    if (neql .eq. 0) then
        vali(1)=rang
        call utmess('F', 'ASSEMBLA_4', si=vali(1))
    endif
!
!---- CREATION DU .NUML.DELG
    call wkvect(nu//'.NUML.DELG', base(1:1)//' V I', neql, jdelgl)
    do j = 1, neql
        zi(jdelgl-1+j)=delg_tmp(j)
    end do
!
!---- CREATION DU .NUML.NEQU
    call wkvect(nu//'.NUML.NEQU', base(1:1)//' V I', 2, j1)
    zi(j1)=neql
!
!---- CREATION DU .NUML.NULG ET DU .NUML.NUEQ
    call wkvect(nu//'.NUML.NULG', base(1:1)//' V I', neql, jnulg)
    call wkvect(nu//'.NUML.NUEQ', base(1:1)//' V I', neql, jnueql)
!
    do ili = 1, nlili
        call jelira(jexnum(nu//'.NUML.PRNO', ili), 'LONMAX', ntot)
        ntot=ntot/(nec+2)
!
        do ino = 1, ntot
            ddl1l=zzprn2(ili,ino,1)
            nddl=zzprn2(ili,ino,2)
            do iddl = 1, nddl
                ieqg=tab_eq(1+ddl1l-1+iddl-1)
                ASSERT(ieqg.gt.0)
!
                zi(jnulg+ddl1l-1+iddl-1)=ieqg
                zi(jnueql+ddl1l-1+iddl-1)=ddl1l+iddl-1
                zi(jnugl+ieqg-1)=ddl1l-1+iddl
            end do
        end do
    end do
!
    call jeveuo(solveu//'.SLVK', 'L', vk24=slvk)
!     POUR PETSC ON A BESOIN D'INFORMATIONS SUPPLEMENTAIRES
    if (slvk(1) .eq. 'PETSC') call nupodd(nu, base, rang, nbproc)
!
    AS_DEALLOCATE(vi=tab_no)
    AS_DEALLOCATE(vi=tab_eq)
    AS_DEALLOCATE(vi=delg_tmp)
    AS_DEALLOCATE(vi=ddl_pres)
!
    call jedema()
!
end subroutine
