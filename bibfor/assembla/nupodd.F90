subroutine nupodd(nu, base, rang, nbproc)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/nugrco.h"
#include "asterfort/nurenu.h"
#include "asterfort/wkvect.h"
    character(len=14) :: nu
    character(len=2) :: base
    integer :: rang, nbproc
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
! ----------------------------------------------------------------------
!  NUME_DDL - CREATION DES TABLEAUX DE POSSESSION DES DDL
!  --                                  --             --
! ----------------------------------------------------------------------
!
!   CETTE ROUTINE CREE LE VECTEUR .NUML.PDDL QUI PRECISE SI UN
!    DDL LOCAL EST POSSEDE PAR LE PROC COURANT
!   RAPPEL : PAR CONVENTION, QUAND UN DDL EST PARTAGE ENTRE DEUX
!            PROCESSEURS, C'EST LE PROCESSEUR DE PLUS BAS NIVEAU
!            QUI POSSEDERA CE DDL
!
! IN  :
!   NU      K14  NOM DU NUME_DDL
!   BASE    K2   BASE(1:1) : BASE POUR CREER LE NUME_DDL
!                    (SAUF LE PROF_CHNO)
!                BASE(2:2) : BASE POUR CREER LE PROF_CHNO
!
    integer :: nbma, nbnoma, jnumsd
    integer :: nlili, ili, igr, nel, iel, numa, jpddl, nbno, ino
    integer :: nuno, iddl, nddl, ddl1g, numpro, curpro, k1, n1
    integer :: ddl1l, ilib, neql, jconx2, idprn2
    integer :: nec
!
    character(len=8) :: noma, mo, partit
    character(len=19) :: ligrmo, nomlig
!
    aster_logical :: ldist, ldgrel
    integer, pointer :: adne(:) => null()
    character(len=24), pointer :: prtk(:) => null()
    integer, pointer :: connex(:) => null()
    integer, pointer :: nugl(:) => null()
    integer, pointer :: adli(:) => null()
    integer, pointer :: prno(:) => null()
    integer, pointer :: nequ(:) => null()
!----------------------------------------------------------------------
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
#define zzprno(ili,nunoel,l) prno(zi(idprn2+ili-1)+ \
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
    call jemarq()
!
!---- RECHERCHE DU MAILLAGE ET DU NOMBRE DE MAILLES ET DE NOEUDS
    call dismoi('NOM_MAILLA', nu, 'NUME_DDL', repk=noma)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbnoma)
!
!---- ON RAMENE EN MEMOIRE LES OBJETS DU .NUME :
!     CALCUL DE NEQG, NLILI
    call jeveuo(nu//'     .ADNE', 'L', vi=adne)
    call jeveuo(nu//'     .ADLI', 'L', vi=adli)
    call jeveuo(nu//'.NUME.PRNO', 'L', vi=prno)
    call jeveuo(jexatr(nu//'.NUME.PRNO', 'LONCUM'), 'L', idprn2)
    call jelira(nu//'.NUME.PRNO', 'NMAXOC', nlili)
    call jelira(jexnum(nu//'.NUME.PRNO', 1), 'LONMAX', n1)
!
!     -- CALCUL DU NOMBRE D'ENTIERS CODES :
    nec=n1/nbnoma-2
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
        ldgrel=prtk(1).eq.'GROUP_ELEM'
        if (ldgrel) then
            jnumsd=1
        else
            call jeveuo(partit//'.NUPROC.MAILLE', 'L', jnumsd)
        endif
    endif
    ASSERT(ldist)
!
!---- LECTURE DE LA CONNECTIVITE
    call jeveuo(noma//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
    call jeveuo(nu//'.NUML.NEQU', 'L', vi=nequ)
    neql=nequ(1)
    call wkvect(nu//'.NUML.PDDL', base(1:1)//' V I', neql, jpddl)
    call jeveuo(nu//'.NUML.NUGL', 'L', vi=nugl)
    do iddl = 0, neql-1
        zi(jpddl+iddl)=nbproc+1
    end do
!
!---- CREATION DU TABLEAU DE POSSESSION DES DDL LOCAUX
    do ili = 2, nlili
        call jenuno(jexnum(nu//'.NUME.LILI', ili), nomlig)
        if (ili .eq. 2) ASSERT(nomlig.eq.ligrmo)
        do igr = 1, zzngel(ili)
            nel=zznelg(ili,igr)
            do iel = 1, nel
                numa=zzliel(ili,igr,iel)
                ASSERT(numa.ne.0)
!
                if (numa .gt. 0) then
                    if (ldgrel) then
                        numpro=mod(igr,nbproc)
                    else
                        numpro=zi(jnumsd-1+numa)
                    endif
!             -- MAILLE DU MAILLAGE :
                    nbno=zi(jconx2+numa)-zi(jconx2+numa-1)
                    do ino = 1, nbno
                        nuno=connex(zi(jconx2+numa-1)+ino-1)
!
                        ddl1g=zzprno(1,nuno,1)
                        nddl=zzprno(1,nuno,2)
                        ddl1l=nugl(ddl1g)
                        if (ddl1l .eq. 0) goto 40
                        curpro=zi(jpddl+ddl1l-1)
!
                        if (numpro .lt. curpro) then
                            curpro=numpro
                        else
                            goto 40
                        endif
!
                        do iddl = 1, nddl
                            zi(jpddl+ddl1l+iddl-2)=curpro
                        end do
 40                     continue
                    end do
!
                else
                    if (ldgrel) then
                        numpro=mod(igr,nbproc)
                    else
                        numpro=0
                    endif
!             -- MAILLE TARDIVE :
                    numa=-numa
                    nbno=zznsup(ili,numa)
                    do k1 = 1, nbno
                        nuno=zznema(ili,numa,k1)
                        if (nuno .lt. 0) then
                            nuno=-nuno
                            ilib=ili
                        else
                            ilib=1
                        endif
                        ddl1g=zzprno(ilib,nuno,1)
                        nddl=zzprno(ilib,nuno,2)
                        ddl1l=nugl(ddl1g)
                        curpro=zi(jpddl+ddl1l-1)
!
                        if (numpro .lt. curpro) then
                            curpro=numpro
                        else
                            goto 70
                        endif
!
                        do iddl = 1, nddl
                            ddl1l=nugl(1+ddl1g+iddl-2)
                            if (ddl1l .eq. 0) goto 60
                            zi(jpddl+ddl1l-1)=curpro
 60                         continue
                        end do
 70                     continue
                    end do
                endif
            end do
        end do
    end do
!
!---- DETERMINATION DU GRAPH DE COMMUNICATION ET DES RACCORDS
    call nugrco(nu, base)
!
!---- CREATION D'UNE NUMEROTATION POUR PETSC
    call nurenu(nu, base)
!
    call jedema()
!
end subroutine
