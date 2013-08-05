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
#include "jeveux.h"
!
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
#include "asterfort/mpicm0.h"
#include "asterfort/nupodd.h"
#include "asterfort/u2mesi.h"
#include "asterfort/wkvect.h"
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
    integer :: nbma, jconx1, jconx2, ier, ibid, jdime
    integer :: jnumsd, rang, numa, nbnoma, nbno, ino, nuno
    integer :: nec, nlili, neql, idprn1, idprn2, ili, ntot
    integer :: idpr21, idpr22, numinc, numec, nddl
    integer :: jtano, neqg, jtaeq, iddl, jnulg, jslvk
    integer :: jnueql, ddl1g, ddl1l, jdelgg, jdelgl, j1
    integer :: jadne, jadli, igrel, iel, igr, nel, k1, n1, j, ilib
    integer :: jdelgt, jddlp, nbproc, vali(1), jnugl, ieqg
!
    character(len=8) :: k8b, noma, partit, mo
    character(len=19) :: ligrmo, nomlig
    character(len=24) :: kbid
!----------------------------------------------------------------------
    integer :: zzprno, izzpr2, zzprn2, nunoel, l
    integer :: zzngel, zznelg, zzliel, zznsup, zznema, jprtk
    logical :: ldgrel, ldist
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
    zzprno(ili,nunoel,l)=zi(idprn1-1+zi(idprn2+ili-1)+&
     &                     (nunoel-1)*(nec+2)+l-1)
!
    izzpr2(ili,nunoel,l)=(idpr21-1+zi(idpr22+ili-1)+&
     &                     (nunoel-1)*(nec+2)+l-1)
    zzprn2(ili,nunoel,l)=zi(idpr21-1+zi(idpr22+ili-1)+&
     &                     (nunoel-1)*(nec+2)+l-1)
!
!---- NBRE DE GROUPES D'ELEMENTS (DE LIEL) DU LIGREL ILI
!
    zzngel(ili)=zi(jadli+3*(ili-1))
!
!---- NBRE D ELEMENTS DU LIEL IGREL DU LIGREL ILI DU REPERTOIRE TEMP.
!     .MATAS.LILI(DIM DU VECTEUR D'ENTIERS .LILI(ILI).LIEL(IGREL) )
!
    zznelg(ili,igrel)=zi(zi(jadli+3*(ili-1)+2)+igrel)-&
     &                  zi(zi(jadli+3*(ili-1)+2)+igrel-1)-1
!
!---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS LIEL DES S.D. LIGREL
!     REPERTORIEES DANS LE REPERTOIRE TEMPORAIRE .MATAS.LILI
!     ZZLIEL(ILI,IGREL,J) =
!      SI LA JIEME MAILLE DU LIEL IGREL DU LIGREL ILI EST:
!          -UNE MAILLE DU MAILLAGE : SON NUMERO DANS LE MAILLAGE
!          -UNE MAILLE TARDIVE : -POINTEUR DANS LE CHAMP .NEMA
!
    zzliel(ili,igrel,j)=zi(zi(jadli+3*(ili-1)+1)-1+&
     &                    zi(zi(jadli+3*(ili-1)+2)+igrel-1)+j-1)
!
!---- NBRE DE NOEUDS DE LA MAILLE TARDIVE IEL ( .NEMA(IEL))
!     DU LIGREL ILI REPERTOIRE .LILI
!     (DIM DU VECTEUR D'ENTIERS .LILI(ILI).NEMA(IEL) )
!
    zznsup(ili,iel)=zi(zi(jadne+3*(ili-1)+2)+iel)-&
     &                zi(zi(jadne+3*(ili-1)+2)+iel-1)-1
!
!---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS NEMA DES S.D. LIGREL
!     REPERTORIEES DANS LE REPERTOIRE TEMPO. .MATAS.LILI
!
    zznema(ili,iel,j)=zi(zi(jadne+3*(ili-1)+1)-1+&
     &                  zi(zi(jadne+3*(ili-1)+2)+iel-1)+j-1)
!
!----------------------------------------------------------------------
!
    call jemarq()
!
!---- SUPPRESSION DU .NUML
    call detrsd('NUML_DDL', nu)
!
!---- RECHERCHE DU MAILLAGE ET DU NOMBRE DE MAILLES ET DE NOEUDS
    call dismoi('F', 'NOM_MAILLA', nu, 'NUME_DDL', ibid,&
                noma, ier)
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbma,&
                k8b, ier)
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnoma,&
                k8b, ier)
!
!---- ON VERIFIE QU'IL N'Y A PAS DE SUPER-MAILLES
    call jeveuo(noma//'.DIME', 'L', jdime)
    ASSERT(zi(jdime+3).eq.0)
!
!---- ON RAMENE EN MEMOIRE LES OBJETS DU .NUME :
!     CALCUL DE NEQG, NLILI
    call jeveuo(nu//'     .ADNE', 'L', jadne)
    call jeveuo(nu//'     .ADLI', 'L', jadli)
    call jeveuo(nu//'.NUME.DELG', 'L', jdelgg)
    call jeveuo(nu//'.NUME.PRNO', 'L', idprn1)
    call jeveuo(jexatr(nu//'.NUME.PRNO', 'LONCUM'), 'L', idprn2)
    call jelira(nu//'.NUME.PRNO', 'NMAXOC', nlili, kbid)
    call jeveuo(nu//'.NUME.NEQU', 'L', j1)
    neqg=zi(j1)
!
!---- ON CREE LE TABLEAU &&NUGLLO.TAB_NO DONT LE ROLE EST DE SE SOUVENIR
!     SI UN NOEUD DU MAILLAGE A DEJA ETE TRAITE (ECONOMIE DE CPU)
    call wkvect('&&NUGLLO.TAB_NO', 'V V I', nbnoma, jtano)
!
!---- CREATION DU TABLEAU &&NUGLLO.TAB_EQ QUI SERVIRA A CREER LE .NUEQ
    call wkvect('&&NUGLLO.TAB_EQ', 'V V I', neqg, jtaeq)
    call wkvect(nu//'.NUML.NUGL', base(1:1)//' V I', neqg, jnugl)
!
!---- RECHERCHE DU TABLEAU PARTITION
    call dismoi('F', 'NOM_MODELE', nu, 'NUME_DDL', ibid,&
                mo, ier)
    call dismoi('F', 'NOM_LIGREL', mo, 'MODELE', ibid,&
                ligrmo, ier)
    call dismoi('F', 'PARTITION', ligrmo, 'LIGREL', ibid,&
                partit, ier)
    ldist=.false.
    ldgrel=.false.
    call mpicm0(rang, nbproc)
    if (partit .ne. ' ') then
        ASSERT(nbproc.gt.1)
        ldist=.true.
        call jeveuo(partit//'.PRTK', 'L', jprtk)
        ldgrel=zk24(jprtk-1+1) .eq. 'GROUP_ELEM'
        if (.not.ldgrel) then
            call jeveuo(partit//'.NUPROC.MAILLE', 'L', jnumsd)
        endif
    endif
    ASSERT(ldist)
!
!---- ALLOCATION DU PRNO DE NUML :
    call jecrec(nu//'.NUML.PRNO', base(1:1)//' V I ', 'NU', 'CONTIG', 'VARIABLE',&
                nlili)
    do 10 ili = 1, nlili
        call jelira(jexnum(nu//'.NUME.PRNO', ili), 'LONMAX', n1, kbid)
        call jeecra(jexnum(nu//'.NUML.PRNO', ili), 'LONMAX', n1, kbid)
!       -- CALCUL DU NOMBRE D'ENTIERS CODES :
        if (ili .eq. 1) nec=n1/nbnoma-2
10  end do
!
!---- LECTURE DE LA CONNECTIVITE
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
!---- RECHERCHE DES ADRESSES DU .PRNO DE .NUML
    call jeveuo(nu//'.NUML.PRNO', 'E', idpr21)
    call jeveuo(jexatr(nu//'.NUML.PRNO', 'LONCUM'), 'L', idpr22)
!
    call wkvect('&&NUGLLO.DDL_PRES', 'V V I', neqg, jddlp)
    call wkvect('&&NUGLLO.DELG_TMP', 'V V I', neqg, jdelgt)
!
!
!---- REMPLISSAGE DU .PRNO ET DU TABLEAU &&NUGLLO.TAB_EQ
!     QUI SERVIRA A CREER LE .NUEQ
!     --------------------------------------------------------------
    numinc=1
    do 100 ili = 2, nlili
        call jenuno(jexnum(nu//'.NUME.LILI', ili), nomlig)
        if (ili .eq. 2) ASSERT(nomlig.eq.ligrmo)
        do 90 igr = 1, zzngel(ili)
            if (ldgrel .and. mod(igr,nbproc) .ne. rang) goto 90
            nel=zznelg(ili,igr)
            do 80 iel = 1, nel
                numa=zzliel(ili,igr,iel)
                ASSERT(numa.ne.0)
                if (.not.ldgrel) then
                    if (numa .gt. 0) then
                        if (zi(jnumsd-1+numa) .ne. rang) goto 80
                    else
                        if (rang .ne. 0) goto 80
                    endif
                endif
!
                if (numa .gt. 0) then
!             -- MAILLE DU MAILLAGE :
                    nbno=zi(jconx2+numa)-zi(jconx2+numa-1)
                    do 40 ino = 1, nbno
                        nuno=zi(jconx1-1+zi(jconx2+numa-1)+ino-1)
                        if (zi(jtano+nuno-1) .eq. 1) goto 40
!
                        ddl1g=zzprno(1,nuno,1)
                        nddl=zzprno(1,nuno,2)
!
                        zi(izzpr2(1,nuno,1))=numinc
                        zi(izzpr2(1,nuno,2))=nddl
                        do 20 numec = 1, nec
                            zi(izzpr2(1,nuno,2+numec))=zzprno(1,nuno,&
                            2+numec)
20                      continue
!
                        do 30 iddl = 1, nddl
                            zi(jddlp+ddl1g+iddl-1)=1
                            zi(jtaeq+numinc-1+iddl-1)=ddl1g+iddl-1
30                      continue
                        numinc=numinc+nddl
                        zi(jtano+nuno-1)=1
40                  continue
!
                else
!             -- MAILLE TARDIVE :
                    numa=-numa
                    nbno=zznsup(ili,numa)
                    do 70 k1 = 1, nbno
                        nuno=zznema(ili,numa,k1)
                        if (nuno .lt. 0) then
                            nuno=-nuno
                            ilib=ili
                        else
                            if (zi(jtano+nuno-1) .eq. 1) goto 70
                            ilib=1
                            zi(jtano+nuno-1)=1
                        endif
                        ddl1g=zzprno(ilib,nuno,1)
                        if (zi(jddlp+ddl1g) .eq. 1) goto 70
                        zi(izzpr2(ilib,nuno,1))=numinc
                        nddl=zzprno(ilib,nuno,2)
                        zi(izzpr2(ilib,nuno,2))=nddl
                        do 50 numec = 1, nec
                            zi(izzpr2(ilib,nuno,2+numec))=zzprno(ilib,&
                            nuno, 2+numec)
50                      continue
                        do 60 iddl = 1, nddl
                            zi(jddlp+ddl1g+iddl-1)=1
                            zi(jtaeq+numinc-1+iddl-1)=ddl1g+iddl-1
                            zi(jdelgt+numinc-1+iddl-1)=zi(jdelgg+&
                            ddl1g-1+iddl-1)
60                      continue
                        numinc=numinc+nddl
70                  continue
                endif
80          continue
90      continue
100  end do
    neql=numinc-1
!
!
!---- ON VERIFIE QUE CHAQUE PROC A AU MOINS UN DDL
    if (neql .eq. 0) then
        vali(1)=rang
        call u2mesi('F', 'ASSEMBLA_4', 1, vali)
    endif
!
!---- CREATION DU .NUML.DELG
    call wkvect(nu//'.NUML.DELG', base(1:1)//' V I', neql, jdelgl)
    do 110 j = 1, neql
        zi(jdelgl-1+j)=zi(jdelgt-1+j)
110  end do
!
!---- CREATION DU .NUML.NEQU
    call wkvect(nu//'.NUML.NEQU', base(1:1)//' V I', 2, j1)
    zi(j1)=neql
!
!---- CREATION DU .NUML.NULG ET DU .NUML.NUEQ
    call wkvect(nu//'.NUML.NULG', base(1:1)//' V I', neql, jnulg)
    call wkvect(nu//'.NUML.NUEQ', base(1:1)//' V I', neql, jnueql)
!
    do 140 ili = 1, nlili
        call jelira(jexnum(nu//'.NUML.PRNO', ili), 'LONMAX', ntot, kbid)
        ntot=ntot/(nec+2)
!
        do 130 ino = 1, ntot
            ddl1l=zzprn2(ili,ino,1)
            nddl=zzprn2(ili,ino,2)
            do 120 iddl = 1, nddl
                ieqg=zi(jtaeq+ddl1l-1+iddl-1)
                ASSERT(ieqg.gt.0)
!
                zi(jnulg+ddl1l-1+iddl-1)=ieqg
                zi(jnueql+ddl1l-1+iddl-1)=ddl1l+iddl-1
                zi(jnugl+ieqg-1)=ddl1l-1+iddl
120          continue
130      continue
140  end do
!
    call jeveuo(solveu//'.SLVK', 'L', jslvk)
!     POUR PETSC ON A BESOIN D'INFORMATIONS SUPPLEMENTAIRES
    if (zk24(jslvk) .eq. 'PETSC') call nupodd(nu, base, rang, nbproc)
!
    call jedetr('&&NUGLLO.TAB_NO')
    call jedetr('&&NUGLLO.TAB_EQ')
    call jedetr('&&NUGLLO.DELG_TMP')
    call jedetr('&&NUGLLO.DDL_PRES')
!
    call jedema()
!
end subroutine
