subroutine cynugl(prof_chno, indirf, modcyc, mailsk)
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!***********************************************************************
!    P. RICHARD     DATE 10/02/92
!-----------------------------------------------------------------------
!  BUT: < CYCLIQUE NUMEROTATION GLOBALE >
!
!    CREER A PARTIR D'UN RESULTAT CYCLIQUE ET UN MAILLAGE GLOBAL
!  SQUELLETTE LE PROFIL CHAMNO ET UNE FAMILLE NUMEROTEE
! ,DONT CHAQUE OBJET CORRESPOND A UN SECTEUR, ET EST DIMENSIONNE
!  A 2*NBDDL ENGENDRE PAR LE SECTEUR:C
!
!            2*(I-1)+1 --> NUMERO EQUATION DANS NUMDDL SECTEUR
!            2*(I-1)+2 --> NUMERO EQUATION DANS PROFNO GLOBAL
!
!-----------------------------------------------------------------------
!
! NOM----- / /:
!
! PROF_CHNO   /I/: NOM K19 DU PROF_CHNO A CREER
! INDIRF   /I/: NOM K24 DE LA FAMILLE DES INDIRECTIONS A CREER
! MODCYC   /I/: NOM DU RESULTAT CYCLIQUE EN AMONT
! MAILSK   /I/: NOM DU MAILLAGE SKELETTE
!
!
!
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/isdeco.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/profchno_crsd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
!
!
!-----------------------------------------------------------------------
    integer :: i, iad, icomp, iec, ieq, ipoint, i_ligr_mesh, i_ligr_link
    integer :: j, lddeeq, ldnueq, ldprno, linueq
    integer ::   llprno,  ltinse, lttds
    integer :: nbcmp, nbcpmx, nbddl, nbec, nbnot, nbsec, nddlt
    integer :: neqsec, nsecpr, ntail, numnos, numsec
!-----------------------------------------------------------------------
    parameter    (nbcpmx=300)
    character(len=6) :: pgc
    character(len=8) :: modcyc, mailsk, basmod, intf
    character(len=19) :: numddl, prof_chno, prnosect
    character(len=24) :: indirf, lili, prno, deeq, nueq
    integer :: idec(nbcpmx)
    character(len=24), pointer :: cycl_refe(:) => null()
    integer, pointer :: skeleton(:) => null()
    integer, pointer :: cycl_nbsc(:) => null()
    integer, pointer :: vnueq(:) => null()
!
!-----------------------------------------------------------------------
!
    call jemarq()
    pgc='CYNUGL'
!
!
!-------------------RECUPERATION DE LA BASE MODALE----------------------
!
    call jeveuo(modcyc//'.CYCL_REFE', 'L', vk24=cycl_refe)
    basmod=cycl_refe(3)(1:8)
    call jelibe(modcyc//'.CYCL_REFE')
!
!----------------RECUPERATION DU NUMDDL ET DE L'INTERF_DYNA--------------
!
!
!
!
    call dismoi('REF_INTD_PREM', basmod, 'RESU_DYNA', repk=intf)
    call dismoi('NUME_DDL', basmod, 'RESU_DYNA', repk=numddl)
!
!
!---------------RECUPERATION DU NOMBRE DE COMPOSANTES-------------------
!
    call dismoi('NB_CMP_MAX', intf, 'INTERF_DYNA', repi=nbcmp)
    call dismoi('NB_EC', intf, 'INTERF_DYNA', repi=nbec)
    ASSERT(nbec.le.10)
!
!---------------RECUPERATION DU NOMBRE DE SECTEURS----------------------
!
!
    call jeveuo(modcyc//'.CYCL_NBSC', 'L', vi=cycl_nbsc)
    nbsec=cycl_nbsc(1)
    call jelibe(modcyc//'.CYCL_NBSC')
!
!-------------RECUPERATION DIMENSION MAILLAGE SQUELETTE-----------------
!
    call dismoi('NB_NO_MAILLA', mailsk, 'MAILLAGE', repi=nbnot)
!
!------------RECUPERATION DU .INV.SKELETON------------------------------
!
    call jeveuo(mailsk//'.INV.SKELETON', 'L', vi=skeleton)
!
!--------------RECUPERATION DU PRNO DU SECTEUR--------------------------
!
    prnosect = numddl(1:14)//'.NUME'
    call jenonu(jexnom(prnosect//'.LILI', '&MAILLA'), i_ligr_mesh)
    call jeveuo(jexnum(prnosect//'.PRNO', i_ligr_mesh), 'L', llprno)
    call jeveuo(prnosect//'.NUEQ', 'L', vi=vnueq)
    call dismoi('NB_EQUA', numddl, 'NUME_DDL', repi=neqsec)
!
!--------------------ALLOCATION DU VECTEUR DE TRAVAIL-------------------
!     POUR STOCKAGE NOMBRE DE DDL GLOBAUX ENGENDRE PAR SECTEUR
!
    call wkvect('&&'//pgc//'.TAIL.DDL.SECT', 'V V IS', nbsec, lttds)
!
!--------------BOUCLE DE COMPTAGE DES DDL FINAUX------------------------
!
    nddlt=0
    do i = 1, nbnot
        numsec=skeleton(i)
        numnos=skeleton(1+nbnot+i-1)
        nddlt=nddlt+zi(llprno+(numnos-1)*(2+nbec)+1)
        zi(lttds+numsec-1)=zi(lttds+numsec-1)+ zi(llprno+(numnos-1)*(&
        2+nbec)+1)
    end do
!
!-----------------ALLOCATION DES DIVERS OBJETS--------------------------
!
    lili=prof_chno//'.LILI'
    prno=prof_chno//'.PRNO'
    deeq=prof_chno//'.DEEQ'
    nueq=prof_chno//'.NUEQ'
!
! - Create PROF_CHNO
!
    call profchno_crsd(prof_chno, 'G', nb_equa = nddlt, nb_ligrz = 2,&
                       prno_lengthz = nbnot*(2+nbec))
    call jeveuo(deeq, 'E', lddeeq)
    call jeveuo(nueq, 'E', ldnueq)
!
! - Create object LIAISON
!
    call jecroc(jexnom(lili, 'LIAISONS'))
    call jenonu(jexnom(lili, 'LIAISONS'), i_ligr_link)
    call jeecra(jexnum(prno, i_ligr_link), 'LONMAX', 1)

!
!
    call jecrec(indirf, 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nbsec)
!
!
    do i = 1, nbsec
        call jecroc(jexnum(indirf, i))
        ntail=2*zi(lttds+i-1)
        call jeecra(jexnum(indirf, i), 'LONMAX', ntail)
        zi(lttds+i-1)=0
    end do
!
!-------------------------REMPLISSAGE DES OBJETS------------------------
!  SUPPOSE QUE LES NOEUDS GLOBAUX SONT RANGES PAR SOUS-STRUCTURES
!
!
    call jenonu(jexnom(lili, '&MAILLA'), i_ligr_mesh)
    call jeveuo(jexnum(prno, i_ligr_mesh), 'E', ldprno)
!
    nsecpr=1
    call jeveuo(jexnum(indirf, nsecpr), 'E', ltinse)
    icomp=0
    do i = 1, nbnot
!
        numsec=skeleton(i)
        numnos=skeleton(1+nbnot+i-1)
        ieq=zi(llprno+(numnos-1)*(2+nbec))
        nbddl=zi(llprno+(numnos-1)*(2+nbec)+1)
        call isdeco(zi(llprno+(numnos-1)*(2+nbec)+2), idec, nbcmp)
!
        zi(ldprno+(i-1)*(2+nbec))=icomp+1
        zi(ldprno+(i-1)*(2+nbec)+1)=nbddl
        do iec = 1, nbec
            zi(ldprno+(i-1)*(2+nbec)+1+iec)= zi(llprno+(numnos-1)*(2+&
            nbec)+1+iec)
        end do
        if (numsec .ne. nsecpr) then
            call jelibe(jexnum(indirf, nsecpr))
            nsecpr=numsec
            call jeveuo(jexnum(indirf, nsecpr), 'E', ltinse)
        endif
        iad=0
        do j = 1, nbcmp
            if (idec(j) .gt. 0) then
                iad=iad+1
                icomp=icomp+1
                zi(lddeeq+(icomp-1)*2)=i
                zi(lddeeq+(icomp-1)*2+1)=j
                zi(ldnueq+icomp-1)=icomp
                linueq=vnueq(1+ieq+iad-2)
                ipoint=ltinse-1+2*zi(lttds+numsec-1)
                zi(ipoint+1)=linueq
                zi(ipoint+2)=icomp
                zi(lttds+numsec-1)=zi(lttds+numsec-1)+1
            endif
        end do
    end do
!
    call jelibe(jexnum(indirf, nsecpr))
!
!------------------SAUVEGARDE DES OBJETS--------------------------------
!
    call jedetr('&&'//pgc//'.TAIL.DDL.SECT')
!
    call jedema()
end subroutine
